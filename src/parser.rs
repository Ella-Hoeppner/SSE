use crate::{
  ast::Sexp,
  parse::Parse,
  syntax::{IdStr, Syntax},
  DocumentSyntaxTree, ParseError,
};
use std::fmt::Debug;
use take_mut::take;

#[derive(Debug, Clone)]
pub struct Parser<'t, S: Syntax> {
  pub(crate) text: &'t str,
  pub(crate) syntax: S,
  parsed_top_level_asts: Vec<DocumentSyntaxTree<S::E, S::O>>,
  top_level_lookahead: usize,
  already_parsed_index: usize,
}

impl<'t, S: Syntax> Parser<'t, S> {
  pub fn new(syntax: S, text: &'t str) -> Self {
    Self {
      text,
      top_level_lookahead: syntax.context(&syntax.root_context()).lookahead(),
      syntax,
      parsed_top_level_asts: vec![],
      already_parsed_index: 0,
    }
  }
  pub fn read_next(
    &mut self,
  ) -> Result<Option<DocumentSyntaxTree<S::E, S::O>>, ParseError> {
    while self.parsed_top_level_asts.len() <= self.top_level_lookahead {
      let mut stolen_top_level_asts = vec![];
      std::mem::swap(
        &mut stolen_top_level_asts,
        &mut self.parsed_top_level_asts,
      );
      match Parse::new(stolen_top_level_asts, &self.syntax, &self.text)
        .complete(self.already_parsed_index)?
      {
        Ok(new_top_level_asts) => {
          self.parsed_top_level_asts = new_top_level_asts;
        }
        Err(original_top_level_asts) => {
          self.parsed_top_level_asts = original_top_level_asts;
          break;
        }
      }
    }
    if self.parsed_top_level_asts.is_empty() {
      Ok(None)
    } else {
      self.already_parsed_index =
        self.parsed_top_level_asts.last().unwrap().position().end();
      let mut first_ast = None;
      take(&mut self.parsed_top_level_asts, |parsed_top_level_asts| {
        let mut iter = parsed_top_level_asts.into_iter();
        first_ast = iter.next();
        iter.collect()
      });
      Ok(first_ast)
    }
  }
  pub fn read_all(
    &mut self,
  ) -> Vec<Result<DocumentSyntaxTree<S::E, S::O>, ParseError>> {
    let mut results = vec![];
    loop {
      match self.read_next() {
        Ok(None) => break,
        Ok(Some(tagged_ast)) => results.push(Ok(tagged_ast)),
        Err(err) => {
          results.push(Err(err));
          break;
        }
      }
    }
    results
  }
}

impl<'t, S: Syntax> Parser<'t, S>
where
  S::E: IdStr,
  S::O: IdStr,
{
  pub fn read_next_as_sexp(&mut self) -> Result<Option<Sexp>, ParseError> {
    self.read_next().map(|maybe_tagged_ast| {
      maybe_tagged_ast.map(|tagged_ast| tagged_ast.into())
    })
  }
  pub fn read_all_as_sexps(&mut self) -> Vec<Result<Sexp, ParseError>> {
    self
      .read_all()
      .into_iter()
      .map(|result| result.map(|tagged_ast| tagged_ast.into()))
      .collect()
  }
}
