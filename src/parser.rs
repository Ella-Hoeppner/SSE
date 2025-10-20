use crate::{
  ContextId, DocumentSyntaxTree, ParseError,
  ast::Sexp,
  parse::Parse,
  syntax::{IdStr, Syntax},
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
  ) -> (Option<DocumentSyntaxTree<S::E, S::O>>, Vec<ParseError>) {
    let mut parse_errors = vec![];
    let last_err_pos_or =
      |errs: &Vec<ParseError>, previous_max: usize| -> usize {
        match errs.iter().map(|err| err.pos.end).max() {
          Some(err_end_index) => err_end_index.max(previous_max),
          None => previous_max,
        }
      };
    while self
      .parsed_top_level_asts
      .iter()
      .map(|ast| {
        if let DocumentSyntaxTree::Inner((_, encloser_or_operator), _) = ast
          && self
            .syntax
            .encloser_or_operator_context(encloser_or_operator)
            .map(|ctx| ctx.is_comment())
            .unwrap_or(false)
        {
          0
        } else {
          1
        }
      })
      .sum::<usize>()
      <= self.top_level_lookahead
    {
      let mut stolen_top_level_asts = vec![];
      std::mem::swap(
        &mut stolen_top_level_asts,
        &mut self.parsed_top_level_asts,
      );
      let (new_top_level_asts, finished, mut inner_errors) =
        Parse::new(stolen_top_level_asts, &self.syntax, self.text)
          .complete(self.already_parsed_index);
      self.already_parsed_index =
        last_err_pos_or(&inner_errors, self.already_parsed_index);
      parse_errors.append(&mut inner_errors);
      self.parsed_top_level_asts = new_top_level_asts;
      if finished {
        break;
      }
    }
    if self.parsed_top_level_asts.is_empty() {
      (None, parse_errors)
    } else {
      let ast_end_index =
        self.parsed_top_level_asts.last().unwrap().position().end();
      self.already_parsed_index = last_err_pos_or(&parse_errors, ast_end_index);
      let mut first_ast = None;
      take(&mut self.parsed_top_level_asts, |parsed_top_level_asts| {
        let mut iter = parsed_top_level_asts.into_iter();
        first_ast = iter.next();
        iter.collect()
      });
      (first_ast, parse_errors)
    }
  }
  pub fn read_all(
    &mut self,
  ) -> (Vec<DocumentSyntaxTree<S::E, S::O>>, Vec<ParseError>) {
    let mut trees = vec![];
    let mut errors = vec![];
    loop {
      let (tree, mut e) = self.read_next();
      errors.append(&mut e);
      let Some(tree) = tree else { break };
      trees.push(tree);
    }
    (trees, errors)
  }
}

impl<'t, S: Syntax> Parser<'t, S>
where
  S::E: IdStr,
  S::O: IdStr,
{
  pub fn read_next_as_sexp(&mut self) -> (Option<Sexp>, Vec<ParseError>) {
    let (tree, errors) = self.read_next();
    (tree.map(|tagged_ast| tagged_ast.into()), errors)
  }
  pub fn read_all_as_sexps(&mut self) -> (Vec<Sexp>, Vec<ParseError>) {
    let (asts, errors) = self.read_all();
    (asts.into_iter().map(|ast| ast.into()).collect(), errors)
  }
}
