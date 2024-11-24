use crate::{
  parse::Parse, syntax::Context, DocumentSyntaxTree, Encloser, Operator,
  ParseError, RawAst, SyntaxGraph,
};
use std::fmt::Debug;
use take_mut::take;

#[derive(Debug, Clone)]
pub struct Parser<'t, C: Context, E: Encloser, O: Operator> {
  pub(crate) text: &'t str,
  pub(crate) syntax_graph: SyntaxGraph<C, E, O>,
  parsed_top_level_Asts: Vec<DocumentSyntaxTree<E, O>>,
  top_level_lookahead: usize,
  already_parsed_index: usize,
}

impl<'t, C: Context, E: Encloser, O: Operator> Parser<'t, C, E, O> {
  pub fn new(syntax_graph: SyntaxGraph<C, E, O>, text: &'t str) -> Self {
    Self {
      text,
      top_level_lookahead: syntax_graph
        .get_context(&syntax_graph.root)
        .operators()
        .iter()
        .map(|operator| operator.left_args())
        .max()
        .unwrap_or(0),
      syntax_graph,
      parsed_top_level_Asts: vec![],
      already_parsed_index: 0,
    }
  }
  pub fn replace_syntax_graph(
    &mut self,
    new_syntax_graph: SyntaxGraph<C, E, O>,
  ) {
    self.syntax_graph = new_syntax_graph;
    self.parsed_top_level_Asts.clear();
  }
  pub fn read_next(
    &mut self,
  ) -> Result<Option<DocumentSyntaxTree<E, O>>, ParseError> {
    while self.parsed_top_level_Asts.len() <= self.top_level_lookahead {
      let mut stolen_top_level_Asts = vec![];
      std::mem::swap(
        &mut stolen_top_level_Asts,
        &mut self.parsed_top_level_Asts,
      );
      match Parse::new(&self.syntax_graph, stolen_top_level_Asts, &self.text)
        .complete(self.already_parsed_index)?
      {
        Ok(new_top_level_Asts) => {
          self.parsed_top_level_Asts = new_top_level_Asts;
        }
        Err(original_top_level_Asts) => {
          self.parsed_top_level_Asts = original_top_level_Asts;
          break;
        }
      }
    }
    if self.parsed_top_level_Asts.is_empty() {
      Ok(None)
    } else {
      self.already_parsed_index =
        self.parsed_top_level_Asts.last().unwrap().position().end();
      let mut first_Ast = None;
      take(&mut self.parsed_top_level_Asts, |parsed_top_level_Asts| {
        let mut iter = parsed_top_level_Asts.into_iter();
        first_Ast = iter.next();
        iter.collect()
      });
      Ok(first_Ast)
    }
  }
  pub fn read_all(
    &mut self,
  ) -> Vec<Result<DocumentSyntaxTree<E, O>, ParseError>> {
    let mut results = vec![];
    loop {
      match self.read_next() {
        Ok(None) => break,
        Ok(Some(tagged_Ast)) => results.push(Ok(tagged_Ast)),
        Err(err) => {
          results.push(Err(err));
          break;
        }
      }
    }
    results
  }
  pub fn read_next_Ast(&mut self) -> Result<Option<RawAst>, ParseError> {
    self.read_next().map(|maybe_tagged_Ast| {
      maybe_tagged_Ast.map(|tagged_Ast| tagged_Ast.into())
    })
  }
  pub fn read_all_Asts(&mut self) -> Vec<Result<RawAst, ParseError>> {
    self
      .read_all()
      .into_iter()
      .map(|result| result.map(|tagged_Ast| tagged_Ast.into()))
      .collect()
  }
}
