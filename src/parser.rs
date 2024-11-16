use crate::{
  parse::Parse, syntax::Context, DocumentSyntaxTree, Encloser, Operator,
  ParseError, RawSexp, SyntaxGraph,
};
use std::fmt::Debug;
use take_mut::take;

#[derive(Debug, Clone)]
pub struct Parser<'t, C: Context, E: Encloser, O: Operator> {
  pub(crate) text: &'t str,
  pub(crate) syntax_graph: SyntaxGraph<C, E, O>,
  parsed_top_level_sexps: Vec<DocumentSyntaxTree<E, O>>,
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
      parsed_top_level_sexps: vec![],
      already_parsed_index: 0,
    }
  }
  pub fn replace_syntax_graph(
    &mut self,
    new_syntax_graph: SyntaxGraph<C, E, O>,
  ) {
    self.syntax_graph = new_syntax_graph;
    self.parsed_top_level_sexps.clear();
  }
  pub fn read_next(
    &mut self,
  ) -> Result<Option<DocumentSyntaxTree<E, O>>, ParseError> {
    while self.parsed_top_level_sexps.len() <= self.top_level_lookahead {
      let mut stolen_top_level_sexps = vec![];
      std::mem::swap(
        &mut stolen_top_level_sexps,
        &mut self.parsed_top_level_sexps,
      );
      match Parse::new(&self.syntax_graph, stolen_top_level_sexps, &self.text)
        .complete(self.already_parsed_index)?
      {
        Ok(new_top_level_sexps) => {
          self.parsed_top_level_sexps = new_top_level_sexps;
        }
        Err(original_top_level_sexps) => {
          self.parsed_top_level_sexps = original_top_level_sexps;
          break;
        }
      }
    }
    if self.parsed_top_level_sexps.is_empty() {
      Ok(None)
    } else {
      self.already_parsed_index =
        self.parsed_top_level_sexps.last().unwrap().range().end;
      let mut first_sexp = None;
      take(&mut self.parsed_top_level_sexps, |parsed_top_level_sexps| {
        let mut iter = parsed_top_level_sexps.into_iter();
        first_sexp = iter.next();
        iter.collect()
      });
      Ok(first_sexp)
    }
  }
  pub fn read_all(
    &mut self,
  ) -> Vec<Result<DocumentSyntaxTree<E, O>, ParseError>> {
    let mut results = vec![];
    loop {
      match self.read_next() {
        Ok(None) => break,
        Ok(Some(tagged_sexp)) => results.push(Ok(tagged_sexp)),
        Err(err) => {
          results.push(Err(err));
          break;
        }
      }
    }
    results
  }
  pub fn read_next_sexp(&mut self) -> Result<Option<RawSexp>, ParseError> {
    self.read_next().map(|maybe_tagged_sexp| {
      maybe_tagged_sexp.map(|tagged_sexp| tagged_sexp.into())
    })
  }
  pub fn read_all_sexps(&mut self) -> Vec<Result<RawSexp, ParseError>> {
    self
      .read_all()
      .into_iter()
      .map(|result| result.map(|tagged_sexp| tagged_sexp.into()))
      .collect()
  }
}
