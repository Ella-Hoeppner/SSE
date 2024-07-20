use crate::{
  parse::Parse, sexp::RawSexp, Encloser, Operator, ParseError, Sexp,
  SyntaxGraph, SyntaxTree,
};
use std::{fmt::Debug, hash::Hash};

#[derive(Debug, Clone)]
pub struct Parser<
  't,
  ContextTag: Clone + Debug + PartialEq + Eq + Hash,
  E: Encloser,
  O: Operator,
> {
  text: &'t str,
  syntax_graph: SyntaxGraph<ContextTag, E, O>,
  parsed_top_level_sexps: Vec<(SyntaxTree<E, O>, usize)>,
  top_level_lookahead: usize,
  already_parsed_index: usize,
}

impl<
    't,
    ContextTag: Clone + Debug + PartialEq + Eq + Hash,
    E: Encloser,
    O: Operator,
  > Parser<'t, ContextTag, E, O>
{
  pub fn new(
    syntax_graph: SyntaxGraph<ContextTag, E, O>,
    text: &'t str,
  ) -> Self {
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
    new_syntax_graph: SyntaxGraph<ContextTag, E, O>,
  ) {
    self.syntax_graph = new_syntax_graph;
    self.parsed_top_level_sexps.clear();
  }
  pub fn read_next_tagged_sexp(
    &mut self,
  ) -> Result<Option<SyntaxTree<E, O>>, ParseError> {
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
    Ok(
      if let Some((sexp, end_index)) = self.parsed_top_level_sexps.pop() {
        self.already_parsed_index = end_index;
        Some(sexp)
      } else {
        None
      },
    )
  }
  pub fn read_next_sexp(&mut self) -> Result<Option<RawSexp>, ParseError> {
    self.read_next_tagged_sexp().map(|maybe_tagged_sexp| {
      maybe_tagged_sexp.map(|tagged_sexp| tagged_sexp.into())
    })
  }
  pub fn read_all_tagged_sexps(
    &mut self,
  ) -> Vec<Result<SyntaxTree<E, O>, ParseError>> {
    let mut results = vec![];
    loop {
      match self.read_next_tagged_sexp() {
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
  pub fn read_all_sexps(&mut self) -> Vec<Result<RawSexp, ParseError>> {
    self
      .read_all_tagged_sexps()
      .into_iter()
      .map(|result| result.map(|tagged_sexp| tagged_sexp.into()))
      .collect()
  }
}