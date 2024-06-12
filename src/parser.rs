use crate::{
  parse::Parse,
  syntax::{SyntaxElement, SyntaxTag},
  Encloser, Operator, ParseError, Sexp, SymmetricEncloser, SyntaxGraph,
  TaggedSexp,
};
use std::{fmt::Debug, hash::Hash};

#[derive(Debug, Clone)]
pub struct Parser<
  't,
  Tag: SyntaxTag,
  ContextTag: Clone + Debug + PartialEq + Eq + Hash,
  E: Encloser<Tag>,
  SE: SymmetricEncloser<Tag>,
  O: Operator<Tag>,
> {
  text: &'t str,
  syntax_graph: SyntaxGraph<Tag, ContextTag, E, SE, O>,
  parsed_top_level_sexps: Vec<(TaggedSexp<Tag>, usize)>,
  top_level_lookahead: usize,
  already_parsed_index: usize,
}

impl<
    't,
    Tag: SyntaxTag,
    ContextTag: Clone + Debug + PartialEq + Eq + Hash,
    E: Encloser<Tag>,
    SE: SymmetricEncloser<Tag>,
    O: Operator<Tag>,
  > Parser<'t, Tag, ContextTag, E, SE, O>
{
  pub fn new(
    syntax_graph: SyntaxGraph<Tag, ContextTag, E, SE, O>,
    text: &'t str,
  ) -> Self {
    Self {
      text,
      top_level_lookahead: syntax_graph
        .get_context(&syntax_graph.root)
        .tags()
        .iter()
        .map(|tag| {
          let x = syntax_graph.get_tag_element(tag);
          match x {
            SyntaxElement::Operator(operator) => operator.left_args(),
            _ => 0,
          }
        })
        .max()
        .unwrap_or(0),
      syntax_graph,
      parsed_top_level_sexps: vec![],
      already_parsed_index: 0,
    }
  }
  pub fn replace_syntax_graph(
    &mut self,
    new_syntax_graph: SyntaxGraph<Tag, ContextTag, E, SE, O>,
  ) {
    self.syntax_graph = new_syntax_graph;
    self.parsed_top_level_sexps.clear();
  }
  pub fn read_next_tagged_sexp(
    &mut self,
  ) -> Result<Option<TaggedSexp<Tag>>, ParseError> {
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
  pub fn read_next_sexp(&mut self) -> Result<Option<Sexp>, ParseError> {
    self.read_next_tagged_sexp().map(|maybe_tagged_sexp| {
      maybe_tagged_sexp.map(|tagged_sexp| tagged_sexp.into())
    })
  }
}
