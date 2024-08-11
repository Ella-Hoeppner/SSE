use std::collections::HashMap;

use crate::{
  standard_whitespace_chars, Encloser, Operator, SyntaxContext, SyntaxGraph,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SexpEncloser;
impl Encloser for SexpEncloser {
  fn id_str(&self) -> &str {
    ""
  }
  fn opening_encloser_str(&self) -> &str {
    "("
  }
  fn closing_encloser_str(&self) -> &str {
    ")"
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum NoOperator {}
impl Operator for NoOperator {
  fn id_str(&self) -> &str {
    unreachable!()
  }
  fn left_args(&self) -> usize {
    unreachable!()
  }
  fn right_args(&self) -> usize {
    unreachable!()
  }
  fn op_str(&self) -> &str {
    unreachable!()
  }
}

pub type SexpGraph = SyntaxGraph<(), SexpEncloser, NoOperator>;

pub fn sexp_graph<'g>() -> SexpGraph {
  let context = SyntaxContext::new(
    vec![SexpEncloser],
    vec![],
    None,
    standard_whitespace_chars(),
  );
  SyntaxGraph::new(
    (),
    [((), context)].into_iter().collect(),
    [(SexpEncloser, ())].into_iter().collect(),
    HashMap::new(),
  )
}
