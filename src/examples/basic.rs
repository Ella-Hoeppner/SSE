use std::collections::HashMap;

use crate::{
  standard_whitespace_chars, Encloser, Operator, SyntaxContext, SyntaxGraph,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AstEncloser;
impl Encloser for AstEncloser {
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

pub type AstGraph = SyntaxGraph<(), AstEncloser, NoOperator>;

pub fn Ast_graph<'g>() -> AstGraph {
  let context = SyntaxContext::new(
    vec![AstEncloser],
    vec![],
    None,
    standard_whitespace_chars(),
  );
  SyntaxGraph::new(
    (),
    [((), context)].into_iter().collect(),
    [(AstEncloser, ())].into_iter().collect(),
    HashMap::new(),
  )
}
