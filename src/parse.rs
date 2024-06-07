use std::{
  fmt::{Debug, Display},
  hash::Hash,
};

use crate::{
  sexp::Sexp,
  syntax::{Encloser, Operator, SymmetricEncloser, SyntaxGraph},
};

pub struct Parser<
  T: Clone + Debug + PartialEq + Eq + Display + Hash,
  D: Encloser<T>,
  SD: SymmetricEncloser<T>,
  O: Operator<T>,
> {
  syntax_graph: SyntaxGraph<T, D, SD, O>,
}

impl<
    T: Clone + Debug + PartialEq + Eq + Display + Hash,
    D: Encloser<T>,
    SD: SymmetricEncloser<T>,
    O: Operator<T>,
  > Parser<T, D, SD, O>
{
  pub fn new(syntax_graph: SyntaxGraph<T, D, SD, O>) -> Self {
    Self { syntax_graph }
  }
  pub fn parse(&self, text: &str) -> Sexp {
    todo!()
  }
}
