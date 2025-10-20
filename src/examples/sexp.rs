use std::sync::LazyLock;

use crate::{
  Context, standard_whitespace_chars,
  syntax::{NoOperator, Syntax},
};

static SEXP_CTX: LazyLock<Context<(), NoOperator>> = LazyLock::new(|| {
  Context::new(vec![()], vec![], None, standard_whitespace_chars())
});

pub struct SexpSyntax;
impl Syntax for SexpSyntax {
  type C = ();
  type E = ();
  type O = NoOperator;

  fn root_context(&self) -> Self::C {}

  fn context<'a>(&'a self, _: &Self::C) -> &'a Context<Self::E, Self::O> {
    &SEXP_CTX
  }

  fn encloser_context(&self, _: &Self::E) -> Option<Self::C> {
    None
  }

  fn operator_context(&self, _: &Self::O) -> Option<Self::C> {
    None
  }
}
