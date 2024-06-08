mod parse;
mod sexp;
mod str_utils;
pub mod syntax;

pub use parse::Parser;

#[cfg(test)]
mod tests {
  use crate::{
    sexp::Sexp::{self, *},
    syntax::str_tagged::{
      StringTaggedEncloser, StringTaggedOperator,
      StringTaggedSymmetricEncloser, StringTaggedSyntaxGraph,
    },
    Parser,
  };

  fn sexp_parser<'s>() -> Parser<
    &'s str,
    StringTaggedEncloser<'s>,
    StringTaggedSymmetricEncloser<'s>,
    StringTaggedOperator<'s>,
  > {
    Parser::new(StringTaggedSyntaxGraph::contextless_from_descriptions(
      "",
      vec![("", "(", ")")],
      vec![],
    ))
  }

  fn assert_sexp_parse(text: &str, sexp: Sexp) {
    assert_eq!(sexp_parser().parse(text), Ok(sexp))
  }

  #[test]
  fn sexp_terminal() {
    assert_sexp_parse("1", Leaf("1"));
  }

  #[test]
  fn sexp_list() {
    assert_sexp_parse("(+ 1 2)", List(vec![Leaf("+"), Leaf("1"), Leaf("2")]));
  }
}
