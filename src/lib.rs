mod parse;
mod sexp;
mod str_utils;
pub mod syntax;

pub use parse::Parser;

#[cfg(test)]
mod tests {
  use crate::{
    sexp::Sexp::*, syntax::str_tagged::StringTaggedSyntaxGraph, Parser,
  };

  #[test]
  fn basic_sexp() {
    let text = "(+ 1 2)";
    let expected_sexp = List(vec![Leaf("+"), Leaf("1"), Leaf("2")]);
    let parser =
      Parser::new(StringTaggedSyntaxGraph::contextless_from_descriptions(
        "",
        vec![("", "(", ")")],
        vec![],
      ));
    assert_eq!(parser.parse(text), Ok(expected_sexp))
  }
}
