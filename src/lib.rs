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
    's,
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

  fn assert_sexp_parse_eq(text: &str, sexp: Sexp) {
    assert_eq!(sexp_parser().parse(text), Ok(sexp))
  }

  #[test]
  fn sexp_terminal() {
    assert_sexp_parse_eq("hello!", Leaf("hello!"));
  }

  #[test]
  fn sexp_list() {
    assert_sexp_parse_eq(
      "( + 1 2 )",
      List(vec![Leaf("+"), Leaf("1"), Leaf("2")]),
    );
  }

  #[test]
  fn non_whitespaced_list() {
    assert_sexp_parse_eq("(1)", List(vec![Leaf("1")]));
  }

  #[test]
  fn sexp_terminal_non_whitespaced_into_opener() {
    assert_sexp_parse_eq("(hello?())", List(vec![Leaf("hello?"), List(vec![])]))
  }

  #[test]
  fn sexp_nested_list() {
    assert_sexp_parse_eq(
      "(+ 1 (* 2 3))",
      List(vec![
        Leaf("+"),
        Leaf("1"),
        List(vec![Leaf("*"), Leaf("2"), Leaf("3")]),
      ]),
    );
  }

  fn bracket_parser<'s>() -> Parser<
    's,
    &'s str,
    StringTaggedEncloser<'s>,
    StringTaggedSymmetricEncloser<'s>,
    StringTaggedOperator<'s>,
  > {
    Parser::new(StringTaggedSyntaxGraph::contextless_from_descriptions(
      "",
      vec![
        ("", "(", ")"),
        (":SQUARE", "[", "]"),
        (":CURLY", "{", "}"),
        (":HASH_CURLY", "#{", "}"),
      ],
      vec![],
    ))
  }

  fn assert_bracket_parse_eq(text: &str, sexp: Sexp) {
    assert_eq!(bracket_parser().parse(text), Ok(sexp))
  }

  #[test]
  fn square_bracket() {
    assert_bracket_parse_eq(
      "[1 2]",
      List(vec![Leaf(":SQUARE"), Leaf("1"), Leaf("2")]),
    )
  }

  #[test]
  fn nested_brackets() {
    assert_bracket_parse_eq(
      "([{#{hello!}}])",
      List(vec![List(vec![
        Leaf(":SQUARE"),
        List(vec![
          Leaf(":CURLY"),
          List(vec![Leaf(":HASH_CURLY"), Leaf("hello!")]),
        ]),
      ])]),
    )
  }

  #[test]
  fn nested_brackets_extra_hash() {
    assert_bracket_parse_eq(
      "([{####{hello!}}])",
      List(vec![List(vec![
        Leaf(":SQUARE"),
        List(vec![
          Leaf(":CURLY"),
          Leaf("###"),
          List(vec![Leaf(":HASH_CURLY"), Leaf("hello!")]),
        ]),
      ])]),
    )
  }
}
