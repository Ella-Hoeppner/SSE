mod parse;
mod parser;
mod sexp;
pub mod str_tagged;
mod str_utils;
pub mod syntax;

pub use parse::ParseError;
pub use parser::Parser;
pub use sexp::Sexp;
pub use sexp::TaggedSexp;
pub use syntax::Encloser;
pub use syntax::Operator;
pub use syntax::SymmetricEncloser;
pub use syntax::SyntaxContext;
pub use syntax::SyntaxGraph;

#[cfg(test)]
mod tests {
  use crate::{
    str_tagged::StringTaggedSyntaxGraph, ParseError, Parser, Sexp::*,
  };

  fn sexp_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![("", "(", ")")],
      vec![],
    )
  }

  fn plus_sexp_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![("", "(", ")")],
      vec![("PLUS", "+", 1, 1)],
    )
  }

  fn pipe_sexp_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![("", "(", ")"), ("PIPE", "|", "|")],
      vec![],
    )
  }

  fn quote_sexp_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![("", "(", ")")],
      vec![("QUOTE", "'", 0, 1)],
    )
  }

  fn multi_bracket_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![
        ("", "(", ")"),
        (":SQUARE", "[", "]"),
        (":CURLY", "{", "}"),
        (":HASH_CURLY", "#{", "}"),
      ],
      vec![],
    )
  }

  #[test]
  fn sexp_terminal() {
    assert_eq!(
      Parser::new(sexp_graph(), "hello!").read_next_sexp(),
      Ok(Some(Leaf("hello!")))
    );
  }

  #[test]
  fn sexp_whitespaced_list() {
    assert_eq!(
      Parser::new(sexp_graph(), "( + 1 2 )").read_next_sexp(),
      Ok(Some(List(vec![Leaf("+"), Leaf("1"), Leaf("2")])))
    );
  }

  #[test]
  fn sexp_list() {
    assert_eq!(
      Parser::new(sexp_graph(), "(1)").read_next_sexp(),
      Ok(Some(List(vec![Leaf("1")])))
    );
  }

  #[test]
  fn sexp_terminal_non_whitespaced_into_opener() {
    assert_eq!(
      Parser::new(sexp_graph(), "(hello?())").read_next_sexp(),
      Ok(Some(List(vec![Leaf("hello?"), List(vec![])])))
    );
  }

  #[test]
  fn sexp_nested_list() {
    assert_eq!(
      Parser::new(sexp_graph(), "(+ 1 (* 2 3))").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("+"),
        Leaf("1"),
        List(vec![Leaf("*"), Leaf("2"), Leaf("3")]),
      ])))
    );
  }

  #[test]
  fn unclosed_list_causes_error() {
    assert_eq!(
      Parser::new(sexp_graph(), "(+ 1 2").read_next_sexp(),
      Err(ParseError::EndOfTextWithOpenEncloser("(".to_string()))
    );
  }

  #[test]
  fn square_bracket() {
    assert_eq!(
      Parser::new(multi_bracket_graph(), "[1 2]").read_next_sexp(),
      Ok(Some(List(vec![Leaf(":SQUARE"), Leaf("1"), Leaf("2")])))
    );
  }

  #[test]
  fn nested_brackets() {
    assert_eq!(
      Parser::new(multi_bracket_graph(), "([{#{hello!}}])").read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf(":SQUARE"),
        List(vec![
          Leaf(":CURLY"),
          List(vec![Leaf(":HASH_CURLY"), Leaf("hello!")]),
        ]),
      ])])))
    );
  }

  #[test]
  fn nested_brackets_extra_hash() {
    assert_eq!(
      Parser::new(multi_bracket_graph(), "([{####{hello!}}])").read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf(":SQUARE"),
        List(vec![
          Leaf(":CURLY"),
          Leaf("###"),
          List(vec![Leaf(":HASH_CURLY"), Leaf("hello!")]),
        ]),
      ])])))
    );
  }

  #[test]
  fn mismatched_brackets_cause_error() {
    assert_eq!(
      Parser::new(multi_bracket_graph(), "([)]").read_next_sexp(),
      Err(ParseError::UnexpectedCloser(")".to_string()))
    );
  }

  #[test]
  fn prefix_op() {
    assert_eq!(
      Parser::new(quote_sexp_graph(), "'hello!").read_next_sexp(),
      Ok(Some(List(vec![Leaf("QUOTE"), Leaf("hello!")])))
    );
  }

  #[test]
  fn prefix_op_in_list() {
    assert_eq!(
      Parser::new(quote_sexp_graph(), "('hello! goodbye!)").read_next_sexp(),
      Ok(Some(List(vec![
        List(vec![Leaf("QUOTE"), Leaf("hello!")]),
        Leaf("goodbye!")
      ])))
    );
  }

  #[test]
  fn top_level_infix_op() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "1+2").read_next_sexp(),
      Ok(Some(List(vec![Leaf("PLUS"), Leaf("1"), Leaf("2")])))
    );
  }

  #[test]
  fn infix_op_in_list() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(1+2)").read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf("PLUS"),
        Leaf("1"),
        Leaf("2")
      ])])))
    );
  }

  #[test]
  fn nested_infix_op_in_list() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(1+2+3)").read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf("PLUS"),
        List(vec![Leaf("PLUS"), Leaf("1"), Leaf("2")]),
        Leaf("3")
      ])])))
    );
  }

  #[test]
  fn terminals_after_infix_op_in_list() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(1+2 3)").read_next_sexp(),
      Ok(Some(List(vec![
        List(vec![Leaf("PLUS"), Leaf("1"), Leaf("2")]),
        Leaf("3")
      ])))
    );
  }

  #[test]
  fn op_missing_left_arg_causes_error() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(+2)").read_next_sexp(),
      Err(ParseError::OperatorMissingLeftArgument("+".to_string()))
    );
  }

  #[test]
  fn unfinished_infix_op_causes_error() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(1+)").read_next_sexp(),
      Err(ParseError::OperatorMissingRightArgument("+".to_string()))
    );
  }

  #[test]
  fn unfinished_top_level_infix_op_causes_error() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "1+").read_next_sexp(),
      Err(ParseError::OperatorMissingRightArgument("+".to_string()))
    );
  }

  #[test]
  fn contextful_brackets() {
    assert_eq!(
      Parser::new(
        StringTaggedSyntaxGraph::from_descriptions(
          "root",
          vec![
            ("root", vec!["", "SQUARE"]),
            ("include_angle", vec!["", "SQUARE", "ANGLE"])
          ],
          vec![
            ("", "(", ")", "root"),
            ("SQUARE", "[", "]", "include_angle"),
            ("ANGLE", "<", ">", "include_angle")
          ],
          vec![]
        ),
        "(> < [<>])"
      )
      .read_next_sexp(),
      Ok(Some(List(vec![
        Leaf(">"),
        Leaf("<"),
        List(vec![Leaf("SQUARE"), List(vec![Leaf("ANGLE")])]),
      ])))
    );
  }

  #[test]
  fn contextful_operator() {
    assert_eq!(
      Parser::new(
        StringTaggedSyntaxGraph::from_descriptions(
          "root",
          vec![
            ("root", vec!["", "COLON"]),
            ("include_angle", vec!["", "ANGLE", "COLON"])
          ],
          vec![("", "(", ")", "root"), ("ANGLE", "<", ">", "include_angle")],
          vec![("COLON", ":", 1, 1, "include_angle")],
        ),
        "((> 1 0) : <Bool>)"
      )
      .read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf("COLON"),
        List(vec![Leaf(">"), Leaf("1"), Leaf("0")]),
        List(vec![Leaf("ANGLE"), Leaf("Bool")])
      ])])))
    );
  }

  #[test]
  fn symmetric_encloser() {
    assert_eq!(
      Parser::new(pipe_sexp_graph(), "|+ 1 2|").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("PIPE"),
        Leaf("+"),
        Leaf("1"),
        Leaf("2")
      ])))
    );
  }

  #[test]
  fn symmetric_enclosers_in_list() {
    assert_eq!(
      Parser::new(pipe_sexp_graph(), "(|+ 1 2| |a|)").read_next_sexp(),
      Ok(Some(List(vec![
        List(vec![Leaf("PIPE"), Leaf("+"), Leaf("1"), Leaf("2")]),
        List(vec![Leaf("PIPE"), Leaf("a")])
      ])))
    );
  }

  #[test]
  fn nested_symmetric_enclosers() {
    assert_eq!(
      Parser::new(pipe_sexp_graph(), "|(|a|)|").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("PIPE"),
        List(vec![List(vec![Leaf("PIPE"), Leaf("a")])])
      ])))
    );
  }
}
