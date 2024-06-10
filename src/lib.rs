mod parse;
mod sexp;
pub mod str_tagged;
mod str_utils;
pub mod syntax;

#[cfg(test)]
mod tests {
  use crate::{
    parse::ParseError, sexp::Sexp::*, str_tagged::StringTaggedSyntaxGraph,
  };

  fn sexp_graph<'s>() -> StringTaggedSyntaxGraph<'s> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![("", "(", ")")],
      vec![],
    )
  }

  fn pipe_sexp_graph<'s>() -> StringTaggedSyntaxGraph<'s> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![("", "(", ")"), ("PIPE", "|", "|")],
      vec![],
    )
  }

  fn quote_sexp_graph<'s>() -> StringTaggedSyntaxGraph<'s> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![("", "(", ")")],
      vec![("QUOTE", "'", 0, 1)],
    )
  }

  fn multi_bracket_graph<'s>() -> StringTaggedSyntaxGraph<'s> {
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
    assert_eq!(sexp_graph().parse_to_sexp("hello!"), Ok(Leaf("hello!")));
  }

  #[test]
  fn sexp_whitespaced_list() {
    assert_eq!(
      sexp_graph().parse_to_sexp("( + 1 2 )"),
      Ok(List(vec![Leaf("+"), Leaf("1"), Leaf("2")]))
    );
  }

  #[test]
  fn sexp_list() {
    assert_eq!(sexp_graph().parse_to_sexp("(1)"), Ok(List(vec![Leaf("1")])));
  }

  #[test]
  fn sexp_terminal_non_whitespaced_into_opener() {
    assert_eq!(
      sexp_graph().parse_to_sexp("(hello?())"),
      Ok(List(vec![Leaf("hello?"), List(vec![])]))
    );
  }

  #[test]
  fn sexp_nested_list() {
    assert_eq!(
      sexp_graph().parse_to_sexp("(+ 1 (* 2 3))"),
      Ok(List(vec![
        Leaf("+"),
        Leaf("1"),
        List(vec![Leaf("*"), Leaf("2"), Leaf("3")]),
      ]))
    );
  }

  #[test]
  fn unclosed_list_causes_error() {
    assert_eq!(
      sexp_graph().parse_to_sexp("(+ 1 2"),
      Err(ParseError::EndOfText)
    );
  }

  #[test]
  fn square_bracket() {
    assert_eq!(
      multi_bracket_graph().parse_to_sexp("[1 2]"),
      Ok(List(vec![Leaf(":SQUARE"), Leaf("1"), Leaf("2")]))
    );
  }

  #[test]
  fn nested_brackets() {
    assert_eq!(
      multi_bracket_graph().parse_to_sexp("([{#{hello!}}])"),
      Ok(List(vec![List(vec![
        Leaf(":SQUARE"),
        List(vec![
          Leaf(":CURLY"),
          List(vec![Leaf(":HASH_CURLY"), Leaf("hello!")]),
        ]),
      ])]))
    );
  }

  #[test]
  fn nested_brackets_extra_hash() {
    assert_eq!(
      multi_bracket_graph().parse_to_sexp("([{####{hello!}}])"),
      Ok(List(vec![List(vec![
        Leaf(":SQUARE"),
        List(vec![
          Leaf(":CURLY"),
          Leaf("###"),
          List(vec![Leaf(":HASH_CURLY"), Leaf("hello!")]),
        ]),
      ])]))
    );
  }

  #[test]
  fn mismatched_brackets_cause_error() {
    assert_eq!(
      multi_bracket_graph().parse_to_sexp("([)]"),
      Err(ParseError::UnexpectedCloser("]".to_string()))
    );
  }

  #[test]
  fn prefix_op() {
    assert_eq!(
      quote_sexp_graph().parse_to_sexp("'hello!"),
      Ok(List(vec![Leaf("QUOTE"), Leaf("hello!")]))
    );
  }

  #[test]
  fn prefix_op_in_list() {
    assert_eq!(
      quote_sexp_graph().parse_to_sexp("('hello! goodbye!)"),
      Ok(List(vec![
        List(vec![Leaf("QUOTE"), Leaf("hello!")]),
        Leaf("goodbye!")
      ]))
    );
  }

  #[test]
  fn top_level_infix_op() {
    assert_eq!(
      StringTaggedSyntaxGraph::contextless_from_descriptions(
        vec![("", "(", ")")],
        vec![("PLUS", "+", 1, 1)],
      )
      .parse_to_sexp("1+2"),
      Ok(List(vec![Leaf("PLUS"), Leaf("1"), Leaf("2")]))
    );
  }

  #[test]
  fn infix_op_in_list() {
    assert_eq!(
      StringTaggedSyntaxGraph::contextless_from_descriptions(
        vec![("", "(", ")")],
        vec![("PLUS", "+", 1, 1)],
      )
      .parse_to_sexp("(1+2)"),
      Ok(List(vec![List(vec![Leaf("PLUS"), Leaf("1"), Leaf("2")])]))
    );
  }

  #[test]
  fn terminals_after_infix_op_in_list() {
    assert_eq!(
      StringTaggedSyntaxGraph::contextless_from_descriptions(
        vec![("", "(", ")")],
        vec![("PLUS", "+", 1, 1)],
      )
      .parse_to_sexp("(1+2 3)"),
      Ok(List(vec![
        List(vec![Leaf("PLUS"), Leaf("1"), Leaf("2")]),
        Leaf("3")
      ]))
    );
  }

  #[test]
  fn op_missing_left_arg_causes_error() {
    assert_eq!(
      StringTaggedSyntaxGraph::contextless_from_descriptions(
        vec![("", "(", ")")],
        vec![("PLUS", "+", 1, 1)],
      )
      .parse_to_sexp("(+2)"),
      Err(ParseError::MissingLeftArgument)
    );
  }

  #[test]
  fn unfinished_infix_op_causes_error() {
    assert_eq!(
      StringTaggedSyntaxGraph::contextless_from_descriptions(
        vec![("", "(", ")")],
        vec![("PLUS", "+", 1, 1)],
      )
      .parse_to_sexp("(1+)"),
      Err(ParseError::MissingRightArgument)
    );
  }

  #[test]
  fn unfinished_top_level_infix_op_causes_error() {
    assert_eq!(
      StringTaggedSyntaxGraph::contextless_from_descriptions(
        vec![("", "(", ")")],
        vec![("PLUS", "+", 1, 1)],
      )
      .parse_to_sexp("1+"),
      Err(ParseError::MissingRightArgument)
    );
  }

  #[test]
  fn contextful_brackets() {
    assert_eq!(
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
      )
      .parse_to_sexp("(< [<>])"),
      Ok(List(vec![
        Leaf("<"),
        List(vec![Leaf("SQUARE"), List(vec![Leaf("ANGLE")])]),
      ]))
    );
  }

  #[test]
  fn contextful_operator() {
    assert_eq!(
      StringTaggedSyntaxGraph::from_descriptions(
        "root",
        vec![
          ("root", vec!["", "COLON"]),
          ("include_angle", vec!["", "ANGLE", "COLON"])
        ],
        vec![("", "(", ")", "root"), ("ANGLE", "<", ">", "include_angle")],
        vec![("COLON", ":", 1, 1, "include_angle")],
      )
      .parse_to_sexp("((> 1 0) : <Bool>)"),
      Ok(List(vec![List(vec![
        Leaf("COLON"),
        List(vec![Leaf(">"), Leaf("1"), Leaf("0")]),
        List(vec![Leaf("ANGLE"), Leaf("Bool")])
      ])]))
    );
  }

  #[test]
  fn symmetric_encloser() {
    assert_eq!(
      pipe_sexp_graph().parse_to_sexp("|+ 1 2|"),
      Ok(List(vec![Leaf("PIPE"), Leaf("+"), Leaf("1"), Leaf("2"),]))
    );
  }

  #[test]
  fn symmetric_enclosers_in_list() {
    assert_eq!(
      pipe_sexp_graph().parse_to_sexp("(|+ 1 2| |a|)"),
      Ok(List(vec![
        List(vec![Leaf("PIPE"), Leaf("+"), Leaf("1"), Leaf("2")]),
        List(vec![Leaf("PIPE"), Leaf("a")])
      ]))
    );
  }

  #[test]
  fn nested_symmetric_enclosers() {
    assert_eq!(
      pipe_sexp_graph().parse_to_sexp("|(|a|)|"),
      Ok(List(vec![
        Leaf("PIPE"),
        List(vec![List(vec![Leaf("PIPE"), Leaf("a")])])
      ]))
    );
  }
}
