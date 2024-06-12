mod parse;
mod parser;
mod sexp;
pub mod str_tagged;
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
      vec![' ', '\n', '\t', '\r'],
      Some('\\'),
      vec![("", "(", ")")],
      vec![],
    )
  }

  fn plus_sexp_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![' ', '\n', '\t', '\r'],
      Some('\\'),
      vec![("", "(", ")")],
      vec![("PLUS", "+", 1, 1)],
    )
  }

  fn pipe_sexp_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![' ', '\n', '\t', '\r'],
      None,
      vec![("", "(", ")"), ("PIPE", "|", "|")],
      vec![],
    )
  }

  fn quote_sexp_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![' ', '\n', '\t', '\r'],
      None,
      vec![("", "(", ")")],
      vec![("QUOTE", "'", 0, 1)],
    )
  }

  fn string_sexp_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::from_descriptions(
      "root",
      vec![
        (
          "root",
          vec!["", "STRING"],
          None,
          vec![' ', '\n', '\t', '\r'],
        ),
        ("string", vec![], Some('\\'), vec![]),
      ],
      vec![("", "(", ")", "root"), ("STRING", "\"", "\"", "string")],
      vec![],
    )
  }

  fn multi_bracket_graph<'g>() -> StringTaggedSyntaxGraph<'g> {
    StringTaggedSyntaxGraph::contextless_from_descriptions(
      vec![' ', '\n', '\t', '\r'],
      None,
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
      Ok(Some(Leaf("hello!".to_string())))
    );
  }

  #[test]
  fn sexp_whitespaced_list() {
    assert_eq!(
      Parser::new(sexp_graph(), "( + 1 2 )").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("+".to_string()),
        Leaf("1".to_string()),
        Leaf("2".to_string())
      ])))
    );
  }

  #[test]
  fn sexp_list() {
    assert_eq!(
      Parser::new(sexp_graph(), "(1)").read_next_sexp(),
      Ok(Some(List(vec![Leaf("1".to_string())])))
    );
  }

  #[test]
  fn sexp_terminal_non_whitespaced_into_opener() {
    assert_eq!(
      Parser::new(sexp_graph(), "(hello?())").read_next_sexp(),
      Ok(Some(List(vec![Leaf("hello?".to_string()), List(vec![])])))
    );
  }

  #[test]
  fn sexp_nested_list() {
    assert_eq!(
      Parser::new(sexp_graph(), "(+ 1 (* 2 3))").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("+".to_string()),
        Leaf("1".to_string()),
        List(vec![
          Leaf("*".to_string()),
          Leaf("2".to_string()),
          Leaf("3".to_string())
        ]),
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
      Ok(Some(List(vec![
        Leaf(":SQUARE".to_string()),
        Leaf("1".to_string()),
        Leaf("2".to_string())
      ])))
    );
  }

  #[test]
  fn nested_brackets() {
    assert_eq!(
      Parser::new(multi_bracket_graph(), "([{#{hello!}}])").read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf(":SQUARE".to_string()),
        List(vec![
          Leaf(":CURLY".to_string()),
          List(vec![
            Leaf(":HASH_CURLY".to_string()),
            Leaf("hello!".to_string())
          ]),
        ]),
      ])])))
    );
  }

  #[test]
  fn nested_brackets_extra_hash() {
    assert_eq!(
      Parser::new(multi_bracket_graph(), "([{####{hello!}}])").read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf(":SQUARE".to_string()),
        List(vec![
          Leaf(":CURLY".to_string()),
          Leaf("###".to_string()),
          List(vec![
            Leaf(":HASH_CURLY".to_string()),
            Leaf("hello!".to_string())
          ]),
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
      Ok(Some(List(vec![
        Leaf("QUOTE".to_string()),
        Leaf("hello!".to_string())
      ])))
    );
  }

  #[test]
  fn prefix_op_in_list() {
    assert_eq!(
      Parser::new(quote_sexp_graph(), "('hello! goodbye!)").read_next_sexp(),
      Ok(Some(List(vec![
        List(vec![Leaf("QUOTE".to_string()), Leaf("hello!".to_string())]),
        Leaf("goodbye!".to_string())
      ])))
    );
  }

  #[test]
  fn top_level_infix_op() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "1+2").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("PLUS".to_string()),
        Leaf("1".to_string()),
        Leaf("2".to_string())
      ])))
    );
  }

  #[test]
  fn infix_op_in_list() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(1+2)").read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf("PLUS".to_string()),
        Leaf("1".to_string()),
        Leaf("2".to_string())
      ])])))
    );
  }

  #[test]
  fn nested_infix_op_in_list() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(1+2+3)").read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf("PLUS".to_string()),
        List(vec![
          Leaf("PLUS".to_string()),
          Leaf("1".to_string()),
          Leaf("2".to_string())
        ]),
        Leaf("3".to_string())
      ])])))
    );
  }

  #[test]
  fn terminals_after_infix_op_in_list() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(1+2 3)").read_next_sexp(),
      Ok(Some(List(vec![
        List(vec![
          Leaf("PLUS".to_string()),
          Leaf("1".to_string()),
          Leaf("2".to_string())
        ]),
        Leaf("3".to_string())
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
            (
              "root",
              vec!["", "SQUARE"],
              None,
              vec![' ', '\n', '\t', '\r'],
            ),
            (
              "include_angle",
              vec!["", "SQUARE", "ANGLE"],
              None,
              vec![' ', '\n', '\t', '\r'],
            )
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
        Leaf(">".to_string()),
        Leaf("<".to_string()),
        List(vec![
          Leaf("SQUARE".to_string()),
          List(vec![Leaf("ANGLE".to_string())])
        ]),
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
            ("root", vec!["", "COLON"], None, vec![' ', '\n', '\t', '\r'],),
            (
              "include_angle",
              vec!["", "ANGLE", "COLON"],
              None,
              vec![' ', '\n', '\t', '\r'],
            )
          ],
          vec![("", "(", ")", "root"), ("ANGLE", "<", ">", "include_angle")],
          vec![("COLON", ":", 1, 1, "include_angle")],
        ),
        "((> 1 0) : <Bool>)"
      )
      .read_next_sexp(),
      Ok(Some(List(vec![List(vec![
        Leaf("COLON".to_string()),
        List(vec![
          Leaf(">".to_string()),
          Leaf("1".to_string()),
          Leaf("0".to_string())
        ]),
        List(vec![Leaf("ANGLE".to_string()), Leaf("Bool".to_string())])
      ])])))
    );
  }

  #[test]
  fn symmetric_encloser() {
    assert_eq!(
      Parser::new(pipe_sexp_graph(), "|+ 1 2|").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("PIPE".to_string()),
        Leaf("+".to_string()),
        Leaf("1".to_string()),
        Leaf("2".to_string())
      ])))
    );
  }

  #[test]
  fn escaped_closer() {
    assert_eq!(
      Parser::new(sexp_graph(), "(\\))").read_next_sexp(),
      Ok(Some(List(vec![Leaf("\\)".to_string())])))
    );
  }

  #[test]
  fn escaped_opener() {
    assert_eq!(
      Parser::new(sexp_graph(), "(\\()").read_next_sexp(),
      Ok(Some(List(vec![Leaf("\\(".to_string())])))
    );
  }

  #[test]
  fn escaped_operator() {
    assert_eq!(
      Parser::new(plus_sexp_graph(), "(\\+)").read_next_sexp(),
      Ok(Some(List(vec![Leaf("\\+".to_string())])))
    );
  }

  #[test]
  fn symmetric_enclosers_in_list() {
    assert_eq!(
      Parser::new(pipe_sexp_graph(), "(|+ 1 2| |a|)").read_next_sexp(),
      Ok(Some(List(vec![
        List(vec![
          Leaf("PIPE".to_string()),
          Leaf("+".to_string()),
          Leaf("1".to_string()),
          Leaf("2".to_string())
        ]),
        List(vec![Leaf("PIPE".to_string()), Leaf("a".to_string())])
      ])))
    );
  }

  #[test]
  fn nested_symmetric_enclosers() {
    assert_eq!(
      Parser::new(pipe_sexp_graph(), "|(|a|)|").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("PIPE".to_string()),
        List(vec![List(vec![
          Leaf("PIPE".to_string()),
          Leaf("a".to_string())
        ])])
      ])))
    );
  }

  #[test]
  fn read_two_sexps() {
    let mut parser = Parser::new(sexp_graph(), "(+ 1 2) (* 3 4)");
    assert_eq!(
      parser.read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("+".to_string()),
        Leaf("1".to_string()),
        Leaf("2".to_string())
      ])))
    );
    assert_eq!(
      parser.read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("*".to_string()),
        Leaf("3".to_string()),
        Leaf("4".to_string())
      ])))
    );
  }

  #[test]
  fn read_all_single_sexp() {
    assert_eq!(
      Parser::new(sexp_graph(), "(+ 1 2)").read_all_sexps(),
      vec![Ok(List(vec![
        Leaf("+".to_string()),
        Leaf("1".to_string()),
        Leaf("2".to_string())
      ]))]
    );
  }

  #[test]
  fn read_all_double_sexp() {
    assert_eq!(
      Parser::new(sexp_graph(), "(+ 1 2) (* 3 4)").read_all_sexps(),
      vec![
        Ok(List(vec![
          Leaf("+".to_string()),
          Leaf("1".to_string()),
          Leaf("2".to_string())
        ])),
        Ok(List(vec![
          Leaf("*".to_string()),
          Leaf("3".to_string()),
          Leaf("4".to_string())
        ]))
      ]
    );
  }

  #[test]
  fn read_all_double_sexp_err() {
    assert_eq!(
      Parser::new(sexp_graph(), "(+ 1 2) (* 3 4").read_all_sexps(),
      vec![
        Ok(List(vec![
          Leaf("+".to_string()),
          Leaf("1".to_string()),
          Leaf("2".to_string())
        ])),
        Err(ParseError::EndOfTextWithOpenEncloser("(".to_string()))
      ]
    );
  }

  #[test]
  fn contextful_whitespace() {
    assert_eq!(
      Parser::new(
        string_sexp_graph(),
        "(before string \" inside string!!! \" after string)"
      )
      .read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("before".to_string()),
        Leaf("string".to_string()),
        List(vec![
          Leaf("STRING".to_string()),
          Leaf(" inside string!!! ".to_string()),
        ]),
        Leaf("after".to_string()),
        Leaf("string".to_string()),
      ])))
    );
  }

  #[test]
  fn contextful_escape() {
    assert_eq!(
      Parser::new(string_sexp_graph(), "\"\\\"\"").read_next_sexp(),
      Ok(Some(List(vec![
        Leaf("STRING".to_string()),
        Leaf("\\\"".to_string()),
      ])))
    );
  }
}
