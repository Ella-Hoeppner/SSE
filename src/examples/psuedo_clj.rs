use crate::{Encloser, Operator, SyntaxContext, SyntaxGraph};
use std::{fmt::Debug, hash::Hash};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CljContext {
  Default,
  String,
  Comment,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CljEncloser {
  List,         // (...)
  Vector,       // [...]
  HashMap,      // {...}
  HashSet,      // #{...}
  FnLiteral,    // #(...)
  String,       // "..."
  Regex,        // #"..."
  LineComment,  // ;...\n
  BlockComment, // ;...\n
}
impl Encloser for CljEncloser {
  fn id_str(&self) -> &str {
    match self {
      CljEncloser::List => "",
      CljEncloser::Vector => "_VECTOR_",
      CljEncloser::HashMap => "_HASHMAP_",
      CljEncloser::HashSet => "_HASHSET_",
      CljEncloser::FnLiteral => "_FN_LITERAL_",
      CljEncloser::String => "_STRING_",
      CljEncloser::Regex => "_REGEX_",
      CljEncloser::LineComment => "_LINE_COMMENT_",
      CljEncloser::BlockComment => "_BLOCK_COMMENT_",
    }
  }
  fn opening_encloser_str(&self) -> &str {
    match self {
      CljEncloser::List => "(",
      CljEncloser::Vector => "[",
      CljEncloser::HashMap => "{",
      CljEncloser::HashSet => "#{",
      CljEncloser::FnLiteral => "#(",
      CljEncloser::String => "\"",
      CljEncloser::Regex => "#\"",
      CljEncloser::LineComment => ";",
      CljEncloser::BlockComment => "/*",
    }
  }
  fn closing_encloser_str(&self) -> &str {
    match self {
      CljEncloser::List => ")",
      CljEncloser::Vector => "]",
      CljEncloser::HashMap => "}",
      CljEncloser::HashSet => "}",
      CljEncloser::FnLiteral => ")",
      CljEncloser::String => "\"",
      CljEncloser::Regex => "\"",
      CljEncloser::LineComment => "\n",
      CljEncloser::BlockComment => {
        "*/
"
      }
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CljOperator {
  Metadata,      // ^... ...
  Quote,         // '...
  SyntaxQuote,   // `...
  Unquote,       // ~...
  UnquoteSplice, // ~@...
  Deref,         // @...
  FormComment,   // #_...
}
impl Operator for CljOperator {
  fn id_str(&self) -> &str {
    match self {
      CljOperator::Metadata => "_METADATA_",
      CljOperator::Quote => "_QUOTE_",
      CljOperator::SyntaxQuote => "_SYNTAX_QUOTE_",
      CljOperator::Unquote => "_UNQUOTE_",
      CljOperator::UnquoteSplice => "_UNQUOTE_SPLICE_",
      CljOperator::Deref => "_DEREF_",
      CljOperator::FormComment => "_FORM_COMMENT_",
    }
  }
  fn left_args(&self) -> usize {
    0
  }

  fn right_args(&self) -> usize {
    match self {
      CljOperator::Metadata => 2,
      CljOperator::Quote => 1,
      CljOperator::SyntaxQuote => 1,
      CljOperator::Unquote => 1,
      CljOperator::UnquoteSplice => 1,
      CljOperator::Deref => 1,
      CljOperator::FormComment => 1,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      CljOperator::Metadata => "^",
      CljOperator::Quote => "'",
      CljOperator::SyntaxQuote => "`",
      CljOperator::Unquote => "~",
      CljOperator::UnquoteSplice => "~@",
      CljOperator::Deref => "@",
      CljOperator::FormComment => "#_",
    }
  }
}

pub type CljSyntaxGraph = SyntaxGraph<CljContext, CljEncloser, CljOperator>;

pub fn clj_graph() -> CljSyntaxGraph {
  CljSyntaxGraph::new(
    CljContext::Default,
    [
      (
        CljContext::Default,
        SyntaxContext::new(
          vec![
            CljEncloser::List,
            CljEncloser::Vector,
            CljEncloser::HashMap,
            CljEncloser::HashSet,
            CljEncloser::FnLiteral,
            CljEncloser::String,
            CljEncloser::Regex,
            CljEncloser::LineComment,
            CljEncloser::BlockComment,
          ],
          vec![
            CljOperator::Metadata,
            CljOperator::Quote,
            CljOperator::SyntaxQuote,
            CljOperator::Unquote,
            CljOperator::UnquoteSplice,
            CljOperator::Deref,
            CljOperator::FormComment,
          ],
          None,
          vec![' ', '\t', '\n', '\r'],
        ),
      ),
      (
        CljContext::String,
        SyntaxContext::new(vec![], vec![], Some('\\'), vec![]),
      ),
      (
        CljContext::Comment,
        SyntaxContext::new(vec![], vec![], None, vec![]),
      ),
    ]
    .into(),
    [
      (CljEncloser::List, CljContext::Default),
      (CljEncloser::Vector, CljContext::Default),
      (CljEncloser::HashMap, CljContext::Default),
      (CljEncloser::HashSet, CljContext::Default),
      (CljEncloser::FnLiteral, CljContext::Default),
      (CljEncloser::LineComment, CljContext::Comment),
      (CljEncloser::BlockComment, CljContext::Comment),
      (CljEncloser::Regex, CljContext::String),
      (CljEncloser::String, CljContext::String),
    ]
    .into_iter()
    .collect(),
    [
      (CljOperator::Metadata, CljContext::Default),
      (CljOperator::Quote, CljContext::Default),
      (CljOperator::SyntaxQuote, CljContext::Default),
      (CljOperator::Unquote, CljContext::Default),
      (CljOperator::UnquoteSplice, CljContext::Default),
      (CljOperator::Deref, CljContext::Default),
      (CljOperator::FormComment, CljContext::Default),
    ]
    .into_iter()
    .collect(),
  )
}

#[cfg(test)]
mod pseudo_clj_tests {
  use crate::{
    examples::psuedo_clj::{clj_graph, CljEncloser, CljOperator},
    syntax::EncloserOrOperator,
    Parser, SyntaxTree,
  };
  use CljEncloser::*;
  use CljOperator::*;
  use EncloserOrOperator::*;

  #[test]
  fn test_data() {
    fn leaf(s: &str) -> SyntaxTree<CljEncloser, CljOperator> {
      SyntaxTree::Leaf((), s.to_string())
    }
    fn inner(
      encloser_op_operator: EncloserOrOperator<CljEncloser, CljOperator>,
      subexpressions: Vec<SyntaxTree<CljEncloser, CljOperator>>,
    ) -> SyntaxTree<CljEncloser, CljOperator> {
      SyntaxTree::Inner(encloser_op_operator, subexpressions)
    }
    assert_eq!(
      Parser::new(
        clj_graph(),
        "(+ (second [1 2 3])
          (count \"this is a string!!!\")
          (first (keys ^my-metadata {1 2 3 4}))
          @my-atom)"
      )
      .read_next()
      .map(|maybe_tree| maybe_tree.map(SyntaxTree::from)),
      Ok(Some(inner(
        Encloser(List),
        vec![
          leaf("+"),
          inner(
            Encloser(List),
            vec![
              leaf("second"),
              inner(Encloser(Vector), vec![leaf("1"), leaf("2"), leaf("3")])
            ]
          ),
          inner(
            Encloser(List),
            vec![
              leaf("count"),
              inner(Encloser(String), vec![leaf("this is a string!!!")])
            ]
          ),
          inner(
            Encloser(List),
            vec![
              leaf("first"),
              inner(
                Encloser(List),
                vec![
                  leaf("keys"),
                  inner(
                    Operator(Metadata),
                    vec![
                      leaf("my-metadata"),
                      inner(
                        Encloser(HashMap),
                        vec![leaf("1"), leaf("2"), leaf("3"), leaf("4")]
                      )
                    ]
                  )
                ]
              )
            ]
          ),
          inner(Operator(Deref), vec![leaf("my-atom"),])
        ]
      )))
    )
  }
}
