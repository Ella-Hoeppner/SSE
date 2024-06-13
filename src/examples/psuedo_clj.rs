use crate::{
  syntax::SyntaxTag, Encloser, Operator, SymmetricEncloser, SyntaxContext,
  SyntaxGraph,
};
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
  Regex,        // #"..."
  LineComment,  // ;...\n
  BlockComment, // ;...\n
}
impl Encloser<CljSyntax> for CljEncloser {
  fn opening_encloser_str(&self) -> &str {
    match self {
      CljEncloser::List => "(",
      CljEncloser::Vector => "[",
      CljEncloser::HashMap => "{",
      CljEncloser::HashSet => "#{",
      CljEncloser::FnLiteral => "#(",
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
      CljEncloser::Regex => "\"",
      CljEncloser::LineComment => "\n",
      CljEncloser::BlockComment => "*/",
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CljSymmetricEncloser {
  String,
}
impl SymmetricEncloser<CljSyntax> for CljSymmetricEncloser {
  fn encloser_str(&self) -> &str {
    match self {
      CljSymmetricEncloser::String => "\"",
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
impl Operator<CljSyntax> for CljOperator {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CljSyntax {
  Encloser(CljEncloser),
  Operator(CljOperator),
  SymmetricEncloser(CljSymmetricEncloser),
}

impl SyntaxTag for CljSyntax {
  fn tag_str(&self) -> &str {
    match self {
      CljSyntax::Encloser(encloser) => match encloser {
        CljEncloser::List => "",
        CljEncloser::Vector => "_VECTOR_",
        CljEncloser::HashMap => "_HASHMAP_",
        CljEncloser::HashSet => "_HASHSET_",
        CljEncloser::FnLiteral => "_FN_LITERAL_",
        CljEncloser::Regex => "_REGEX_",
        CljEncloser::LineComment => "_LINE_COMMENT_",
        CljEncloser::BlockComment => "_BLOCK_COMMENT_",
      },
      CljSyntax::SymmetricEncloser(symmetric_encloser) => {
        match symmetric_encloser {
          CljSymmetricEncloser::String => "_STRING_",
        }
      }
      CljSyntax::Operator(operator) => match operator {
        CljOperator::Metadata => "_METADATA_",
        CljOperator::Quote => "_QUOTE_",
        CljOperator::SyntaxQuote => "_SYNTAX_QUOTE_",
        CljOperator::Unquote => "_UNQUOTE_",
        CljOperator::UnquoteSplice => "_UNQUOTE_SPLICE_",
        CljOperator::Deref => "_DEREF_",
        CljOperator::FormComment => "_FORM_COMMENT_",
      },
    }
  }
}

pub type CljSyntaxGraph = SyntaxGraph<
  CljSyntax,
  CljContext,
  CljEncloser,
  CljSymmetricEncloser,
  CljOperator,
>;

pub fn clj_graph() -> CljSyntaxGraph {
  CljSyntaxGraph::new(
    CljContext::Default,
    [
      (
        CljContext::Default,
        SyntaxContext::new(
          vec![
            CljSyntax::Encloser(CljEncloser::List),
            CljSyntax::Encloser(CljEncloser::Vector),
            CljSyntax::Encloser(CljEncloser::HashMap),
            CljSyntax::Encloser(CljEncloser::HashSet),
            CljSyntax::Encloser(CljEncloser::FnLiteral),
            CljSyntax::Encloser(CljEncloser::Regex),
            CljSyntax::Encloser(CljEncloser::LineComment),
            CljSyntax::Encloser(CljEncloser::BlockComment),
            CljSyntax::SymmetricEncloser(CljSymmetricEncloser::String),
            CljSyntax::Operator(CljOperator::Metadata),
            CljSyntax::Operator(CljOperator::Quote),
            CljSyntax::Operator(CljOperator::SyntaxQuote),
            CljSyntax::Operator(CljOperator::Unquote),
            CljSyntax::Operator(CljOperator::UnquoteSplice),
            CljSyntax::Operator(CljOperator::Deref),
            CljSyntax::Operator(CljOperator::FormComment),
          ],
          None,
          vec![' ', '\t', '\n', '\r'],
        ),
      ),
      (
        CljContext::String,
        SyntaxContext::new(vec![], Some('\\'), vec![]),
      ),
      (
        CljContext::Comment,
        SyntaxContext::new(vec![], None, vec![]),
      ),
    ]
    .into(),
    vec![
      (
        CljSyntax::Encloser(CljEncloser::List),
        CljEncloser::List,
        CljContext::Default,
      ),
      (
        CljSyntax::Encloser(CljEncloser::Vector),
        CljEncloser::Vector,
        CljContext::Default,
      ),
      (
        CljSyntax::Encloser(CljEncloser::HashMap),
        CljEncloser::HashMap,
        CljContext::Default,
      ),
      (
        CljSyntax::Encloser(CljEncloser::HashSet),
        CljEncloser::HashSet,
        CljContext::Default,
      ),
      (
        CljSyntax::Encloser(CljEncloser::FnLiteral),
        CljEncloser::FnLiteral,
        CljContext::Default,
      ),
      (
        CljSyntax::Encloser(CljEncloser::LineComment),
        CljEncloser::LineComment,
        CljContext::Comment,
      ),
      (
        CljSyntax::Encloser(CljEncloser::BlockComment),
        CljEncloser::BlockComment,
        CljContext::Comment,
      ),
      (
        CljSyntax::Encloser(CljEncloser::Regex),
        CljEncloser::Regex,
        CljContext::String,
      ),
    ],
    vec![(
      CljSyntax::SymmetricEncloser(CljSymmetricEncloser::String),
      CljSymmetricEncloser::String,
      CljContext::String,
    )],
    vec![
      (
        CljSyntax::Operator(CljOperator::Metadata),
        CljOperator::Metadata,
        CljContext::Default,
      ),
      (
        CljSyntax::Operator(CljOperator::Quote),
        CljOperator::Quote,
        CljContext::Default,
      ),
      (
        CljSyntax::Operator(CljOperator::SyntaxQuote),
        CljOperator::SyntaxQuote,
        CljContext::Default,
      ),
      (
        CljSyntax::Operator(CljOperator::Unquote),
        CljOperator::Unquote,
        CljContext::Default,
      ),
      (
        CljSyntax::Operator(CljOperator::UnquoteSplice),
        CljOperator::UnquoteSplice,
        CljContext::Default,
      ),
      (
        CljSyntax::Operator(CljOperator::Deref),
        CljOperator::Deref,
        CljContext::Default,
      ),
      (
        CljSyntax::Operator(CljOperator::FormComment),
        CljOperator::FormComment,
        CljContext::Default,
      ),
    ],
  )
}

#[cfg(test)]
mod tests {
  use crate::{
    examples::psuedo_clj::{
      clj_graph, CljEncloser, CljOperator, CljSymmetricEncloser, CljSyntax,
    },
    Parser, TaggedSexp,
  };
  use CljSyntax::*;
  use TaggedSexp::*;

  #[test]
  fn test_data() {
    assert_eq!(
      Parser::new(
        clj_graph(),
        "(+ (second [1 2 3])
            (count \"this is a string!!!\")
            (first (keys {1 2 3 4}))
            @my-atom)"
      )
      .read_next_tagged_sexp(),
      Ok(Some(List((
        Encloser(CljEncloser::List),
        vec![
          Leaf("+".to_string()),
          List((
            Encloser(CljEncloser::List),
            vec![
              Leaf("second".to_string()),
              List((
                Encloser(CljEncloser::Vector),
                vec![
                  Leaf("1".to_string()),
                  Leaf("2".to_string()),
                  Leaf("3".to_string())
                ]
              ))
            ]
          )),
          List((
            Encloser(CljEncloser::List),
            vec![
              Leaf("count".to_string()),
              List((
                SymmetricEncloser(CljSymmetricEncloser::String),
                vec![Leaf("this is a string!!!".to_string())]
              ))
            ]
          )),
          List((
            Encloser(CljEncloser::List),
            vec![
              Leaf("first".to_string()),
              List((
                Encloser(CljEncloser::List),
                vec![List((
                  Encloser(CljEncloser::List),
                  vec![
                    Leaf("keys".to_string()),
                    List((
                      Encloser(CljEncloser::HashMap),
                      vec![
                        Leaf("1".to_string()),
                        Leaf("2".to_string()),
                        Leaf("3".to_string()),
                        Leaf("4".to_string())
                      ]
                    ))
                  ]
                ))]
              ))
            ]
          )),
          List((
            Operator(CljOperator::Deref),
            vec![Leaf("my-atom".to_string()),]
          ))
        ]
      ))))
    )
  }
}
