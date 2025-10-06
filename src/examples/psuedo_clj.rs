use crate::{syntax::Syntax, Context, ContextId, Encloser, Operator};
use std::{fmt::Debug, hash::Hash, sync::LazyLock};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CljContext {
  Default,
  InsideString,
  UnstructuredComment,
  StructuredComment,
}
use CljContext::*;

impl ContextId for CljContext {
  fn is_comment(&self) -> bool {
    match self {
      UnstructuredComment | StructuredComment => true,
      _ => false,
    }
  }
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
  BlockComment, // /*...*/
}
use CljEncloser::*;
impl Encloser for CljEncloser {
  fn opening_encloser_str(&self) -> &str {
    match self {
      List => "(",
      Vector => "[",
      HashMap => "{",
      HashSet => "#{",
      FnLiteral => "#(",
      String => "\"",
      Regex => "#\"",
      LineComment => ";",
      BlockComment => "/*",
    }
  }
  fn closing_encloser_str(&self) -> &str {
    match self {
      List => ")",
      Vector => "]",
      HashMap => "}",
      HashSet => "}",
      FnLiteral => ")",
      String => "\"",
      Regex => "\"",
      LineComment => "\n",
      BlockComment => "*/",
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

use CljOperator::*;
impl Operator for CljOperator {
  fn left_args(&self) -> usize {
    0
  }

  fn right_args(&self) -> usize {
    match self {
      Metadata => 2,
      _ => 1,
    }
  }

  fn op_str(&self) -> &str {
    match self {
      Metadata => "^",
      Quote => "'",
      SyntaxQuote => "`",
      Unquote => "~",
      UnquoteSplice => "~@",
      Deref => "@",
      FormComment => "#_",
    }
  }
}

static DEFAULT_CTX: LazyLock<Context<CljEncloser, CljOperator>> =
  LazyLock::new(|| {
    Context::new(
      vec![
        List,
        Vector,
        HashMap,
        HashSet,
        FnLiteral,
        String,
        Regex,
        LineComment,
        BlockComment,
      ],
      vec![
        Metadata,
        Quote,
        SyntaxQuote,
        Unquote,
        UnquoteSplice,
        Deref,
        FormComment,
      ],
      None,
      vec![
        " ".to_string(),
        "\n".to_string(),
        "\t".to_string(),
        "\r".to_string(),
      ],
    )
  });

static STRING_CTX: LazyLock<Context<CljEncloser, CljOperator>> =
  LazyLock::new(|| {
    Context::new(vec![], vec![], Some('\\'.to_string()), vec![])
  });

static TRIVIAL_CTX: LazyLock<Context<CljEncloser, CljOperator>> =
  LazyLock::new(|| Context::trivial());

#[derive(Debug, Clone)]
pub struct CljSyntax;
impl Syntax for CljSyntax {
  type C = CljContext;
  type E = CljEncloser;
  type O = CljOperator;

  fn root_context(&self) -> Self::C {
    Default
  }

  fn context<'a>(&'a self, id: &Self::C) -> &'a Context<Self::E, Self::O> {
    match id {
      Default | StructuredComment => &*DEFAULT_CTX,
      InsideString => &*STRING_CTX,
      UnstructuredComment => &*TRIVIAL_CTX,
    }
  }

  fn encloser_context(&self, encloser: &Self::E) -> Self::C {
    match encloser {
      LineComment | BlockComment => UnstructuredComment,
      Regex | String => InsideString,
      _ => Default,
    }
  }

  fn operator_context(&self, operator: &Self::O) -> Self::C {
    match operator {
      FormComment => StructuredComment,
      _ => Default,
    }
  }
}

#[cfg(test)]
mod pseudo_clj_tests {
  use crate::{
    examples::psuedo_clj::{CljEncloser, CljOperator, CljSyntax},
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
        CljSyntax,
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
