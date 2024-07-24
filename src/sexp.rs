use std::fmt::Debug;
use std::{fmt, ops::Range};

use crate::{syntax::EncloserOrOperator, Encloser, Operator};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Sexp<
  LeafData: Clone + PartialEq + Eq + Debug,
  InnerData: Clone + PartialEq + Eq + Debug,
> {
  Leaf(LeafData, String),
  Inner(InnerData, Vec<Sexp<LeafData, InnerData>>),
}

pub type RawSexp = Sexp<(), ()>;
impl RawSexp {
  pub fn leaf(s: String) -> Self {
    Self::Leaf((), s)
  }
  pub fn inner(subexpressions: Vec<RawSexp>) -> Self {
    Self::Inner((), subexpressions)
  }
}

impl fmt::Display for RawSexp {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Sexp::Leaf(_, token) => fmt.write_str(token)?,
      Sexp::Inner(_, sub_expressions) => {
        fmt.write_str("(")?;
        let mut separator = "";
        for sexp in sub_expressions {
          fmt.write_str(separator)?;
          fmt.write_str(&sexp.to_string())?;
          separator = " ";
        }
        fmt.write_str(")")?;
      }
    }
    Ok(())
  }
}

pub type SyntaxTree<E, O> = Sexp<(), EncloserOrOperator<E, O>>;

impl<E: Encloser, O: Operator> From<SyntaxTree<E, O>> for RawSexp {
  fn from(tree: SyntaxTree<E, O>) -> Self {
    match tree {
      SyntaxTree::Leaf((), leaf) => RawSexp::Leaf((), leaf),
      SyntaxTree::Inner(encloser_or_opener, sub_sexps) => RawSexp::inner({
        let translated_sub_sexps =
          sub_sexps.into_iter().map(|sub_sexp| sub_sexp.into());
        let tag_str = encloser_or_opener.id_str();
        if tag_str.is_empty() {
          translated_sub_sexps.collect()
        } else {
          std::iter::once(RawSexp::leaf(tag_str.to_string()))
            .chain(translated_sub_sexps)
            .collect()
        }
      }),
    }
  }
}

pub type DocumentSyntaxTree<E, O> =
  Sexp<Range<usize>, (Range<usize>, EncloserOrOperator<E, O>)>;

impl<E: Encloser, O: Operator> DocumentSyntaxTree<E, O> {
  pub fn range(&self) -> &Range<usize> {
    match self {
      Sexp::Leaf(range, _) => range,
      Sexp::Inner((range, _), _) => range,
    }
  }
}

impl<E: Encloser, O: Operator> From<DocumentSyntaxTree<E, O>>
  for SyntaxTree<E, O>
{
  fn from(tree: DocumentSyntaxTree<E, O>) -> Self {
    match tree {
      DocumentSyntaxTree::Leaf(_, leaf) => SyntaxTree::Leaf((), leaf),
      DocumentSyntaxTree::Inner((_, encloser_or_opener), sub_sexps) => {
        SyntaxTree::Inner(encloser_or_opener, {
          sub_sexps.into_iter().map(SyntaxTree::from).collect()
        })
      }
    }
  }
}

impl<E: Encloser, O: Operator> From<DocumentSyntaxTree<E, O>> for RawSexp {
  fn from(tree: DocumentSyntaxTree<E, O>) -> Self {
    SyntaxTree::from(tree).into()
  }
}
