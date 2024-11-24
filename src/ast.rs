use std::fmt;
use std::fmt::Debug;

use crate::{syntax::EncloserOrOperator, Encloser, Operator};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Ast<
  LeafData: Clone + PartialEq + Eq + Debug,
  InnerData: Clone + PartialEq + Eq + Debug,
> {
  Leaf(LeafData, String),
  Inner(InnerData, Vec<Ast<LeafData, InnerData>>),
}

#[derive(Debug)]
pub struct InvalidTreePath;

impl<
    LeafData: Clone + PartialEq + Eq + Debug,
    InnerData: Clone + PartialEq + Eq + Debug,
  > Ast<LeafData, InnerData>
{
  pub(crate) fn get_subtree_inner(
    &self,
    mut path: impl Iterator<Item = usize>,
  ) -> Result<&Self, InvalidTreePath> {
    if let Some(child_index) = path.next() {
      match self {
        Ast::Leaf(_, _) => Err(InvalidTreePath),
        Ast::Inner(_, children) => {
          if let Some(child) = children.get(child_index) {
            child.get_subtree_inner(path)
          } else {
            Err(InvalidTreePath)
          }
        }
      }
    } else {
      Ok(self)
    }
  }
  pub fn get_subtree(&self, path: &[usize]) -> Result<&Self, InvalidTreePath> {
    self.get_subtree_inner(path.iter().copied())
  }
}

pub type RawAst = Ast<(), ()>;
impl RawAst {
  pub fn leaf(s: String) -> Self {
    Self::Leaf((), s)
  }
  pub fn inner(subexpressions: Vec<RawAst>) -> Self {
    Self::Inner((), subexpressions)
  }
}

impl fmt::Display for RawAst {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Ast::Leaf(_, token) => fmt.write_str(token)?,
      Ast::Inner(_, sub_expressions) => {
        fmt.write_str("(")?;
        let mut separator = "";
        for Ast in sub_expressions {
          fmt.write_str(separator)?;
          fmt.write_str(&Ast.to_string())?;
          separator = " ";
        }
        fmt.write_str(")")?;
      }
    }
    Ok(())
  }
}

pub type SyntaxTree<E, O> = Ast<(), EncloserOrOperator<E, O>>;

impl<E: Encloser, O: Operator> From<SyntaxTree<E, O>> for RawAst {
  fn from(tree: SyntaxTree<E, O>) -> Self {
    match tree {
      SyntaxTree::Leaf((), leaf) => RawAst::Leaf((), leaf),
      SyntaxTree::Inner(encloser_or_opener, sub_Asts) => RawAst::inner({
        let translated_sub_Asts =
          sub_Asts.into_iter().map(|sub_Ast| sub_Ast.into());
        let tag_str = encloser_or_opener.id_str();
        if tag_str.is_empty() {
          translated_sub_Asts.collect()
        } else {
          std::iter::once(RawAst::leaf(tag_str.to_string()))
            .chain(translated_sub_Asts)
            .collect()
        }
      }),
    }
  }
}
