use std::fmt;
use std::fmt::Debug;

use crate::{syntax::EncloserOrOperator, Encloser, Operator};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Ast<
  LeafData: Clone + PartialEq + Debug,
  InnerData: Clone + PartialEq + Debug,
> {
  Leaf(LeafData, String),
  Inner(InnerData, Vec<Ast<LeafData, InnerData>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
  pub(crate) fn get_subtree_inner_mut(
    &mut self,
    mut path: impl Iterator<Item = usize>,
  ) -> Result<&mut Self, InvalidTreePath> {
    if let Some(child_index) = path.next() {
      match self {
        Ast::Leaf(_, _) => Err(InvalidTreePath),
        Ast::Inner(_, children) => {
          if let Some(child) = children.get_mut(child_index) {
            child.get_subtree_inner_mut(path)
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
  pub fn get_subtree_mut(
    &mut self,
    path: &[usize],
  ) -> Result<&mut Self, InvalidTreePath> {
    self.get_subtree_inner_mut(path.iter().copied())
  }
  pub(crate) fn innermost_predicate_reverse_path(
    &self,
    predicate: &impl Fn(&Self) -> bool,
  ) -> Option<Vec<usize>> {
    predicate(self).then(|| match self {
      Ast::Leaf(_, _) => vec![],
      Ast::Inner(_, children) => children
        .iter()
        .enumerate()
        .find_map(|(i, child)| {
          child.innermost_predicate_reverse_path(predicate).map(
            |mut reverse_path| {
              reverse_path.push(i);
              reverse_path
            },
          )
        })
        .unwrap_or(vec![]),
    })
  }
  pub fn innermost_predicate_path(
    &self,
    predicate: &impl Fn(&Self) -> bool,
  ) -> Option<Vec<usize>> {
    if let Some(mut reverse_path) =
      self.innermost_predicate_reverse_path(predicate)
    {
      reverse_path.reverse();
      Some(reverse_path)
    } else {
      None
    }
  }
  pub(crate) fn innermost_partial_predicate_reverse_path(
    &self,
    predicate: &impl Fn(&Self) -> Option<bool>,
  ) -> Option<Vec<usize>> {
    let result = predicate(self);
    if result == Some(false) {
      return None;
    }
    match self {
      Ast::Leaf(_, _) => result.map(|_| vec![]),
      Ast::Inner(_, children) => {
        if let Some(x) = children.iter().enumerate().find_map(|(i, child)| {
          child
            .innermost_partial_predicate_reverse_path(predicate)
            .map(|mut reverse_path| {
              reverse_path.push(i);
              reverse_path
            })
        }) {
          Some(x)
        } else {
          result.map(|_| vec![])
        }
      }
    }
  }
  pub fn innermost_partial_predicate_path(
    &self,
    predicate: &impl Fn(&Self) -> Option<bool>,
  ) -> Option<Vec<usize>> {
    if let Some(mut reverse_path) =
      self.innermost_partial_predicate_reverse_path(predicate)
    {
      reverse_path.reverse();
      Some(reverse_path)
    } else {
      None
    }
  }
  pub fn map<
    NewLeafData: Clone + PartialEq + Eq + Debug,
    NewInnerData: Clone + PartialEq + Eq + Debug,
  >(
    &self,
    leaf_processor: &impl Fn(&LeafData, &String) -> (NewLeafData, String),
    inner_processor: &impl Fn(&InnerData) -> NewInnerData,
  ) -> Ast<NewLeafData, NewInnerData> {
    match self {
      Ast::Leaf(data, label) => {
        let (new_data, new_label) = leaf_processor(data, label);
        Ast::Leaf(new_data, new_label)
      }
      Ast::Inner(data, children) => Ast::Inner(
        inner_processor(data),
        children
          .into_iter()
          .map(|child| child.map(leaf_processor, inner_processor))
          .collect(),
      ),
    }
  }
  pub fn map_owned<
    NewLeafData: Clone + PartialEq + Eq + Debug,
    NewInnerData: Clone + PartialEq + Eq + Debug,
  >(
    self,
    leaf_processor: &impl Fn(LeafData, String) -> (NewLeafData, String),
    inner_processor: &impl Fn(InnerData) -> NewInnerData,
  ) -> Ast<NewLeafData, NewInnerData> {
    match self {
      Ast::Leaf(data, label) => {
        let (new_data, new_label) = leaf_processor(data, label);
        Ast::Leaf(new_data, new_label)
      }
      Ast::Inner(data, children) => Ast::Inner(
        inner_processor(data),
        children
          .into_iter()
          .map(|child| child.map_owned(leaf_processor, inner_processor))
          .collect(),
      ),
    }
  }
  pub fn matches_pattern(
    &self,
    pattern: &Self,
    holes: &Vec<(String, bool)>,
    leaf_equivalence_checker: &impl Fn(&LeafData, &LeafData) -> bool,
    inner_equivalence_checker: &impl Fn(&InnerData, &InnerData) -> bool,
  ) -> bool {
    match pattern {
      Ast::Leaf(pattern_data, pattern_leaf) => {
        if holes.iter().find(|(s, _)| s == pattern_leaf).is_some() {
          true
        } else {
          if let Ast::Leaf(data, leaf) = self {
            (leaf == pattern_leaf)
              && leaf_equivalence_checker(data, pattern_data)
          } else {
            false
          }
        }
      }
      Ast::Inner(pattern_data, pattern_children) => {
        if let Ast::Inner(data, children) = self {
          if !inner_equivalence_checker(pattern_data, data) {
            return false;
          }
          for i in 0..pattern_children.len() {
            let pattern_child = &pattern_children[i];
            if let Ast::Leaf(_, pattern_leaf) = pattern_child {
              if let Some(open) = holes
                .iter()
                .find_map(|(s, open)| (s == pattern_leaf).then(|| open))
              {
                if *open {
                  return true;
                }
                continue;
              }
            }
            if children.len() <= i
              || !children[i].matches_pattern(
                pattern_child,
                holes,
                leaf_equivalence_checker,
                inner_equivalence_checker,
              )
            {
              return false;
            }
          }
          pattern_children.len() == children.len()
        } else {
          false
        }
      }
    }
  }
  fn find_leaf_reverse_path(&self, searched_leaf: &str) -> Option<Vec<usize>> {
    match self {
      Ast::Leaf(_, leaf) => {
        if leaf == searched_leaf {
          Some(vec![])
        } else {
          None
        }
      }
      Ast::Inner(_, children) => {
        children.iter().enumerate().find_map(|(i, child)| {
          child.find_leaf_reverse_path(searched_leaf).map(|mut path| {
            path.push(i);
            path
          })
        })
      }
    }
  }
  pub fn find_leaf_path(&self, searched_leaf: &str) -> Option<Vec<usize>> {
    self.find_leaf_reverse_path(searched_leaf).map(|mut path| {
      path.reverse();
      path
    })
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
        for ast in sub_expressions {
          fmt.write_str(separator)?;
          fmt.write_str(&ast.to_string())?;
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
      SyntaxTree::Inner(encloser_or_opener, subtrees) => RawAst::inner({
        let translated_subtrees =
          subtrees.into_iter().map(|subtrees| subtrees.into());
        let tag_str = encloser_or_opener.id_str();
        if tag_str.is_empty() {
          translated_subtrees.collect()
        } else {
          std::iter::once(RawAst::leaf(tag_str.to_string()))
            .chain(translated_subtrees)
            .collect()
        }
      }),
    }
  }
}
