use std::{fmt::Debug, hash::Hash, ops::Range};

use crate::{
  ast::InvalidTreePath, DocumentSyntaxTree, Encloser, Operator, ParseError,
  Parser,
};

pub struct Document<E: Encloser, O: Operator> {
  syntax_trees: Vec<DocumentSyntaxTree<E, O>>,
}

impl<
    ContextTag: Clone + Debug + PartialEq + Eq + Hash,
    E: Encloser,
    O: Operator,
  > TryFrom<Parser<'_, ContextTag, E, O>> for Document<E, O>
{
  type Error = ParseError;

  fn try_from(
    mut parser: Parser<'_, ContextTag, E, O>,
  ) -> Result<Self, ParseError> {
    parser
      .read_all()
      .into_iter()
      .collect::<Result<Vec<_>, ParseError>>()
      .map(|syntax_trees| Self { syntax_trees })
  }
}

impl<E: Encloser, O: Operator> Document<E, O> {
  pub fn get_subtree(
    &self,
    path: &[usize],
  ) -> Result<&DocumentSyntaxTree<E, O>, InvalidTreePath> {
    let mut path_iter = path.iter().copied();
    if let Some(top_level_tree_index) = path_iter.next() {
      if let Some(top_level_tree) = self.syntax_trees.get(top_level_tree_index)
      {
        top_level_tree.get_subtree_inner(path_iter)
      } else {
        Err(InvalidTreePath)
      }
    } else {
      Err(InvalidTreePath)
    }
  }
  pub fn innermost_predicate_path(
    &self,
    predicate: &impl Fn(&DocumentSyntaxTree<E, O>) -> bool,
  ) -> Vec<usize> {
    self
      .syntax_trees
      .iter()
      .enumerate()
      .find_map(|(i, syntax_tree)| {
        syntax_tree.innermost_predicate_reverse_path(predicate).map(
          |mut reverse_path| {
            reverse_path.push(i);
            reverse_path.reverse();
            reverse_path
          },
        )
      })
      .unwrap_or(vec![])
  }
  pub fn innermost_enclosing_path(
    &self,
    selection: &Range<usize>,
  ) -> Vec<usize> {
    self.innermost_predicate_path(&|tree| tree.encloses(selection))
  }
  pub fn expand_selection(
    &self,
    selection: &Range<usize>,
  ) -> Option<Range<usize>> {
    let path = self.innermost_enclosing_path(selection);
    if path.is_empty() {
      None
    } else {
      let tree = self.get_subtree(&path).unwrap();
      if tree.range() == selection {
        if path.len() == 1 {
          if self.syntax_trees.len() < 2 {
            None
          } else {
            Some(
              self.syntax_trees.first().unwrap().range().start
                ..self.syntax_trees.last().unwrap().range().end,
            )
          }
        } else {
          let mut path = path;
          path.pop();
          Some(self.get_subtree(&path).unwrap().range().clone())
        }
      } else {
        Some(tree.range().clone())
      }
    }
  }
  /*pub fn outermost_enclosed_paths(
    &self,
    selection: &Range<usize>,
  ) -> Vec<Vec<usize>> {
    self
      .syntax_trees
      .iter()
      .enumerate()
      .map(|(i, tree)| {
        tree
          .outermost_enclosed_reverse_paths(selection)
          .into_iter()
          .map(move |mut reverse_path| {
            reverse_path.push(i);
            reverse_path.reverse();
            reverse_path
          })
      })
      .flatten()
      .collect()
  }*/
}
