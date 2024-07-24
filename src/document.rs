use std::{fmt::Debug, hash::Hash, ops::Range};

use crate::{DocumentSyntaxTree, Encloser, Operator, ParseError, Parser};

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
  pub fn innermost_predicate_path(
    &self,
    predicate: &impl Fn(&DocumentSyntaxTree<E, O>) -> bool,
  ) -> Vec<usize> {
    self
      .syntax_trees
      .iter()
      .enumerate()
      .find_map(|(i, syntax_tree)| {
        syntax_tree.reverse_innermost_predicate_path(predicate).map(
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
    self.innermost_predicate_path(&|tree| tree.encloses_selection(selection))
  }
}
