use std::{fmt::Debug, hash::Hash, ops::Range};
use unicode_segmentation::UnicodeSegmentation;

use crate::{
  ast::InvalidTreePath, DocumentSyntaxTree, Encloser, Operator, ParseError,
  Parser, SyntaxGraph,
};

pub struct Document<
  't,
  C: Clone + Debug + PartialEq + Eq + Hash,
  E: Encloser,
  O: Operator,
> {
  text: &'t str,
  grapheme_indeces: Vec<usize>,
  newline_indeces: Vec<usize>,
  syntax_graph: SyntaxGraph<C, E, O>,
  syntax_trees: Vec<DocumentSyntaxTree<E, O>>,
}

impl<'t, C: Clone + Debug + PartialEq + Eq + Hash, E: Encloser, O: Operator>
  TryFrom<Parser<'t, C, E, O>> for Document<'t, C, E, O>
{
  type Error = ParseError;

  fn try_from(mut parser: Parser<'t, C, E, O>) -> Result<Self, ParseError> {
    parser
      .read_all()
      .into_iter()
      .collect::<Result<Vec<_>, ParseError>>()
      .map(|syntax_trees| {
        let (mut grapheme_indeces, newline_indeces) =
          parser.text.grapheme_indices(true).fold(
            (vec![], vec![]),
            |(mut grapheme_indeces, mut newline_indeces), (i, char)| {
              grapheme_indeces.push(i);
              if char == "\n" {
                newline_indeces.push(i);
              }
              (grapheme_indeces, newline_indeces)
            },
          );
        grapheme_indeces.push(parser.text.len());
        Self {
          text: parser.text,
          grapheme_indeces,
          newline_indeces,
          syntax_graph: parser.syntax_graph,
          syntax_trees,
        }
      })
  }
}

impl<'t, C: Clone + Debug + PartialEq + Eq + Hash, E: Encloser, O: Operator>
  Document<'t, C, E, O>
{
  pub fn from_text_with_syntax(
    syntax_graph: SyntaxGraph<C, E, O>,
    text: &'t str,
  ) -> Result<Self, ParseError> {
    Parser::new(syntax_graph, text).try_into()
  }
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
  pub fn get_subtree_text(
    &self,
    path: &[usize],
  ) -> Result<&'t str, InvalidTreePath> {
    let range = self.get_subtree(path)?.range();
    Ok(
      &self.text
        [self.grapheme_indeces[range.start]..self.grapheme_indeces[range.end]],
    )
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
}
