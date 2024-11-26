use std::{fmt::Debug, ops::Range};
use take_mut::take;
use unicode_segmentation::UnicodeSegmentation;

use crate::{
  ast::InvalidTreePath,
  syntax::{Context, EncloserOrOperator},
  Ast, Encloser, Operator, ParseError, Parser, RawAst, SyntaxGraph, SyntaxTree,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DocumentPosition {
  pub span: Range<usize>,
  pub path: Vec<usize>,
}
impl DocumentPosition {
  pub fn new(span: Range<usize>, path: Vec<usize>) -> Self {
    Self { span, path }
  }
  pub fn start(&self) -> usize {
    self.span.start
  }
  pub fn end(&self) -> usize {
    self.span.end
  }
}

pub type DocumentSyntaxTree<E, O> =
  Ast<DocumentPosition, (DocumentPosition, EncloserOrOperator<E, O>)>;

impl<E: Encloser, O: Operator> DocumentSyntaxTree<E, O> {
  pub fn position(&self) -> &DocumentPosition {
    match self {
      Ast::Leaf(position, _) => position,
      Ast::Inner((position, _), _) => position,
    }
  }
  pub fn encloses(&self, selection: &Range<usize>) -> bool {
    self.position().start() <= selection.start
      && self.position().end() >= selection.end
  }
  pub fn enclosed_by(&self, selection: &Range<usize>) -> bool {
    self.position().start() >= selection.start
      && self.position().end() <= selection.end
  }
  pub fn calculate_paths(self, parent_path: Vec<usize>) -> Self {
    use Ast::*;
    match self {
      Leaf(DocumentPosition { span, .. }, leaf) => Leaf(
        DocumentPosition {
          span,
          path: parent_path,
        },
        leaf,
      ),
      Inner(
        (DocumentPosition { span, .. }, encloser_or_operator),
        children,
      ) => {
        let children = children
          .into_iter()
          .enumerate()
          .map(|(i, child)| {
            let mut sub_path = parent_path.clone();
            sub_path.push(i);
            child.calculate_paths(sub_path)
          })
          .collect();
        Inner(
          (
            DocumentPosition {
              span,
              path: parent_path,
            },
            encloser_or_operator,
          ),
          children,
        )
      }
    }
  }
  pub fn filter_comments<C: Context>(
    self,
    syntax_graph: &SyntaxGraph<C, E, O>,
  ) -> Option<Self> {
    match self {
      Ast::Inner((span, encloser_or_operator), children) => (!syntax_graph
        .get_context_tag(&encloser_or_operator)
        .is_comment())
      .then(|| {
        Ast::Inner(
          (span, encloser_or_operator),
          children
            .into_iter()
            .filter_map(|child| child.filter_comments(syntax_graph))
            .collect(),
        )
      }),
      leaf => Some(leaf),
    }
  }
}

impl<E: Encloser, O: Operator> From<DocumentSyntaxTree<E, O>>
  for SyntaxTree<E, O>
{
  fn from(tree: DocumentSyntaxTree<E, O>) -> Self {
    match tree {
      DocumentSyntaxTree::Leaf(_, leaf) => SyntaxTree::Leaf((), leaf),
      DocumentSyntaxTree::Inner((_, encloser_or_opener), subtrees) => {
        SyntaxTree::Inner(encloser_or_opener, {
          subtrees.into_iter().map(SyntaxTree::from).collect()
        })
      }
    }
  }
}

impl<E: Encloser, O: Operator> From<DocumentSyntaxTree<E, O>> for RawAst {
  fn from(tree: DocumentSyntaxTree<E, O>) -> Self {
    SyntaxTree::from(tree).into()
  }
}

#[derive(Debug)]
pub struct Document<'t, C: Context, E: Encloser, O: Operator> {
  pub text: &'t str,
  grapheme_indeces: Vec<usize>,
  newline_indeces: Vec<usize>,
  pub syntax_graph: SyntaxGraph<C, E, O>,
  pub syntax_trees: Vec<DocumentSyntaxTree<E, O>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InvalidDocumentIndex;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct InvalidDocumentCharPos;

impl<'t, C: Context, E: Encloser, O: Operator> TryFrom<Parser<'t, C, E, O>>
  for Document<'t, C, E, O>
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
          syntax_trees: syntax_trees
            .into_iter()
            .enumerate()
            .map(|(i, tree)| tree.calculate_paths(vec![i]))
            .collect(),
        }
      })
  }
}

impl<'t, C: Context, E: Encloser, O: Operator> Document<'t, C, E, O> {
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
    let pos = self.get_subtree(path)?.position();
    Ok(
      &self.text
        [self.grapheme_indeces[pos.start()]..self.grapheme_indeces[pos.end()]],
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
      if tree.position().span == *selection {
        if path.len() == 1 {
          if self.syntax_trees.len() < 2 {
            None
          } else {
            Some(
              self.syntax_trees.first().unwrap().position().start()
                ..self.syntax_trees.last().unwrap().position().end(),
            )
          }
        } else {
          let mut path = path;
          path.pop();
          Some(self.get_subtree(&path).unwrap().position().span.clone())
        }
      } else {
        Some(tree.position().span.clone())
      }
    }
  }
  pub fn move_cursor_to_start(&self, selection: &Range<usize>) -> usize {
    if selection.start != selection.end {
      return selection.start;
    }
    let cursor = selection.end;
    let mut enclosing_path = self.innermost_enclosing_path(selection);
    if enclosing_path.is_empty() {
      if let Some(preceding_tree_index) = self
        .syntax_trees
        .iter()
        .enumerate()
        .rev()
        .filter_map(|(i, tree)| (tree.position().end() < cursor).then(|| i))
        .next()
      {
        enclosing_path.push(preceding_tree_index);
      }
    } else {
      let enclosing_subtree = self.get_subtree(&enclosing_path).unwrap();
      let start_of_enclosing = enclosing_subtree.position().start();
      let end_of_enclosing = enclosing_subtree.position().end();
      if cursor == start_of_enclosing {
        if enclosing_path.is_empty() {
          return cursor;
        } else if enclosing_path.last().unwrap() == &0 {
          enclosing_path.pop();
        } else {
          let last_index = enclosing_path.len() - 1;
          enclosing_path[last_index] -= 1;
        }
      } else if cursor != end_of_enclosing {
        return cursor;
      }
    }
    if enclosing_path.is_empty() {
      cursor
    } else {
      self
        .get_subtree(&enclosing_path)
        .unwrap()
        .position()
        .start()
    }
  }
  pub fn move_cursor_to_end(&self, selection: &Range<usize>) -> usize {
    if selection.start != selection.end {
      return selection.end;
    }
    let cursor = selection.end;
    let mut enclosing_path = self.innermost_enclosing_path(selection);
    if enclosing_path.is_empty() {
      if let Some(preceding_tree_index) = self
        .syntax_trees
        .iter()
        .enumerate()
        .filter_map(|(i, tree)| (tree.position().start() > cursor).then(|| i))
        .next()
      {
        enclosing_path.push(preceding_tree_index);
      }
    } else {
      if cursor == self.get_subtree(&enclosing_path).unwrap().position().end() {
        loop {
          match enclosing_path.len() {
            0 => {
              return cursor;
            }
            1 => {
              if enclosing_path[0] < self.syntax_trees.len() - 1 {
                enclosing_path[0] += 1;
              }
            }
            path_length => {
              let parent_subtree = self
                .get_subtree(&enclosing_path[0..path_length - 1])
                .unwrap();
              if let DocumentSyntaxTree::Inner(
                (_, EncloserOrOperator::Operator(_)),
                _,
              ) = parent_subtree
              {
                if parent_subtree.position().end() == cursor {
                  enclosing_path.pop();
                  continue;
                }
              }
              let parent = self
                .get_subtree(&enclosing_path[0..path_length - 1])
                .unwrap();
              let sibling_count = if let Ast::Inner(_, siblings) = parent {
                siblings.len()
              } else {
                unreachable!("uh oh")
              };
              if *enclosing_path.last().unwrap() == sibling_count - 1 {
                enclosing_path.pop();
              } else {
                enclosing_path[path_length - 1] += 1;
              }
            }
          }
          break;
        }
      }
    }
    if enclosing_path.is_empty() {
      cursor
    } else {
      self.get_subtree(&enclosing_path).unwrap().position().end()
    }
  }
  pub fn row_and_col_to_index(
    &self,
    row: usize,
    col: usize,
  ) -> Result<usize, InvalidDocumentCharPos> {
    if row == 0 {
      if (self.newline_indeces.is_empty() && col <= self.text.len())
        || (!self.newline_indeces.is_empty() && col <= self.newline_indeces[0])
      {
        Ok(col)
      } else {
        Err(InvalidDocumentCharPos)
      }
    } else {
      if row == self.newline_indeces.len() {
        let last_line_start = self.newline_indeces.last().unwrap();
        let index = last_line_start + 1 + col;
        if index <= self.text.len() {
          Ok(index)
        } else {
          Err(InvalidDocumentCharPos)
        }
      } else if row < self.newline_indeces.len() {
        let previous_line_start = self.newline_indeces[row - 1];
        let line_length = self.newline_indeces[row] - previous_line_start;
        if col < line_length {
          Ok(previous_line_start + 1 + col)
        } else {
          Err(InvalidDocumentCharPos)
        }
      } else {
        Err(InvalidDocumentCharPos)
      }
    }
  }
  pub fn index_to_row_and_col(
    &self,
    index: usize,
  ) -> Result<(usize, usize), InvalidDocumentIndex> {
    if index <= self.text.len() {
      Ok(
        self
          .newline_indeces
          .iter()
          .copied()
          .enumerate()
          .rev()
          .find_map(|(line_index, previous_newline_index)| {
            (previous_newline_index < index)
              .then(|| (line_index + 1, index - previous_newline_index - 1))
          })
          .unwrap_or((0, index)),
      )
    } else {
      Err(InvalidDocumentIndex)
    }
  }
  pub fn strip_comments(&mut self) {
    take(&mut self.syntax_trees, |syntax_trees| {
      syntax_trees
        .into_iter()
        .filter_map(|tree| tree.filter_comments(&self.syntax_graph))
        .collect()
    })
  }
}
