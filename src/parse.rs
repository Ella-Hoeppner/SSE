use std::fmt::{Debug, Display};

use unicode_segmentation::UnicodeSegmentation;

use crate::{
  document::{DocumentPosition, DocumentSyntaxTree},
  syntax::{Context, Encloser, EncloserOrOperator, Operator, SyntaxGraph},
};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum ParseError {
  EndOfTextWithOpenEncloser(String),
  UnexpectedCloser(String),
  OperatorMissingLeftArgument(String),
  OperatorMissingRightArgument(String),
}

impl Display for ParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use ParseError::*;
    match self {
      EndOfTextWithOpenEncloser(opener) => write!(
        f,
        "texted ended with encloser open, starting with \"{opener}\""
      ),
      UnexpectedCloser(closer) => {
        write!(f, "unexpected closer {} while parsing", closer)
      }
      OperatorMissingLeftArgument(operator) => {
        write!(f, "operator \"{operator}\" missing left argument")
      }
      OperatorMissingRightArgument(operator) => {
        write!(f, "operator \"{operator}\" missing right argument")
      }
    }
  }
}

pub(crate) struct Parse<'t, 'g, C: Context, E: Encloser, O: Operator> {
  text: &'t str,
  inherited_top_level_asts: Vec<DocumentSyntaxTree<E, O>>,
  syntax_graph: &'g SyntaxGraph<C, E, O>,
  open_asts: Vec<(
    usize,
    EncloserOrOperator<E, O>,
    Vec<DocumentSyntaxTree<E, O>>,
  )>,
}

impl<'t, 'g, C: Context, E: Encloser, O: Operator> Parse<'t, 'g, C, E, O> {
  pub(crate) fn new(
    syntax_graph: &'g SyntaxGraph<C, E, O>,
    inherited_top_level_asts: Vec<DocumentSyntaxTree<E, O>>,
    text: &'t str,
  ) -> Self {
    Self {
      text,
      inherited_top_level_asts,
      syntax_graph,
      open_asts: vec![],
    }
  }
  fn consume_left_asts(
    &mut self,
    operator: &O,
  ) -> Result<Vec<DocumentSyntaxTree<E, O>>, ParseError> {
    let n = operator.left_args();
    if n == 0 {
      Ok(vec![])
    } else {
      if let Some((_, _, subtrees)) = self.open_asts.last_mut() {
        if subtrees.len() >= n {
          Ok(subtrees.split_off(subtrees.len() - n))
        } else {
          Err(ParseError::OperatorMissingLeftArgument(
            operator.op_str().to_string(),
          ))
        }
      } else {
        if self.inherited_top_level_asts.len() >= n {
          Ok(
            self
              .inherited_top_level_asts
              .split_off(self.inherited_top_level_asts.len() - n),
          )
        } else {
          Err(ParseError::OperatorMissingLeftArgument(
            operator.op_str().to_string(),
          ))
        }
      }
    }
  }
  fn close_ast(
    &mut self,
    closing_index: usize,
  ) -> Option<DocumentSyntaxTree<E, O>> {
    let (opening_index, encloser_or_operator, subtrees) = self
      .open_asts
      .pop()
      .expect("called close_ast with no open partial Ast");
    self.push_closed_ast(DocumentSyntaxTree::Inner(
      (
        DocumentPosition::new(opening_index..closing_index, vec![]),
        encloser_or_operator,
      ),
      subtrees,
    ))
  }
  fn push_closed_ast(
    &mut self,
    ast: DocumentSyntaxTree<E, O>,
  ) -> Option<DocumentSyntaxTree<E, O>> {
    if let Some((_, encloser_or_operator, subtrees)) = self.open_asts.last_mut()
    {
      let end = ast.position().end();
      subtrees.push(ast);
      if let EncloserOrOperator::Operator(operator) = encloser_or_operator {
        let left_args = operator.left_args();
        let right_args = operator.right_args();
        if subtrees.len() == left_args + right_args {
          self.close_ast(end)
        } else {
          None
        }
      } else {
        None
      }
    } else {
      Some(ast)
    }
  }
  fn awaited_closer(&'g self) -> Option<&'g str> {
    self
      .open_asts
      .iter()
      .rev()
      .filter_map(|(_, encloser_or_operator, _)| {
        if let EncloserOrOperator::Encloser(encloser) = encloser_or_operator {
          Some(encloser.closing_encloser_str())
        } else {
          None
        }
      })
      .next()
  }
  pub(crate) fn complete(
    mut self,
    already_parsed_index: usize,
  ) -> Result<
    Result<Vec<DocumentSyntaxTree<E, O>>, Vec<DocumentSyntaxTree<E, O>>>,
    ParseError,
  > {
    let beginning_index = self
      .inherited_top_level_asts
      .last()
      .map(|syntax_tree| syntax_tree.position().end())
      .unwrap_or(already_parsed_index);
    if beginning_index >= self.text.len() {
      return Ok(Err(self.inherited_top_level_asts));
    }

    let mut indexed_characters = self.text[beginning_index..]
      .grapheme_indices(true)
      .chain(std::iter::once((self.text.len() - beginning_index, " ")))
      .peekable();

    let mut current_terminal_beginning: Option<usize> = None;
    let mut escaped = false;

    'outer: while let Some((character_index_offset, character)) =
      indexed_characters.next()
    {
      let character_index = beginning_index + character_index_offset;

      macro_rules! finish_terminal {
        () => {
          if let Some(terminal_beginning) = current_terminal_beginning {
            if let Some(completed_ast) =
              self.push_closed_ast(DocumentSyntaxTree::Leaf(
                DocumentPosition::new(
                  terminal_beginning..character_index,
                  vec![],
                ),
                self.text[terminal_beginning..character_index].to_string(),
              ))
            {
              let mut top_level_asts = self.inherited_top_level_asts;
              top_level_asts.push(completed_ast);
              return Ok(Ok(top_level_asts));
            }
            current_terminal_beginning = None;
          }
        };
      }
      macro_rules! skip_n_chars {
        ($n:expr) => {
          if $n > 1 {
            indexed_characters.nth($n - 2);
          }
        };
      }

      let active_context = self.syntax_graph.get_context(
        self
          .open_asts
          .last()
          .map(|(_, tag, _)| self.syntax_graph.get_context_tag(tag))
          .unwrap_or(&self.syntax_graph.root),
      );

      if escaped {
        escaped = false;
      } else if active_context
        .escape_char
        .as_ref()
        .map(|char| char.as_str())
        == Some(character)
      {
        escaped = true;
        current_terminal_beginning =
          current_terminal_beginning.or(Some(character_index));
      } else if active_context.is_whitespace(character) {
        finish_terminal!();
      } else {
        let remaining_text = &self.text[character_index..];

        if let Some(awaited_closer) = self.awaited_closer() {
          if remaining_text.starts_with(awaited_closer) {
            let closer_len = awaited_closer.len();
            finish_terminal!();

            if let Some((_, encloser_or_operator, _)) = self.open_asts.last() {
              if let EncloserOrOperator::Operator(operator) =
                encloser_or_operator
              {
                return Err(ParseError::OperatorMissingRightArgument(
                  operator.op_str().to_string(),
                ));
              }
            }

            if let Some(completed_ast) = self.close_ast(character_index + 1) {
              let mut top_level_asts = self.inherited_top_level_asts;
              top_level_asts.push(completed_ast);
              return Ok(Ok(top_level_asts));
            } else {
              skip_n_chars!(closer_len);
              continue;
            }
          }
        }

        let active_context_tag = self
          .open_asts
          .last()
          .map(|(_, tag, _)| self.syntax_graph.get_context_tag(tag))
          .unwrap_or(&self.syntax_graph.root);

        for encloser in self
          .syntax_graph
          .get_context(active_context_tag)
          .enclosers()
        {
          let beginning_marker = encloser.opening_encloser_str();
          if remaining_text.starts_with(beginning_marker) {
            finish_terminal!();
            self.open_asts.push((
              character_index,
              EncloserOrOperator::Encloser(encloser.clone()),
              vec![],
            ));
            skip_n_chars!(beginning_marker.len());
            continue 'outer;
          }
        }

        for operator in self
          .syntax_graph
          .get_context(active_context_tag)
          .operators()
        {
          let op_marker = operator.op_str();
          if remaining_text.starts_with(op_marker) {
            finish_terminal!();
            let leftward_args = self.consume_left_asts(operator)?;
            self.open_asts.push((
              leftward_args
                .first()
                .map(|first_arg| first_arg.position().start())
                .unwrap_or(character_index),
              EncloserOrOperator::Operator(operator.clone()),
              leftward_args,
            ));
            skip_n_chars!(op_marker.len());
            if operator.right_args() == 0 {
              if let Some(completed_ast) = self.close_ast(character_index + 1) {
                let mut top_level_asts = self.inherited_top_level_asts;
                top_level_asts.push(completed_ast);
                return Ok(Ok(top_level_asts));
              }
            }
            continue 'outer;
          }
        }

        for closer in self.syntax_graph.get_closers(active_context_tag) {
          if remaining_text.starts_with(closer) {
            return Err(ParseError::UnexpectedCloser(closer.to_string()));
          }
        }

        if current_terminal_beginning.is_none() {
          current_terminal_beginning = Some(character_index)
        }
      }
    }

    match self.open_asts.last() {
      None => Ok(Err(self.inherited_top_level_asts)),
      Some((_, encloser_or_operator, _)) => match encloser_or_operator {
        EncloserOrOperator::Encloser(encloser) => {
          Err(ParseError::EndOfTextWithOpenEncloser(
            encloser.opening_encloser_str().to_string(),
          ))
        }
        EncloserOrOperator::Operator(operator) => {
          Err(ParseError::OperatorMissingRightArgument(
            operator.op_str().to_string(),
          ))
        }
      },
    }
  }
}
