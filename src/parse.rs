use std::{
  fmt::{Debug, Display},
  ops::Range,
};

use unicode_segmentation::UnicodeSegmentation;

use crate::{
  document::{Document, DocumentPosition, DocumentSyntaxTree},
  syntax::{Encloser, EncloserOrOperator, Operator, Syntax},
};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum ParseErrorKind {
  EndOfTextWithOpenEncloser(String),
  UnexpectedCloser(String),
  OperatorMissingLeftArgument(String),
  OperatorMissingRightArgument(String),
}

impl Display for ParseErrorKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use ParseErrorKind::*;
    match self {
      EndOfTextWithOpenEncloser(opener) => {
        write!(f, "Opener \"{opener}\" was never closed")
      }
      UnexpectedCloser(closer) => {
        write!(f, "Encountered unmatched closer \"{closer}\"")
      }
      OperatorMissingLeftArgument(operator) => {
        write!(f, "Operator \"{operator}\" is missing its left argument")
      }
      OperatorMissingRightArgument(operator) => {
        write!(f, "Operator \"{operator}\" is missing its right argument")
      }
    }
  }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct ParseError {
  pub pos: Range<usize>,
  pub kind: ParseErrorKind,
}

impl ParseError {
  pub fn describe(&self, document: &Document<'_, impl Syntax>) -> String {
    document.describe_document_position(self.pos.clone())
      + "\n"
      + &format!("{}", self.kind)
  }
}

pub struct OpenAst<S: Syntax> {
  opening_index: usize,
  encloser_or_operator_index: usize,
  encloser_or_operator: EncloserOrOperator<S::E, S::O>,
  subtrees: Vec<DocumentSyntaxTree<S::E, S::O>>,
}

pub(crate) struct Parse<'t, 's, S: Syntax> {
  text: &'t str,
  syntax: &'s S,
  inherited_top_level_asts: Vec<DocumentSyntaxTree<S::E, S::O>>,
  open_asts: Vec<OpenAst<S>>,
}

impl<'t, 's, S: Syntax> Parse<'t, 's, S> {
  pub(crate) fn new(
    inherited_top_level_asts: Vec<DocumentSyntaxTree<S::E, S::O>>,
    syntax: &'s S,
    text: &'t str,
  ) -> Self {
    Self {
      text,
      syntax,
      inherited_top_level_asts,
      open_asts: vec![],
    }
  }
  fn active_context(&self) -> S::C {
    self
      .open_asts
      .last()
      .map(
        |OpenAst {
           encloser_or_operator,
           ..
         }| {
          self
            .syntax
            .enclose_or_operator_context(encloser_or_operator)
        },
      )
      .unwrap_or(self.syntax.root_context())
  }
  fn consume_left_asts(
    &mut self,
    operator: &S::O,
    character_index: usize,
  ) -> Result<Vec<DocumentSyntaxTree<S::E, S::O>>, ParseError> {
    let n = operator.left_args();
    if n == 0 {
      Ok(vec![])
    } else {
      if let Some(OpenAst { subtrees, .. }) = self.open_asts.last_mut() {
        if subtrees.len() >= n {
          Ok(subtrees.split_off(subtrees.len() - n))
        } else {
          Err(ParseError {
            kind: ParseErrorKind::OperatorMissingLeftArgument(
              operator.op_str().to_string(),
            ),
            pos: (character_index)..(character_index + operator.op_str().len()),
          })
        }
      } else {
        if self.inherited_top_level_asts.len() >= n {
          Ok(
            self
              .inherited_top_level_asts
              .split_off(self.inherited_top_level_asts.len() - n),
          )
        } else {
          Err(ParseError {
            kind: ParseErrorKind::OperatorMissingLeftArgument(
              operator.op_str().to_string(),
            ),
            pos: (character_index)..(character_index + operator.op_str().len()),
          })
        }
      }
    }
  }
  fn close_ast(
    &mut self,
    closing_index: usize,
  ) -> Option<DocumentSyntaxTree<S::E, S::O>> {
    let OpenAst {
      opening_index,
      encloser_or_operator,
      subtrees,
      ..
    } = self
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
    ast: DocumentSyntaxTree<S::E, S::O>,
  ) -> Option<DocumentSyntaxTree<S::E, S::O>> {
    if let Some(OpenAst {
      encloser_or_operator,
      subtrees,
      ..
    }) = self.open_asts.last_mut()
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
  fn awaited_closer<'a>(&'a self) -> Option<&'a str> {
    self
      .open_asts
      .iter()
      .rev()
      .filter_map(
        |OpenAst {
           encloser_or_operator,
           ..
         }| {
          if let EncloserOrOperator::Encloser(encloser) = encloser_or_operator {
            Some(encloser.closing_encloser_str())
          } else {
            None
          }
        },
      )
      .next()
  }
  pub(crate) fn complete(
    mut self,
    already_parsed_index: usize,
  ) -> Result<
    Result<
      Vec<DocumentSyntaxTree<S::E, S::O>>,
      Vec<DocumentSyntaxTree<S::E, S::O>>,
    >,
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

      macro_rules! push_ast_and_maybe_return {
        ($ast:expr) => {
          if let Some(completed_ast) = self.push_closed_ast($ast) {
            let mut top_level_asts = self.inherited_top_level_asts;
            top_level_asts.push(completed_ast);
            return Ok(Ok(top_level_asts));
          }
        };
      }

      macro_rules! finish_terminal {
        () => {
          if let Some(terminal_beginning) = current_terminal_beginning {
            push_ast_and_maybe_return!(DocumentSyntaxTree::Leaf(
              DocumentPosition::new(
                terminal_beginning..character_index,
                vec![],
              ),
              self.text[terminal_beginning..character_index].to_string(),
            ));
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

      let active_context = self.syntax.context(&self.active_context());

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

        let active_context_tag = self.active_context();

        let mut matched_reserved = false;
        for reserved_token in self.syntax.reserved_tokens() {
          if remaining_text.starts_with(reserved_token) {
            push_ast_and_maybe_return!(DocumentSyntaxTree::Leaf(
              DocumentPosition::new(
                character_index..character_index + reserved_token.len(),
                vec![],
              ),
              reserved_token.to_string(),
            ));
            skip_n_chars!(reserved_token.len());
            matched_reserved = true;
          }
        }
        if matched_reserved {
          continue;
        }

        if let Some(awaited_closer) = self.awaited_closer() {
          if remaining_text.starts_with(awaited_closer) {
            let closer_len = awaited_closer.len();
            finish_terminal!();

            if let Some(OpenAst {
              encloser_or_operator,
              encloser_or_operator_index,
              ..
            }) = self.open_asts.last()
            {
              if let EncloserOrOperator::Operator(operator) =
                encloser_or_operator
              {
                return Err(ParseError {
                  kind: ParseErrorKind::OperatorMissingRightArgument(
                    operator.op_str().to_string(),
                  ),
                  pos: *encloser_or_operator_index
                    ..(encloser_or_operator_index + operator.op_str().len()),
                });
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

        for encloser in self.syntax.context(&active_context_tag).enclosers() {
          let beginning_marker = encloser.opening_encloser_str();
          if remaining_text.starts_with(beginning_marker) {
            finish_terminal!();
            self.open_asts.push(OpenAst {
              opening_index: character_index,
              encloser_or_operator_index: character_index,
              encloser_or_operator: EncloserOrOperator::Encloser(
                encloser.clone(),
              ),
              subtrees: vec![],
            });
            skip_n_chars!(beginning_marker.len());
            continue 'outer;
          }
        }

        for operator in self.syntax.context(&active_context_tag).operators() {
          let op_marker = operator.op_str();
          if remaining_text.starts_with(op_marker) {
            finish_terminal!();
            let leftward_args =
              self.consume_left_asts(operator, character_index)?;
            self.open_asts.push(OpenAst {
              opening_index: leftward_args
                .first()
                .map(|first_arg| first_arg.position().start())
                .unwrap_or(character_index),
              encloser_or_operator: EncloserOrOperator::Operator(
                operator.clone(),
              ),
              encloser_or_operator_index: character_index,
              subtrees: leftward_args,
            });
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

        if let Some(closer) = self
          .syntax
          .prefix_closer_in_context(&active_context_tag, remaining_text)
        {
          return Err(ParseError {
            kind: ParseErrorKind::UnexpectedCloser(closer.to_string()),
            pos: character_index..(character_index + closer.len()),
          });
        }

        if current_terminal_beginning.is_none() {
          current_terminal_beginning = Some(character_index)
        }
      }
    }

    match self.open_asts.last() {
      None => Ok(Err(self.inherited_top_level_asts)),
      Some(OpenAst {
        encloser_or_operator,
        encloser_or_operator_index,
        ..
      }) => match encloser_or_operator {
        EncloserOrOperator::Encloser(encloser) => Err(ParseError {
          kind: ParseErrorKind::EndOfTextWithOpenEncloser(
            encloser.opening_encloser_str().to_string(),
          ),
          pos: (*encloser_or_operator_index)
            ..(*encloser_or_operator_index
              + encloser.opening_encloser_str().len()),
        }),
        EncloserOrOperator::Operator(operator) => Err(ParseError {
          kind: ParseErrorKind::OperatorMissingRightArgument(
            operator.op_str().to_string(),
          ),
          pos: (*encloser_or_operator_index)
            ..(*encloser_or_operator_index + operator.op_str().len()),
        }),
      },
    }
  }
}
