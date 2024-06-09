use std::fmt::{Debug, Display};

use crate::{
  sexp::{TaggedSexp, TaggedSexpList},
  str_utils::is_whitespace,
  syntax::{
    Encloser, Operator, SymmetricEncloser, SyntaxGraph, SyntaxScope, SyntaxTag,
  },
};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ParseError {
  EndOfText,
  UnexpectedCloser(String),
  MissingLeftArgument,
  MissingRightArgument,
}

impl Display for ParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use ParseError::*;
    match self {
      EndOfText => write!(f, "end of text while parsing"),
      UnexpectedCloser(closer) => {
        write!(f, "unexpected closer {} while parsing", closer)
      }
      MissingLeftArgument => {
        write!(f, "operator missing left argument")
      }
      MissingRightArgument => {
        write!(f, "operator missing right argument")
      }
    }
  }
}

pub(crate) struct Parse<
  's,
  Tag: SyntaxTag<'s>,
  E: Encloser<'s, Tag>,
  SE: SymmetricEncloser<'s, Tag>,
  O: Operator<'s, Tag>,
> {
  text: &'s str,
  syntax_graph: &'s SyntaxGraph<'s, Tag, E, SE, O>,
  partial_sexps: Vec<TaggedSexpList<'s, Tag>>,
}

impl<
    's,
    Tag: SyntaxTag<'s>,
    E: Encloser<'s, Tag>,
    SE: SymmetricEncloser<'s, Tag>,
    O: Operator<'s, Tag>,
  > Parse<'s, Tag, E, SE, O>
{
  pub(crate) fn new(
    syntax_graph: &'s SyntaxGraph<'s, Tag, E, SE, O>,
    text: &'s str,
  ) -> Self {
    Self {
      text,
      syntax_graph,
      partial_sexps: vec![],
    }
  }
  fn consume_left_sexps(
    &mut self,
    n: usize,
  ) -> Result<Vec<TaggedSexp<'s, Tag>>, ParseError> {
    if n == 0 {
      Ok(vec![])
    } else {
      if let Some((_, subsexps)) = self.partial_sexps.last_mut() {
        if subsexps.len() >= n {
          Ok(subsexps.split_off(subsexps.len() - n))
        } else {
          Err(ParseError::MissingLeftArgument)
        }
      } else {
        todo!()
      }
    }
  }
  fn close_sexp(&mut self) -> Option<TaggedSexp<'s, Tag>> {
    let (tag, sub_sexps) = self
      .partial_sexps
      .pop()
      .expect("called close_sexp with no open partial sexp");
    let finished_sexp = TaggedSexp::List((tag, sub_sexps));
    self.push_closed_sexp(finished_sexp)
  }
  fn is_scope_operator(&self) -> Option<bool> {
    self.partial_sexps.last().map(|(tag, subsexps)| {
      match self.syntax_graph.get_tag_scope(tag) {
        SyntaxScope::Enclosed { .. } => false,
        SyntaxScope::Operated { .. } => true,
      }
    })
  }
  fn push_closed_sexp(
    &mut self,
    sexp: TaggedSexp<'s, Tag>,
  ) -> Option<TaggedSexp<'s, Tag>> {
    if let Some((tag, subsexps)) = self.partial_sexps.last_mut() {
      subsexps.push(sexp);
      if let SyntaxScope::Operated {
        left_args,
        right_args,
      } = self.syntax_graph.get_tag_scope(tag)
      {
        if subsexps.len() == left_args + right_args {
          self.close_sexp()
        } else {
          None
        }
      } else {
        None
      }
    } else {
      Some(sexp)
    }
  }
  fn awaited_closer(&self) -> Option<&'s str> {
    self
      .partial_sexps
      .iter()
      .rev()
      .filter_map(|open_sexp| {
        if let SyntaxScope::Enclosed { awaited_closer } =
          self.syntax_graph.get_tag_scope(&open_sexp.0)
        {
          return Some(awaited_closer);
        } else {
          None
        }
      })
      .next()
  }
  pub(crate) fn complete(&mut self) -> Result<TaggedSexp<'s, Tag>, ParseError> {
    let mut current_terminal_beginning: Option<usize> = None;
    let mut indexed_characters = self
      .text
      .char_indices()
      .chain(std::iter::once((self.text.len(), ' ')))
      .peekable();
    'outer: while let Some((character_index, character)) =
      indexed_characters.next()
    {
      macro_rules! finish_terminal {
        () => {
          if let Some(terminal_beginning) = current_terminal_beginning {
            if let Some(completed_sexp) = self.push_closed_sexp(
              TaggedSexp::Leaf(&self.text[terminal_beginning..character_index]),
            ) {
              return Ok(completed_sexp);
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
      if is_whitespace(character) {
        finish_terminal!();
      } else {
        if let Some(awaited_closer) = self.awaited_closer() {
          if self.text[character_index..].starts_with(awaited_closer) {
            finish_terminal!();
            let closer_len = awaited_closer.len();
            if self.is_scope_operator() == Some(true) {
              return Err(ParseError::MissingRightArgument);
            }
            if let Some(completed_sexp) = self.close_sexp() {
              return Ok(completed_sexp);
            } else {
              skip_n_chars!(closer_len);
              continue;
            }
          }
        }
        for tag in self.syntax_graph.get_child_tags(
          self
            .partial_sexps
            .last()
            .map(|(tag, _)| tag)
            .unwrap_or(&self.syntax_graph.root),
        ) {
          let beginning_marker = self.syntax_graph.get_beginning_marker(tag);
          if self.text[character_index..].starts_with(beginning_marker) {
            finish_terminal!();
            let leftward_args = self
              .consume_left_sexps(self.syntax_graph.get_left_arg_count(tag))?;
            self.partial_sexps.push((tag.clone(), leftward_args));
            skip_n_chars!(beginning_marker.len());
            continue 'outer;
          }
        }
        if current_terminal_beginning.is_none() {
          current_terminal_beginning = Some(character_index)
        }
      }
    }
    Err(ParseError::EndOfText)
  }
}
