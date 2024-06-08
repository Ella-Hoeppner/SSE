use std::fmt::{Debug, Display};

use crate::{
  sexp::{TaggedSexp, TaggedSexpList},
  str_utils::is_whitespace,
  syntax::{
    Encloser, Operator, SymmetricEncloser, SyntaxGraph, SyntaxScope, SyntaxTag,
  },
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ParseError<'s> {
  EndOfText,
  UnexpectedCloser(&'s str),
}

impl<'s> Display for ParseError<'s> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    use ParseError::*;
    match self {
      EndOfText => write!(f, "end of text while parsing"),
      UnexpectedCloser(closer) => {
        write!(f, "unexpected closer {} while parsing", closer)
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
  fn close_sexp(&mut self) -> Option<TaggedSexp<'s, Tag>> {
    let (tag, sub_sexps) = self
      .partial_sexps
      .pop()
      .expect("called close_sexp with no open partial sexp");
    let finished_sexp = TaggedSexp::List((tag, sub_sexps));
    self.push_closed_sexp(finished_sexp)
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
  pub(crate) fn complete(
    &mut self,
  ) -> Result<TaggedSexp<'s, Tag>, ParseError<'s>> {
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
        let open_sexp = self.partial_sexps.last();
        if let Some(SyntaxScope::Enclosed { awaited_closer }) =
          open_sexp.map(|(tag, _subsexps)| self.syntax_graph.get_tag_scope(tag))
        {
          if self.text[character_index..].starts_with(awaited_closer) {
            finish_terminal!();
            let closer_len = awaited_closer.len();
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
            //todo! handle operators consuming args to the left
            self.partial_sexps.push((tag.clone(), vec![]));
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
