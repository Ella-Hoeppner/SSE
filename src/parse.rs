use std::{
  fmt::{Debug, Display},
  hash::Hash,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ParseError {
  EndOfText,
}

impl Display for ParseError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ParseError::EndOfText => {
        write!(f, "encountered end of text while reading")
      }
    }
  }
}

enum Scope<'s> {
  Enclosed { awaited_closer: &'s str },
  Operated { remaining_args: usize },
}

struct Parse<
  's,
  Tag: Clone + Debug + PartialEq + Eq + Display + Hash,
  E: Encloser<Tag>,
  SE: SymmetricEncloser<Tag>,
  O: Operator<Tag>,
> {
  syntax_graph: &'s SyntaxGraph<Tag, E, SE, O>,
  current_tag: &'s Tag,
  active_tags: &'s [Tag],
  text: &'s str,
  text_index: usize,
  partial_sexps: Vec<(Scope<'s>, Vec<Sexp<'s>>)>,
}

impl<
    's,
    Tag: Clone + Debug + PartialEq + Eq + Display + Hash,
    E: Encloser<Tag>,
    SE: SymmetricEncloser<Tag>,
    O: Operator<Tag>,
  > Parse<'s, Tag, E, SE, O>
{
  fn new(syntax_graph: &'s SyntaxGraph<Tag, E, SE, O>, text: &'s str) -> Self {
    let initial_tag = &syntax_graph.root;
    Self {
      syntax_graph,
      current_tag: initial_tag,
      active_tags: syntax_graph.get_child_tags(initial_tag),
      text,
      text_index: 0,
      partial_sexps: vec![],
    }
  }
  fn close_sexp(&mut self) -> Option<Sexp<'s>> {
    let finished_sexp = Sexp::List(
      self
        .partial_sexps
        .pop()
        .expect("called close_sexp with no open partial sexp")
        .1,
    );
    self.push_sexp(finished_sexp)
  }
  fn push_sexp(&mut self, sexp: Sexp<'s>) -> Option<Sexp<'s>> {
    if let Some((scope, last_partial_sexp)) = self.partial_sexps.last_mut() {
      last_partial_sexp.push(sexp);
      if let Scope::Operated { remaining_args } = scope {
        *remaining_args -= 1;
        if *remaining_args == 0 {
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
  fn complete(&mut self) -> Result<Sexp<'s>, ParseError> {
    let mut indexed_characters = self.text.char_indices();
    loop {
      let (character_index, character) =
        indexed_characters.next().ok_or(ParseError::EndOfText)?;
      if !is_whitespace(character) {
        if let Some((Scope::Enclosed { awaited_closer }, _)) =
          self.partial_sexps.last()
        {
          if self.text[character_index..].starts_with(awaited_closer) {
            let closer_len = awaited_closer.len();
            if let Some(completed_sexp) = self.close_sexp() {
              return Ok(completed_sexp);
            } else {
              indexed_characters.nth(closer_len - 1);
              continue;
            }
          }
        }
        todo!()
      }
    }
  }
}

use crate::{
  sexp::Sexp,
  str_utils::is_whitespace,
  syntax::{Encloser, Operator, SymmetricEncloser, SyntaxGraph},
};

pub struct Parser<
  Tag: Clone + Debug + PartialEq + Eq + Display + Hash,
  E: Encloser<Tag>,
  SE: SymmetricEncloser<Tag>,
  O: Operator<Tag>,
> {
  syntax_graph: SyntaxGraph<Tag, E, SE, O>,
}

impl<
    Tag: Clone + Debug + PartialEq + Eq + Display + Hash,
    E: Encloser<Tag>,
    SE: SymmetricEncloser<Tag>,
    O: Operator<Tag>,
  > Parser<Tag, E, SE, O>
{
  pub fn new(syntax_graph: SyntaxGraph<Tag, E, SE, O>) -> Self {
    Self { syntax_graph }
  }
  pub fn parse<'s>(&'s self, text: &'s str) -> Result<Sexp<'s>, ParseError> {
    Parse::new(&self.syntax_graph, text).complete()
  }
}
