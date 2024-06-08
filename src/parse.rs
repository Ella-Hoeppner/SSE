use std::fmt::{Debug, Display};

use crate::{
  sexp::Sexp,
  str_utils::is_whitespace,
  syntax::{Encloser, Operator, SymmetricEncloser, SyntaxGraph, SyntaxTag},
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

#[derive(Clone, Debug)]
enum TaggedSexp<'s, Tag: SyntaxTag<'s>> {
  Leaf(&'s str),
  List(Tag, Vec<TaggedSexp<'s, Tag>>),
}

struct Parse<
  's,
  Tag: SyntaxTag<'s>,
  E: Encloser<'s, Tag>,
  SE: SymmetricEncloser<'s, Tag>,
  O: Operator<'s, Tag>,
> {
  syntax_graph: &'s SyntaxGraph<'s, Tag, E, SE, O>,
  text: &'s str,
  partial_sexps: Vec<(Scope<'s>, Tag, Vec<TaggedSexp<'s, Tag>>)>,
}

impl<
    's,
    Tag: SyntaxTag<'s>,
    E: Encloser<'s, Tag>,
    SE: SymmetricEncloser<'s, Tag>,
    O: Operator<'s, Tag>,
  > Parse<'s, Tag, E, SE, O>
{
  fn new(
    syntax_graph: &'s SyntaxGraph<'s, Tag, E, SE, O>,
    text: &'s str,
  ) -> Self {
    Self {
      syntax_graph,
      text,
      partial_sexps: vec![],
    }
  }
  fn close_sexp(&mut self) -> Option<TaggedSexp<'s, Tag>> {
    let (_, tag, sub_sexps) = self
      .partial_sexps
      .pop()
      .expect("called close_sexp with no open partial sexp");
    let finished_sexp = TaggedSexp::List(tag, sub_sexps);
    self.push_sexp(finished_sexp)
  }
  fn push_sexp(
    &mut self,
    sexp: TaggedSexp<'s, Tag>,
  ) -> Option<TaggedSexp<'s, Tag>> {
    if let Some((scope, tag, last_partial_sexp)) = self.partial_sexps.last_mut()
    {
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
  fn complete(&mut self) -> Result<TaggedSexp<'s, Tag>, ParseError> {
    let mut indexed_characters = self.text.char_indices().peekable();
    while let Some((character_index, character)) = indexed_characters.next() {
      if !is_whitespace(character) {
        if let Some((Scope::Enclosed { awaited_closer }, _, _)) =
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
        for tag in self.syntax_graph.get_child_tags(
          self
            .partial_sexps
            .last()
            .map(|(_, tag, _)| tag)
            .unwrap_or(&self.syntax_graph.root),
        ) {
          if self.text[character_index..]
            .starts_with(self.syntax_graph.get_beginning_marker(tag))
          {
            if let Some((left_args, right_args)) =
              self.syntax_graph.get_tag_operator_args(tag)
            {
              todo!()
            } else {
              todo!()
            }
          }
        }
        loop {
          match indexed_characters.peek() {
            Some((next_character_index, next_character)) => {
              if is_whitespace(*next_character) {
                self.push_sexp(TaggedSexp::Leaf(
                  &self.text[character_index..*next_character_index],
                ));
                break;
              }
            }
            None => {
              return if self.partial_sexps.is_empty() {
                Ok(TaggedSexp::Leaf(&self.text[character_index..]))
              } else {
                Err(ParseError::EndOfText)
              }
            }
          }
          indexed_characters.next();
        }
      }
    }
    Err(ParseError::EndOfText)
  }
}

pub struct Parser<
  's,
  Tag: SyntaxTag<'s>,
  E: Encloser<'s, Tag>,
  SE: SymmetricEncloser<'s, Tag>,
  O: Operator<'s, Tag>,
> {
  syntax_graph: SyntaxGraph<'s, Tag, E, SE, O>,
}

impl<
    's,
    Tag: SyntaxTag<'s>,
    E: Encloser<'s, Tag>,
    SE: SymmetricEncloser<'s, Tag>,
    O: Operator<'s, Tag>,
  > Parser<'s, Tag, E, SE, O>
{
  pub fn new(syntax_graph: SyntaxGraph<'s, Tag, E, SE, O>) -> Self {
    Self { syntax_graph }
  }
  pub fn parse(&'s self, text: &'s str) -> Result<Sexp<'s>, ParseError> {
    Parse::new(&self.syntax_graph, text)
      .complete()
      .map(|tagged_sexp| tagged_sexp.into())
  }
}

impl<'s, Tag: SyntaxTag<'s>> Into<Sexp<'s>> for TaggedSexp<'s, Tag> {
  fn into(self) -> Sexp<'s> {
    match self {
      TaggedSexp::Leaf(leaf) => Sexp::Leaf(leaf),
      TaggedSexp::List(tag, sub_sexps) => Sexp::List({
        let translated_sub_sexps =
          sub_sexps.into_iter().map(|sub_sexp| sub_sexp.into());
        let tag_str = tag.tag_str();
        if tag_str.is_empty() {
          translated_sub_sexps.collect()
        } else {
          std::iter::once(Sexp::Leaf(&tag_str))
            .chain(translated_sub_sexps)
            .collect()
        }
      }),
    }
  }
}
