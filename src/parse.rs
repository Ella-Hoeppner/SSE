use std::{
  fmt::{Debug, Display},
  hash::Hash,
};

use crate::{
  sexp::{TaggedSexp, TaggedSexpList},
  syntax::{
    Encloser, Operator, SymmetricEncloser, SyntaxElement, SyntaxGraph,
    SyntaxTag,
  },
};

#[derive(Clone, PartialEq, Eq, Debug)]
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

pub(crate) struct Parse<
  't,
  'g,
  Tag: SyntaxTag,
  ContextTag: Clone + Debug + PartialEq + Eq + Hash,
  E: Encloser<Tag>,
  SE: SymmetricEncloser<Tag>,
  O: Operator<Tag>,
> {
  text: &'t str,
  inherited_top_level_sexps: Vec<(TaggedSexp<Tag>, usize)>,
  syntax_graph: &'g SyntaxGraph<Tag, ContextTag, E, SE, O>,
  open_sexps: Vec<TaggedSexpList<Tag>>,
}

impl<
    't,
    'g,
    Tag: SyntaxTag,
    ContextTag: Clone + Debug + PartialEq + Eq + Hash,
    E: Encloser<Tag>,
    SE: SymmetricEncloser<Tag>,
    O: Operator<Tag>,
  > Parse<'t, 'g, Tag, ContextTag, E, SE, O>
{
  pub(crate) fn new(
    syntax_graph: &'g SyntaxGraph<Tag, ContextTag, E, SE, O>,
    inherited_top_level_sexps: Vec<(TaggedSexp<Tag>, usize)>,
    text: &'t str,
  ) -> Self {
    Self {
      text,
      inherited_top_level_sexps,
      syntax_graph,
      open_sexps: vec![],
    }
  }
  fn consume_left_sexps(
    &mut self,
    tag: &Tag,
  ) -> Result<Vec<TaggedSexp<Tag>>, ParseError> {
    match self.syntax_graph.get_tag_element(tag) {
      SyntaxElement::Operator(operator) => {
        let n = operator.left_args();
        if n == 0 {
          Ok(vec![])
        } else {
          if let Some((_, subsexps)) = self.open_sexps.last_mut() {
            if subsexps.len() >= n {
              Ok(subsexps.split_off(subsexps.len() - n))
            } else {
              Err(ParseError::OperatorMissingLeftArgument(
                operator.op_str().to_string(),
              ))
            }
          } else {
            if self.inherited_top_level_sexps.len() >= n {
              Ok(
                self
                  .inherited_top_level_sexps
                  .split_off(self.inherited_top_level_sexps.len() - n)
                  .into_iter()
                  .map(|(sexp, _)| sexp)
                  .collect(),
              )
            } else {
              Err(ParseError::OperatorMissingLeftArgument(
                operator.op_str().to_string(),
              ))
            }
          }
        }
      }
      _ => Ok(vec![]),
    }
  }
  fn close_sexp(&mut self) -> Option<TaggedSexp<Tag>> {
    let (tag, sub_sexps) = self
      .open_sexps
      .pop()
      .expect("called close_sexp with no open partial sexp");
    let finished_sexp = TaggedSexp::List((tag, sub_sexps));
    self.push_closed_sexp(finished_sexp)
  }
  fn push_closed_sexp(
    &mut self,
    sexp: TaggedSexp<Tag>,
  ) -> Option<TaggedSexp<Tag>> {
    if let Some((tag, subsexps)) = self.open_sexps.last_mut() {
      subsexps.push(sexp);
      match self.syntax_graph.get_tag_element(tag) {
        SyntaxElement::Operator(operator) => {
          let left_args = operator.left_args();
          let right_args = operator.right_args();
          if subsexps.len() == left_args + right_args {
            self.close_sexp()
          } else {
            None
          }
        }
        _ => None,
      }
    } else {
      Some(sexp)
    }
  }
  fn awaited_closer(&self) -> Option<&'g str> {
    self
      .open_sexps
      .iter()
      .rev()
      .filter_map(|open_sexp| {
        match self.syntax_graph.get_tag_element(&open_sexp.0) {
          SyntaxElement::Encloser(encloser) => {
            Some(encloser.closing_encloser_str())
          }
          SyntaxElement::SymmetricEncloser(symmetric_encloser) => {
            Some(symmetric_encloser.encloser_str())
          }
          _ => None,
        }
      })
      .next()
  }
  pub(crate) fn complete(
    mut self,
    already_parsed_index: usize,
  ) -> Result<
    Result<Vec<(TaggedSexp<Tag>, usize)>, Vec<(TaggedSexp<Tag>, usize)>>,
    ParseError,
  > {
    let mut current_terminal_beginning: Option<usize> = None;
    let beginning_index = self
      .inherited_top_level_sexps
      .last()
      .map(|(_, end_index)| *end_index)
      .unwrap_or(already_parsed_index);
    if beginning_index >= self.text.len() {
      return Ok(Err(self.inherited_top_level_sexps));
    }
    let mut indexed_characters = self.text[beginning_index..]
      .char_indices()
      .chain(std::iter::once((self.text.len() - beginning_index, ' ')))
      .peekable();
    let mut escaped = false;
    'outer: while let Some((character_index_offset, character)) =
      indexed_characters.next()
    {
      let character_index = beginning_index + character_index_offset;
      println!("\n{character_index}: {character} ({escaped})");
      macro_rules! finish_terminal {
        () => {
          if let Some(terminal_beginning) = current_terminal_beginning {
            if let Some(completed_sexp) =
              self.push_closed_sexp(TaggedSexp::Leaf(
                self.text[terminal_beginning..character_index].to_string(),
              ))
            {
              let mut top_level_sexps = self.inherited_top_level_sexps;
              top_level_sexps.push((completed_sexp, character_index));
              return Ok(Ok(top_level_sexps));
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
          .open_sexps
          .last()
          .map(|(tag, _)| self.syntax_graph.get_context_tag(tag))
          .unwrap_or(&self.syntax_graph.root),
      );
      if escaped {
        escaped = false;
      } else if active_context.escape_char == Some(character) {
        escaped = true;
        current_terminal_beginning =
          current_terminal_beginning.or(Some(character_index));
      } else if active_context.is_whitespace(character) {
        finish_terminal!();
      } else {
        let remaining_text = &self.text[character_index..];
        if let Some(awaited_closer) = self.awaited_closer() {
          if remaining_text.starts_with(awaited_closer) {
            finish_terminal!();
            let closer_len = awaited_closer.len();
            if let Some((tag, _)) = self.open_sexps.last() {
              if let SyntaxElement::Operator(operator) =
                self.syntax_graph.get_tag_element(tag)
              {
                return Err(ParseError::OperatorMissingRightArgument(
                  operator.op_str().to_string(),
                ));
              }
            }
            if let Some(completed_sexp) = self.close_sexp() {
              let mut top_level_sexps = self.inherited_top_level_sexps;
              top_level_sexps
                .push((completed_sexp, character_index + awaited_closer.len()));
              return Ok(Ok(top_level_sexps));
            } else {
              skip_n_chars!(closer_len);
              continue;
            }
          }
        }
        let active_context_tag = self
          .open_sexps
          .last()
          .map(|(tag, _)| self.syntax_graph.get_context_tag(tag))
          .unwrap_or(&self.syntax_graph.root);
        for closer in
          self.syntax_graph.get_asymmetric_closers(active_context_tag)
        {
          if remaining_text.starts_with(closer) {
            return Err(ParseError::UnexpectedCloser(closer.to_string()));
          }
        }
        for tag in self.syntax_graph.get_context(active_context_tag).tags() {
          let beginning_marker = self.syntax_graph.get_beginning_marker(tag);
          if remaining_text.starts_with(beginning_marker) {
            finish_terminal!();
            let leftward_args = self.consume_left_sexps(tag)?;
            self.open_sexps.push((tag.clone(), leftward_args));
            skip_n_chars!(beginning_marker.len());
            continue 'outer;
          }
        }
        if current_terminal_beginning.is_none() {
          current_terminal_beginning = Some(character_index)
        }
      }
    }
    match self.open_sexps.last() {
      None => Ok(Err(self.inherited_top_level_sexps)),
      Some((tag, _)) => match self.syntax_graph.get_tag_element(tag) {
        SyntaxElement::Encloser(encloser) => {
          Err(ParseError::EndOfTextWithOpenEncloser(
            encloser.opening_encloser_str().to_string(),
          ))
        }
        SyntaxElement::SymmetricEncloser(symmetric_encloser) => {
          Err(ParseError::EndOfTextWithOpenEncloser(
            symmetric_encloser.encloser_str().to_string(),
          ))
        }
        SyntaxElement::Operator(operator) => {
          Err(ParseError::OperatorMissingRightArgument(
            operator.op_str().to_string(),
          ))
        }
        SyntaxElement::_Unusable(_) => unreachable!(),
      },
    }
  }
}
