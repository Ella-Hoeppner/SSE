use std::{
  fmt::{Debug, Display},
  hash::Hash,
};

use crate::{
  sexp::SyntaxTree,
  syntax::{Encloser, EncloserOrOperator, Operator, SyntaxGraph},
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
  ContextTag: Clone + Debug + PartialEq + Eq + Hash,
  E: Encloser,
  O: Operator,
> {
  text: &'t str,
  inherited_top_level_sexps: Vec<(SyntaxTree<E, O>, usize)>,
  syntax_graph: &'g SyntaxGraph<ContextTag, E, O>,
  open_sexps: Vec<(EncloserOrOperator<E, O>, Vec<SyntaxTree<E, O>>)>,
}

impl<
    't,
    'g,
    ContextTag: Clone + Debug + PartialEq + Eq + Hash,
    E: Encloser,
    O: Operator,
  > Parse<'t, 'g, ContextTag, E, O>
{
  pub(crate) fn new(
    syntax_graph: &'g SyntaxGraph<ContextTag, E, O>,
    inherited_top_level_sexps: Vec<(SyntaxTree<E, O>, usize)>,
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
    operator: &O,
  ) -> Result<Vec<SyntaxTree<E, O>>, ParseError> {
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
  fn close_sexp(&mut self) -> Option<SyntaxTree<E, O>> {
    let (encloser_or_operator, sub_sexps) = self
      .open_sexps
      .pop()
      .expect("called close_sexp with no open partial sexp");
    self.push_closed_sexp(match encloser_or_operator {
      EncloserOrOperator::Encloser(encloser) => {
        SyntaxTree::Inner(EncloserOrOperator::Encloser(encloser), sub_sexps)
      }
      EncloserOrOperator::Operator(operator) => {
        SyntaxTree::Inner(EncloserOrOperator::Operator(operator), sub_sexps)
      }
    })
  }
  fn push_closed_sexp(
    &mut self,
    sexp: SyntaxTree<E, O>,
  ) -> Option<SyntaxTree<E, O>> {
    if let Some((encloser_or_operator, subsexps)) = self.open_sexps.last_mut() {
      subsexps.push(sexp);
      if let EncloserOrOperator::Operator(operator) = encloser_or_operator {
        let left_args = operator.left_args();
        let right_args = operator.right_args();
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
  fn awaited_closer(&'g self) -> Option<&'g str> {
    self
      .open_sexps
      .iter()
      .rev()
      .filter_map(|(encloser_or_operator, _)| {
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
    Result<Vec<(SyntaxTree<E, O>, usize)>, Vec<(SyntaxTree<E, O>, usize)>>,
    ParseError,
  > {
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

    let mut current_terminal_beginning: Option<usize> = None;
    let mut escaped = false;

    'outer: while let Some((character_index_offset, character)) =
      indexed_characters.next()
    {
      let character_index = beginning_index + character_index_offset;
      println!("\n{character_index}: {character}");

      macro_rules! finish_terminal {
        () => {
          if let Some(terminal_beginning) = current_terminal_beginning {
            if let Some(completed_sexp) =
              self.push_closed_sexp(SyntaxTree::Leaf(
                (),
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
      //println!("active_context: {active_context:?}");

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

        //println!("awaiting closer: {:?}", self.awaited_closer());
        if let Some(awaited_closer) = self.awaited_closer() {
          if remaining_text.starts_with(awaited_closer) {
            println!("matched awaited closer {:?}", self.awaited_closer());
            let closer_len = awaited_closer.len();
            finish_terminal!();

            if let Some((encloser_or_operator, _)) = self.open_sexps.last() {
              if let EncloserOrOperator::Operator(operator) =
                encloser_or_operator
              {
                return Err(ParseError::OperatorMissingRightArgument(
                  operator.op_str().to_string(),
                ));
              }
            }

            if let Some(completed_sexp) = self.close_sexp() {
              let mut top_level_sexps = self.inherited_top_level_sexps;
              top_level_sexps
                .push((completed_sexp, character_index + closer_len));
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

        for encloser in self
          .syntax_graph
          .get_context(active_context_tag)
          .enclosers()
        {
          let beginning_marker = encloser.opening_encloser_str();
          if remaining_text.starts_with(beginning_marker) {
            finish_terminal!();
            self
              .open_sexps
              .push((EncloserOrOperator::Encloser(encloser.clone()), vec![]));
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
            let leftward_args = self.consume_left_sexps(operator)?;
            self.open_sexps.push((
              EncloserOrOperator::Operator(operator.clone()),
              leftward_args,
            ));
            skip_n_chars!(op_marker.len());
            continue 'outer;
          }
        }

        for closer in self.syntax_graph.get_closers(active_context_tag) {
          if remaining_text.starts_with(closer) {
            println!(
              "matched unexpected closer!! {:?}",
              self.syntax_graph.get_closers(active_context_tag)
            );
            return Err(ParseError::UnexpectedCloser(closer.to_string()));
          }
        }

        if current_terminal_beginning.is_none() {
          current_terminal_beginning = Some(character_index)
        }
      }
    }

    match self.open_sexps.last() {
      None => Ok(Err(self.inherited_top_level_sexps)),
      Some((encloser_or_operator, _)) => match encloser_or_operator {
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
