use std::collections::HashMap;

use crate::{
  document::Document,
  syntax::{Encloser, Operator, SyntaxGraph},
  SyntaxContext,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringTaggedEncloser<'g> {
  id: &'g str,
  opener: &'g str,
  closer: &'g str,
}
impl<'g> StringTaggedEncloser<'g> {
  pub fn new(id: &'g str, opener: &'g str, closer: &'g str) -> Self {
    Self { id, opener, closer }
  }
}
impl<'g> Encloser for StringTaggedEncloser<'g> {
  fn id_str(&self) -> &str {
    self.id
  }

  fn opening_encloser_str(&self) -> &'g str {
    self.opener
  }

  fn closing_encloser_str(&self) -> &'g str {
    self.closer
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringTaggedOperator<'g> {
  id: &'g str,
  operator: &'g str,
  left_args: usize,
  right_args: usize,
}
impl<'g> StringTaggedOperator<'g> {
  pub fn new(
    id: &'g str,
    operator: &'g str,
    left_args: usize,
    right_args: usize,
  ) -> Self {
    Self {
      id,
      operator,
      left_args,
      right_args,
    }
  }
}
impl<'g> Operator for StringTaggedOperator<'g> {
  fn id_str(&self) -> &str {
    self.id
  }
  fn op_str(&self) -> &'g str {
    self.operator
  }

  fn left_args(&self) -> usize {
    self.left_args
  }

  fn right_args(&self) -> usize {
    self.right_args
  }
}

pub type StringTaggedSyntaxGraph<'g> =
  SyntaxGraph<&'g str, StringTaggedEncloser<'g>, StringTaggedOperator<'g>>;

impl<'g> StringTaggedSyntaxGraph<'g> {
  pub fn from_descriptions(
    root: &'g str,
    context_descriptions: Vec<(
      &'g str,
      Vec<&'g str>,
      Option<String>,
      Vec<String>,
    )>,
    encloser_descriptions: Vec<(&'g str, &'g str, &'g str, &'g str)>,
    operator_descriptions: Vec<(&'g str, &'g str, usize, usize, &'g str)>,
  ) -> Self {
    let enclosers = encloser_descriptions
      .into_iter()
      .map(|(tag, opener, closer, context_tag)| {
        (StringTaggedEncloser::new(tag, opener, closer), context_tag)
      })
      .collect::<HashMap<_, _>>();
    let operators = operator_descriptions
      .into_iter()
      .map(|(tag, operator, left_args, right_args, context_tag)| {
        (
          StringTaggedOperator::new(tag, operator, left_args, right_args),
          context_tag,
        )
      })
      .collect::<HashMap<_, _>>();
    Self::new(
      root,
      context_descriptions
        .into_iter()
        .map(
          |(context_name, internal_tags, escape_char, whitespace_chars)| {
            (
              context_name,
              SyntaxContext::new(
                enclosers
                  .keys()
                  .filter_map(|encloser| {
                    if internal_tags.contains(&encloser.id_str()) {
                      Some(encloser.clone())
                    } else {
                      None
                    }
                  })
                  .collect(),
                operators
                  .keys()
                  .filter_map(|encloser| {
                    if internal_tags.contains(&encloser.id_str()) {
                      Some(encloser.clone())
                    } else {
                      None
                    }
                  })
                  .collect(),
                escape_char,
                whitespace_chars,
              ),
            )
          },
        )
        .collect(),
      enclosers,
      operators,
    )
  }
  pub fn contextless_from_descriptions(
    whitespace_chars: Vec<String>,
    escape_char: Option<String>,
    encloser_descriptions: Vec<(&'g str, &'g str, &'g str)>,
    operator_descriptions: Vec<(&'g str, &'g str, usize, usize)>,
  ) -> Self {
    Self::from_descriptions(
      "",
      vec![(
        "",
        encloser_descriptions
          .iter()
          .map(|(tag, _, _)| tag)
          .chain(operator_descriptions.iter().map(|(tag, _, _, _)| tag))
          .cloned()
          .collect(),
        escape_char,
        whitespace_chars,
      )],
      encloser_descriptions
        .into_iter()
        .map(|(tag, opener, closer)| (tag, opener, closer, ""))
        .collect(),
      operator_descriptions
        .into_iter()
        .map(|(tag, operator, left_args, right_args)| {
          (tag, operator, left_args, right_args, "")
        })
        .collect(),
    )
  }
}

pub type StringTaggedDocument<'t, 'g> =
  Document<'t, &'g str, StringTaggedEncloser<'g>, StringTaggedOperator<'g>>;
