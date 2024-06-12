use std::collections::HashMap;

use crate::syntax::{
  Encloser, Operator, SymmetricEncloser, SyntaxContext, SyntaxGraph, SyntaxTag,
};

impl<'g> SyntaxTag<'g> for &'g str {
  fn tag_str(&self) -> &'g str {
    self
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedEncloser<'g> {
  opener: &'g str,
  closer: &'g str,
}
impl<'g> StringTaggedEncloser<'g> {
  pub fn new(opener: &'g str, closer: &'g str) -> Self {
    Self { opener, closer }
  }
}
impl<'g> Encloser<'g, &'g str> for StringTaggedEncloser<'g> {
  fn opening_encloser_str(&self) -> &'g str {
    self.opener
  }

  fn closing_encloser_str(&self) -> &'g str {
    self.closer
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedSymmetricEncloser<'g> {
  encloser: &'g str,
}
impl<'g> StringTaggedSymmetricEncloser<'g> {
  pub fn new(encloser: &'g str) -> Self {
    Self { encloser }
  }
}
impl<'g> SymmetricEncloser<'g, &'g str> for StringTaggedSymmetricEncloser<'g> {
  fn encloser_str(&self) -> &'g str {
    self.encloser
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedOperator<'g> {
  operator: &'g str,
  left_args: usize,
  right_args: usize,
}
impl<'g> StringTaggedOperator<'g> {
  pub fn new(operator: &'g str, left_args: usize, right_args: usize) -> Self {
    Self {
      operator,
      left_args,
      right_args,
    }
  }
}
impl<'g> Operator<'g, &'g str> for StringTaggedOperator<'g> {
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

pub type StringTaggedSyntaxGraph<'g> = SyntaxGraph<
  'g,
  &'g str,
  &'g str,
  StringTaggedEncloser<'g>,
  StringTaggedSymmetricEncloser<'g>,
  StringTaggedOperator<'g>,
>;

impl<'g> StringTaggedSyntaxGraph<'g> {
  pub fn from_descriptions(
    root: &'g str,
    context_descriptions: Vec<(&'g str, Vec<&'g str>)>,
    encloser_descriptions: Vec<(&'g str, &'g str, &'g str, &'g str)>,
    operator_descriptions: Vec<(&'g str, &'g str, usize, usize, &'g str)>,
  ) -> Self {
    let mut enclosers = vec![];
    let mut symmetric_enclosers = vec![];
    for (tag, opener, closer, context_tag) in encloser_descriptions {
      if opener == closer {
        symmetric_enclosers.push((
          tag,
          StringTaggedSymmetricEncloser::new(opener),
          context_tag,
        ));
      } else {
        enclosers.push((
          tag,
          StringTaggedEncloser::new(opener, closer),
          context_tag,
        ));
      }
    }
    Self::new(
      root,
      context_descriptions
        .into_iter()
        .map(|(context_name, internal_tags)| {
          (context_name, SyntaxContext::new(internal_tags))
        })
        .collect::<HashMap<_, _>>(),
      enclosers,
      symmetric_enclosers,
      operator_descriptions
        .into_iter()
        .map(|(tag, operator, left_args, right_args, context_tag)| {
          (
            tag,
            StringTaggedOperator::new(operator, left_args, right_args),
            context_tag,
          )
        })
        .collect(),
    )
  }
  pub fn contextless_from_descriptions(
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
