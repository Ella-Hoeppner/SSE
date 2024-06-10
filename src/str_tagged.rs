use std::collections::HashMap;

use crate::syntax::{
  Encloser, Operator, SymmetricEncloser, SyntaxContext, SyntaxGraph, SyntaxTag,
};

impl<'s> SyntaxTag<'s> for &'s str {
  fn tag_str(&self) -> &'s str {
    self
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedEncloser<'s> {
  opener: &'s str,
  closer: &'s str,
}
impl<'s> StringTaggedEncloser<'s> {
  pub fn new(opener: &'s str, closer: &'s str) -> Self {
    Self { opener, closer }
  }
}
impl<'s> Encloser<'s, &'s str> for StringTaggedEncloser<'s> {
  fn opening_encloser_str(&self) -> &str {
    self.opener
  }

  fn closing_encloser_str(&self) -> &str {
    self.closer
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedSymmetricEncloser<'s> {
  encloser: &'s str,
}
impl<'s> StringTaggedSymmetricEncloser<'s> {
  pub fn new(encloser: &'s str) -> Self {
    Self { encloser }
  }
}
impl<'s> SymmetricEncloser<'s, &'s str> for StringTaggedSymmetricEncloser<'s> {
  fn encloser_str(&self) -> &str {
    self.encloser
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedOperator<'s> {
  operator: &'s str,
  left_args: usize,
  right_args: usize,
}
impl<'s> StringTaggedOperator<'s> {
  pub fn new(operator: &'s str, left_args: usize, right_args: usize) -> Self {
    Self {
      operator,
      left_args,
      right_args,
    }
  }
}
impl<'s> Operator<'s, &'s str> for StringTaggedOperator<'s> {
  fn op_str(&self) -> &str {
    self.operator
  }

  fn left_args(&self) -> usize {
    self.left_args
  }

  fn right_args(&self) -> usize {
    self.right_args
  }
}

pub type StringTaggedSyntaxGraph<'s> = SyntaxGraph<
  's,
  &'s str,
  &'s str,
  StringTaggedEncloser<'s>,
  StringTaggedSymmetricEncloser<'s>,
  StringTaggedOperator<'s>,
>;

impl<'s> StringTaggedSyntaxGraph<'s> {
  pub fn from_descriptions(
    root: &'s str,
    context_descriptions: Vec<(&'s str, Vec<&'s str>)>,
    encloser_descriptions: Vec<(&'s str, &'s str, &'s str, &'s str)>,
    operator_descriptions: Vec<(&'s str, &'s str, usize, usize, &'s str)>,
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
    encloser_descriptions: Vec<(&'s str, &'s str, &'s str)>,
    operator_descriptions: Vec<(&'s str, &'s str, usize, usize)>,
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
