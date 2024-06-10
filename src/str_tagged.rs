use std::collections::HashMap;

use crate::syntax::{
  Encloser, Operator, SymmetricEncloser, Syntax, SyntaxContext, SyntaxGraph,
  SyntaxTag,
};

impl<'s> SyntaxTag<'s> for &'s str {
  fn tag_str(&self) -> &'s str {
    self
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedEncloser<'s> {
  tag: &'s str,
  opener: &'s str,
  closer: &'s str,
}
impl<'s> StringTaggedEncloser<'s> {
  pub fn new(tag: &'s str, opener: &'s str, closer: &'s str) -> Self {
    Self {
      tag,
      opener,
      closer,
    }
  }
}
impl<'s> Syntax<'s, &'s str> for StringTaggedEncloser<'s> {
  fn tag(&self) -> &'s str {
    self.tag
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
  tag: &'s str,
  encloser: &'s str,
}
impl<'s> StringTaggedSymmetricEncloser<'s> {
  pub fn new(tag: &'s str, encloser: &'s str) -> Self {
    Self { tag, encloser }
  }
}
impl<'s> Syntax<'s, &'s str> for StringTaggedSymmetricEncloser<'s> {
  fn tag(&self) -> &'s str {
    self.tag
  }
}
impl<'s> SymmetricEncloser<'s, &'s str> for StringTaggedSymmetricEncloser<'s> {
  fn encloser_str(&self) -> &str {
    self.encloser
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedOperator<'s> {
  tag: &'s str,
  operator: &'s str,
  left_args: usize,
  right_args: usize,
}
impl<'s> StringTaggedOperator<'s> {
  pub fn new(
    tag: &'s str,
    operator: &'s str,
    left_args: usize,
    right_args: usize,
  ) -> Self {
    Self {
      tag,
      operator,
      left_args,
      right_args,
    }
  }
}
impl<'s> Syntax<'s, &'s str> for StringTaggedOperator<'s> {
  fn tag(&self) -> &'s str {
    self.tag
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
          StringTaggedSymmetricEncloser::new(tag, opener),
          context_tag,
        ));
      } else {
        enclosers.push((
          tag,
          StringTaggedEncloser::new(tag, opener, closer),
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
            StringTaggedOperator::new(tag, operator, left_args, right_args),
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
