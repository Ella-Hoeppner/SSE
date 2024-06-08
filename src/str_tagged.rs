use crate::syntax::{
  Encloser, Operator, SymmetricEncloser, Syntax, SyntaxGraph, SyntaxTag,
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
  child_tags: Vec<&'s str>,
}
impl<'s> StringTaggedEncloser<'s> {
  pub fn new(tag: &'s str, opener: &'s str, closer: &'s str) -> Self {
    Self {
      tag,
      opener,
      closer,
      child_tags: vec![],
    }
  }
  pub fn with_child_tags(mut self, child_tags: Vec<&'s str>) -> Self {
    self.child_tags = child_tags;
    self
  }
}
impl<'s> Syntax<'s, &'s str> for StringTaggedEncloser<'s> {
  fn tag(&self) -> &'s str {
    self.tag
  }

  fn child_tags(&self) -> &[&'s str] {
    &self.child_tags
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
  child_tags: Vec<&'s str>,
}
impl<'s> StringTaggedSymmetricEncloser<'s> {
  pub fn new(tag: &'s str, encloser: &'s str) -> Self {
    Self {
      tag,
      encloser,
      child_tags: vec![],
    }
  }
  pub fn with_child_tags(mut self, child_tags: Vec<&'s str>) -> Self {
    self.child_tags = child_tags;
    self
  }
}
impl<'s> Syntax<'s, &'s str> for StringTaggedSymmetricEncloser<'s> {
  fn tag(&self) -> &'s str {
    self.tag
  }

  fn child_tags(&self) -> &[&'s str] {
    &self.child_tags
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
  child_tags: Vec<&'s str>,
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
      child_tags: vec![],
    }
  }
  pub fn with_child_tags(mut self, child_tags: Vec<&'s str>) -> Self {
    self.child_tags = child_tags;
    self
  }
}
impl<'s> Syntax<'s, &'s str> for StringTaggedOperator<'s> {
  fn tag(&self) -> &'s str {
    self.tag
  }

  fn child_tags(&self) -> &[&'s str] {
    &self.child_tags
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
  StringTaggedEncloser<'s>,
  StringTaggedSymmetricEncloser<'s>,
  StringTaggedOperator<'s>,
>;

impl<'s> StringTaggedSyntaxGraph<'s> {
  pub fn contextful(
    root: &'s str,
    enclosers: Vec<(&'s str, &'s str, &'s str, Vec<&'s str>)>,
    operators: Vec<(&'s str, &'s str, usize, usize, Vec<&'s str>)>,
  ) -> Self {
    operators.into_iter().fold(
      enclosers.into_iter().fold(
        Self::new(root),
        |graph, (tag, opener, closer, child_tags)| {
          if opener == closer {
            graph.with_symmetric_encloser(
              tag,
              StringTaggedSymmetricEncloser::new(tag, opener)
                .with_child_tags(child_tags),
            )
          } else {
            graph.with_encloser(
              tag,
              StringTaggedEncloser::new(tag, opener, closer)
                .with_child_tags(child_tags),
            )
          }
        },
      ),
      |graph, (tag, operator, left_args, right_args, child_tags)| {
        graph.with_operator(
          tag,
          StringTaggedOperator::new(tag, operator, left_args, right_args)
            .with_child_tags(child_tags),
        )
      },
    )
  }
  pub fn contextless(
    root: &'s str,
    enclosers: Vec<(&'s str, &'s str, &'s str)>,
    operators: Vec<(&'s str, &'s str, usize, usize)>,
  ) -> Self {
    let tags: Vec<&'s str> = enclosers
      .iter()
      .map(|(tag, _, _)| *tag)
      .chain(operators.iter().map(|(tag, _, _, _)| *tag))
      .collect();
    Self::contextful(
      root,
      enclosers
        .into_iter()
        .map(|(tag, opener, closer)| (tag, opener, closer, tags.clone()))
        .collect(),
      operators
        .into_iter()
        .map(|(tag, marker, left_args, right_args)| {
          (tag, marker, left_args, right_args, tags.clone())
        })
        .collect(),
    )
  }
}
