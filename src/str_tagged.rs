use crate::syntax::{
  Encloser, Operator, SyntaxContext, SyntaxGraph, SyntaxTag,
};

impl SyntaxTag for &str {
  fn tag_str(&self) -> &str {
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
impl<'g> Encloser<&'g str> for StringTaggedEncloser<'g> {
  fn opening_encloser_str(&self) -> &'g str {
    self.opener
  }

  fn closing_encloser_str(&self) -> &'g str {
    self.closer
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
impl<'g> Operator<&'g str> for StringTaggedOperator<'g> {
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
  &'g str,
  &'g str,
  StringTaggedEncloser<'g>,
  StringTaggedOperator<'g>,
>;

impl<'g> StringTaggedSyntaxGraph<'g> {
  pub fn from_descriptions(
    root: &'g str,
    context_descriptions: Vec<(&'g str, Vec<&'g str>, Option<char>, Vec<char>)>,
    encloser_descriptions: Vec<(&'g str, &'g str, &'g str, &'g str)>,
    operator_descriptions: Vec<(&'g str, &'g str, usize, usize, &'g str)>,
  ) -> Self {
    Self::new(
      root,
      context_descriptions
        .into_iter()
        .map(
          |(context_name, internal_tags, escape_char, whitespace_chars)| {
            (
              context_name,
              SyntaxContext::new(internal_tags, escape_char, whitespace_chars),
            )
          },
        )
        .collect(),
      encloser_descriptions
        .into_iter()
        .map(|(tag, opener, closer, context_tag)| {
          (tag, StringTaggedEncloser::new(opener, closer), context_tag)
        })
        .collect(),
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
    whitespace_chars: Vec<char>,
    escape_char: Option<char>,
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
