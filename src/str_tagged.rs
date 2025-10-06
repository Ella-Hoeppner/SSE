use std::collections::HashMap;

use crate::{
  syntax::{Encloser, IdStr, Operator, Syntax},
  Context,
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
  fn opening_encloser_str(&self) -> &'g str {
    self.opener
  }

  fn closing_encloser_str(&self) -> &'g str {
    self.closer
  }
}

impl<'g> IdStr for StringTaggedEncloser<'g> {
  fn id_str(&self) -> &str {
    self.id
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

impl<'g> IdStr for StringTaggedOperator<'g> {
  fn id_str(&self) -> &str {
    self.id
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedSyntax<'a> {
  pub(crate) root: &'a str,
  contexts: HashMap<
    &'a str,
    Context<StringTaggedEncloser<'a>, StringTaggedOperator<'a>>,
  >,
  reserved_tokens: Vec<String>,
  encloser_contexts: HashMap<StringTaggedEncloser<'a>, &'a str>,
  operator_contexts: HashMap<StringTaggedOperator<'a>, &'a str>,
}

impl<'a> Syntax for StringTaggedSyntax<'a> {
  type C = &'a str;
  type E = StringTaggedEncloser<'a>;
  type O = StringTaggedOperator<'a>;

  fn root_context(&self) -> &'a str {
    self.root
  }
  fn context<'s>(&'s self, id: &&'a str) -> &'s Context<Self::E, Self::O> {
    &self.contexts[id]
  }
  fn encloser_context(&self, encloser: &Self::E) -> Self::C {
    self.encloser_contexts[encloser]
  }
  fn operator_context(&self, operator: &Self::O) -> Self::C {
    self.operator_contexts[operator]
  }
  fn reserved_tokens(&self) -> impl Iterator<Item = &str> {
    self.reserved_tokens.iter().map(|s| s.as_str())
  }
}

impl<'g> StringTaggedSyntax<'g> {
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
    let encloser_contexts = encloser_descriptions
      .into_iter()
      .map(|(tag, opener, closer, context_tag)| {
        (StringTaggedEncloser::new(tag, opener, closer), context_tag)
      })
      .collect::<HashMap<_, _>>();
    let operator_contexts = operator_descriptions
      .into_iter()
      .map(|(tag, operator, left_args, right_args, context_tag)| {
        (
          StringTaggedOperator::new(tag, operator, left_args, right_args),
          context_tag,
        )
      })
      .collect::<HashMap<_, _>>();
    Self {
      root,
      contexts: context_descriptions
        .into_iter()
        .map(
          |(context_name, internal_tags, escape_char, whitespace_chars)| {
            (
              context_name,
              Context::new(
                encloser_contexts
                  .keys()
                  .filter_map(|encloser| {
                    if internal_tags.contains(&encloser.id) {
                      Some(encloser.clone())
                    } else {
                      None
                    }
                  })
                  .collect(),
                operator_contexts
                  .keys()
                  .filter_map(|encloser| {
                    if internal_tags.contains(&encloser.id) {
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
      encloser_contexts,
      operator_contexts,
      reserved_tokens: vec![],
    }
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
  pub fn with_reserved_tokens<S: Into<String>>(
    mut self,
    tokens: impl IntoIterator<Item = S>,
  ) -> Self {
    self.reserved_tokens = tokens.into_iter().map(|s| s.into()).collect();
    self
  }
}
