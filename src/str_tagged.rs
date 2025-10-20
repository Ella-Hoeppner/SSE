use std::collections::HashMap;

use crate::{
  Context, ContextId,
  syntax::{Encloser, IdStr, Operator, Syntax},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringTaggedEncloser<'a> {
  id: &'a str,
  opener: &'a str,
  closer: &'a str,
}
impl<'a> StringTaggedEncloser<'a> {
  pub fn new(id: &'a str, opener: &'a str, closer: &'a str) -> Self {
    Self { id, opener, closer }
  }
}
impl<'a> Encloser for StringTaggedEncloser<'a> {
  fn opening_encloser_str(&self) -> &'a str {
    self.opener
  }

  fn closing_encloser_str(&self) -> &'a str {
    self.closer
  }
}

impl<'a> IdStr for StringTaggedEncloser<'a> {
  fn id_str(&self) -> &str {
    self.id
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringTaggedOperator<'a> {
  id: &'a str,
  operator: &'a str,
  left_args: usize,
  right_args: usize,
}
impl<'a> StringTaggedOperator<'a> {
  pub fn new(
    id: &'a str,
    operator: &'a str,
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
impl<'a> Operator for StringTaggedOperator<'a> {
  fn op_str(&self) -> &'a str {
    self.operator
  }

  fn left_args(&self) -> usize {
    self.left_args
  }

  fn right_args(&self) -> usize {
    self.right_args
  }
}

impl<'a> IdStr for StringTaggedOperator<'a> {
  fn id_str(&self) -> &str {
    self.id
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringTaggedContextId<'a> {
  id: &'a str,
  is_comment: bool,
}
impl<'a> ContextId for StringTaggedContextId<'a> {
  fn is_comment(&self) -> bool {
    self.is_comment
  }
}

#[derive(Debug, Clone)]
pub struct StringTaggedSyntax<'a> {
  pub(crate) root: StringTaggedContextId<'a>,
  contexts: HashMap<
    StringTaggedContextId<'a>,
    Context<StringTaggedEncloser<'a>, StringTaggedOperator<'a>>,
  >,
  reserved_tokens: Vec<String>,
  encloser_contexts:
    HashMap<StringTaggedEncloser<'a>, Option<StringTaggedContextId<'a>>>,
  operator_contexts:
    HashMap<StringTaggedOperator<'a>, Option<StringTaggedContextId<'a>>>,
}

impl<'a> Syntax for StringTaggedSyntax<'a> {
  type C = StringTaggedContextId<'a>;
  type E = StringTaggedEncloser<'a>;
  type O = StringTaggedOperator<'a>;

  fn root_context(&self) -> StringTaggedContextId<'a> {
    self.root
  }
  fn context<'s>(
    &'s self,
    id: &StringTaggedContextId<'a>,
  ) -> &'s Context<Self::E, Self::O> {
    &self.contexts[id]
  }
  fn encloser_context(&self, encloser: &Self::E) -> Option<Self::C> {
    self.encloser_contexts[encloser]
  }
  fn operator_context(&self, operator: &Self::O) -> Option<Self::C> {
    self.operator_contexts[operator]
  }
  fn reserved_tokens(&self) -> impl Iterator<Item = &str> {
    self.reserved_tokens.iter().map(|s| s.as_str())
  }
}

impl<'a> StringTaggedSyntax<'a> {
  pub fn from_descriptions(
    root: &'a str,
    context_descriptions: Vec<(
      &'a str,
      bool,
      Vec<&'a str>,
      Option<String>,
      Vec<String>,
    )>,
    encloser_descriptions: Vec<(&'a str, &'a str, &'a str, Option<&'a str>)>,
    operator_descriptions: Vec<(
      &'a str,
      &'a str,
      usize,
      usize,
      Option<&'a str>,
    )>,
  ) -> Self {
    let context_from_name = |id: &'a str| -> StringTaggedContextId<'a> {
      StringTaggedContextId {
        id,
        is_comment: context_descriptions
          .iter()
          .find_map(|(ctx_name, is_comment, _, _, _)| {
            (*ctx_name == id).then_some(*is_comment)
          })
          .unwrap(),
      }
    };
    let encloser_contexts = encloser_descriptions
      .into_iter()
      .map(|(tag, opener, closer, context_tag)| {
        (
          StringTaggedEncloser::new(tag, opener, closer),
          context_tag.map(context_from_name),
        )
      })
      .collect::<HashMap<_, _>>();
    let operator_contexts = operator_descriptions
      .into_iter()
      .map(|(tag, operator, left_args, right_args, context_tag)| {
        (
          StringTaggedOperator::new(tag, operator, left_args, right_args),
          context_tag.map(context_from_name),
        )
      })
      .collect::<HashMap<_, _>>();
    Self {
      root: StringTaggedContextId {
        id: root,
        is_comment: false,
      },
      contexts: context_descriptions
        .into_iter()
        .map(
          |(
            context_name,
            is_comment,
            internal_tags,
            escape_char,
            whitespace_chars,
          )| {
            (
              StringTaggedContextId {
                id: context_name,
                is_comment,
              },
              Context::new(
                encloser_contexts
                  .keys()
                  .filter_map(|encloser| {
                    if internal_tags.contains(&encloser.id) {
                      Some(*encloser)
                    } else {
                      None
                    }
                  })
                  .collect(),
                operator_contexts
                  .keys()
                  .filter_map(|encloser| {
                    if internal_tags.contains(&encloser.id) {
                      Some(*encloser)
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
    encloser_descriptions: Vec<(&'a str, &'a str, &'a str)>,
    operator_descriptions: Vec<(&'a str, &'a str, usize, usize)>,
  ) -> Self {
    Self::from_descriptions(
      "",
      vec![(
        "",
        false,
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
        .map(|(tag, opener, closer)| (tag, opener, closer, None))
        .collect(),
      operator_descriptions
        .into_iter()
        .map(|(tag, operator, left_args, right_args)| {
          (tag, operator, left_args, right_args, None)
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
