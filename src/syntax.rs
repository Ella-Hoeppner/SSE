use std::{collections::HashMap, fmt::Debug, hash::Hash};

pub trait Encloser: Clone + Eq + Hash {
  fn id_str(&self) -> &str;
  fn opening_encloser_str(&self) -> &str;
  fn closing_encloser_str(&self) -> &str;
}

pub trait Operator: Clone + Eq + Hash {
  fn id_str(&self) -> &str;
  fn left_args(&self) -> usize;
  fn right_args(&self) -> usize;
  fn op_str(&self) -> &str;
}

#[derive(Clone, Debug)]
pub struct SyntaxContext<E: Encloser, O: Operator> {
  whitespace_chars: Vec<char>,
  pub(crate) escape_char: Option<char>,
  enclosers: Vec<E>,
  operators: Vec<O>,
}

impl<'g, E: Encloser, O: Operator> SyntaxContext<E, O> {
  pub fn new(
    enclosers: Vec<E>,
    operators: Vec<O>,
    escape_char: Option<char>,
    whitespace_chars: Vec<char>,
  ) -> Self {
    Self {
      whitespace_chars,
      escape_char,
      enclosers,
      operators,
    }
  }
  pub fn enclosers(&self) -> &[E] {
    &self.enclosers
  }
  pub fn operators(&self) -> &[O] {
    &self.operators
  }
  pub fn is_whitespace(&self, c: char) -> bool {
    self.whitespace_chars.contains(&c)
  }
}

#[derive(Debug, Clone, Hash)]
pub(crate) enum EncloserOrOperator<E: Encloser, O: Operator> {
  Encloser(E),
  Operator(O),
}

#[derive(Debug, Clone)]
pub struct SyntaxGraph<
  ContextTag: Clone + Debug + PartialEq + Eq + Hash,
  E: Encloser,
  O: Operator,
> {
  pub(crate) root: ContextTag,
  contexts: HashMap<ContextTag, SyntaxContext<E, O>>,
  encloser_contexts: HashMap<E, ContextTag>,
  operator_contexts: HashMap<O, ContextTag>,
}

impl<
    ContextTag: Clone + Debug + PartialEq + Eq + Hash,
    E: Encloser,
    O: Operator,
  > SyntaxGraph<ContextTag, E, O>
{
  pub fn new(
    root: ContextTag,
    contexts: HashMap<ContextTag, SyntaxContext<E, O>>,
    encloser_contexts: HashMap<E, ContextTag>,
    operator_contexts: HashMap<O, ContextTag>,
  ) -> Self {
    Self {
      root,
      contexts,
      encloser_contexts,
      operator_contexts,
    }
  }
  pub fn get_context(&self, context_tag: &ContextTag) -> &SyntaxContext<E, O> {
    &self.contexts[&context_tag]
  }
  pub fn get_encloser_context_tag(&self, encloser: &E) -> &ContextTag {
    &self.encloser_contexts[encloser]
  }
  pub fn get_operator_context_tag(&self, operator: &O) -> &ContextTag {
    &self.operator_contexts[operator]
  }
  pub fn get_context_tag(
    &self,
    encloser_or_operator: &EncloserOrOperator<E, O>,
  ) -> &ContextTag {
    match encloser_or_operator {
      EncloserOrOperator::Encloser(encloser) => {
        self.get_encloser_context_tag(encloser)
      }
      EncloserOrOperator::Operator(operator) => {
        self.get_operator_context_tag(operator)
      }
    }
  }
  pub(crate) fn get_closers<'g>(
    &'g self,
    context_tag: &ContextTag,
  ) -> Vec<&'g str> {
    self.contexts[context_tag]
      .enclosers()
      .iter()
      .map(|encloser| encloser.closing_encloser_str())
      .collect()
  }
}
