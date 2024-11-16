use std::{collections::HashMap, fmt::Debug, hash::Hash};

pub trait Context: Clone + Debug + PartialEq + Eq + Hash {
  fn is_comment(&self) -> bool;
}

impl Context for () {
  fn is_comment(&self) -> bool {
    false
  }
}

impl Context for &str {
  fn is_comment(&self) -> bool {
    false
  }
}

pub trait Encloser: Debug + Clone + Eq + Hash {
  fn id_str(&self) -> &str;
  fn opening_encloser_str(&self) -> &str;
  fn closing_encloser_str(&self) -> &str;
}

pub trait Operator: Debug + Clone + Eq + Hash {
  fn id_str(&self) -> &str;
  fn left_args(&self) -> usize;
  fn right_args(&self) -> usize;
  fn op_str(&self) -> &str;
}

#[derive(Clone, Debug)]
pub struct SyntaxContext<E: Encloser, O: Operator> {
  whitespace_chars: Vec<String>,
  pub(crate) escape_char: Option<String>,
  enclosers: Vec<E>,
  operators: Vec<O>,
}

impl<'g, E: Encloser, O: Operator> SyntaxContext<E, O> {
  pub fn new(
    enclosers: Vec<E>,
    operators: Vec<O>,
    escape_char: Option<String>,
    whitespace_chars: Vec<String>,
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
  pub fn is_whitespace(&self, c: &str) -> bool {
    self.whitespace_chars.contains(&c.to_string())
  }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum EncloserOrOperator<E: Encloser, O: Operator> {
  Encloser(E),
  Operator(O),
}
impl<E: Encloser, O: Operator> EncloserOrOperator<E, O> {
  pub fn id_str(&self) -> &str {
    match self {
      EncloserOrOperator::Encloser(encloser) => encloser.id_str(),
      EncloserOrOperator::Operator(operator) => operator.id_str(),
    }
  }
}

#[derive(Debug, Clone)]
pub struct SyntaxGraph<C: Context, E: Encloser, O: Operator> {
  pub(crate) root: C,
  contexts: HashMap<C, SyntaxContext<E, O>>,
  encloser_contexts: HashMap<E, C>,
  operator_contexts: HashMap<O, C>,
}

impl<C: Context, E: Encloser, O: Operator> SyntaxGraph<C, E, O> {
  pub fn new(
    root: C,
    contexts: HashMap<C, SyntaxContext<E, O>>,
    encloser_contexts: HashMap<E, C>,
    operator_contexts: HashMap<O, C>,
  ) -> Self {
    Self {
      root,
      contexts,
      encloser_contexts,
      operator_contexts,
    }
  }
  pub fn get_context(&self, context_tag: &C) -> &SyntaxContext<E, O> {
    &self.contexts[&context_tag]
  }
  pub fn get_encloser_context_tag(&self, encloser: &E) -> &C {
    &self.encloser_contexts[encloser]
  }
  pub fn get_operator_context_tag(&self, operator: &O) -> &C {
    &self.operator_contexts[operator]
  }
  pub fn get_context_tag(
    &self,
    encloser_or_operator: &EncloserOrOperator<E, O>,
  ) -> &C {
    match encloser_or_operator {
      EncloserOrOperator::Encloser(encloser) => {
        self.get_encloser_context_tag(encloser)
      }
      EncloserOrOperator::Operator(operator) => {
        self.get_operator_context_tag(operator)
      }
    }
  }
  pub(crate) fn get_closers<'g>(&'g self, context_tag: &C) -> Vec<&'g str> {
    self.contexts[context_tag]
      .enclosers()
      .iter()
      .map(|encloser| encloser.closing_encloser_str())
      .collect()
  }
}

impl<E: Encloser, O: Operator> SyntaxGraph<(), E, O> {
  pub fn contextless(
    enclosers: Vec<E>,
    operators: Vec<O>,
    whitespace_chars: Vec<String>,
  ) -> Self {
    Self::new(
      (),
      [(
        (),
        SyntaxContext::new(
          enclosers.clone(),
          operators.clone(),
          None,
          whitespace_chars,
        ),
      )]
      .into_iter()
      .collect(),
      enclosers
        .into_iter()
        .map(|encloser| (encloser, ()))
        .collect(),
      operators
        .into_iter()
        .map(|operator| (operator, ()))
        .collect(),
    )
  }
}
