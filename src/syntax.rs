use std::{fmt::Debug, hash::Hash};

pub trait ContextId: Clone + Debug + PartialEq + Eq + Hash {
  fn is_comment(&self) -> bool;
}

impl ContextId for () {
  fn is_comment(&self) -> bool {
    false
  }
}

impl ContextId for &str {
  fn is_comment(&self) -> bool {
    false
  }
}

pub trait Encloser: Debug + Clone + Eq + Hash {
  fn opening_encloser_str(&self) -> &str;
  fn closing_encloser_str(&self) -> &str;
}

impl Encloser for () {
  fn opening_encloser_str(&self) -> &str {
    "("
  }
  fn closing_encloser_str(&self) -> &str {
    ")"
  }
}
impl IdStr for () {
  fn id_str(&self) -> &str {
    ""
  }
}

pub trait Operator: Debug + Clone + Eq + Hash {
  fn left_args(&self) -> usize;
  fn right_args(&self) -> usize;
  fn op_str(&self) -> &str;
}

#[derive(Clone, Debug)]
pub struct Context<E: Encloser, O: Operator> {
  whitespace_chars: Vec<String>,
  pub(crate) escape_char: Option<String>,
  enclosers: Vec<E>,
  operators: Vec<O>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NoOperator {}
impl Operator for NoOperator {
  fn left_args(&self) -> usize {
    unreachable!()
  }
  fn right_args(&self) -> usize {
    unreachable!()
  }
  fn op_str(&self) -> &str {
    unreachable!()
  }
}
impl IdStr for NoOperator {
  fn id_str(&self) -> &str {
    unreachable!()
  }
}

impl<E: Encloser, O: Operator> Context<E, O> {
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
  pub fn trivial() -> Self {
    Self::new(vec![], vec![], None, vec![])
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
  pub fn lookahead(&self) -> usize {
    self
      .operators()
      .iter()
      .map(|operator| operator.left_args())
      .max()
      .unwrap_or(0)
  }
}

pub trait IdStr {
  fn id_str(&self) -> &str;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum EncloserOrOperator<E: Encloser, O: Operator> {
  Encloser(E),
  Operator(O),
}
impl<E: Encloser + IdStr, O: Operator + IdStr> IdStr
  for EncloserOrOperator<E, O>
{
  fn id_str(&self) -> &str {
    match self {
      EncloserOrOperator::Encloser(encloser) => encloser.id_str(),
      EncloserOrOperator::Operator(operator) => operator.id_str(),
    }
  }
}

pub trait ContainsEncloserOrOperator<E: Encloser, O: Operator> {
  fn get_encloser_or_operator(&self) -> &EncloserOrOperator<E, O>;
  fn into_encloser_or_operator(self) -> EncloserOrOperator<E, O>;
}

impl<E: Encloser, O: Operator> ContainsEncloserOrOperator<E, O>
  for EncloserOrOperator<E, O>
{
  fn get_encloser_or_operator(&self) -> &EncloserOrOperator<E, O> {
    self
  }
  fn into_encloser_or_operator(self) -> EncloserOrOperator<E, O> {
    self
  }
}

pub trait Syntax {
  type C: ContextId;
  type E: Encloser;
  type O: Operator;
  fn root_context(&self) -> Self::C;
  fn context<'a>(&'a self, id: &Self::C) -> &'a Context<Self::E, Self::O>;
  fn encloser_context(&self, encloser: &Self::E) -> Option<Self::C>;
  fn operator_context(&self, operator: &Self::O) -> Option<Self::C>;
  fn reserved_tokens(&self) -> impl Iterator<Item = &str> {
    std::iter::empty()
  }
  fn encloser_or_operator_context(
    &self,
    encloser_or_operator: &EncloserOrOperator<Self::E, Self::O>,
  ) -> Option<Self::C> {
    match encloser_or_operator {
      EncloserOrOperator::Encloser(e) => self.encloser_context(e),
      EncloserOrOperator::Operator(o) => self.operator_context(o),
    }
  }
  fn prefix_closer_in_context<'a>(
    &self,
    tag: &Self::C,
    s: &'a str,
  ) -> Option<&'a str> {
    for closer in self
      .context(tag)
      .enclosers()
      .iter()
      .map(|encloser| encloser.closing_encloser_str())
    {
      if s.starts_with(closer) {
        return Some(&s[..closer.len()]);
      }
    }
    None
  }
}
