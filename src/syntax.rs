use std::{
  collections::HashMap,
  fmt::{Debug, Display},
  hash::Hash,
  marker::PhantomData,
};

pub trait Syntax<Tag: Clone + Debug + PartialEq + Eq + Display + Hash>:
  Clone + Debug
{
  fn tag(&self) -> Tag;
  fn allowed_child_tags(&self) -> &[Tag];
}

pub trait Encloser<Tag: Clone + Debug + PartialEq + Eq + Display + Hash>:
  Syntax<Tag>
{
  fn opening_encloser_str(&self) -> &str;
  fn closing_encloser_str(&self) -> &str;
}

pub trait SymmetricEncloser<
  Tag: Clone + Debug + PartialEq + Eq + Display + Hash,
>: Syntax<Tag>
{
  fn encloser_str(&self) -> &str;
}

pub trait Operator<Tag: Clone + Debug + PartialEq + Eq + Display + Hash>:
  Syntax<Tag>
{
  fn left_args(&self) -> usize;
  fn right_args(&self) -> usize;
  fn op_str(&self) -> &str;
}

#[derive(Hash)]
pub enum SyntaxElement<
  Tag: Clone + Debug + PartialEq + Eq + Display + Hash,
  D: Encloser<Tag>,
  SD: SymmetricEncloser<Tag>,
  O: Operator<Tag>,
> {
  Encloser(D),
  SymmetricEncloser(SD),
  Operator(O),
  _Unusable(PhantomData<Tag>),
}

pub struct SyntaxGraph<
  T: Clone + Debug + PartialEq + Eq + Display + Hash,
  D: Encloser<T>,
  SD: SymmetricEncloser<T>,
  O: Operator<T>,
> {
  root: T,
  syntax_elements: HashMap<T, SyntaxElement<T, D, SD, O>>,
}

impl<
    T: Clone + Debug + PartialEq + Eq + Display + Hash,
    D: Encloser<T>,
    SD: SymmetricEncloser<T>,
    O: Operator<T>,
  > SyntaxGraph<T, D, SD, O>
{
  pub fn new(root: T) -> Self {
    Self {
      root,
      syntax_elements: HashMap::new(),
    }
  }
  pub fn with_encloser(mut self, tag: T, encloser: D) -> Self {
    self
      .syntax_elements
      .insert(tag, SyntaxElement::Encloser(encloser));
    self
  }
  pub fn with_symmetric_encloser(
    mut self,
    tag: T,
    symmetric_encloser: SD,
  ) -> Self {
    self
      .syntax_elements
      .insert(tag, SyntaxElement::SymmetricEncloser(symmetric_encloser));
    self
  }
  pub fn with_operator(mut self, tag: T, operator: O) -> Self {
    self
      .syntax_elements
      .insert(tag, SyntaxElement::Operator(operator));
    self
  }
}
