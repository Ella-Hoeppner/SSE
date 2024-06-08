pub mod str_tagged;

use std::{
  collections::HashMap,
  convert::Infallible,
  fmt::{Debug, Display},
  hash::Hash,
  marker::PhantomData,
};

pub trait Syntax<Tag: Clone + Debug + PartialEq + Eq + Display + Hash>:
  Clone + Debug
{
  fn tag(&self) -> Tag;
  fn child_tags(&self) -> &[Tag];
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
  E: Encloser<Tag>,
  SE: SymmetricEncloser<Tag>,
  O: Operator<Tag>,
> {
  Encloser(E),
  SymmetricEncloser(SE),
  Operator(O),
  _Unusable(PhantomData<Tag>, Infallible),
}

pub struct SyntaxGraph<
  Tag: Clone + Debug + PartialEq + Eq + Display + Hash,
  E: Encloser<Tag>,
  SE: SymmetricEncloser<Tag>,
  O: Operator<Tag>,
> {
  pub(crate) root: Tag,
  syntax_elements: HashMap<Tag, SyntaxElement<Tag, E, SE, O>>,
}

impl<
    Tag: Clone + Debug + PartialEq + Eq + Display + Hash,
    E: Encloser<Tag>,
    SE: SymmetricEncloser<Tag>,
    O: Operator<Tag>,
  > SyntaxGraph<Tag, E, SE, O>
{
  pub fn new(root: Tag) -> Self {
    Self {
      root,
      syntax_elements: HashMap::new(),
    }
  }
  pub fn with_encloser(mut self, tag: Tag, encloser: E) -> Self {
    self
      .syntax_elements
      .insert(tag, SyntaxElement::Encloser(encloser));
    self
  }
  pub fn with_symmetric_encloser(
    mut self,
    tag: Tag,
    symmetric_encloser: SE,
  ) -> Self {
    self
      .syntax_elements
      .insert(tag, SyntaxElement::SymmetricEncloser(symmetric_encloser));
    self
  }
  pub fn with_operator(mut self, tag: Tag, operator: O) -> Self {
    self
      .syntax_elements
      .insert(tag, SyntaxElement::Operator(operator));
    self
  }
  pub fn get_child_tags(&self, tag: &Tag) -> &[Tag] {
    match &self.syntax_elements[tag] {
      SyntaxElement::Encloser(encloser) => encloser.child_tags(),
      SyntaxElement::SymmetricEncloser(symmetric_encloser) => {
        symmetric_encloser.child_tags()
      }
      SyntaxElement::Operator(operator) => operator.child_tags(),
      SyntaxElement::_Unusable(_, _) => unreachable!(),
    }
  }
  pub fn get_beginning_marker(&self, tag: &Tag) -> &str {
    match &self.syntax_elements[tag] {
      SyntaxElement::Encloser(encloser) => encloser.opening_encloser_str(),
      SyntaxElement::SymmetricEncloser(symmetric_encloser) => {
        symmetric_encloser.encloser_str()
      }
      SyntaxElement::Operator(operator) => operator.op_str(),
      SyntaxElement::_Unusable(_, _) => unreachable!(),
    }
  }
}
