pub mod str_tagged;

use std::{
  collections::HashMap,
  convert::Infallible,
  fmt::{Debug, Display},
  hash::Hash,
  marker::PhantomData,
};

pub trait SyntaxTag<'s>: Clone + Debug + PartialEq + Eq + Hash {
  fn tag_str(&self) -> &'s str;
}

pub trait Syntax<'s, Tag: SyntaxTag<'s>>: Clone + Debug {
  fn tag(&self) -> Tag;
  fn child_tags(&self) -> &[Tag];
}

pub trait Encloser<'s, Tag: SyntaxTag<'s>>: Syntax<'s, Tag> {
  fn opening_encloser_str(&self) -> &str;
  fn closing_encloser_str(&self) -> &str;
}

pub trait SymmetricEncloser<'s, Tag: SyntaxTag<'s>>: Syntax<'s, Tag> {
  fn encloser_str(&self) -> &str;
}

pub trait Operator<'s, Tag: SyntaxTag<'s>>: Syntax<'s, Tag> {
  fn left_args(&self) -> usize;
  fn right_args(&self) -> usize;
  fn op_str(&self) -> &str;
}

#[derive(Hash)]
pub enum SyntaxElement<
  's,
  Tag: SyntaxTag<'s>,
  E: Encloser<'s, Tag>,
  SE: SymmetricEncloser<'s, Tag>,
  O: Operator<'s, Tag>,
> {
  Encloser(E),
  SymmetricEncloser(SE),
  Operator(O),
  _Unusable(PhantomData<&'s Tag>, Infallible),
}

pub(crate) enum SyntaxScope<'s> {
  Enclosed { awaited_closer: &'s str },
  Operated { left_args: usize, right_args: usize },
}

pub struct SyntaxGraph<
  's,
  Tag: SyntaxTag<'s>,
  E: Encloser<'s, Tag>,
  SE: SymmetricEncloser<'s, Tag>,
  O: Operator<'s, Tag>,
> {
  pub(crate) root: Tag,
  syntax_elements: HashMap<Tag, SyntaxElement<'s, Tag, E, SE, O>>,
}

impl<
    's,
    Tag: SyntaxTag<'s>,
    E: Encloser<'s, Tag>,
    SE: SymmetricEncloser<'s, Tag>,
    O: Operator<'s, Tag>,
  > SyntaxGraph<'s, Tag, E, SE, O>
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
  pub fn get_tag_scope(&self, tag: &Tag) -> SyntaxScope {
    match &self.syntax_elements[tag] {
      SyntaxElement::Encloser(encloser) => SyntaxScope::Enclosed {
        awaited_closer: encloser.closing_encloser_str(),
      },
      SyntaxElement::SymmetricEncloser(symmetric_encloser) => {
        SyntaxScope::Enclosed {
          awaited_closer: symmetric_encloser.encloser_str(),
        }
      }
      SyntaxElement::Operator(operator) => SyntaxScope::Operated {
        left_args: operator.left_args(),
        right_args: operator.right_args(),
      },
      SyntaxElement::_Unusable(_, _) => unreachable!(),
    }
  }
}
