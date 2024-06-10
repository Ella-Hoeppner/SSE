use std::{
  collections::HashMap, convert::Infallible, fmt::Debug, hash::Hash,
  marker::PhantomData,
};

use crate::{
  parse::{Parse, ParseError},
  sexp::{Sexp, TaggedSexp},
};

pub trait SyntaxTag<'s>: Clone + Debug + PartialEq + Eq + Hash {
  fn tag_str(&self) -> &'s str;
}

pub trait Encloser<'s, Tag: SyntaxTag<'s>> {
  fn opening_encloser_str(&self) -> &str;
  fn closing_encloser_str(&self) -> &str;
}

pub trait SymmetricEncloser<'s, Tag: SyntaxTag<'s>> {
  fn encloser_str(&self) -> &str;
}

pub trait Operator<'s, Tag: SyntaxTag<'s>> {
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

#[derive(Clone, Debug)]
pub struct SyntaxContext<'s, Tag: SyntaxTag<'s>> {
  _phantom: PhantomData<&'s ()>,
  tags: Vec<Tag>,
}
impl<'s, Tag: SyntaxTag<'s>> SyntaxContext<'s, Tag> {
  pub fn new(tags: Vec<Tag>) -> Self {
    Self {
      _phantom: PhantomData,
      tags,
    }
  }
  pub fn tags(&self) -> &[Tag] {
    &self.tags
  }
}

pub(crate) enum SyntaxScope<'s> {
  Enclosed { awaited_closer: &'s str },
  Operated { left_args: usize, right_args: usize },
}

pub struct SyntaxGraph<
  's,
  Tag: SyntaxTag<'s>,
  ContextTag: Clone + Debug + PartialEq + Eq + Hash,
  E: Encloser<'s, Tag>,
  SE: SymmetricEncloser<'s, Tag>,
  O: Operator<'s, Tag>,
> {
  pub(crate) root: ContextTag,
  contexts: HashMap<ContextTag, SyntaxContext<'s, Tag>>,
  syntax_elements: HashMap<Tag, (SyntaxElement<'s, Tag, E, SE, O>, ContextTag)>,
}

impl<
    's,
    Tag: SyntaxTag<'s>,
    ContextTag: Clone + Debug + PartialEq + Eq + Hash,
    E: Encloser<'s, Tag>,
    SE: SymmetricEncloser<'s, Tag>,
    O: Operator<'s, Tag>,
  > SyntaxGraph<'s, Tag, ContextTag, E, SE, O>
{
  pub fn new(
    root: ContextTag,
    contexts: HashMap<ContextTag, SyntaxContext<'s, Tag>>,
    enclosers: Vec<(Tag, E, ContextTag)>,
    symmetric_enclosers: Vec<(Tag, SE, ContextTag)>,
    operators: Vec<(Tag, O, ContextTag)>,
  ) -> Self {
    let mut syntax_elements = HashMap::new();
    for (tag, encloser, context) in enclosers {
      syntax_elements.insert(tag, (SyntaxElement::Encloser(encloser), context));
    }
    for (tag, symmetric_encloser, context) in symmetric_enclosers {
      syntax_elements.insert(
        tag,
        (
          SyntaxElement::SymmetricEncloser(symmetric_encloser),
          context,
        ),
      );
    }
    for (tag, operator, context) in operators {
      syntax_elements.insert(tag, (SyntaxElement::Operator(operator), context));
    }
    Self {
      root,
      contexts,
      syntax_elements,
    }
  }
  pub fn get_context(
    &self,
    context_tag: &ContextTag,
  ) -> &SyntaxContext<'s, Tag> {
    &self.contexts[&context_tag]
  }
  pub fn get_context_tag(&self, tag: &Tag) -> &ContextTag {
    &self.syntax_elements[tag].1
  }
  pub(crate) fn get_beginning_marker(&self, tag: &Tag) -> &str {
    match &self.syntax_elements[tag].0 {
      SyntaxElement::Encloser(encloser) => encloser.opening_encloser_str(),
      SyntaxElement::SymmetricEncloser(symmetric_encloser) => {
        symmetric_encloser.encloser_str()
      }
      SyntaxElement::Operator(operator) => operator.op_str(),
      SyntaxElement::_Unusable(_, _) => unreachable!(),
    }
  }
  pub(crate) fn get_left_arg_count(&self, tag: &Tag) -> usize {
    match &self.syntax_elements[tag].0 {
      SyntaxElement::Encloser(_) => 0,
      SyntaxElement::SymmetricEncloser(_) => 0,
      SyntaxElement::Operator(operator) => operator.left_args(),
      SyntaxElement::_Unusable(_, _) => unreachable!(),
    }
  }
  pub(crate) fn get_tag_scope(&self, tag: &Tag) -> SyntaxScope {
    match &self.syntax_elements[tag].0 {
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
  /*pub(crate) fn get_active_closers(&'s self, tag: &Tag) -> Vec<&'s str> {
    match &self.syntax_elements[tag] {
      SyntaxElement::Encloser(encloser) => encloser.child_tags(),
      SyntaxElement::SymmetricEncloser(symmetric_encloser) => {
        symmetric_encloser.child_tags()
      }
      SyntaxElement::Operator(operator) => operator.child_tags(),
      SyntaxElement::_Unusable(_, _) => unreachable!(),
    }
    .iter()
    .filter_map(|child_tag| {
      if let SyntaxScope::Enclosed { awaited_closer } =
        self.get_tag_scope(child_tag)
      {
        Some(awaited_closer)
      } else {
        None
      }
    })
    .collect()
  }*/
  pub fn parse(
    &'s self,
    text: &'s str,
  ) -> Result<TaggedSexp<'s, Tag>, ParseError> {
    Parse::new(&self, text).complete()
  }
  pub fn parse_to_sexp(
    &'s self,
    text: &'s str,
  ) -> Result<Sexp<'s>, ParseError> {
    self.parse(text).map(|tagged_sexp| tagged_sexp.into())
  }
}
