use std::{collections::HashMap, fmt::Debug, hash::Hash, marker::PhantomData};

pub trait SyntaxTag: Clone + Debug + PartialEq + Eq + Hash {
  fn tag_str(&self) -> &str;
}

pub trait Encloser<Tag: SyntaxTag> {
  fn opening_encloser_str(&self) -> &str;
  fn closing_encloser_str(&self) -> &str;
}

pub trait Operator<Tag: SyntaxTag> {
  fn left_args(&self) -> usize;
  fn right_args(&self) -> usize;
  fn op_str(&self) -> &str;
}

#[derive(Debug, Clone, Hash)]
pub(crate) enum SyntaxElement<
  Tag: SyntaxTag,
  E: Encloser<Tag>,
  O: Operator<Tag>,
> {
  Encloser(E),
  Operator(O),
  _Unusable(PhantomData<Tag>),
}

#[derive(Clone, Debug)]
pub struct SyntaxContext<Tag: SyntaxTag> {
  whitespace_chars: Vec<char>,
  pub(crate) escape_char: Option<char>,
  tags: Vec<Tag>,
}
impl<'g, Tag: SyntaxTag> SyntaxContext<Tag> {
  pub fn new(
    tags: Vec<Tag>,
    escape_char: Option<char>,
    whitespace_chars: Vec<char>,
  ) -> Self {
    Self {
      whitespace_chars,
      escape_char,
      tags,
    }
  }
  pub fn tags(&self) -> &[Tag] {
    &self.tags
  }
  pub fn is_whitespace(&self, c: char) -> bool {
    self.whitespace_chars.contains(&c)
  }
}

#[derive(Debug, Clone)]
pub struct SyntaxGraph<
  Tag: SyntaxTag,
  ContextTag: Clone + Debug + PartialEq + Eq + Hash,
  E: Encloser<Tag>,
  O: Operator<Tag>,
> {
  pub(crate) root: ContextTag,
  contexts: HashMap<ContextTag, SyntaxContext<Tag>>,
  syntax_elements: HashMap<Tag, (SyntaxElement<Tag, E, O>, ContextTag)>,
}

impl<
    Tag: SyntaxTag,
    ContextTag: Clone + Debug + PartialEq + Eq + Hash,
    E: Encloser<Tag>,
    O: Operator<Tag>,
  > SyntaxGraph<Tag, ContextTag, E, O>
{
  pub fn new(
    root: ContextTag,
    contexts: HashMap<ContextTag, SyntaxContext<Tag>>,
    enclosers: Vec<(Tag, E, ContextTag)>,
    operators: Vec<(Tag, O, ContextTag)>,
  ) -> Self {
    let mut syntax_elements = HashMap::new();
    for (tag, encloser, context) in enclosers {
      syntax_elements.insert(tag, (SyntaxElement::Encloser(encloser), context));
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
  pub fn get_context(&self, context_tag: &ContextTag) -> &SyntaxContext<Tag> {
    &self.contexts[&context_tag]
  }
  pub fn get_context_tag(&self, tag: &Tag) -> &ContextTag {
    &self.syntax_elements[tag].1
  }
  pub(crate) fn get_beginning_marker(&self, tag: &Tag) -> &str {
    match &self.syntax_elements[tag].0 {
      SyntaxElement::Encloser(encloser) => encloser.opening_encloser_str(),
      SyntaxElement::Operator(operator) => operator.op_str(),
      SyntaxElement::_Unusable(_) => unreachable!(),
    }
  }
  pub(crate) fn get_tag_element(&self, tag: &Tag) -> &SyntaxElement<Tag, E, O> {
    &self.syntax_elements[tag].0
  }
  pub(crate) fn get_asymmetric_closers<'g>(
    &'g self,
    context_tag: &ContextTag,
  ) -> Vec<&'g str> {
    self.contexts[context_tag]
      .tags()
      .iter()
      .filter_map(|child_tag| match self.get_tag_element(child_tag) {
        SyntaxElement::Encloser(encloser) => {
          Some(encloser.closing_encloser_str())
        }
        _ => None,
      })
      .collect()
  }
}
