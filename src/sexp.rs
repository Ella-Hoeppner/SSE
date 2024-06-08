use std::fmt;
use std::fmt::Debug;

use crate::syntax::SyntaxTag;

#[derive(Clone, PartialEq, Debug)]
pub enum Sexp<'s> {
  List(Vec<Sexp<'s>>),
  Leaf(&'s str),
}

impl fmt::Display for Sexp<'_> {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Sexp::Leaf(token) => fmt.write_str(token)?,
      Sexp::List(sub_expressions) => {
        fmt.write_str("(")?;
        let mut separator = "";
        for sexp in sub_expressions {
          fmt.write_str(separator)?;
          fmt.write_str(&sexp.to_string())?;
          separator = " ";
        }
        fmt.write_str(")")?;
      }
    }
    Ok(())
  }
}

pub(crate) type TaggedSexpList<'s, Tag> = (Tag, Vec<TaggedSexp<'s, Tag>>);

#[derive(Clone, Debug)]
pub enum TaggedSexp<'s, Tag: SyntaxTag<'s>> {
  Leaf(&'s str),
  List(TaggedSexpList<'s, Tag>),
}

impl<'s, Tag: SyntaxTag<'s>> From<TaggedSexp<'s, Tag>> for Sexp<'s> {
  fn from(tagged_sexp: TaggedSexp<'s, Tag>) -> Self {
    match tagged_sexp {
      TaggedSexp::Leaf(leaf) => Sexp::Leaf(leaf),
      TaggedSexp::List((tag, sub_sexps)) => Sexp::List({
        let translated_sub_sexps =
          sub_sexps.into_iter().map(|sub_sexp| sub_sexp.into());
        let tag_str = tag.tag_str();
        if tag_str.is_empty() {
          translated_sub_sexps.collect()
        } else {
          std::iter::once(Sexp::Leaf(&tag_str))
            .chain(translated_sub_sexps)
            .collect()
        }
      }),
    }
  }
}
