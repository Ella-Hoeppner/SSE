use std::fmt;
use std::fmt::Debug;

use crate::syntax::SyntaxTag;

#[derive(Clone, PartialEq, Debug)]
pub enum Sexp {
  List(Vec<Sexp>),
  Leaf(String),
}

impl fmt::Display for Sexp {
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

pub(crate) type TaggedSexpList<Tag> = (Tag, Vec<TaggedSexp<Tag>>);

#[derive(Clone, Debug)]
pub enum TaggedSexp<Tag: SyntaxTag> {
  Leaf(String),
  List(TaggedSexpList<Tag>),
}

impl<Tag: SyntaxTag> From<TaggedSexp<Tag>> for Sexp {
  fn from(tagged_sexp: TaggedSexp<Tag>) -> Self {
    match tagged_sexp {
      TaggedSexp::Leaf(leaf) => Sexp::Leaf(leaf),
      TaggedSexp::List((tag, sub_sexps)) => Sexp::List({
        let translated_sub_sexps =
          sub_sexps.into_iter().map(|sub_sexp| sub_sexp.into());
        let tag_str = tag.tag_str();
        if tag_str.is_empty() {
          translated_sub_sexps.collect()
        } else {
          std::iter::once(Sexp::Leaf(tag_str.to_string()))
            .chain(translated_sub_sexps)
            .collect()
        }
      }),
    }
  }
}
