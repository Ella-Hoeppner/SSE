use std::fmt;
use std::fmt::Debug;
use std::marker::PhantomData;

use crate::syntax::SyntaxTag;

#[derive(Clone, PartialEq, Debug)]
pub enum Sexp<'t> {
  List(Vec<Sexp<'t>>),
  Leaf(&'t str),
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

pub(crate) type TaggedSexpList<'t, 'g, Tag> =
  (Tag, Vec<TaggedSexp<'t, 'g, Tag>>);

#[derive(Clone, Debug)]
pub enum TaggedSexp<'t, 'g, Tag: SyntaxTag<'g>> {
  Leaf(&'t str),
  List(TaggedSexpList<'t, 'g, Tag>, &'g PhantomData<()>),
}

impl<'t, 'g: 't, Tag: SyntaxTag<'g>> From<TaggedSexp<'t, 'g, Tag>>
  for Sexp<'t>
{
  fn from(tagged_sexp: TaggedSexp<'t, 'g, Tag>) -> Self {
    match tagged_sexp {
      TaggedSexp::Leaf(leaf) => Sexp::Leaf(leaf),
      TaggedSexp::List((tag, sub_sexps), _) => Sexp::List({
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
