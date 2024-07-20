use std::fmt;
use std::fmt::Debug;

use crate::{Encloser, Operator};

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TaggedSexp<E: Encloser, O: Operator> {
  Leaf(String),
  Enclosed(E, Vec<TaggedSexp<E, O>>),
  Operated(O, Vec<TaggedSexp<E, O>>),
}

impl<E: Encloser, O: Operator> From<TaggedSexp<E, O>> for Sexp {
  fn from(tagged_sexp: TaggedSexp<E, O>) -> Self {
    match tagged_sexp {
      TaggedSexp::Leaf(leaf) => Sexp::Leaf(leaf),
      TaggedSexp::Enclosed(encloser, sub_sexps) => Sexp::List({
        let translated_sub_sexps =
          sub_sexps.into_iter().map(|sub_sexp| sub_sexp.into());
        let tag_str = encloser.id_str();
        if tag_str.is_empty() {
          translated_sub_sexps.collect()
        } else {
          std::iter::once(Sexp::Leaf(tag_str.to_string()))
            .chain(translated_sub_sexps)
            .collect()
        }
      }),
      TaggedSexp::Operated(operator, sub_sexps) => Sexp::List({
        let translated_sub_sexps =
          sub_sexps.into_iter().map(|sub_sexp| sub_sexp.into());
        let tag_str = operator.id_str();
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
