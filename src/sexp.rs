use std::fmt;
use std::fmt::Debug;

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
