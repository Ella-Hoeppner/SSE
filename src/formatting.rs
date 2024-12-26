use crate::{
  document::DocumentPosition, Ast, Encloser, EncloserOrOperator, Operator,
};
use std::fmt::Debug;

#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum FormattingStyle {
  SingleLine,
  MultiLineAlignedToSecond,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub struct FormattingContext {
  indentation: usize,
}

impl FormattingContext {
  fn indent(mut self, indentation: usize) -> Self {
    self.indentation += indentation;
    self
  }
}

use FormattingStyle::*;

#[derive(Debug, Clone, Hash, PartialEq)]
pub enum FormattingError {
  WrongOperatorChildCount {
    operator_id: String,
    left_arg_count: usize,
    right_arg_count: usize,
    actual_child_count: usize,
  },
}
pub type FormattingResult<T> = Result<T, FormattingError>;

pub trait ContainsEncloserOrOperator {
  type E: Encloser;
  type O: Operator;
  fn get_encloser_or_operator(&self) -> &EncloserOrOperator<Self::E, Self::O>;
}

impl<E: Encloser, O: Operator> ContainsEncloserOrOperator
  for EncloserOrOperator<E, O>
{
  type E = E;
  type O = O;
  fn get_encloser_or_operator(&self) -> &EncloserOrOperator<Self::E, Self::O> {
    self
  }
}

impl<
    LeafData: Clone + PartialEq + Eq + Debug,
    InnerData: Clone + PartialEq + Eq + Debug + ContainsEncloserOrOperator,
  > Ast<LeafData, InnerData>
{
  pub fn format_inner(
    &self,
    style: FormattingStyle,
    context: FormattingContext,
  ) -> FormattingResult<String> {
    match self {
      Ast::Leaf(_, leaf) => Ok(leaf.clone()),
      Ast::Inner(inner_label, children) => {
        match inner_label.get_encloser_or_operator() {
          EncloserOrOperator::Encloser(encloser) => Ok({
            match style {
              SingleLine => format!(
                "{}{}{}",
                encloser.opening_encloser_str(),
                children
                  .into_iter()
                  .map(|child| child.format_inner(style, context))
                  .collect::<FormattingResult<Vec<String>>>()?
                  .join(" "),
                encloser.closing_encloser_str()
              ),
              MultiLineAlignedToSecond => {
                let opener = encloser.opening_encloser_str();
                let closer = encloser.closing_encloser_str();
                match children.len() {
                  0 => opener.to_string() + closer,
                  1 => {
                    opener.to_string()
                      + &children[0].format_inner(style, context)?
                      + closer
                  }
                  _ => {
                    let first_child_string =
                      children[0].format_inner(style, context)?;
                    let inner_context = context
                      .indent(opener.len() + first_child_string.len() + 1);
                    let child_strings = children
                      .iter()
                      .skip(1)
                      .map(|child| child.format_inner(style, inner_context))
                      .collect::<FormattingResult<Vec<String>>>()?;
                    format!(
                      "{opener}{first_child_string} {}{closer}",
                      child_strings
                        .into_iter()
                        .map(|child| child)
                        .collect::<Vec<String>>()
                        .join(
                          &("\n".to_string()
                            + &" ".repeat(inner_context.indentation))
                        )
                    )
                  }
                }
              }
            }
          }),
          EncloserOrOperator::Operator(operator) => {
            let mut child_strings = children
              .into_iter()
              .map(|child| child.format_inner(style, context))
              .collect::<FormattingResult<Vec<String>>>()?;
            let left_args = operator.left_args();
            let right_args = operator.right_args();
            if left_args + right_args == child_strings.len() {
              let right_children = child_strings.split_off(left_args);
              Ok(format!(
                "{}{}{}",
                child_strings.join(" "),
                operator.op_str(),
                right_children.join(" ")
              ))
            } else {
              Err(FormattingError::WrongOperatorChildCount {
                operator_id: operator.id_str().to_string(),
                left_arg_count: left_args,
                right_arg_count: right_args,
                actual_child_count: child_strings.len(),
              })
            }
          }
        }
      }
    }
  }
  pub fn format(&self, style: FormattingStyle) -> FormattingResult<String> {
    self.format_inner(style, FormattingContext { indentation: 0 })
  }
}

impl<E: Encloser, O: Operator> ContainsEncloserOrOperator
  for (DocumentPosition, EncloserOrOperator<E, O>)
{
  type E = E;
  type O = O;

  fn get_encloser_or_operator(&self) -> &EncloserOrOperator<Self::E, Self::O> {
    &self.1
  }
}
