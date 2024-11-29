use crate::{
  document::DocumentPosition, Ast, DocumentSyntaxTree, Encloser,
  EncloserOrOperator, Operator,
};
use std::fmt::Debug;

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct FormatingContext {}

impl FormatingContext {
  pub fn separator(&self) -> &str {
    " "
  }
}
impl Default for FormatingContext {
  fn default() -> Self {
    Self {}
  }
}

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

impl<E: Encloser, O: Operator> EncloserOrOperator<E, O> {
  pub fn format(
    &self,
    mut child_strings: Vec<String>,
    formatting_context: &FormatingContext,
  ) -> FormattingResult<String> {
    let separator = formatting_context.separator();
    match self {
      EncloserOrOperator::Encloser(encloser) => Ok(format!(
        "{}{}{}",
        encloser.opening_encloser_str(),
        child_strings.join(separator),
        encloser.closing_encloser_str()
      )),
      EncloserOrOperator::Operator(operator) => {
        let left_args = operator.left_args();
        let right_args = operator.right_args();
        if left_args + right_args == child_strings.len() {
          let right_children = child_strings.split_off(left_args);
          Ok(format!(
            "{}{}{}",
            child_strings.join(separator),
            operator.op_str(),
            right_children.join(separator)
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
  pub fn format(
    &self,
    formatting_context: &FormatingContext,
  ) -> FormattingResult<String> {
    match self {
      Ast::Leaf(_, leaf) => Ok(leaf.clone()),
      Ast::Inner(inner_label, children) => {
        inner_label.get_encloser_or_operator().format(
          children
            .into_iter()
            .map(|child| child.format(formatting_context))
            .collect::<FormattingResult<Vec<String>>>()?,
          formatting_context,
        )
      }
    }
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
