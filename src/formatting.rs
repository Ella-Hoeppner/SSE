use crate::{
  document::DocumentPosition, Ast, Encloser, EncloserOrOperator, Operator,
};
use std::{collections::HashMap, fmt::Debug, hash::Hash};
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

#[derive(Debug, Clone, Copy, Hash, PartialEq)]
pub enum FormattingStyle {
  SingleLine,
  MultiLineAlignedToSecond { single_line_threshold: usize },
  MultiLineOffsetByConstantAfterSecond { offset: usize },
  NPerLine { n: usize },
}

use FormattingStyle::*;

#[derive(Debug, Clone, PartialEq)]
pub struct FormattingContext {
  indentation: usize,
}

impl FormattingContext {
  fn indent(mut self, indentation: usize) -> Self {
    self.indentation += indentation;
    self
  }
  fn indented_newline(&self) -> String {
    "\n".to_string() + &" ".repeat(self.indentation)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FormattingSpecialCase<
  LeafData: Clone + PartialEq + Eq + Debug,
  InnerData: Clone + PartialEq + Eq + Debug + ContainsEncloserOrOperator,
> {
  pub pattern: Ast<LeafData, InnerData>,
  pub holes: Vec<(String, bool)>,
  pub style: Option<FormattingStyle>,
  pub hole_styles: HashMap<String, FormattingStyle>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Formatter<
  LeafData: Clone + PartialEq + Eq + Debug,
  InnerData: Clone + PartialEq + Eq + Debug + ContainsEncloserOrOperator,
> {
  default_style: FormattingStyle,
  special_cases: Vec<FormattingSpecialCase<LeafData, InnerData>>,
}

fn narrow_paths<T: Clone>(
  styles: &HashMap<Vec<usize>, T>,
  child_index: usize,
) -> HashMap<Vec<usize>, T> {
  styles
    .iter()
    .fold(HashMap::new(), |mut substyles, (path, value)| {
      if !path.is_empty() && path[0] == child_index {
        substyles.insert(path.iter().skip(1).copied().collect(), value.clone());
      }
      substyles
    })
}

impl<
    LeafData: Clone + PartialEq + Eq + Debug,
    InnerData: Clone + PartialEq + Eq + Debug + ContainsEncloserOrOperator,
  > Formatter<LeafData, InnerData>
{
  pub fn new_with_default_style(default_style: FormattingStyle) -> Self {
    Self {
      default_style,
      special_cases: vec![],
    }
  }
  pub fn with_special_case(
    mut self,
    case: FormattingSpecialCase<LeafData, InnerData>,
  ) -> Self {
    self.special_cases.push(case);
    self
  }
  pub fn format_children(
    &self,
    mut children: Vec<Ast<LeafData, InnerData>>,
    predetermined_closed_styles: HashMap<Vec<usize>, FormattingStyle>,
    predetermined_open_styles: HashMap<Vec<usize>, (usize, FormattingStyle)>,
    context: &FormattingContext,
  ) -> FormattingResult<String> {
    let style = predetermined_closed_styles
      .get(&vec![])
      .copied()
      .unwrap_or(self.default_style);
    let open_style = predetermined_open_styles.get(&vec![]).copied();
    let removed_children = open_style
      .map(|(beginning, _)| children.split_off(beginning))
      .unwrap_or(vec![]);
    let format_open_style_children = |context: &FormattingContext| {
      self.format_children(
        removed_children,
        [(vec![], open_style.unwrap().1)].into_iter().collect(),
        HashMap::new(),
        context,
      )
    };
    Ok(match style {
      SingleLine => {
        let s = children
          .into_iter()
          .map(|child| {
            child.format_inner(
              self,
              [(vec![], SingleLine)].into_iter().collect(),
              HashMap::new(),
              context,
            )
          })
          .collect::<FormattingResult<Vec<String>>>()?
          .join(" ");
        if open_style.is_some() {
          s + " " + &format_open_style_children(context)?
        } else {
          s
        }
      }
      MultiLineAlignedToSecond {
        single_line_threshold,
      } => match children.len() {
        0 => {
          if open_style.is_some() {
            format_open_style_children(context)?
          } else {
            String::new()
          }
        }
        _ => {
          let first_child_string = children.remove(0).format_inner(
            self,
            narrow_paths(&predetermined_closed_styles, 0),
            narrow_paths(&predetermined_open_styles, 0),
            context,
          )?;
          let inner_context =
            context.clone().indent(first_child_string.len() + 1);
          let mut child_strings = children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
              child.format_inner(
                self,
                narrow_paths(&predetermined_closed_styles, i + 1),
                narrow_paths(&predetermined_open_styles, i + 1),
                &inner_context,
              )
            })
            .collect::<FormattingResult<Vec<String>>>()?;
          if open_style.is_some() {
            child_strings.push(format_open_style_children(&inner_context)?);
          }
          let total_length = first_child_string.len()
            + child_strings.len()
            + child_strings.iter().map(|child| child.len()).sum::<usize>();
          if total_length <= single_line_threshold {
            format!("{} {}", first_child_string, child_strings.join(" "))
          } else {
            first_child_string
              + " "
              + &child_strings
                .into_iter()
                .map(|child| child)
                .collect::<Vec<String>>()
                .join(&inner_context.indented_newline())
          }
        }
      },
      MultiLineOffsetByConstantAfterSecond { offset } => match children.len() {
        0 => String::new(),
        _ => {
          let first_child_string = children.remove(0).format_inner(
            self,
            narrow_paths(&predetermined_closed_styles, 0),
            narrow_paths(&predetermined_open_styles, 0),
            context,
          )?;
          let inner_context = context.clone().indent(offset);
          let mut child_strings = children
            .into_iter()
            .enumerate()
            .map(|(i, child)| {
              if i == 0 {
                child.format_inner(
                  self,
                  narrow_paths(&predetermined_closed_styles, 1),
                  narrow_paths(&predetermined_open_styles, 1),
                  &context.clone().indent(first_child_string.len() + 1),
                )
              } else {
                child.format_inner(
                  self,
                  narrow_paths(&predetermined_closed_styles, i + 1),
                  narrow_paths(&predetermined_open_styles, i + 1),
                  &inner_context,
                )
              }
            })
            .collect::<FormattingResult<Vec<String>>>()?;
          if open_style.is_some() {
            child_strings.push(format_open_style_children(&inner_context)?);
          }
          first_child_string
            + " "
            + &child_strings
              .into_iter()
              .map(|child| child)
              .collect::<Vec<String>>()
              .join(&inner_context.indented_newline())
        }
      },
      NPerLine { n } => {
        let mut s = String::new();
        let mut leftover_context = context.clone();
        loop {
          let mut chunk = if children.len() <= n {
            vec![]
          } else {
            children.split_off(n)
          };
          std::mem::swap(&mut chunk, &mut children);
          let chunk_length = chunk.len();
          let mut inner_context = context.clone();
          for (i, child) in chunk.into_iter().enumerate() {
            let child_string = child.clone().format_inner(
              self,
              narrow_paths(&predetermined_closed_styles, i),
              narrow_paths(&predetermined_open_styles, i),
              &inner_context,
            )?;
            if i > 0 {
              s += " ";
            }
            s += &child_string;
            inner_context = inner_context.indent(child_string.len() + 1);
          }
          if children.is_empty() {
            if chunk_length != n {
              leftover_context = inner_context;
            }
            break;
          }
          s += &context.indented_newline();
        }
        if open_style.is_some() {
          s += &format_open_style_children(&leftover_context)?;
        }
        s
      }
    })
  }
  fn check_special_case(
    &self,
    ast: &Ast<LeafData, InnerData>,
  ) -> Option<&FormattingSpecialCase<LeafData, InnerData>> {
    self.special_cases.iter().find(
      |FormattingSpecialCase { pattern, holes, .. }| {
        ast.matches_pattern(pattern, holes, &|_, _| true, &|a, b| {
          a.get_encloser_or_operator() == b.get_encloser_or_operator()
        })
      },
    )
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
  pub fn format_inner(
    self,
    formatter: &Formatter<LeafData, InnerData>,
    mut predetermined_closed_styles: HashMap<Vec<usize>, FormattingStyle>,
    mut predetermined_open_styles: HashMap<
      Vec<usize>,
      (usize, FormattingStyle),
    >,
    context: &FormattingContext,
  ) -> FormattingResult<String> {
    if let Some(special_case) = formatter.check_special_case(&self) {
      println!("matched special case!!");
      special_case
        .style
        .map(|style| predetermined_closed_styles.insert(vec![], style));
      for (hole, open) in special_case.holes.iter() {
        if let Some(style) = special_case.hole_styles.get(hole) {
          if let Some(mut path) = special_case.pattern.find_leaf_path(&hole) {
            if *open {
              let open_range_beginning_index = path.pop().unwrap();
              predetermined_open_styles
                .insert(path, (open_range_beginning_index, *style));
            } else {
              predetermined_closed_styles.insert(path, *style);
            }
          }
        }
      }
      println!(
        "{predetermined_closed_styles:?}\n\n{predetermined_open_styles:?}\n\n"
      )
    }
    match self {
      Ast::Leaf(_, leaf) => Ok(leaf.clone()),
      Ast::Inner(inner_label, children) => {
        match inner_label.get_encloser_or_operator() {
          EncloserOrOperator::Encloser(encloser) => {
            let opener = encloser.opening_encloser_str();
            Ok(
              opener.to_string()
                + &formatter.format_children(
                  children,
                  predetermined_closed_styles,
                  predetermined_open_styles,
                  &context.clone().indent(opener.len()),
                )?
                + encloser.closing_encloser_str(),
            )
          }
          EncloserOrOperator::Operator(operator) => {
            let mut child_strings = children
              .into_iter()
              .map(|child| {
                child.format_inner(
                  formatter,
                  HashMap::new(),
                  HashMap::new(),
                  context,
                )
              })
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
  pub fn format(
    self,
    formatter: &Formatter<LeafData, InnerData>,
  ) -> FormattingResult<String> {
    self.format_inner(
      formatter,
      HashMap::new(),
      HashMap::new(),
      &FormattingContext { indentation: 0 },
    )
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
