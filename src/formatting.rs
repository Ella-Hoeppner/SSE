use crate::{
  document::{Document, DocumentPosition},
  syntax::Syntax,
  Ast, DocumentSyntaxTree, Encloser, EncloserOrOperator, Operator,
};

pub trait Formatter<E: Encloser, O: Operator> {
  fn format_encloser(
    &mut self,
    encloser: &E,
    document_position: &DocumentPosition,
    children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String;
  fn format_operator_left(
    &mut self,
    operator: &O,
    document_position: &DocumentPosition,
    left_children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String;
  fn format_operator_right(
    &mut self,
    operator: &O,
    document_position: &DocumentPosition,
    right_children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String;
  fn format(&mut self, ast: DocumentSyntaxTree<E, O>) -> String {
    match ast {
      Ast::Leaf(_, leaf) => leaf,
      Ast::Inner((document_position, encloser_or_operator), mut children) => {
        match encloser_or_operator {
          EncloserOrOperator::Encloser(encloser) => {
            encloser.opening_encloser_str().to_string()
              + &self.format_encloser(&encloser, &document_position, children)
              + encloser.closing_encloser_str()
          }
          EncloserOrOperator::Operator(operator) => {
            let right_children = children.split_off(operator.left_args());
            self.format_operator_left(&operator, &document_position, children)
              + operator.op_str()
              + &self.format_operator_right(
                &operator,
                &document_position,
                right_children,
              )
          }
        }
      }
    }
  }
  fn format_document<'t, S: Syntax<E = E, O = O>>(
    &mut self,
    document: Document<'t, S>,
  ) -> String {
    document
      .syntax_trees
      .into_iter()
      .fold(String::new(), |mut s, ast| {
        s += &self.format(ast);
        s
      })
  }
}

pub struct SingleLineFormatter;
impl SingleLineFormatter {
  fn format_children<E: Encloser, O: Operator>(
    &mut self,
    children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String {
    let child_strings = children
      .into_iter()
      .map(|child| self.format(child))
      .collect::<Vec<String>>();
    child_strings.join(" ")
  }
}

impl<E: Encloser, O: Operator> Formatter<E, O> for SingleLineFormatter {
  fn format_encloser(
    &mut self,
    _encloser: &E,
    _document_position: &DocumentPosition,
    children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String {
    self.format_children(children)
  }

  fn format_operator_left(
    &mut self,
    _operator: &O,
    _document_position: &DocumentPosition,
    left_children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String {
    self.format_children(left_children)
  }

  fn format_operator_right(
    &mut self,
    _operator: &O,
    _document_position: &DocumentPosition,
    right_children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String {
    self.format_children(right_children)
  }
}

pub struct AlignedToSecondFormatter {
  indentation: usize,
}
impl AlignedToSecondFormatter {
  fn format_children_single_line<E: Encloser, O: Operator>(
    &mut self,
    children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String {
    let child_strings = children
      .into_iter()
      .map(|child| self.format(child))
      .collect::<Vec<String>>();
    child_strings.join(" ")
  }
}

impl<E: Encloser, O: Operator> Formatter<E, O> for AlignedToSecondFormatter {
  fn format_encloser(
    &mut self,
    encloser: &E,
    _document_position: &DocumentPosition,
    mut children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String {
    match children.len() {
      0 => String::new(),
      1 => self.format(children.remove(0)),
      _ => {
        let mut s = self.format(children.remove(0));
        let inner_indentation =
          encloser.opening_encloser_str().len() + s.len() + 1;
        self.indentation += inner_indentation;
        s += " ";
        s += &children
          .into_iter()
          .map(|child| self.format(child))
          .collect::<Vec<String>>()
          .join(&("\n".to_string() + &" ".repeat(self.indentation)));
        self.indentation -= inner_indentation;
        s
      }
    }
  }

  fn format_operator_left(
    &mut self,
    _operator: &O,
    _document_position: &DocumentPosition,
    left_children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String {
    self.format_children_single_line(left_children)
  }

  fn format_operator_right(
    &mut self,
    _operator: &O,
    _document_position: &DocumentPosition,
    right_children: Vec<DocumentSyntaxTree<E, O>>,
  ) -> String {
    self.format_children_single_line(right_children)
  }
}

impl Default for AlignedToSecondFormatter {
  fn default() -> Self {
    Self { indentation: 0 }
  }
}
