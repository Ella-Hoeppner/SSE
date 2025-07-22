use crate::{Ast, InvalidTreePath};
use std::fmt::Debug;

type Path = Vec<usize>;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum AstSource<
  L: Clone + PartialEq + Eq + Debug,
  I: Clone + PartialEq + Eq + Debug,
> {
  New(Ast<L, I>),
  Existing(Path),
}

impl<L: Clone + PartialEq + Eq + Debug, I: Clone + PartialEq + Eq + Debug>
  AstSource<L, I>
{
  pub fn ast(
    &self,
    trees: &Vec<Ast<L, I>>,
  ) -> Result<Ast<L, I>, InvalidTreePath> {
    match self {
      AstSource::New(ast) => Ok(ast.clone()),
      AstSource::Existing(path) => match path.len() {
        0 => Err(InvalidTreePath),
        1 => Ok(trees[path[0]].clone()),
        _ => Ok(trees[path[0]].get_subtree(&path[1..])?.clone()),
      },
    }
  }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum AstDiff<
  L: Clone + PartialEq + Eq + Debug,
  I: Clone + PartialEq + Eq + Debug,
> {
  Insert(Path, AstSource<L, I>),
  Delete(Path),
  Replace(Path, AstSource<L, I>),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct DiffApplicationError<
  L: Clone + PartialEq + Eq + Debug,
  I: Clone + PartialEq + Eq + Debug,
> {
  diff: AstDiff<L, I>,
  trees: Vec<Ast<L, I>>,
}

impl<L: Clone + PartialEq + Eq + Debug, I: Clone + PartialEq + Eq + Debug>
  AstDiff<L, I>
{
  pub fn apply<'a: 'b, 'b>(
    self,
    trees: &mut Vec<Ast<L, I>>,
  ) -> Result<&mut Vec<Ast<L, I>>, DiffApplicationError<L, I>> {
    if 'breakable: {
      match &self {
        AstDiff::Insert(path, source) => {
          let tree = if let Ok(tree) = source.ast(trees) {
            tree
          } else {
            break 'breakable true;
          };
          match path.len() {
            0 => false,
            1 => {
              let index = path[0];
              if index > trees.len() {
                break 'breakable false;
              }
              trees.insert(index, tree);
              true
            }
            n => match trees[path[0]].get_subtree_mut(&path[1..(n - 1)]) {
              Ok(Ast::Inner(_, children)) => {
                let index = path[n - 1];
                if index > children.len() {
                  break 'breakable false;
                }
                children.insert(index, tree);
                true
              }
              _ => false,
            },
          }
        }
        AstDiff::Delete(path) => match path.len() {
          0 => false,
          1 => {
            let index = path[0];
            if index >= trees.len() {
              break 'breakable false;
            }
            trees.remove(index);
            true
          }
          n => match trees[path[0]].get_subtree_mut(&path[1..(n - 1)]) {
            Ok(Ast::Inner(_, children)) => {
              let index = path[n - 1];
              if index >= children.len() {
                break 'breakable false;
              }
              children.remove(index);
              true
            }
            _ => false,
          },
        },
        AstDiff::Replace(path, source) => {
          let tree = if let Ok(tree) = source.ast(trees) {
            tree
          } else {
            break 'breakable true;
          };
          match path.len() {
            0 => false,
            1 => {
              let index = path[0];
              if index >= trees.len() {
                break 'breakable false;
              }
              trees[index] = tree;
              true
            }
            n => match trees[path[0]].get_subtree_mut(&path[1..(n - 1)]) {
              Ok(Ast::Inner(_, children)) => {
                let index = path[n - 1];
                if index >= children.len() {
                  break 'breakable false;
                }
                children[index] = tree;
                true
              }
              _ => false,
            },
          }
        }
      }
    } {
      Ok(trees)
    } else {
      Err(DiffApplicationError {
        diff: self,
        trees: trees.clone(),
      })
    }
  }
}
