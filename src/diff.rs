use crate::{Ast, InvalidTreePath};
use std::fmt::Debug;

type Path = Vec<usize>;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum AstSource<
  L: Clone + PartialEq + Eq + Debug + Default,
  I: Clone + PartialEq + Eq + Debug,
> {
  New(Ast<L, I>),
  Existing(Path),
}

impl<
    L: Clone + PartialEq + Eq + Debug + Default,
    I: Clone + PartialEq + Eq + Debug,
  > AstSource<L, I>
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

impl<
    L: Clone + PartialEq + Eq + Debug + Default,
    I: Clone + PartialEq + Eq + Debug,
  > From<Ast<L, I>> for AstSource<L, I>
{
  fn from(ast: Ast<L, I>) -> Self {
    AstSource::New(ast)
  }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum AstDiff<
  L: Clone + PartialEq + Eq + Debug + Default,
  I: Clone + PartialEq + Eq + Debug,
> {
  Insert(Path, AstSource<L, I>),
  Delete(Path),
  Replace(Path, AstSource<L, I>),
  InsertSnippet(Path, AstSource<L, I>, Path),
  DeleteSnippet(Path, Path),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct DiffApplicationError<
  L: Clone + PartialEq + Eq + Debug + Default,
  I: Clone + PartialEq + Eq + Debug,
> {
  diff: AstDiff<L, I>,
  trees: Vec<Ast<L, I>>,
}

impl<
    L: Clone + PartialEq + Eq + Debug + Default,
    I: Clone + PartialEq + Eq + Debug,
  > AstDiff<L, I>
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
        AstDiff::InsertSnippet(path, source, sub_path) => {
          let mut tree = if let Ok(tree) = source.ast(trees) {
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
              std::mem::swap(&mut tree, &mut trees[index]);
              let Ok(hole) = trees[index].get_subtree_mut(&sub_path) else {
                break 'breakable false;
              };
              std::mem::swap(&mut tree, hole);
              true
            }
            n => match trees[path[0]].get_subtree_mut(&path[1..(n - 1)]) {
              Ok(Ast::Inner(_, children)) => {
                let index = path[n - 1];
                if index >= children.len() {
                  break 'breakable false;
                }
                std::mem::swap(&mut tree, &mut children[index]);
                let Ok(hole) = children[index].get_subtree_mut(&sub_path)
                else {
                  break 'breakable false;
                };
                std::mem::swap(&mut tree, hole);
                true
              }
              _ => false,
            },
          }
        }
        AstDiff::DeleteSnippet(path, sub_path) => match path.len() {
          0 => false,
          1 => {
            let index = path[0];
            if index >= trees.len() {
              break 'breakable false;
            }
            let mut inner = Ast::Leaf(L::default(), String::new());
            std::mem::swap(&mut trees[index], &mut inner);
            let Ok(remaining_subtree) = inner.get_subtree_mut(&sub_path) else {
              break 'breakable false;
            };
            std::mem::swap(&mut trees[index], remaining_subtree);
            true
          }
          n => match trees[path[0]].get_subtree_mut(&path[1..(n - 1)]) {
            Ok(Ast::Inner(_, children)) => {
              let index = path[n - 1];
              if index >= children.len() {
                break 'breakable false;
              }
              let mut inner = Ast::Leaf(L::default(), String::new());
              std::mem::swap(&mut children[index], &mut inner);
              let Ok(remaining_subtree) = inner.get_subtree_mut(&sub_path)
              else {
                break 'breakable false;
              };
              std::mem::swap(&mut children[index], remaining_subtree);
              true
            }
            _ => false,
          },
        },
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
  pub fn reverse(
    &self,
    trees: &Vec<Ast<L, I>>,
  ) -> Result<Self, InvalidTreePath> {
    Ok(match self {
      AstDiff::Insert(path, _) => AstDiff::Delete(path.clone()),
      AstDiff::Delete(path) => AstDiff::Insert(
        path.clone(),
        trees[path[0]].get_subtree(&path[1..])?.clone().into(),
      ),
      AstDiff::Replace(path, _) => AstDiff::Replace(
        path.clone(),
        trees[path[0]].get_subtree(&path[1..])?.clone().into(),
      ),
      AstDiff::InsertSnippet(path, _, sub_path) => {
        AstDiff::DeleteSnippet(path.clone(), sub_path.clone())
      }
      AstDiff::DeleteSnippet(path, sub_path) => AstDiff::InsertSnippet(
        path.clone(),
        trees[path[0]].get_subtree(&path[1..])?.clone().into(),
        sub_path.clone(),
      ),
    })
  }
}
