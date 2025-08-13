use crate::{Ast, InvalidTreePath};
use std::fmt::Debug;

fn is_prefix_of(a: &[usize], b: &[usize]) -> bool {
  a.len() < b.len() && a.iter().zip(b.iter()).find(|(a, b)| a != b).is_none()
}

pub trait AstWrapper<
  L: Clone + PartialEq + Eq + Debug,
  I: Clone + PartialEq + Eq + Debug,
>
{
  fn ast<'a>(&'a self) -> &'a Ast<L, I>;
  fn ast_mut<'a>(&'a mut self) -> &'a mut Ast<L, I>;
  fn from_raw(ast: Ast<L, I>) -> Self;
}

impl<L: Clone + PartialEq + Eq + Debug, I: Clone + PartialEq + Eq + Debug>
  AstWrapper<L, I> for Ast<L, I>
{
  fn ast(&self) -> &Ast<L, I> {
    self
  }
  fn ast_mut(&mut self) -> &mut Ast<L, I> {
    self
  }
  fn from_raw(ast: Ast<L, I>) -> Self {
    ast
  }
}

type Path = Vec<usize>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AstSource<
  L: Clone + PartialEq + Eq + Debug,
  I: Clone + PartialEq + Eq + Debug,
> {
  New(Ast<L, I>),
  Existing(Path),
}

impl<
    L: Clone + PartialEq + Eq + Debug,
    I: Clone + PartialEq + Eq + Debug,
    P: Into<Path>,
  > From<P> for AstSource<I, L>
{
  fn from(path: P) -> Self {
    Self::Existing(path.into())
  }
}

impl<L: Clone + PartialEq + Eq + Debug, I: Clone + PartialEq + Eq + Debug>
  From<Ast<L, I>> for AstSource<L, I>
{
  fn from(ast: Ast<L, I>) -> Self {
    AstSource::New(ast)
  }
}

impl<L: Clone + PartialEq + Eq + Debug, I: Clone + PartialEq + Eq + Debug>
  AstSource<L, I>
{
  pub fn ast<'a, W: AstWrapper<L, I>>(
    &'a self,
    trees: &'a Vec<W>,
  ) -> Result<&'a Ast<L, I>, InvalidTreePath> {
    match self {
      AstSource::New(ast) => Ok(ast),
      AstSource::Existing(path) => match path.len() {
        0 => Err(InvalidTreePath),
        1 => Ok(trees[path[0]].ast()),
        _ => Ok(trees[path[0]].ast().get_subtree(&path[1..])?),
      },
    }
  }
  pub fn try_map<
    NewL: Clone + PartialEq + Eq + Debug,
    NewI: Clone + PartialEq + Eq + Debug,
    E,
  >(
    self,
    ast_mapper: &impl Fn(Ast<L, I>) -> Result<Ast<NewL, NewI>, E>,
  ) -> Result<AstSource<NewL, NewI>, E> {
    Ok(match self {
      AstSource::New(ast) => AstSource::New(ast_mapper(ast)?),
      AstSource::Existing(path) => AstSource::Existing(path),
    })
  }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct AstDiff<
  L: Clone + PartialEq + Eq + Debug,
  I: Clone + PartialEq + Eq + Debug,
> {
  pub path: Path,
  pub op: AstDiffOp<L, I>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AstDiffOp<
  L: Clone + PartialEq + Eq + Debug,
  I: Clone + PartialEq + Eq + Debug,
> {
  Insert(AstSource<L, I>),
  Delete,
  Replace(AstSource<L, I>),
  InsertSnippet(AstSource<L, I>, Path),
  DeleteSnippet(Path),
}

#[derive(Debug)]
enum Never {}

impl<L: Clone + PartialEq + Eq + Debug, I: Clone + PartialEq + Eq + Debug>
  AstDiff<L, I>
{
  pub fn insert<P: Into<Path>, S: Into<AstSource<L, I>>>(
    path: P,
    source: S,
  ) -> Self {
    Self {
      path: path.into(),
      op: AstDiffOp::Insert(source.into()),
    }
  }
  pub fn delete<P: Into<Path>>(path: P) -> Self {
    Self {
      path: path.into(),
      op: AstDiffOp::Delete,
    }
  }
  pub fn replace<P: Into<Path>, S: Into<AstSource<L, I>>>(
    path: P,
    source: S,
  ) -> Self {
    Self {
      path: path.into(),
      op: AstDiffOp::Replace(source.into()),
    }
  }
  pub fn insert_snippet<
    P1: Into<Path>,
    P2: Into<Path>,
    S: Into<AstSource<L, I>>,
  >(
    path: P1,
    source: S,
    sub_path: P2,
  ) -> Self {
    Self {
      path: path.into(),
      op: AstDiffOp::InsertSnippet(source.into(), sub_path.into()),
    }
  }
  pub fn delete_snippet<P1: Into<Path>, P2: Into<Path>>(
    path: P1,
    sub_path: P2,
  ) -> Self {
    Self {
      path: path.into(),
      op: AstDiffOp::DeleteSnippet(sub_path.into()),
    }
  }
  pub fn apply<'a: 'b, 'b, W: AstWrapper<L, I>>(
    self,
    trees: &mut Vec<W>,
  ) -> Result<&mut Vec<W>, Self> {
    if 'breakable: {
      let path = &self.path;
      match &self.op {
        AstDiffOp::Insert(source) => {
          let tree = if let Ok(tree) = source.ast(trees) {
            tree.clone()
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
              trees.insert(index, W::from_raw(tree));
              true
            }
            n => {
              match trees[path[0]].ast_mut().get_subtree_mut(&path[1..(n - 1)])
              {
                Ok(Ast::Inner(_, children)) => {
                  let index = path[n - 1];
                  if index > children.len() {
                    break 'breakable false;
                  }
                  children.insert(index, tree);
                  true
                }
                _ => false,
              }
            }
          }
        }
        AstDiffOp::Delete => match path.len() {
          0 => false,
          1 => {
            let index = path[0];
            if index >= trees.len() {
              break 'breakable false;
            }
            trees.remove(index);
            true
          }
          n => {
            match trees[path[0]].ast_mut().get_subtree_mut(&path[1..(n - 1)]) {
              Ok(Ast::Inner(_, children)) => {
                let index = path[n - 1];
                if index >= children.len() {
                  break 'breakable false;
                }
                children.remove(index);
                true
              }
              _ => false,
            }
          }
        },
        AstDiffOp::Replace(source) => {
          let tree = if let Ok(tree) = source.ast(trees) {
            tree.clone()
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
              let _ = std::mem::replace(trees[index].ast_mut(), tree);
              true
            }
            n => {
              match trees[path[0]].ast_mut().get_subtree_mut(&path[1..(n - 1)])
              {
                Ok(Ast::Inner(_, children)) => {
                  let index = path[n - 1];
                  if index >= children.len() {
                    break 'breakable false;
                  }
                  children[index] = tree;
                  true
                }
                _ => false,
              }
            }
          }
        }
        AstDiffOp::InsertSnippet(source, sub_path) => {
          let mut tree = if let Ok(tree) = source.ast(trees) {
            tree.clone()
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
              std::mem::swap(&mut tree, trees[index].ast_mut());
              let Ok(hole) = trees[index].ast_mut().get_subtree_mut(&sub_path)
              else {
                break 'breakable false;
              };
              std::mem::swap(&mut tree, hole);
              true
            }
            n => {
              match trees[path[0]].ast_mut().get_subtree_mut(&path[1..(n - 1)])
              {
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
              }
            }
          }
        }
        AstDiffOp::DeleteSnippet(sub_path) => match path.len() {
          0 => false,
          1 => {
            let index = path[0];
            if index >= trees.len() {
              break 'breakable false;
            }
            let inner = &mut trees[index];
            let Ok(remaining_subtree) =
              inner.ast_mut().get_subtree_mut(&sub_path)
            else {
              break 'breakable false;
            };
            let remaining_subtree = remaining_subtree.clone();
            let _ =
              std::mem::replace(trees[index].ast_mut(), remaining_subtree);
            true
          }
          n => {
            match trees[path[0]].ast_mut().get_subtree_mut(&path[1..(n - 1)]) {
              Ok(Ast::Inner(_, children)) => {
                let index = path[n - 1];
                if index >= children.len() {
                  break 'breakable false;
                }
                let mut inner = children[index].clone();
                let Ok(remaining_subtree) = inner.get_subtree_mut(&sub_path)
                else {
                  break 'breakable false;
                };
                std::mem::swap(&mut children[index], remaining_subtree);
                true
              }
              _ => false,
            }
          }
        },
      }
    } {
      Ok(trees)
    } else {
      Err(self)
    }
  }
  pub fn inverse<W: AstWrapper<L, I>>(
    &self,
    trees: &Vec<W>,
  ) -> Result<Self, &Self> {
    let path = &self.path;
    Ok(AstDiff {
      path: path.clone(),
      op: match &self.op {
        AstDiffOp::Insert(_) => AstDiffOp::Delete,
        AstDiffOp::Delete => AstDiffOp::Insert(AstSource::New(
          trees[path[0]]
            .ast()
            .get_subtree(&path[1..])
            .map_err(|_| self)?
            .clone(),
        )),
        AstDiffOp::Replace(_) => AstDiffOp::Replace(AstSource::New(
          trees[path[0]]
            .ast()
            .get_subtree(&path[1..])
            .map_err(|_| self)?
            .clone(),
        )),
        AstDiffOp::InsertSnippet(_, sub_path) => {
          AstDiffOp::DeleteSnippet(sub_path.clone())
        }
        AstDiffOp::DeleteSnippet(sub_path) => AstDiffOp::InsertSnippet(
          AstSource::New(
            trees[path[0]]
              .ast()
              .get_subtree(&path[1..])
              .map_err(|_| self)?
              .clone(),
          ),
          sub_path.clone(),
        ),
      },
    })
  }
  pub fn try_map<
    NewL: Clone + PartialEq + Eq + Debug,
    NewI: Clone + PartialEq + Eq + Debug,
    E,
  >(
    self,
    ast_mapper: &impl Fn(Ast<L, I>, &Vec<usize>) -> Result<Ast<NewL, NewI>, E>,
  ) -> Result<AstDiff<NewL, NewI>, E> {
    Ok(AstDiff {
      path: self.path.clone(),
      op: match self.op {
        AstDiffOp::Insert(source) => {
          let new_source =
            source.try_map(&|ast| ast_mapper(ast, &self.path))?;
          AstDiffOp::Insert(new_source)
        }
        AstDiffOp::Delete => AstDiffOp::Delete,
        AstDiffOp::Replace(source) => {
          let new_source =
            source.try_map(&|ast| ast_mapper(ast, &self.path))?;
          AstDiffOp::Replace(new_source)
        }
        AstDiffOp::InsertSnippet(source, sub_path) => {
          let new_source =
            source.try_map(&|ast| ast_mapper(ast, &self.path))?;
          AstDiffOp::InsertSnippet(new_source, sub_path)
        }
        AstDiffOp::DeleteSnippet(sub_path) => {
          AstDiffOp::DeleteSnippet(sub_path)
        }
      },
    })
  }
  pub fn map<
    NewL: Clone + PartialEq + Eq + Debug,
    NewI: Clone + PartialEq + Eq + Debug,
  >(
    self,
    ast_mapper: &impl Fn(Ast<L, I>, &Vec<usize>) -> Ast<NewL, NewI>,
  ) -> AstDiff<NewL, NewI> {
    self
      .try_map(&|ast, path| Ok::<_, Never>(ast_mapper(ast, path)))
      .unwrap()
  }
  fn modify_paths<E>(
    &mut self,
    modifier: &impl Fn(Path) -> Result<Path, E>,
  ) -> Result<(), E> {
    self.path = modifier(self.path.clone())?;
    match &mut self.op {
      AstDiffOp::Insert(ast_source)
      | AstDiffOp::Replace(ast_source)
      | AstDiffOp::InsertSnippet(ast_source, _) => match ast_source {
        AstSource::Existing(path) => {
          *path = modifier(path.clone())?;
        }
        _ => {}
      },
      _ => {}
    }
    Ok(())
  }
  pub fn sequentialize(mut diffs: Vec<Self>) -> Result<Vec<Self>, ()> {
    // Takes a vector of diffs describing a set of "simultaneous" changes
    // modifies them such that they can be safely applied in a sequential order.
    // The diffs are expected to be "simulatneous" in the sense that the paths
    // within them refer to the original tree, rather than the tree that would
    // be produced by applying the earlier diffs within the list.
    //
    // Returns an error if there is no way to resolve the simultaneous
    // modifications (i.e. a merge conflict)
    for i in 0..diffs.len() {
      let path = diffs[i].path.clone();
      match &diffs[i].op {
        AstDiffOp::Insert(_) => {
          for diff in diffs.iter_mut().skip(i + 1) {
            diff
              .modify_paths::<Never>(&|mut other_path| {
                if is_prefix_of(&path[0..path.len() - 1], &other_path)
                  && *path.last().unwrap() <= other_path[path.len() - 1]
                {
                  other_path[path.len() - 1] += 1;
                }
                Ok(other_path)
              })
              .unwrap()
          }
        }
        AstDiffOp::Delete => {
          for diff in diffs.iter_mut().skip(i + 1) {
            diff.modify_paths(&|mut other_path| {
              if is_prefix_of(&path, &other_path) {
                return Err(());
              } else if is_prefix_of(&path[0..path.len() - 1], &other_path)
                && *path.last().unwrap() <= other_path[path.len() - 1]
              {
                other_path[path.len() - 1] -= 1;
              }
              Ok(other_path)
            })?;
          }
        }
        AstDiffOp::Replace(_) => {
          //todo!()
        }
        AstDiffOp::InsertSnippet(_, _) => {
          //todo!()
        }
        AstDiffOp::DeleteSnippet(_) => {
          //todo!()
        }
      }
    }
    Ok(diffs)
  }
}
