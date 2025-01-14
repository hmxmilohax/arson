// SPDX-License-Identifier: LGPL-3.0-or-later

mod crypt;
mod read;
mod write;

pub use read::*;
pub use write::*;

pub mod prelude {
    pub use super::{DataArray, DataKind, DataNode, WriteEncoding, WriteSettings};
}

#[repr(u32)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum DataKind {
    Integer = 0,
    Float = 1,
    Variable = 2,
    Function = 3,
    Object = 4,
    Symbol = 5,
    Unhandled = 6,

    Ifdef = 7,
    Else = 8,
    Endif = 9,

    Array = 16,
    Command = 17,
    String = 18,
    Property = 19,
    Glob = 20,

    Define = 32,
    Include = 33,
    Merge = 34,
    Ifndef = 35,
    Autorun = 36,
    Undefine = 37,
}

impl TryFrom<u32> for DataKind {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        let kind = match value {
            0 => Self::Integer,
            1 => Self::Float,
            2 => Self::Variable,
            3 => Self::Function,
            4 => Self::Object,
            5 => Self::Symbol,
            6 => Self::Unhandled,

            7 => Self::Ifdef,
            8 => Self::Else,
            9 => Self::Endif,

            16 => Self::Array,
            17 => Self::Command,
            18 => Self::String,
            19 => Self::Property,
            20 => Self::Glob,

            32 => Self::Define,
            33 => Self::Include,
            34 => Self::Merge,
            35 => Self::Ifndef,
            36 => Self::Autorun,
            37 => Self::Undefine,

            _ => return Err(()),
        };
        Ok(kind)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataNode {
    Integer(i32),
    Float(f32),
    Variable(String),
    Function(String),
    Object(String),
    Symbol(String),
    Unhandled,

    Ifdef(String),
    Else,
    Endif,

    Array(DataArray),
    Command(DataArray),
    String(String),
    Property(DataArray),
    Glob(Vec<u8>),

    Define(String, DataArray),
    Include(String),
    Merge(String),
    Ifndef(String),
    Autorun(DataArray),
    Undefine(String),
}

impl DataNode {
    pub fn get_kind(&self) -> DataKind {
        match self {
            Self::Integer(_) => DataKind::Integer,
            Self::Float(_) => DataKind::Float,
            Self::Variable(_) => DataKind::Variable,
            Self::Function(_) => DataKind::Function,
            Self::Object(_) => DataKind::Object,
            Self::Symbol(_) => DataKind::Symbol,
            Self::Unhandled => DataKind::Unhandled,

            Self::Ifdef(_) => DataKind::Ifdef,
            Self::Else => DataKind::Else,
            Self::Endif => DataKind::Endif,

            Self::Array(_) => DataKind::Array,
            Self::Command(_) => DataKind::Command,
            Self::String(_) => DataKind::String,
            Self::Property(_) => DataKind::Property,
            Self::Glob(_) => DataKind::Glob,

            Self::Define(_, _) => DataKind::Define,
            Self::Include(_) => DataKind::Include,
            Self::Merge(_) => DataKind::Merge,
            Self::Ifndef(_) => DataKind::Ifndef,
            Self::Autorun(_) => DataKind::Autorun,
            Self::Undefine(_) => DataKind::Undefine,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataArray {
    line: usize,
    nodes: Vec<DataNode>,
}

impl DataArray {
    pub fn new(line: usize) -> Self {
        Self { line, nodes: Vec::new() }
    }

    pub fn with_capacity(line: usize, capacity: usize) -> Self {
        Self { line, nodes: Vec::with_capacity(capacity) }
    }

    pub fn from_nodes(line: usize, nodes: Vec<DataNode>) -> Self {
        Self { line, nodes }
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

impl Default for DataArray {
    fn default() -> Self {
        Self::new(1)
    }
}

impl std::ops::Deref for DataArray {
    type Target = Vec<DataNode>;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl std::ops::DerefMut for DataArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}

impl IntoIterator for DataArray {
    type Item = <Vec<DataNode> as IntoIterator>::Item;
    type IntoIter = <Vec<DataNode> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.nodes.into_iter()
    }
}

impl<'a> IntoIterator for &'a DataArray {
    type Item = <&'a Vec<DataNode> as IntoIterator>::Item;
    type IntoIter = <&'a Vec<DataNode> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.nodes).into_iter()
    }
}

impl<'a> IntoIterator for &'a mut DataArray {
    type Item = <&'a mut Vec<DataNode> as IntoIterator>::Item;
    type IntoIter = <&'a mut Vec<DataNode> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.nodes).into_iter()
    }
}
