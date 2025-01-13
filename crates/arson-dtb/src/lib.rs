// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ffi::OsString;

#[repr(u32)]
pub enum DataKind {
    Integer = 0,
    Float = 1,
    Var = 2,
    Func = 3,
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
    Undef = 37,
}

pub enum Data {
    Integer(i32),
    Float(f32),
    Var(OsString),
    Func(OsString),
    Object(OsString),
    Symbol(OsString),
    Unhandled,

    Ifdef(OsString),
    Else,
    Endif,

    Array(Vec<Data>),
    Command(Vec<Data>),
    String(OsString),
    Property(Vec<Data>),
    Glob(Vec<u8>),

    Define(OsString),
    Include(OsString),
    Merge(OsString),
    Ifndef(OsString),
    Autorun(Vec<Data>),
    Undef(OsString),
}

impl Data {
    pub fn get_kind(&self) -> DataKind {
        match self {
            Self::Integer(_) => DataKind::Integer,
            Self::Float(_) => DataKind::Float,
            Self::Var(_) => DataKind::Var,
            Self::Func(_) => DataKind::Func,
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

            Self::Define(_) => DataKind::Define,
            Self::Include(_) => DataKind::Include,
            Self::Merge(_) => DataKind::Merge,
            Self::Ifndef(_) => DataKind::Ifndef,
            Self::Autorun(_) => DataKind::Autorun,
            Self::Undef(_) => DataKind::Undef,
        }
    }
}
