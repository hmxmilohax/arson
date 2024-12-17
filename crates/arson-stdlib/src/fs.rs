// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io;

use arson_core::prelude::*;
use arson_fs::prelude::*;
use arson_fs::Metadata;

use crate::{StdlibContextExt, StdlibState};

pub fn register_funcs<S: StdlibState>(context: &mut Context<S>) {
    context.register_func("basename", self::basename);
    context.register_func("dirname", self::dirname);

    context.register_func("read_file", self::read_file);
    context.register_func("write_file", self::write_file);

    context.register_func("file_exists", self::file_exists);
    context.register_func("file_read_only", self::file_read_only);
    context.register_func("file_list", self::file_list);
    context.register_func("file_list_paths", self::file_list_paths);
}

pub fn basename<S: StdlibState>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path_str = args.string(context, 0)?;
    let path = VirtualPath::new(path_str.as_ref());

    match path.file_stem() {
        Some(base_name) => Ok(base_name.into()),
        None => Ok(path_str.clone().into()),
    }
}

pub fn dirname<S: StdlibState>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path_str = args.string(context, 0)?;
    let path = VirtualPath::new(path_str.as_ref());

    match path.parent().and_then(|p| p.file_name()) {
        Some(dir_name) => Ok(dir_name.into()),
        None => Ok(path_str.clone().into()),
    }
}

pub fn read_file<S: StdlibState>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path = args.string(context, 0)?;
    let array = context.load_path(path.as_ref())?;
    Ok(array.into())
}

pub fn write_file<S: StdlibState>(_context: &mut Context<S>, _args: &NodeSlice) -> ExecuteResult {
    todo!("write_file")
}

pub fn file_exists<S: StdlibState>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path = args.string(context, 0)?;
    let exists = context.file_system().is_file(path.as_ref());
    Ok(exists.into())
}

pub fn file_read_only<S: StdlibState>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path = args.string(context, 0)?;
    match context.file_system().metadata(path.as_ref())? {
        Metadata::File { is_readonly, .. } => Ok(is_readonly.into()),
        Metadata::Directory { .. } => Err(io::Error::from(io::ErrorKind::NotADirectory).into()),
    }
}

pub fn file_list<S: StdlibState>(_context: &mut Context<S>, _args: &NodeSlice) -> ExecuteResult {
    todo!("file_list")
}

pub fn file_list_paths<S: StdlibState>(_context: &mut Context<S>, _args: &NodeSlice) -> ExecuteResult {
    todo!("file_list_paths")
}
