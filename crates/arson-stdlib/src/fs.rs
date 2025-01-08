// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io;

use arson_core::prelude::*;
use arson_fs::prelude::*;
use arson_fs::Metadata;

use crate::StdlibOptions;

pub fn register_funcs(context: &mut Context) {
    context.register_func("basename", self::basename);
    context.register_func("dirname", self::dirname);

    context.register_func("read_file", self::read_file);
    context.register_func("write_file", self::write_file);
    context.register_func("run", self::run);

    context.register_func("file_exists", self::file_exists);
    context.register_func("file_read_only", self::file_read_only);
    context.register_func("file_list", self::file_list);
    context.register_func("file_list_paths", self::file_list_paths);
}

pub fn basename(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path_str = args.string(context, 0)?;
    let path = VirtualPath::new(path_str.as_ref());

    match path.file_stem() {
        Some(base_name) => Ok(base_name.into()),
        None => Ok(path_str.clone().into()),
    }
}

pub fn dirname(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path_str = args.string(context, 0)?;
    let path = VirtualPath::new(path_str.as_ref());

    match path.parent().and_then(|p| p.file_name()) {
        Some(dir_name) => Ok(dir_name.into()),
        None => Ok(path_str.clone().into()),
    }
}

pub fn read_file(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let options = context.get_state::<StdlibOptions>().map(|s| s.file_load_options.clone())?;

    let path = args.string(context, 0)?;
    let array = context.load_path(options, path.as_ref())?;
    Ok(array.into())
}

pub fn write_file(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 2);

    let path = args.string(context, 0)?;
    let array = args.array(context, 1)?;

    let mut file = context.file_system()?.create(path.as_ref())?;
    for node in array.borrow()?.iter() {
        writeln!(file, "{node:#}")?;
    }

    Ok(Node::HANDLED)
}

pub fn run(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let options = context.get_state::<StdlibOptions>().map(|s| s.file_load_options.clone())?;

    let path = args.string(context, 0)?;
    match context.load_path(options, path.as_ref()) {
        Ok(array) => context.execute_block(&array),
        Err(_err) => {
            // TODO: log error
            Ok(Node::HANDLED)
        },
    }
}

pub fn file_exists(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path = args.string(context, 0)?;
    let exists = context.file_system()?.is_file(path.as_ref());
    Ok(exists.into())
}

pub fn file_read_only(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);

    let path = args.string(context, 0)?;
    match context.file_system()?.metadata(path.as_ref())? {
        Metadata::File { is_readonly, .. } => Ok(is_readonly.into()),
        Metadata::Directory { .. } => Err(io::Error::from(io::ErrorKind::NotADirectory).into()),
    }
}

pub fn file_list(_context: &mut Context, _args: &NodeSlice) -> ExecuteResult {
    todo!("file_list")
}

pub fn file_list_paths(_context: &mut Context, _args: &NodeSlice) -> ExecuteResult {
    todo!("file_list_paths")
}
