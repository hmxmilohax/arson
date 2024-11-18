// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::arson_assert_len;
use crate::core::*;
use crate::LoadOptions;

pub fn register_funcs(context: &mut Context) {
    fs::register_funcs(context);
}

pub mod fs {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func_by_name("basename", self::basename);
        context.register_func_by_name("dirname", self::dirname);

        context.register_func_by_name("read_file", self::read_file);
        context.register_func_by_name("write_file", self::write_file);

        context.register_func_by_name("file_exists", self::file_exists);
        context.register_func_by_name("file_read_only", self::file_read_only);
        context.register_func_by_name("file_list", self::file_list);
        context.register_func_by_name("file_list_paths", self::file_list_paths);
    }

    pub fn basename(_context: &mut Context, _args: &NodeSlice) -> HandleResult {
        todo!("basename")
    }

    pub fn dirname(_context: &mut Context, _args: &NodeSlice) -> HandleResult {
        todo!("dirname")
    }

    pub fn read_file(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);

        let path = args.string(context, 0)?;
        let array = context.load_path(LoadOptions { allow_include: true }, path.as_ref())?;
        Ok(array.into())
    }

    pub fn write_file(_context: &mut Context, _args: &NodeSlice) -> HandleResult {
        todo!("write_file")
    }

    pub fn file_exists(context: &mut Context, args: &NodeSlice) -> HandleResult {
        arson_assert_len!(args, 1);

        let path = args.string(context, 0)?;
        let exists = context.file_system()?.is_file(path.as_ref());
        Ok(exists.into())
    }

    pub fn file_read_only(_context: &mut Context, _args: &NodeSlice) -> HandleResult {
        todo!("file_read_only")
    }

    pub fn file_list(_context: &mut Context, _args: &NodeSlice) -> HandleResult {
        todo!("file_list")
    }

    pub fn file_list_paths(_context: &mut Context, _args: &NodeSlice) -> HandleResult {
        todo!("file_list_paths")
    }
}
