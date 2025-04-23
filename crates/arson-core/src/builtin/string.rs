// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;
use crate::{NumericError, StringError};

pub fn register_funcs(context: &mut Context) {
    basic::register_funcs(context);
    compare::register_funcs(context);
    search::register_funcs(context);
    manip::register_funcs(context);
    convert::register_funcs(context);
}

mod basic {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("str_elem", self::str_elem);
        context.register_func("strlen", self::strlen);
    }

    fn str_elem(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let string = args.string(context, 0)?;
        let index = args.size_integer(context, 1)?;

        match string.get(index..index + 1) {
            Some(char) => Ok(crate::intern_string(char).into()),
            None => {
                if index > string.len() {
                    Err(NumericError::IndexOutOfRange(index, 0..string.len()).into())
                } else {
                    Err(StringError::NotCharBoundary(index).into())
                }
            },
        }
    }

    fn strlen(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let string = args.string(context, 0)?;

        string.len().try_into()
    }
}

mod compare {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("streq", self::streq);
        context.register_func("strieq", self::strieq);
        context.register_func("strcmp", self::strcmp);
    }

    fn streq(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let left = args.string(context, 0)?;
        let right = args.string(context, 1)?;

        Ok((left == right).into())
    }

    fn strieq(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let left = args.string(context, 0)?;
        let right = args.string(context, 1)?;

        Ok(left.eq_ignore_ascii_case(&right).into())
    }

    fn strcmp(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let left = args.string(context, 0)?;
        let right = args.string(context, 1)?;

        Ok(left.cmp(&right).into())
    }
}

mod search {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("has_substr", self::has_substr);
        context.register_func("has_any_substr", self::has_any_substr);
        context.register_func("find_substr", self::find_substr);

        context.register_func("startswith", self::startswith);
        context.register_func("endswith", self::endswith);

        context.register_func("search_replace", self::search_replace);
    }

    fn has_substr(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let haystack = args.string(context, 0)?;
        let needle = args.string(context, 1)?;
        Ok(haystack.contains(needle.as_ref()).into())
    }

    fn has_any_substr(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let haystack = args.string(context, 0)?;
        let needles = args.array(context, 1)?;

        for node in needles.borrow()?.iter() {
            let needle = node.string(context)?;
            if haystack.contains(needle.as_ref()) {
                return Ok(Node::TRUE);
            }
        }

        Ok(Node::FALSE)
    }

    fn find_substr(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let haystack = args.string(context, 0)?;
        let needle = args.string(context, 1)?;

        match haystack.find(needle.as_ref()) {
            Some(index) => match Node::try_from(index) {
                Ok(index) => Ok(index),
                Err(_) => Ok((-1).into()),
            },
            None => Ok((-1).into()),
        }
    }

    fn startswith(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let haystack = args.string(context, 0)?;
        let needle = args.string(context, 1)?;
        Ok(haystack.starts_with(needle.as_ref()).into())
    }

    fn endswith(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let haystack = args.string(context, 0)?;
        let needle = args.string(context, 1)?;
        Ok(haystack.ends_with(needle.as_ref()).into())
    }

    fn search_replace(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 4);
        let source = args.string(context, 0)?;
        let search = args.string(context, 1)?;
        let replace = args.string(context, 2)?;
        let dest_var = args.variable(3)?;

        if source.contains(search.as_ref()) {
            let replaced = source.replace(search.as_ref(), replace.as_ref());
            dest_var.set(context, replaced);
            Ok(Node::TRUE)
        } else {
            dest_var.set(context, source);
            Ok(Node::FALSE)
        }
    }
}

mod manip {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("substr", self::substr);
        context.register_func("strcat", self::strcat);
        context.register_func("tolower", self::tolower);
        context.register_func("toupper", self::toupper);
    }

    fn substr(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);
        let string = args.string(context, 0)?;
        let start = args.size_integer(context, 1)?;
        let end = args.size_integer(context, 2)?;

        match string.get(start..end) {
            Some(slice) => Ok(slice.into()),
            None => {
                if start > string.len() || end > string.len() {
                    Err(NumericError::slice_out_of_range(start..end, 0..string.len()).into())
                } else if !string.is_char_boundary(start) {
                    Err(StringError::NotCharBoundary(start).into())
                } else {
                    Err(StringError::NotCharBoundary(end).into())
                }
            },
        }
    }

    fn strcat(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        let string_var = args.variable(0)?;
        let mut string = string_var.get(context).string(context)?.as_ref().clone();

        for node in args.get(1..)? {
            let piece = node.string(context)?;
            string.push_str(piece.as_ref());
        }

        string_var.set(context, string);

        Ok(Node::HANDLED)
    }

    fn tolower(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let string = args.string(context, 0)?;
        Ok(string.to_lowercase().into())
    }

    fn toupper(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let string = args.string(context, 0)?;
        Ok(string.to_uppercase().into())
    }
}

mod convert {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("char", self::char);
        context.register_func("symbol", self::symbol);
        context.register_func("string_flags", self::string_flags);
    }

    fn char(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let value = args.integer(context, 0)?;

        let value = match u32::try_from(value.0) {
            Ok(value) => value,
            Err(error) => return Err(NumericError::IntegerConversion(error).into()),
        };

        match char::from_u32(value) {
            Some(char) => Ok(char.to_string().into()),
            None => arson_fail!("value {value} is not a valid Unicode scalar value"),
        }
    }

    fn symbol(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let symbol = args.force_symbol(context, 0)?;
        Ok(symbol.into())
    }

    fn string_flags(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let mask = args.integer(context, 0)?;
        let flag_defines = args.array(context, 1)?;

        let mut flags_string = String::new();

        for node in flag_defines.borrow()?.iter() {
            let define_name = node.force_symbol(context)?;
            let define = match context.get_macro(&define_name) {
                Some(define) => define,
                None => arson_fail!("flag macro {define_name} does not exist"),
            };

            arson_assert_len!(define, 1, "flag macro {define_name} has too many elements");
            let NodeValue::Integer(flag) = define.unevaluated(0)? else {
                arson_fail!("value of flag macro {define_name} is not an integer");
            };

            if (mask & flag).0 != 0 {
                if !flags_string.is_empty() {
                    flags_string.push('|');
                }

                flags_string.push_str(define_name.name());
            }
        }

        Ok(flags_string.into())
    }
}
