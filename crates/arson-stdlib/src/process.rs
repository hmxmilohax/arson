// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::prelude::*;
use arson_core::ConcatSlice;

#[derive(Debug)]
pub struct ExitError(pub Option<String>);

impl ExitError {
    pub fn is_exit(error: &arson_core::Error) -> Option<&Option<String>> {
        if let arson_core::ErrorKind::Custom(error) = error.kind() {
            if let Some(error) = error.downcast_ref::<ExitError>() {
                return Some(&error.0);
            }
        }

        None
    }
}

impl std::fmt::Display for ExitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "exit error")
    }
}

impl std::error::Error for ExitError {}

pub fn register_funcs(context: &mut Context) {
    context.register_func("exit", self::exit);
    context.register_func("abort", self::abort);
    context.register_func("panic", self::panic);
}

pub fn exit(_context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 0);
    Err(arson_core::Error::from_custom(ExitError(None)))
}

pub fn abort(_context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 0);
    Err(arson_core::Error::from_custom(ExitError(Some(
        "an abnormal exit has occurred".to_owned(),
    ))))
}

pub fn panic(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    let message = ConcatSlice::new(context, args).to_string();
    Err(arson_core::Error::from_custom(ExitError(Some(message))))
}
