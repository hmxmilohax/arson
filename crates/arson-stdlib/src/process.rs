// SPDX-License-Identifier: LGPL-3.0-or-later

use std::process::ExitCode;

use arson_core::prelude::*;

#[derive(Debug)]
pub struct ExitError(pub ExitCode);

impl ExitError {
    pub fn is_exit(error: &arson_core::Error) -> Option<ExitCode> {
        if let arson_core::ErrorKind::Custom(error) = error.kind() {
            if let Some(error) = error.downcast_ref::<ExitError>() {
                return Some(error.0);
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

pub fn register_funcs<S>(context: &mut Context<S>) {
    context.register_func("exit", self::exit);
    context.register_func("abort", self::abort);
}

pub fn exit<S>(_context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 0);
    Err(arson_core::Error::from_custom(ExitError(ExitCode::SUCCESS)))
}

pub fn abort<S>(_context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 0);
    Err(arson_core::Error::from_custom(ExitError(ExitCode::FAILURE)))
}
