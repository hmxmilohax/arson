// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::prelude::*;

pub struct StdlibOptions {
    pub file_load_options: LoadOptions,
}

impl ContextState for StdlibOptions {}
