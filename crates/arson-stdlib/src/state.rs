// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::prelude::*;

pub trait StdlibState {
    fn file_load_options(&self) -> LoadOptions;
}

impl<S: StdlibState> StdlibState for Context<S> {
    fn file_load_options(&self) -> LoadOptions {
        self.state.file_load_options()
    }
}
