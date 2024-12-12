// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_core::prelude::*;
use arson_fs::prelude::*;
use arson_parse::prelude::*;

pub trait StdlibState: FileSystemState {
    fn file_load_options(&self) -> LoadOptions;
}

impl<S: StdlibState> StdlibState for Context<S> {
    fn file_load_options(&self) -> LoadOptions {
        self.state.file_load_options()
    }
}

pub trait StdlibContextExt {
    fn load_path(&mut self, path: impl AsRef<VirtualPath>) -> Result<NodeArray, LoadError>;
    fn load_path_with_options(
        &mut self,
        options: LoadOptions,
        path: impl AsRef<VirtualPath>,
    ) -> Result<NodeArray, LoadError>;

    fn load_text(&mut self, text: &str) -> Result<NodeArray, LoadError>;
    fn load_text_with_options(&mut self, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError>;
}

impl<S: StdlibState> StdlibContextExt for Context<S> {
    fn load_path(&mut self, path: impl AsRef<VirtualPath>) -> Result<NodeArray, LoadError> {
        self.load_path_with_options(self.file_load_options(), path)
    }

    fn load_path_with_options(
        &mut self,
        options: LoadOptions,
        path: impl AsRef<VirtualPath>,
    ) -> Result<NodeArray, LoadError> {
        arson_parse::load_path(self, options, path)
    }

    fn load_text(&mut self, text: &str) -> Result<NodeArray, LoadError> {
        self.load_text_with_options(self.file_load_options(), text)
    }

    fn load_text_with_options(&mut self, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError> {
        arson_parse::load_text(self, options, text)
    }
}
