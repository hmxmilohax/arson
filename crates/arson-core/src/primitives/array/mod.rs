// SPDX-License-Identifier: LGPL-3.0-or-later

#[allow(
    clippy::module_inception,
    reason = "module is private, and it makes sense file-wise"
)]
mod array;
mod display;
mod error;
mod find;
mod index;
mod macros;
mod r#ref;
mod slice;
mod util;

pub use array::*;
pub use display::*;
pub use error::*;
pub use find::*;
pub use index::*;
pub use r#ref::*;
pub use slice::*;
