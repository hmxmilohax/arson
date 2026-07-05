// SPDX-License-Identifier: LGPL-3.0-or-later

#[allow(
    clippy::module_inception,
    reason = "module is private, and it makes things more navigable"
)]
mod symbol;
mod table;

pub use symbol::*;
pub(crate) use table::*;

/// Maps symbols by hash to an associated value. Alias for [`HashMap<Symbol, T>`].
///
/// [`HashMap<Symbol, T>`]: std::collections::HashMap
pub type SymbolMap<T> = std::collections::HashMap<Symbol, T>;
