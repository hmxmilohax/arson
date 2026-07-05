// SPDX-License-Identifier: LGPL-3.0-or-later

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum StringError {
    #[error("index {0} is not a UTF-8 character boundary")]
    NotCharBoundary(usize),
}
