// SPDX-License-Identifier: LGPL-3.0-or-later

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum ArrayError {
    #[error("array already mutably borrowed")]
    BadBorrow(#[from] std::cell::BorrowError),

    #[error("array already immutably borrowed")]
    BadMutBorrow(#[from] std::cell::BorrowMutError),

    #[error("tag {0} was not found in the array")]
    TagNotFound(String),
}
