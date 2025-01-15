// SPDX-License-Identifier: LGPL-3.0-or-later

use super::CryptAlgorithm;

pub struct NoopCrypt;

impl CryptAlgorithm for NoopCrypt {
    fn next(&mut self) -> u8 {
        // 0 is the identity value for XOR
        0
    }
}

impl Iterator for NoopCrypt {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        Some(CryptAlgorithm::next(self))
    }
}
