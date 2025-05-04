// SPDX-License-Identifier: LGPL-3.0-or-later

use super::CryptAlgorithm;

pub struct NoopCrypt;

impl CryptAlgorithm for NoopCrypt {
    const DEFAULT_SEED: u32 = 0;

    fn next(&mut self) -> u32 {
        // 0 is the identity value for XOR
        0
    }
}
