// SPDX-License-Identifier: LGPL-3.0-or-later

use super::CryptAlgorithm;

pub struct NewRandom {
    seed: i32,
}

impl NewRandom {
    pub const DEFAULT_SEED: i32 = 0x30171609; // seed used by dtab

    pub const fn new(seed: i32) -> Self {
        let seed = match seed {
            0 => 1,
            seed if seed < 0 => -seed,
            seed => seed,
        };

        Self { seed }
    }

    pub fn next(&mut self) -> i32 {
        let a = self.seed.wrapping_rem(0x1F31D).wrapping_mul(0x41A7);
        let b = self.seed.wrapping_div(0x1F31D).wrapping_mul(0xB14);

        let c = match a.wrapping_sub(b) {
            c if c <= 0 => c.wrapping_add(0x7FFFFFFF),
            c => c,
        };

        self.seed = c;
        c
    }
}

impl CryptAlgorithm for NewRandom {
    fn next(&mut self) -> u8 {
        NewRandom::next(self) as u8
    }
}

impl Iterator for NewRandom {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        Some(NewRandom::next(self))
    }
}
