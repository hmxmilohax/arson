// SPDX-License-Identifier: LGPL-3.0-or-later

use super::CryptAlgorithm;

pub struct NewRandom {
    seed: i32,
}

impl NewRandom {
    pub const fn new(seed: u32) -> Self {
        let seed = seed as i32;
        let seed = match seed {
            0 => 1,
            seed if seed < 0 => -seed,
            seed => seed,
        };

        Self { seed }
    }

    pub const fn default() -> Self {
        Self::new(Self::DEFAULT_SEED)
    }
}

impl CryptAlgorithm for NewRandom {
    const DEFAULT_SEED: u32 = 0x30171609; // seed used by dtab

    fn next(&mut self) -> u32 {
        let a = self.seed.wrapping_rem(0x1F31D).wrapping_mul(0x41A7);
        let b = self.seed.wrapping_div(0x1F31D).wrapping_mul(0xB14);

        let c = match a.wrapping_sub(b) {
            c if c <= 0 => c.wrapping_add(0x7FFFFFFF),
            c => c,
        };

        self.seed = c;
        c as u32
    }
}
