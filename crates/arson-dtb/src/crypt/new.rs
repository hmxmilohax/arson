// SPDX-License-Identifier: LGPL-3.0-or-later

use super::CryptAlgorithm;

pub struct NewRandom {
    seed: i32,
}

impl NewRandom {
    pub fn new(seed: i32) -> Self {
        let seed = match seed {
            0 => 1,
            seed if seed < 0 => -seed,
            seed => seed,
        };

        Self { seed }
    }
}

impl CryptAlgorithm for NewRandom {
    fn next(&mut self) -> u8 {
        let a = self.seed.wrapping_rem(0x1F31D).wrapping_mul(0x41A7);
        let b = self.seed.wrapping_div(0x1F31D).wrapping_mul(0xB14);

        let c = match a - b {
            c if c <= 0 => c + 0x7FFFFFFF,
            c => c,
        };

        self.seed = c;
        c as u8
    }
}

impl Iterator for NewRandom {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        Some(CryptAlgorithm::next(self))
    }
}
