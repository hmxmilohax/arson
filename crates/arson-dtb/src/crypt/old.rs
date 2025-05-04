// SPDX-License-Identifier: LGPL-3.0-or-later

use super::CryptAlgorithm;

pub struct OldRandom {
    index1: usize,
    index2: usize,
    table: [u32; 256],
}

impl OldRandom {
    pub const fn new(mut seed: u32) -> Self {
        const fn permute(value: u32) -> u32 {
            value.wrapping_mul(0x41C64E6D).wrapping_add(12345)
        }

        let mut table = [0u32; 256];
        let mut i = 0;
        while i < table.len() {
            let value = permute(seed);
            seed = permute(value);
            table[i] = (seed & 0x7FFF0000) | (value >> 16);
            i += 1;
        }

        Self { index1: 0, index2: 103, table }
    }

    pub const fn default() -> Self {
        Self::new(Self::DEFAULT_SEED)
    }
}

impl CryptAlgorithm for OldRandom {
    const DEFAULT_SEED: u32 = 0x52534F4C; // seed used by DtbCrypt

    fn next(&mut self) -> u32 {
        fn increment(mut index: usize) -> usize {
            index = index.wrapping_add(1);
            if index > 248 {
                index = 0x00;
            }
            index
        }

        let a = self.table[self.index1];
        let b = self.table[self.index2];
        let value = a ^ b;

        self.table[self.index1] = value;
        self.index1 = increment(self.index1);
        self.index2 = increment(self.index2);

        value
    }
}
