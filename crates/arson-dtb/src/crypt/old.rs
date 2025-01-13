// SPDX-License-Identifier: LGPL-3.0-or-later

use super::CryptAlgorithm;

pub struct OldRandom {
    index1: usize,
    index2: usize,
    table: [u32; 256],
}

impl OldRandom {
    pub fn new(mut seed: u32) -> Self {
        fn permute(value: u32) -> u32 {
            value.wrapping_mul(0x41C64E6D).wrapping_add(12345)
        }

        let table = std::array::from_fn::<u32, 256, _>(|_| {
            let i = permute(seed);
            seed = permute(i);
            (seed & 0x7FFF0000) | (i >> 16)
        });

        Self { index1: 0, index2: 103, table }
    }
}

impl CryptAlgorithm for OldRandom {
    fn next(&mut self) -> u8 {
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

        value as u8
    }
}

impl Iterator for OldRandom {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        Some(CryptAlgorithm::next(self))
    }
}
