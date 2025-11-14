// SPDX-License-Identifier: LGPL-3.0-or-later

use std::cell::Cell;

use arson_core::prelude::*;
use squirrel3_rs::sq3;

pub struct RandomState {
    // note: sq3::Rng is not used due to several
    // inadequacies with its implementation
    position: Cell<u32>,
    seed: Cell<u32>,
}

impl RandomState {
    fn new(seed: Option<u32>) -> Self {
        Self {
            position: Cell::new(0),
            seed: Cell::new(seed.unwrap_or(0)),
        }
    }

    pub fn seed(&self, seed: u32) {
        self.position.set(0);
        self.seed.set(seed);
    }

    /// Samples the next random value.
    pub fn next(&self) -> u32 {
        let value = sq3::squirrel3(self.position.get(), self.seed.get());
        self.position.set(self.position.get().wrapping_add(1));
        value
    }

    /// Samples the next random value as an [`f64`], normalized to the range of `[0.0, 1.0)`.
    pub fn f64(&self) -> f64 {
        // 1.0 is deliberately excluded from the possible sample set,
        // to guarantee proper bounds in range_* methods
        const MAX: f64 = (u32::MAX as u64 + 1) as f64;
        self.next() as f64 / MAX
    }

    /// Samples the next random value as an [`i64`], normalized to the range of `[min, max)`.
    pub fn range_i64(&self, min: i64, max: i64) -> i64 {
        let range = (max - min) as f64;
        let offset = (self.f64() * range) as i64;
        min + offset
    }

    /// Samples the next random value as a [`usize`], normalized to the range of `[min, max)`.
    pub fn range_usize(&self, min: usize, max: usize) -> usize {
        let range = (max - min) as f64;
        let offset = (self.f64() * range) as usize;
        min + offset
    }

    /// Samples the next random value as an [`f64`], normalized to the range of `[min, max)`.
    pub fn range_f64(&self, min: f64, max: f64) -> f64 {
        let range = max - min;
        let offset = self.f64() * range;
        min + offset
    }
}

impl ContextState for RandomState {}

pub fn register_funcs(context: &mut Context, seed: Option<u32>) {
    context.register_state(RandomState::new(seed));

    context.register_func("random_seed", self::random_seed);
    context.register_func("random", self::random);
    context.register_func("random_int", self::random_int);
    context.register_func("random_float", self::random_float);
    context.register_func("random_elem", self::random_elem);
    context.register_func("random_sort", self::random_sort);
}

fn random_seed(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 1);
    let seed = args.integer(context, 0)?;

    let rng = context.get_state_mut::<RandomState>()?;
    rng.seed(seed.0 as u32);

    Ok(Node::HANDLED)
}

fn random(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert!(args.len() > 0, "need at least one element to pick from");

    let rng = context.get_state_mut::<RandomState>()?;
    let index = rng.range_usize(0, args.len());
    let result = args.evaluate(context, index)?;

    Ok(result.into())
}

fn random_int(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    arson_assert_len!(args, 2);
    let min = args.integer(context, 0)?;
    let max = args.integer(context, 1)?;

    arson_assert!(min < max, "min ({min}) must be less than max ({max})");

    let rng = context.get_state_mut::<RandomState>()?;
    let value = rng.range_i64(min.0, max.0);

    Ok(value.into())
}

fn random_float(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    if args.len() > 0 {
        // {random_float <min: float> <max: float>} -> [<min>, <max>)
        arson_assert_len!(args, 2);
        let min = args.float(context, 0)?;
        let max = args.float(context, 1)?;

        arson_assert!(min < max, "min ({min}) must be less than max ({max})");

        let rng = context.get_state_mut::<RandomState>()?;
        let value = rng.range_f64(min, max);

        Ok(value.into())
    } else {
        // {random_float} -> [0.0, 1.0)
        let rng = context.get_state_mut::<RandomState>()?;
        let value = rng.f64();
        Ok(value.into())
    }
}

fn random_elem(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    let array = args.array(context, 0)?;
    let borrow = array.borrow()?;

    arson_assert!(borrow.len() > 0, "need at least one element to pick from");

    let rng = context.get_state_mut::<RandomState>()?;
    let index = rng.range_usize(0, borrow.len());
    let result = borrow.evaluate(context, index)?;

    Ok(result.into())
}

fn random_sort(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    let array = args.array(context, 0)?;
    let mut borrow = array.borrow_mut()?;

    // Fisher-Yates shuffle algorithm
    let rng = context.get_state_mut::<RandomState>()?;
    for i in (0..borrow.len()).rev() {
        let j = rng.range_usize(0, i + 1);
        borrow.swap(i, j);
    }

    Ok(Node::HANDLED)
}
