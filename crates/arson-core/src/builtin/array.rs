// SPDX-License-Identifier: LGPL-3.0-or-later

use std::panic::AssertUnwindSafe;

use crate::prelude::*;
use crate::ExecutionError;

pub fn register_funcs<S>(context: &mut Context<S>) {
    size::register_funcs(context);
    elem::register_funcs(context);
    manip::register_funcs(context);
    search::register_funcs(context);
    algo::register_funcs(context);
}

mod size {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("size", self::size);
        context.register_func("resize", self::resize);
        context.register_func("reserve", self::reserve);
    }

    fn size<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let array = args.array(context, 0)?;

        array.borrow().and_then(|a| a.len().try_into())
    }

    fn resize<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let array = args.array(context, 0)?;
        let size = args.size_integer(context, 1)?;

        array.borrow_mut()?.resize_with(size, Default::default);
        Ok(Node::HANDLED)
    }

    fn reserve<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let array = args.array(context, 0)?;
        let additional = args.size_integer(context, 1)?;

        array.borrow_mut()?.reserve(additional);
        Ok(Node::HANDLED)
    }
}

mod elem {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("elem", self::elem);
        context.register_func("first_elem", self::first_elem);
        context.register_func("last_elem", self::last_elem);
        context.register_func("set_elem", self::set_elem);
    }

    fn elem<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let array = args.array(context, 0)?;
        let index = args.size_integer(context, 1)?;

        let borrow = array.borrow()?;
        borrow.get(index).cloned()
    }

    fn first_elem<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let array = args.array(context, 0)?;
        let borrow = array.borrow()?;

        match borrow.first() {
            Some(last) => Ok(last.into()),
            None => arson_fail!("cannot get the first element of an empty array"),
        }
    }

    fn last_elem<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let array = args.array(context, 0)?;
        let borrow = array.borrow()?;

        match borrow.last() {
            Some(last) => Ok(last.into()),
            None => arson_fail!("cannot get the last element of an empty array"),
        }
    }

    fn set_elem<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);
        let array = args.array(context, 0)?;
        let index = args.size_integer(context, 1)?;
        let value: Node = args.evaluate(context, 2)?.into();

        let mut borrow = array.borrow_mut()?;
        *borrow.get_mut(index)? = value.clone();
        Ok(value)
    }
}

mod manip {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("push_back", self::push_back);
        context.register_func("pop_back", self::pop_back);

        context.register_func("insert_elem", self::insert_elem);
        context.register_func("insert_elems", self::insert_elems);
        context.register_func("remove_elem", self::remove_elem);
        context.register_func("remove_elems", self::remove_elems);
    }

    fn push_back<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let array = args.array(context, 0)?;
        let value = args.evaluate(context, 1)?;

        let mut borrow = array.borrow_mut()?;
        borrow.push(value);
        Ok(Node::HANDLED)
    }

    fn pop_back<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let array = args.array(context, 0)?;

        let mut borrow = array.borrow_mut()?;
        match borrow.pop() {
            Some(value) => Ok(value),
            None => Ok(Node::UNHANDLED),
        }
    }

    fn insert_elem<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);
        let array = args.array(context, 0)?;
        let index = args.size_integer(context, 1)?;
        let value = args.evaluate(context, 2)?;

        let mut borrow = array.borrow_mut()?;
        borrow.insert(index, value.clone())?;
        Ok(value.into())
    }

    fn insert_elems<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);
        let array = args.array(context, 0)?;
        let index = args.size_integer(context, 1)?;
        let values = args.array(context, 2)?;

        let mut borrow = array.borrow_mut()?;
        borrow.insert_slice(index, &values.borrow()?)?;
        Ok(Node::HANDLED)
    }

    fn remove_elem<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let array = args.array(context, 0)?;
        let value: Node = args.evaluate(context, 1)?.into();

        let mut borrow = array.borrow_mut()?;
        borrow.remove_item(&value);
        Ok(Node::HANDLED)
    }

    fn remove_elems<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 3);
        let array = args.array(context, 0)?;
        let index = args.size_integer(context, 1)?;
        let count = args.size_integer(context, 2)?;

        let mut borrow = array.borrow_mut()?;
        borrow.drain(index..index + count)?;
        Ok(Node::HANDLED)
    }
}

mod search {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("find", self::find);
        context.register_func("find_exists", self::find_exists);
        context.register_func("find_elem", self::find_elem);
        context.register_func("contains", self::contains);
    }

    fn contains<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let array = args.array(context, 0)?;
        let value: Node = args.evaluate(context, 1)?.into();

        let borrow = array.borrow()?;
        Ok(borrow.contains(&value).into())
    }

    fn find<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let mut array = args.array(context, 0)?;

        for (i, arg) in args.slice(1..)?.iter().enumerate() {
            let tag = arg.array_tag(context)?;
            let borrow = array.borrow()?;
            match borrow.find_tag_opt(tag.clone()) {
                Some(found) => {
                    drop(borrow);
                    array = found;
                },
                None => arson_fail!("Couldn't find key {} (depth {})", tag, i),
            }
        }

        Ok(array.into())
    }

    fn find_exists<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let mut array = args.array(context, 0)?;

        for arg in args.slice(1..)? {
            let tag = arg.array_tag(context)?;
            let borrow = array.borrow()?;
            match borrow.find_tag_opt(tag) {
                Some(found) => {
                    drop(borrow);
                    array = found;
                },
                None => return Ok(Node::UNHANDLED),
            }
        }

        Ok(array.into())
    }

    fn find_elem<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 2);
        let array = args.array(context, 0)?;
        let target: Node = args.evaluate(context, 1)?.into();

        let borrow = array.borrow()?;
        for (i, value) in borrow.iter().enumerate() {
            if *value != target {
                continue;
            }

            if let Some(var) = args.get_opt(2) {
                let i: Node = i.try_into()?;
                var.variable()?.set(context, i);
            }

            return Ok(Node::TRUE);
        }

        Ok(Node::FALSE)
    }
}

mod algo {
    use super::*;

    pub fn register_funcs<S>(context: &mut Context<S>) {
        context.register_func("sort", self::sort);
        context.register_func("sort_by", self::sort_by);
    }

    fn sort<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        arson_assert_len!(args, 1);
        let array = args.array(context, 0)?;

        let array = AssertUnwindSafe(array);
        let result = std::panic::catch_unwind(|| -> ExecuteResult {
            let mut borrow = array.borrow_mut()?;
            borrow.sort();
            Ok(Node::HANDLED)
        });

        convert_panic(result)
    }

    fn sort_by<S>(context: &mut Context<S>, args: &NodeSlice) -> ExecuteResult {
        let mut context = AssertUnwindSafe(context);
        let args = AssertUnwindSafe(args);
        let result = std::panic::catch_unwind(move || -> ExecuteResult {
            arson_assert_len!(args, 4);
            let array = args.array(context.0, 0)?;
            let left_var = args.variable(1)?;
            let right_var = args.variable(2)?;
            let predicate = args.command(3)?;

            let mut borrow = array.borrow_mut()?;
            borrow.sort_by(|left, right| {
                left_var.set(context.0, left);
                right_var.set(context.0, right);
                let ordering = match context.execute(predicate) {
                    Ok(value) => value,
                    Err(error) => std::panic::panic_any(error),
                };
                let ordering = ordering.unevaluated().integer().expect("");
                match ordering.0 {
                    ..0 => std::cmp::Ordering::Less,
                    0 => std::cmp::Ordering::Equal,
                    1.. => std::cmp::Ordering::Greater,
                }
            });
            Ok(Node::HANDLED)
        });

        convert_panic(result)
    }
}

// TODO: Find a more reusable place to put this
fn convert_panic(result: Result<ExecuteResult, Box<dyn std::any::Any + Send + 'static>>) -> ExecuteResult {
    let error = match result {
        Ok(result) => return result,
        Err(error) => error,
    };

    let error = match error.downcast::<crate::Error>() {
        Ok(inner) => return Err(*inner),
        Err(error) => error,
    };

    let msg = match error.downcast::<String>() {
        Ok(s) => *s,
        Err(error) => match error.downcast_ref::<&'static str>() {
            Some(s) => s.to_string(),
            None => "function panicked for an unknown reason".to_string(),
        },
    };

    Err(ExecutionError::Failure(msg).into())
}
