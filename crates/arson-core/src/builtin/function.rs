// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::prelude::*;

pub fn register_funcs(context: &mut Context) {
    context.register_func("func", self::func);
    context.register_func("closure", self::closure);
}

struct Function {
    name: Symbol,
    body: NodeArray,
}

impl Object for Function {
    fn name(&self) -> Option<&str> {
        Some(self.name.name())
    }

    fn handle(&self, context: &mut Context, msg: &NodeSlice) -> ExecuteResult {
        context.execute_args(&self.body, msg)
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Function").field(&self.name).finish()
    }
}

struct Closure {
    captures: Vec<VariableSave>,
    body: NodeArray,
}

impl Object for Closure {
    fn name(&self) -> Option<&str> {
        None
    }

    fn handle(&self, context: &mut Context, msg: &NodeSlice) -> ExecuteResult {
        let mut saved_variables = VariableStack::new(context);
        saved_variables.push_saved(&self.captures);

        let result = saved_variables.context().execute_args(&self.body, msg);
        drop(saved_variables); // ensure drop does not occur until after execution
        result
    }
}

impl std::fmt::Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Closure").finish_non_exhaustive()
    }
}

fn func(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    let name = args.symbol(context, 0)?;
    let body = args.slice(1..)?.to_owned();
    let function = Function { name: name.clone(), body };

    context.register_object(name, function).map(|o| o.into())
}

fn closure(context: &mut Context, args: &NodeSlice) -> ExecuteResult {
    let captures_raw = args.array(context, 0)?;
    let body = args.slice(1..)?.to_owned();

    let mut captures = Vec::new();
    for capture in captures_raw.borrow()?.iter() {
        let variable = capture.variable()?;
        captures.push(variable.save(context))
    }

    let closure = Closure { captures, body };
    Ok(closure.into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn func() -> crate::Result {
        let mut context = Context::new();

        let sym_func = context.add_required_symbol("func");
        let sym_plus = context.add_required_symbol("+");
        let sym_three = context.add_required_symbol("three");
        let sym_add = context.add_required_symbol("add");

        let var_num1 = Variable::new_required("num1", &mut context);
        let var_num2 = Variable::new_required("num2", &mut context);

        assert_eq!(context.get_func("three"), None);
        assert_eq!(context.get_func("add"), None);
        assert_eq!(context.get_object("three"), None);
        assert_eq!(context.get_object("add"), None);

        // no arguments
        /*
        {func three
            {+ 1 2}
        }
        */
        let script = NodeCommand::from(arson_array![
            sym_func.clone(),
            sym_three,
            NodeCommand::from(arson_array![sym_plus.clone(), 1, 2]),
        ]);
        let func = context.execute(&script)?;

        let func = func.unevaluated().object().expect("func returns an object");
        assert!(func.is::<Function>());
        assert_eq!(context.get_object("three"), Some(func.clone()));

        assert_eq!(func.handle(&mut context, NodeSlice::empty())?, Node::from(3));

        // with arguments
        /*
        {func add ($num1 $num2)
            {+ $num1 $num2}
        }
        */
        let script = NodeCommand::from(arson_array![
            sym_func,
            sym_add,
            arson_array![var_num1.clone(), var_num2.clone()],
            NodeCommand::from(arson_array![sym_plus, var_num1, var_num2]),
        ]);
        let func = context.execute(&script)?;

        let func = func.unevaluated().object().expect("func returns an object");
        assert!(func.is::<Function>());
        assert_eq!(context.get_object("add"), Some(func.clone()));

        assert_eq!(func.handle(&mut context, arson_slice![1, 2])?, Node::from(3));

        Ok(())
    }

    #[test]
    fn closure() -> crate::Result {
        let mut context = Context::new();

        let sym_do = context.add_required_symbol("do");
        let sym_closure = context.add_required_symbol("closure");
        let sym_plus = context.add_required_symbol("+");

        let var_num1 = Variable::new_required("num1", &mut context);
        let var_num2 = Variable::new_required("num2", &mut context);
        let var_base = Variable::new_required("base", &mut context);

        // no arguments
        /*
        {do
            ($num1 5)
            ($num2 10)
            {closure
                ($num1 $num2)
                {+ $num1 $num2}
            }
        }
        */
        #[rustfmt::skip]
        let script = NodeCommand::from(arson_array![
            sym_do.clone(),
            arson_array![var_num1.clone(), 5],
            arson_array![var_num2.clone(), 10],
            NodeCommand::from(arson_array![
                sym_closure.clone(),
                arson_array![var_num1.clone(), var_num2.clone()],
                NodeCommand::from(arson_array![sym_plus.clone(), var_num1.clone(), var_num2.clone()])
            ]),
        ]);
        let closure = context.execute(&script)?;

        let closure = closure.unevaluated().object().expect("closure returns an object");
        assert!(closure.is::<Closure>());

        assert_eq!(closure.handle(&mut context, NodeSlice::empty())?, Node::from(15));

        // with arguments
        /*
        {do
            ($base 25)
            {closure
                ($base)
                ($num1 $num2)
                {+ $num1 $num2 $base}
            }
        }
        */
        #[rustfmt::skip]
        let script = NodeCommand::from(arson_array![
            sym_do.clone(),
            arson_array![var_base.clone(), 25],
            NodeCommand::from(arson_array![
                sym_closure.clone(),
                arson_array![var_base.clone()],
                arson_array![var_num1.clone(), var_num2.clone()],
                NodeCommand::from(arson_array![sym_plus.clone(), var_num1.clone(), var_num2.clone(), var_base.clone()])
            ]),
        ]);
        let closure = context.execute(&script)?;

        let closure = closure.unevaluated().object().expect("closure returns an object");
        assert!(closure.is::<Closure>());

        assert_eq!(closure.handle(&mut context, arson_slice![5, 10])?, Node::from(40));

        Ok(())
    }
}
