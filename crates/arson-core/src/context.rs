// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::arson_array;
use crate::primitives::*;

/// A function which is callable from script.
pub type HandleFn<State> = fn(context: &mut Context<State>, args: &NodeSlice) -> ExecuteResult;

/// The result of a script execution.
pub type ExecuteResult = crate::Result<Node>;

#[derive(thiserror::Error, Debug)]
pub enum ExecutionError {
    #[error("No function registered for name '{0}'")]
    FunctionNotFound(Symbol),

    #[error("{0}")]
    Failure(String),
}

pub struct Context<State> {
    pub state: State,

    symbol_table: SymbolTable,

    macros: SymbolMap<NodeArray>,
    variables: SymbolMap<Node>,
    functions: SymbolMap<HandleFn<State>>,
}

impl<State> Context<State> {
    pub fn new(state: State) -> Self {
        let mut context = Self {
            state,

            symbol_table: SymbolTable::new(),

            macros: SymbolMap::new(),
            variables: SymbolMap::new(),
            functions: SymbolMap::new(),
        };

        crate::builtin::register_funcs(&mut context);

        context
    }

    pub fn add_symbol(&mut self, name: &str) -> Symbol {
        self.symbol_table.add(name)
    }

    // pub fn remove_symbol(&mut self, name: &Symbol) {
    //     self.symbol_table.remove(name);
    // }

    pub fn get_symbol(&mut self, name: &str) -> Option<Symbol> {
        self.symbol_table.get(name)
    }

    pub fn add_macro<N>(&mut self, name: N, array: NodeArray)
    where
        Self: MakeSymbol<N>,
    {
        let name = self.make_symbol(name);
        self.macros.insert(name, array);
    }

    pub fn add_macro_define<N>(&mut self, name: N)
    where
        Self: MakeSymbol<N>,
    {
        self.add_macro(name, arson_array![1]);
    }

    pub fn remove_macro<N>(&mut self, name: N)
    where
        Self: MakeSymbol<N>,
    {
        let name = self.make_symbol(name);
        self.macros.remove(&name);
    }

    pub fn get_macro<N>(&mut self, name: N) -> Option<&NodeArray>
    where
        Self: MakeSymbol<N>,
    {
        let name = self.make_symbol(name);
        self.macros.get(&name)
    }

    pub fn get_variable<N>(&mut self, name: N) -> Node
    where
        Self: MakeSymbol<N>,
    {
        let name = self.make_symbol(name);
        match self.variables.get(&name) {
            Some(value) => value.clone(),
            None => {
                let value = Node::from(0);
                self.variables.insert(name, value.clone());
                value
            },
        }
    }

    pub fn set_variable<N>(&mut self, name: N, value: impl Into<Node>)
    where
        Self: MakeSymbol<N>,
    {
        let name = self.make_symbol(name);
        self.variables.insert(name, value.into());
    }

    pub fn register_func<N>(&mut self, name: N, func: HandleFn<State>) -> bool
    where
        Self: MakeSymbol<N>,
    {
        let name = self.make_symbol(name);
        self.functions.insert(name, func).is_none()
    }

    pub fn register_func_alias<N1, N2>(&mut self, alias: N1, actual: N2) -> bool
    where
        Self: MakeSymbol<N1>,
        Self: MakeSymbol<N2>,
    {
        let actual = self.make_symbol(actual);
        match self.functions.get(&actual) {
            Some(func) => self.register_func(alias, *func),
            None => false,
        }
    }

    pub fn execute(&mut self, command: &NodeCommand) -> ExecuteResult {
        let result = match command.evaluate(self, 0)? {
            NodeValue::Symbol(symbol) => match self.functions.get(&symbol) {
                // TODO: cache function/object lookups
                Some(func) => func(self, command.slice(1..)?)?,
                None => return Err(ExecutionError::FunctionNotFound(symbol).into()),
            },
            _ => Node::UNHANDLED,
        };

        if let NodeValue::Unhandled = result.unevaluated() {
            todo!("default handler")
        }

        Ok(result)
    }

    pub fn execute_block(&mut self, script: &NodeSlice) -> ExecuteResult {
        for node in script.slice(..script.len() - 1)? {
            node.command()?.execute(self)?;
        }

        script.evaluate(self, script.len() - 1).map(|v| v.into())
    }

    pub fn execute_args(&mut self, mut script: &NodeSlice, args: &NodeSlice) -> ExecuteResult {
        let mut saved_variables = VariableStack::new(self);
        saved_variables.push_args(&mut script, args)?;

        let result = saved_variables.context().execute_block(script);
        drop(saved_variables); // ensure drop does not occur until after execution
        result
    }
}

impl<State: Default> Default for Context<State> {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

/// Trait to make APIs which require [`Symbol`]s more convenient to use.
///
/// ```rust
/// use arson_core::{arson_array, Context, MakeSymbol};
///
/// let mut context = Context::new(());
/// let symbol = context.add_symbol("kDefine");
///
/// // These are both equivalent, the latter will produce a Symbol behind the scenes.
/// context.add_macro(&symbol, arson_array![1]);
/// context.add_macro("kDefine", arson_array![1]);
///
/// // This function can take &str, Symbol, or &Symbol as a name input.
/// fn do_something_with_symbol<N, S>(context: &mut Context<S>, name: N)
///     where Context<S>: MakeSymbol<N>
/// {
///     // ...
/// }
///
/// do_something_with_symbol(&mut context, &symbol);
/// do_something_with_symbol(&mut context, "kDefine");
/// ```
pub trait MakeSymbol<N> {
    fn make_symbol(&mut self, name: N) -> Symbol;
}

impl<S, N: AsRef<str>> MakeSymbol<N> for Context<S> {
    fn make_symbol(&mut self, name: N) -> Symbol {
        self.symbol_table.add(name.as_ref())
    }
}

impl<S> MakeSymbol<Symbol> for Context<S> {
    fn make_symbol(&mut self, name: Symbol) -> Symbol {
        name
    }
}

impl<S> MakeSymbol<&Symbol> for Context<S> {
    fn make_symbol(&mut self, name: &Symbol) -> Symbol {
        name.clone()
    }
}
