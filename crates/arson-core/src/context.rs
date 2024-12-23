// SPDX-License-Identifier: LGPL-3.0-or-later

#[cfg(feature = "file-system")]
use arson_fs::{FileSystem, FileSystemDriver, VirtualPath};

use crate::builtin::BuiltinState;
use crate::prelude::*;
use crate::{FindDataPredicate, SymbolTable};

/// A function which is callable from script.
pub type HandleFn<State> = fn(context: &mut Context<State>, args: &NodeSlice) -> ExecuteResult;

/// The result of a script execution.
pub type ExecuteResult = crate::Result<Node>;

#[derive(thiserror::Error, Debug)]
pub enum ExecutionError {
    #[error("No function registered for name '{0}'")]
    FunctionNotFound(String),

    #[error("{0}")]
    Failure(String),
}

#[allow(
    clippy::partial_pub_fields,
    reason = "The `state` field is solely external and has no impact on internal invariants"
)]
pub struct Context<State> {
    symbol_table: SymbolTable,

    macros: SymbolMap<NodeArray>,
    variables: SymbolMap<Node>,
    functions: SymbolMap<HandleFn<State>>,

    #[cfg(feature = "file-system")]
    file_system: FileSystem,

    pub(crate) builtin_state: BuiltinState,
    pub state: State,
}

impl<State> Context<State> {
    pub fn new(
        state: State,
        #[cfg(feature = "file-system")] driver: impl FileSystemDriver + 'static,
    ) -> Self {
        let mut symbol_table = SymbolTable::new();
        let builtin_state = BuiltinState::new(&mut symbol_table);

        let mut context = Self {
            symbol_table,

            macros: SymbolMap::new(),
            variables: SymbolMap::new(),
            functions: SymbolMap::new(),

            #[cfg(feature = "file-system")]
            file_system: FileSystem::new(driver),

            builtin_state,
            state,
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

    pub fn add_macro(&mut self, name: impl IntoSymbol, array: NodeArray) {
        let name = name.into_symbol(self);
        self.macros.insert(name, array);
    }

    pub fn add_macro_define(&mut self, name: impl IntoSymbol) {
        self.add_macro(name, arson_array![1]);
    }

    pub fn remove_macro(&mut self, name: impl IntoSymbol) {
        let name = name.into_symbol(self);
        self.macros.remove(&name);
    }

    pub fn get_macro(&mut self, name: impl IntoSymbol) -> Option<&NodeArray> {
        let name = name.into_symbol(self);
        self.macros.get(&name)
    }

    pub fn find_macro(&self, prefix: &str, predicate: impl FindDataPredicate) -> Option<&NodeArray> {
        for (name, r#macro) in &self.macros {
            let Some(tag) = r#macro.unevaluated_opt(0) else {
                continue;
            };
            if predicate.matches(tag) && name.name().starts_with(prefix) {
                return Some(r#macro);
            }
        }

        None
    }

    pub fn get_variable(&mut self, name: impl IntoSymbol) -> Node {
        let name = name.into_symbol(self);
        match self.variables.get(&name) {
            Some(value) => value.clone(),
            None => {
                let value = Node::from(0);
                self.variables.insert(name, value.clone());
                value
            },
        }
    }

    pub fn set_variable(&mut self, name: impl IntoSymbol, value: impl Into<Node>) {
        let name = name.into_symbol(self);
        self.variables.insert(name, value.into());
    }

    pub fn register_func(&mut self, name: impl IntoSymbol, func: HandleFn<State>) -> bool {
        let name = name.into_symbol(self);
        self.functions.insert(name, func).is_none()
    }

    pub fn execute(&mut self, command: &NodeCommand) -> ExecuteResult {
        let result = match command.evaluate(self, 0)? {
            NodeValue::Symbol(symbol) => match self.functions.get(&symbol) {
                // TODO: cache function/object lookups
                Some(func) => func(self, command.slice(1..)?)?,
                None => return Err(ExecutionError::FunctionNotFound(symbol.name().to_string()).into()),
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

    pub fn load_text(&mut self, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError> {
        crate::loader::load_text(self, options, text)
    }
}

#[cfg(test)]
pub(crate) fn make_test_context<S>(state: S) -> Context<S> {
    Context::new(
        state,
        #[cfg(feature = "file-loading")]
        arson_fs::drivers::MockFileSystemDriver::new(),
    )
}

#[cfg(not(feature = "file-system"))]
impl<State: Default> Default for Context<State> {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

/// Trait to make APIs which require [`Symbol`]s more convenient to use.
///
/// ```rust
/// use arson_core::{arson_array, Context, IntoSymbol};
#[cfg_attr(feature = "file-loading", doc = "use arson_fs::drivers::MockFileSystemDriver;")]
///
#[cfg_attr(feature = "file-loading", doc = "let driver = MockFileSystemDriver::new();")]
#[cfg_attr(feature = "file-loading", doc = "let mut context = Context::new((), driver);")]
#[cfg_attr(not(feature = "file-loading"), doc = "let mut context = Context::new(());")]
///
/// // add_macro makes use of this trait.
/// // You can use either a Symbol, which gets used as-is...
/// let symbol = context.add_symbol("kDefine");
/// context.add_macro(&symbol, arson_array![1]);
/// assert_eq!(context.get_macro(&symbol), Some(&arson_array![1]));
/// 
/// // ...or a &str, which gets converted to a Symbol behind the scenes.
/// context.add_macro("kDefine", arson_array![2]);
/// assert_eq!(context.get_macro("kDefine"), Some(&arson_array![2]));
///
/// // An implementation example, which sets a variable
/// // with the given name to the text "some text".
/// fn do_something_with_symbol<S>(context: &mut Context<S>, name: impl IntoSymbol) {
///     context.set_variable(name, "some text");
/// }
///
/// let symbol = context.add_symbol("text");
/// do_something_with_symbol(&mut context, &symbol);
/// assert_eq!(context.get_variable(&symbol), "some text".into());
/// 
/// do_something_with_symbol(&mut context, "text2");
/// assert_eq!(context.get_variable("text2"), "some text".into());
/// ```
pub trait IntoSymbol {
    fn into_symbol<S>(self, context: &mut Context<S>) -> Symbol;
}

impl<N: AsRef<str>> IntoSymbol for N {
    fn into_symbol<S>(self, context: &mut Context<S>) -> Symbol {
        context.add_symbol(self.as_ref())
    }
}

impl IntoSymbol for Symbol {
    fn into_symbol<S>(self, _context: &mut Context<S>) -> Symbol {
        self
    }
}

impl IntoSymbol for &Symbol {
    fn into_symbol<S>(self, _context: &mut Context<S>) -> Symbol {
        self.clone()
    }
}

#[cfg(feature = "file-system")]
impl<S> Context<S> {
    pub fn file_system(&self) -> &FileSystem {
        &self.file_system
    }

    pub fn file_system_mut(&mut self) -> &mut FileSystem {
        &mut self.file_system
    }
}

#[cfg(feature = "file-loading")]
impl<S> Context<S> {
    pub fn load_path<P: AsRef<VirtualPath>>(
        &mut self,
        options: LoadOptions,
        path: P,
    ) -> Result<NodeArray, LoadError> {
        crate::loader::load_path(self, options, path)
    }
}
