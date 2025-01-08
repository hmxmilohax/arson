// SPDX-License-Identifier: LGPL-3.0-or-later

use std::any::{Any, TypeId};
use std::collections::HashMap;

#[cfg(feature = "file-system")]
use arson_fs::FileSystem;

use crate::builtin::BuiltinState;
use crate::prelude::*;
use crate::{ExecutionError, FindDataPredicate, SymbolTable};

/// A function which is callable from script.
pub type HandleFn = fn(context: &mut Context, args: &NodeSlice) -> ExecuteResult;

/// The result of a script execution.
pub type ExecuteResult = crate::Result<Node>;

pub trait ContextState: Any {}

pub struct Context {
    symbol_table: SymbolTable,

    macros: SymbolMap<NodeArray>,
    variables: SymbolMap<Node>,
    functions: SymbolMap<HandleFn>,

    #[cfg(feature = "file-system")]
    file_system: Option<FileSystem>,
    pub(crate) builtin_state: BuiltinState,

    states: HashMap<TypeId, Box<dyn Any>>,
}

impl Context {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable::new();
        let builtin_state = BuiltinState::new(&mut symbol_table);

        let mut context = Self {
            symbol_table,

            macros: SymbolMap::new(),
            variables: SymbolMap::new(),
            functions: SymbolMap::new(),

            #[cfg(feature = "file-system")]
            file_system: None,
            builtin_state,

            states: HashMap::new(),
        };

        crate::builtin::register_funcs(&mut context);

        context
    }

    pub fn register_state<S: ContextState>(&mut self, state: S) {
        self.states.insert(state.type_id(), Box::new(state));
    }

    pub fn get_state<S: ContextState>(&self) -> crate::Result<&S> {
        self.get_state_opt()
            .ok_or_else(|| ExecutionError::StateNotFound(std::any::type_name::<S>()).into())
    }

    pub fn get_state_opt<S: ContextState>(&self) -> Option<&S> {
        self.states.get(&TypeId::of::<S>()).and_then(|s| s.downcast_ref())
    }

    pub fn get_state_mut<S: ContextState>(&mut self) -> crate::Result<&mut S> {
        self.get_state_mut_opt()
            .ok_or_else(|| ExecutionError::StateNotFound(std::any::type_name::<S>()).into())
    }

    pub fn get_state_mut_opt<S: ContextState>(&mut self) -> Option<&mut S> {
        self.states.get_mut(&TypeId::of::<S>()).and_then(|s| s.downcast_mut())
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

    pub fn register_func(&mut self, name: impl IntoSymbol, func: HandleFn) -> bool {
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
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait to make APIs which require [`Symbol`]s more convenient to use.
///
/// ```rust
/// use arson_core::{arson_array, Context, IntoSymbol};
///
/// let mut context = Context::new();
///
/// // Context::add_macro makes use of this trait.
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
/// fn do_something_with_symbol(context: &mut Context, name: impl IntoSymbol) {
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
    fn into_symbol(self, context: &mut Context) -> Symbol;
}

impl<N: AsRef<str>> IntoSymbol for N {
    fn into_symbol(self, context: &mut Context) -> Symbol {
        context.add_symbol(self.as_ref())
    }
}

impl IntoSymbol for Symbol {
    fn into_symbol(self, _context: &mut Context) -> Symbol {
        self
    }
}

impl IntoSymbol for &Symbol {
    fn into_symbol(self, _context: &mut Context) -> Symbol {
        self.clone()
    }
}

#[cfg(feature = "text-loading")]
impl Context {
    pub fn load_text(&mut self, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError> {
        crate::loader::load_text(self, options, text)
    }
}

#[cfg(feature = "file-system")]
impl Context {
    pub fn with_filesystem_driver(self, driver: impl arson_fs::FileSystemDriver + 'static) -> Self {
        Self { file_system: Some(FileSystem::new(driver)), ..self }
    }

    fn no_fs_error() -> std::io::Error {
        std::io::Error::new(std::io::ErrorKind::Unsupported, "no file system driver registered")
    }

    pub fn file_system(&self) -> std::io::Result<&FileSystem> {
        self.file_system_opt().ok_or_else(Self::no_fs_error)
    }

    pub fn file_system_mut(&mut self) -> std::io::Result<&mut FileSystem> {
        self.file_system_opt_mut().ok_or_else(Self::no_fs_error)
    }

    pub fn file_system_opt(&self) -> Option<&FileSystem> {
        self.file_system.as_ref()
    }

    pub fn file_system_opt_mut(&mut self) -> Option<&mut FileSystem> {
        self.file_system.as_mut()
    }
}

#[cfg(feature = "file-loading")]
impl Context {
    pub fn load_path<P: AsRef<arson_fs::VirtualPath>>(
        &mut self,
        options: LoadOptions,
        path: P,
    ) -> Result<NodeArray, LoadError> {
        crate::loader::load_path(self, options, path)
    }
}
