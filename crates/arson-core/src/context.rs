// SPDX-License-Identifier: LGPL-3.0-or-later

use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::rc::Rc;

#[cfg(feature = "file-system")]
use arson_fs::FileSystem;

use crate::builtin::BuiltinState;
use crate::prelude::*;
use crate::{ExecutionError, FindDataPredicate, SymbolTable};

/// The result of a script execution.
pub type ExecuteResult = crate::Result<Node>;

pub trait ContextState: Any {}

pub struct Context {
    symbol_table: SymbolTable,

    macros: SymbolMap<Rc<NodeArray>>,
    variables: SymbolMap<Node>,
    functions: SymbolMap<HandleFn>,
    objects: SymbolMap<ObjectRef>,

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
            objects: SymbolMap::new(),

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

    pub fn get_symbol(&self, name: &str) -> Option<Symbol> {
        self.symbol_table.get(name)
    }

    pub fn add_macro(&mut self, name: impl IntoSymbol, array: impl Into<Rc<NodeArray>>) {
        let name = name.into_symbol(self);
        self.macros.insert(name, array.into());
    }

    pub fn add_macro_define(&mut self, name: impl IntoSymbol) {
        self.add_macro(name, arson_array![1]);
    }

    pub fn remove_macro(&mut self, name: impl IntoSymbol) {
        let name = name.into_symbol(self);
        self.macros.remove(&name);
    }

    pub fn get_macro(&self, name: impl IntoSymbol) -> Option<Rc<NodeArray>> {
        name.get_symbol(self).and_then(|name| self.macros.get(&name).cloned())
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

    pub fn get_variable(&self, name: impl IntoSymbol) -> Node {
        self.get_variable_opt(name).unwrap_or(Node::UNHANDLED)
    }

    pub fn get_variable_opt(&self, name: impl IntoSymbol) -> Option<Node> {
        name.get_symbol(self).and_then(|name| self.variables.get(&name).cloned())
    }

    pub fn set_variable(&mut self, name: impl IntoSymbol, value: impl Into<Node>) -> Option<Node> {
        let name = name.into_symbol(self);
        self.variables.insert(name, value.into())
    }

    pub fn register_func<F>(&mut self, name: impl IntoSymbol, func: F) -> bool
    where
        // note: mutable closures are not allowed due to the out-to-in execution model, which causes
        // problems when a command argument to a function calls the very same function again
        F: Fn(&mut Context, &NodeSlice) -> ExecuteResult + 'static,
    {
        let name = name.into_symbol(self);
        self.functions.insert(name, HandleFn::new(func)).is_none()
    }

    pub fn get_func(&self, name: impl IntoSymbol) -> Option<HandleFn> {
        name.get_symbol(self).and_then(|name| self.functions.get(&name).cloned())
    }

    pub fn register_object<O>(&mut self, name: impl IntoSymbol, object: O) -> ObjectRef
    where
        O: Object + 'static,
    {
        let name = name.into_symbol(self);
        let object = Rc::new(object);
        self.objects.insert(name, object.clone());
        object
    }

    pub fn get_object(&self, name: impl IntoSymbol) -> Option<ObjectRef> {
        name.get_symbol(self).and_then(|name| self.objects.get(&name).cloned())
    }

    pub fn execute(&mut self, command: &NodeCommand) -> ExecuteResult {
        let result = match command.evaluate(self, 0)? {
            NodeValue::Symbol(symbol) => {
                let args = command.slice(1..)?;
                if let Some(func) = self.functions.get(&symbol).cloned() {
                    func.call(self, args)?
                } else if let Some(obj) = self.objects.get(&symbol) {
                    obj.clone().handle(self, args)?
                } else {
                    return Err(ExecutionError::HandlerNotFound(symbol.name().to_string()).into());
                }
            },
            NodeValue::String(name) => {
                match self.get_symbol(name.as_ref()).and_then(|name| self.objects.get(&name)) {
                    Some(obj) => obj.clone().handle(self, command.slice(1..)?)?,
                    None => return Err(ExecutionError::HandlerNotFound(name.as_ref().clone()).into()),
                }
            },
            NodeValue::Function(function) => function.call(self, command.slice(1..)?)?,
            result @ _ => {
                return Err(ExecutionError::NotAHandler(result.to_string(), result.get_kind()).into())
            },
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
/// use std::rc::Rc;
///
/// use arson_core::{arson_array, Context, IntoSymbol};
///
/// let mut context = Context::new();
///
/// // Context::add_macro makes use of this trait.
/// // You can use either a Symbol, which gets used as-is...
/// let symbol = context.add_symbol("kDefine");
/// context.add_macro(&symbol, arson_array![1]);
/// assert_eq!(context.get_macro(&symbol), Some(Rc::new(arson_array![1])));
///
/// // ...or a &str, which gets converted to a Symbol behind the scenes.
/// context.add_macro("kDefine", arson_array![2]);
/// assert_eq!(context.get_macro("kDefine"), Some(Rc::new(arson_array![2])));
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
    fn get_symbol(self, context: &Context) -> Option<Symbol>;
}

impl<N: AsRef<str>> IntoSymbol for N {
    fn into_symbol(self, context: &mut Context) -> Symbol {
        context.add_symbol(self.as_ref())
    }

    fn get_symbol(self, context: &Context) -> Option<Symbol> {
        context.get_symbol(self.as_ref())
    }
}

impl IntoSymbol for Symbol {
    fn into_symbol(self, _context: &mut Context) -> Symbol {
        self
    }

    fn get_symbol(self, _context: &Context) -> Option<Symbol> {
        Some(self)
    }
}

impl IntoSymbol for &Symbol {
    fn into_symbol(self, _context: &mut Context) -> Symbol {
        self.clone()
    }

    fn get_symbol(self, _context: &Context) -> Option<Symbol> {
        Some(self.clone())
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
