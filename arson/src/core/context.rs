// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io;

use crate::fs::{AbsolutePath, FileSystem, FileSystemDriver, VirtualPath};
use crate::parse::loader::{self, LoadOptions};
use crate::{builtin, LoadError};

use super::{Error, HandleFn, NodeArray, NodeCommand, NodeValue, Symbol, SymbolMap, SymbolTable};

pub struct Context {
    symbol_table: SymbolTable,
    macros: SymbolMap<NodeArray>,
    variables: SymbolMap<NodeValue>,
    functions: SymbolMap<HandleFn>,
    file_system: Option<FileSystem>,
}

impl Context {
    pub fn new() -> Self {
        let mut context = Self {
            symbol_table: SymbolTable::new(),
            macros: SymbolMap::new(),
            variables: SymbolMap::new(),
            functions: SymbolMap::new(),
            file_system: None,
        };

        builtin::register_funcs(&mut context);

        context
    }

    pub fn with_file_driver(driver: Box<dyn FileSystemDriver>) -> Self {
        let mut context = Self::new();
        context.file_system = Some(FileSystem::new(driver));
        context
    }

    pub fn add_symbol(&mut self, name: &str) -> Symbol {
        self.symbol_table.add(name)
    }

    pub fn remove_symbol(&mut self, name: &Symbol) {
        self.symbol_table.remove(name);
    }

    pub fn get_symbol(&mut self, name: &str) -> Option<Symbol> {
        self.symbol_table.get(name)
    }

    pub fn add_macro(&mut self, name: Symbol, array: NodeArray) {
        self.macros.insert(name, array);
    }

    pub fn remove_macro(&mut self, name: &Symbol) {
        self.macros.remove(name);
    }

    pub fn get_macro(&mut self, name: &Symbol) -> Option<&NodeArray> {
        self.macros.get(name)
    }

    pub fn get_variable(&mut self, name: &Symbol) -> NodeValue {
        match self.variables.get(name) {
            Some(value) => value.clone(),
            None => {
                let value = NodeValue::from(0);
                self.variables.insert(name.clone(), value.clone());
                value
            },
        }
    }

    pub fn set_variable(&mut self, name: &Symbol, value: NodeValue) {
        self.variables.insert(name.clone(), value);
    }

    pub fn register_func_by_name(&mut self, name: &str, func: HandleFn) -> bool {
        let symbol = self.symbol_table.add(name);
        self.register_func(&symbol, func)
    }

    pub fn register_func(&mut self, name: &Symbol, func: HandleFn) -> bool {
        self.functions.insert(name.clone(), func).is_none()
    }

    pub fn file_system(&self) -> io::Result<&FileSystem> {
        self.file_system_opt()
            .ok_or_else(|| io::Error::new(io::ErrorKind::Unsupported, "no file system registered").into())
    }

    pub fn file_system_opt(&self) -> Option<&FileSystem> {
        self.file_system.as_ref()
    }

    pub fn cwd(&self) -> Option<&AbsolutePath> {
        self.file_system.as_ref().map(|fs| fs.cwd())
    }

    pub fn set_cwd(&mut self, path: &VirtualPath) -> Option<AbsolutePath> {
        self.file_system.as_mut().map(|fs| fs.set_cwd(path))
    }

    pub fn load_path(&mut self, options: LoadOptions, path: &VirtualPath) -> Result<NodeArray, LoadError> {
        let file_system = self.file_system()?;

        let mut file = file_system.open_execute(path)?;
        let text = io::read_to_string(file.as_mut())?;

        let canon = file_system.canonicalize(path);
        let Some(dir) = canon.parent() else {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "file has no containing directory (how???)").into());
        };

        let old_cwd = self.set_cwd(dir).expect("file system is known to be registered");
        let array = loader::load_text(self, options, &text)?;
        self.set_cwd(old_cwd.as_ref());

        Ok(array)
    }

    pub fn load_text(&mut self, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError> {
        loader::load_text(self, options, text)
    }

    pub fn execute(&mut self, command: &NodeCommand) -> crate::Result<NodeValue> {
        let result = match command.evaluate(self, 0)? {
            NodeValue::Symbol(symbol) => match self.functions.get(&symbol) {
                Some(func) => func(self, command.slice(1..)?)?,
                None => return Err(Error::EntryNotFound),
            },
            NodeValue::Object(_obj) => todo!("obj.handle(self, command)?"),
            NodeValue::Function(func) => func(self, command.slice(1..)?)?,

            _ => NodeValue::Unhandled,
        };

        if let NodeValue::Unhandled = result {
            todo!("default handler")
        }

        Ok(result)
    }
}
