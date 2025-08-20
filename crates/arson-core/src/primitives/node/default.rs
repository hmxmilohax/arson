// SPDX-License-Identifier: LGPL-3.0-or-later

use std::rc::Rc;

use crate::{
    ArrayRef,
    ArrayTag,
    Context,
    FloatValue,
    Integer,
    IntoSymbol,
    Node,
    NodeCommand,
    NodeProperty,
    NodeValue,
    Number,
    ObjectRef,
    Symbol,
    Variable,
};

macro_rules! get_value_or_else {
    ($value:expr, $default:expr) => {{
        if let Some(value) = $value {
            Ok(value)
        } else {
            $default
        }
    }};
    ($value:expr => Result, $default:expr) => {{
        if let Some(value) = $value {
            value
        } else {
            $default
        }
    }};
}

pub trait SymbolDefault {
    fn into_symbol(self) -> crate::Result<Symbol>;
}

impl SymbolDefault for &Symbol {
    fn into_symbol(self) -> crate::Result<Symbol> {
        Ok(self.clone())
    }
}

impl<N: IntoSymbol> SymbolDefault for (&mut Context, N) {
    fn into_symbol(self) -> crate::Result<Symbol> {
        self.1.into_symbol(self.0)
    }
}

impl NodeValue {
    pub fn integer_or(&self, default: Integer) -> Integer {
        self.integer().unwrap_or(default)
    }

    pub fn float_or(&self, default: FloatValue) -> FloatValue {
        self.float().unwrap_or(default)
    }

    pub fn number_or(&self, default: Number) -> Number {
        self.number().unwrap_or(default)
    }

    pub fn size_integer_or(&self, default: usize) -> usize {
        self.size_integer_opt().unwrap_or(default)
    }

    pub fn string_or(&self, default: &Rc<String>) -> Rc<String> {
        self.string().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn symbol_or(&self, default: impl SymbolDefault) -> crate::Result<Symbol> {
        get_value_or_else!(self.symbol().cloned(), default.into_symbol())
    }

    pub fn force_symbol_or(&self, context: &mut Context, default: impl IntoSymbol) -> crate::Result<Symbol> {
        get_value_or_else!(self.force_symbol(context) => Result, default.into_symbol(context))
    }

    pub fn array_tag_or(&self, default: &ArrayTag) -> ArrayTag {
        self.array_tag().unwrap_or_else(|| default.clone())
    }

    pub fn variable_or(&self, default: &Variable) -> Variable {
        self.variable().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn object_or(&self, default: &ObjectRef) -> ObjectRef {
        self.object().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn array_or(&self, default: &ArrayRef) -> ArrayRef {
        self.array().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn command_or(&self, default: &NodeCommand) -> NodeCommand {
        self.command().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn property_or(&self, default: &NodeProperty) -> NodeProperty {
        self.property().cloned().unwrap_or_else(|| default.clone())
    }

    pub fn integer_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Integer>,
    ) -> crate::Result<Integer> {
        get_value_or_else!(self.integer(), default())
    }

    pub fn float_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<FloatValue>,
    ) -> crate::Result<FloatValue> {
        get_value_or_else!(self.float(), default())
    }

    pub fn number_or_else(&self, default: impl FnOnce() -> crate::Result<Number>) -> crate::Result<Number> {
        get_value_or_else!(self.number(), default())
    }

    pub fn size_integer_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<usize>,
    ) -> crate::Result<usize> {
        get_value_or_else!(self.size_integer_opt(), default())
    }

    pub fn string_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Rc<String>>,
    ) -> crate::Result<Rc<String>> {
        get_value_or_else!(self.string().cloned(), default())
    }

    pub fn symbol_or_else(&self, default: impl FnOnce() -> crate::Result<Symbol>) -> crate::Result<Symbol> {
        get_value_or_else!(self.symbol().cloned(), default())
    }

    pub fn force_symbol_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        if let Some(value) = self.force_symbol(context) {
            value
        } else {
            default(context)
        }
    }

    pub fn array_tag_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<ArrayTag>,
    ) -> crate::Result<ArrayTag> {
        get_value_or_else!(self.array_tag(), default())
    }

    pub fn variable_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Variable>,
    ) -> crate::Result<Variable> {
        get_value_or_else!(self.variable().cloned(), default())
    }

    pub fn object_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<ObjectRef>,
    ) -> crate::Result<ObjectRef> {
        get_value_or_else!(self.object().cloned(), default())
    }

    pub fn array_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<ArrayRef>,
    ) -> crate::Result<ArrayRef> {
        get_value_or_else!(self.array().cloned(), default())
    }

    pub fn command_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<NodeCommand>,
    ) -> crate::Result<NodeCommand> {
        get_value_or_else!(self.command().cloned(), default())
    }

    pub fn property_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<NodeProperty>,
    ) -> crate::Result<NodeProperty> {
        get_value_or_else!(self.property().cloned(), default())
    }
}

// Optional value retrieval, with defaults
impl Node {
    pub fn integer_or(&self, context: &mut Context, default: Integer) -> crate::Result<Integer> {
        self.evaluate(context).map(|v| v.integer_or(default))
    }

    pub fn float_or(&self, context: &mut Context, default: FloatValue) -> crate::Result<FloatValue> {
        self.evaluate(context).map(|v| v.float_or(default))
    }

    pub fn number_or(&self, context: &mut Context, default: Number) -> crate::Result<Number> {
        self.evaluate(context).map(|v| v.number_or(default))
    }

    pub fn size_integer_or(&self, context: &mut Context, default: usize) -> crate::Result<usize> {
        self.evaluate(context).map(|v| v.size_integer_or(default))
    }

    pub fn string_or(&self, context: &mut Context, default: &Rc<String>) -> crate::Result<Rc<String>> {
        self.evaluate(context).map(|v| v.string_or(default))
    }

    pub fn symbol_or(&self, context: &mut Context, default: impl IntoSymbol) -> crate::Result<Symbol> {
        self.evaluate(context).and_then(|v| v.symbol_or((context, default)))
    }

    pub fn force_symbol_or(&self, context: &mut Context, default: impl IntoSymbol) -> crate::Result<Symbol> {
        self.evaluate(context).and_then(|v| v.force_symbol_or(context, default))
    }

    pub fn array_tag_or(&self, context: &mut Context, default: &ArrayTag) -> crate::Result<ArrayTag> {
        self.evaluate(context).map(|v| v.array_tag_or(default))
    }

    pub fn variable_or(&self, default: &Variable) -> Variable {
        self.unevaluated().variable_or(default)
    }

    pub fn object_or(&self, context: &mut Context, default: &ObjectRef) -> crate::Result<ObjectRef> {
        self.evaluate(context).map(|v| v.object_or(default))
    }

    pub fn array_or(&self, context: &mut Context, default: &ArrayRef) -> crate::Result<ArrayRef> {
        self.evaluate(context).map(|v| v.array_or(default))
    }

    pub fn command_or(&self, default: &NodeCommand) -> NodeCommand {
        self.unevaluated().command_or(default)
    }

    pub fn property_or(&self, default: &NodeProperty) -> NodeProperty {
        self.unevaluated().property_or(default)
    }

    pub fn integer_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Integer>,
    ) -> crate::Result<Integer> {
        get_value_or_else!(self.evaluate(context)?.integer(), default(context))
    }

    pub fn float_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<FloatValue>,
    ) -> crate::Result<FloatValue> {
        get_value_or_else!(self.evaluate(context)?.float(), default(context))
    }

    pub fn number_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Number>,
    ) -> crate::Result<Number> {
        get_value_or_else!(self.evaluate(context)?.number(), default(context))
    }

    pub fn size_integer_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<usize>,
    ) -> crate::Result<usize> {
        get_value_or_else!(self.evaluate(context)?.size_integer() => Result, default(context))
    }

    pub fn string_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Rc<String>>,
    ) -> crate::Result<Rc<String>> {
        get_value_or_else!(self.evaluate(context)?.string().cloned(), default(context))
    }

    pub fn symbol_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        get_value_or_else!(self.evaluate(context)?.symbol().cloned(), default(context))
    }

    pub fn force_symbol_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<Symbol>,
    ) -> crate::Result<Symbol> {
        get_value_or_else!(self.evaluate(context)?.force_symbol(context) => Result, default(context))
    }

    pub fn array_tag_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<ArrayTag>,
    ) -> crate::Result<ArrayTag> {
        get_value_or_else!(self.evaluate(context)?.array_tag(), default(context))
    }

    pub fn variable_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<Variable>,
    ) -> crate::Result<Variable> {
        self.unevaluated().variable_or_else(default)
    }

    pub fn object_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<ObjectRef>,
    ) -> crate::Result<ObjectRef> {
        get_value_or_else!(self.evaluate(context)?.object().cloned(), default(context))
    }

    pub fn array_or_else(
        &self,
        context: &mut Context,
        default: impl FnOnce(&mut Context) -> crate::Result<ArrayRef>,
    ) -> crate::Result<ArrayRef> {
        get_value_or_else!(self.evaluate(context)?.array().cloned(), default(context))
    }

    pub fn command_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<NodeCommand>,
    ) -> crate::Result<NodeCommand> {
        self.unevaluated().command_or_else(default)
    }

    pub fn property_or_else(
        &self,
        default: impl FnOnce() -> crate::Result<NodeProperty>,
    ) -> crate::Result<NodeProperty> {
        self.unevaluated().property_or_else(default)
    }
}
