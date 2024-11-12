use super::{Context, NodeValue, Symbol};

#[derive(Debug, Clone)]
pub struct Variable {
    symbol: Symbol,
}

impl Variable {
    pub fn symbol(&self) -> &Symbol {
        &self.symbol
    }

    pub fn get(&self, context: &mut Context) -> NodeValue {
        context.get_variable(&self.symbol)
    }

    pub fn set(&self, context: &mut Context, value: NodeValue) {
        context.set_variable(&self.symbol, value)
    }
}

impl From<Symbol> for Variable {
    fn from(value: Symbol) -> Self {
        Self { symbol: value }
    }
}
