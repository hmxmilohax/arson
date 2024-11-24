use super::{Context, NodeValue, Symbol};

#[derive(Debug, Clone)]
pub struct Variable {
    symbol: Symbol,
}

impl Variable {
    pub fn new(name: &str, context: &mut Context) -> Self {
        let symbol = context.add_symbol(name);
        Self { symbol }
    }

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

#[derive(Debug)]
struct VariableEntry {
    variable: Variable,
    value: NodeValue,
}

#[derive(Debug, Default)]
pub struct VariableStack {
    stack: Vec<VariableEntry>,
}

impl VariableStack {
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    pub fn save(&mut self, context: &mut Context, variable: &Variable) {
        self.stack.push(VariableEntry {
            variable: variable.clone(),
            value: variable.get(context),
        })
    }

    // Would have liked if there were a way to do this implicitly,
    // but the borrow checker said no, so this will have to do...
    pub fn restore(&mut self, context: &mut Context) {
        for entry in self.stack.iter() {
            entry.variable.set(context, entry.value.clone());
        }

        self.stack.clear();
    }
}

impl Drop for VariableStack {
    fn drop(&mut self) {
        // Ensure no unbalanced stacks are left behind
        assert_eq!(self.stack.len(), 0);
    }
}
