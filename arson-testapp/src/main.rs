// SPDX-License-Identifier: LGPL-3.0-or-later

use arson::{Context, NodeValue};

fn main() -> arson::Result<()> {
    println!("Hello from native!");

    let mut context = Context::new();

    context.register_func_by_name("print", |context, args| {
        let message = args.string(context, 1)?;
        println!("{message}");
        Ok(NodeValue::HANDLED)
    });

    let file = context.load_text(include_str!("../run/main.dta"))?;
    let command = file.find_array(&context.add_symbol("main"))?.command(1)?;
    context.execute(&command)?;

    Ok(())
}
