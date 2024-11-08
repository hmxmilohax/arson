// SPDX-License-Identifier: LGPL-3.0-or-later

use arson::{Context, Node};

fn main() -> arson::Result<()> {
    println!("Hello from native!");

    let mut context = Context::new();

    context.register_func_by_name("print", |_context, args| {
        let message = args.string(1)?;
        println!("{message}");
        Ok(Node::Integer(0))
    })?;

    let file = context.load_text(include_str!("../run/main.dta"))?;
    let command = file.find_array(&context.add_symbol("main"))?.command(1)?;
    context.execute(command)?;

    Ok(())
}
