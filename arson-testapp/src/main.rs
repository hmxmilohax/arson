// SPDX-License-Identifier: LGPL-3.0-or-later

use std::path::Path;

use arson::fs::drivers::BasicFileSystemDriver;
use arson::{Context, HandleResult, LoadOptions, NodeSlice, NodeValue};

fn main() -> arson::Result<()> {
    println!("> Hello from native!");

    // Mount `run` directory for scripts
    let mount_dir = Path::new(file!()).join("../../run");
    let driver = BasicFileSystemDriver::new(&mount_dir)?;

    // Make context
    let mut context = Context::with_file_driver(driver);
    funcs::register_funcs(&mut context);
    println!("Created context.");

    // Load main.dta file
    let options = LoadOptions { allow_include: true };
    let file = context.load_path(options, "/main.dta")?;
    println!("Loaded main.dta");

    // Execute (main {...}) script
    let command = file.find_array(&context.add_symbol("main"))?.command(1)?;
    context.execute(&command)?;
    println!("Ran main.dta!");

    Ok(())
}

mod funcs {
    use super::*;

    pub fn register_funcs(context: &mut Context) {
        context.register_func("print", self::print);
    }

    pub fn print(context: &mut Context, args: &NodeSlice) -> HandleResult {
        if !args.is_empty() {
            // Manual enumeration of nodes to avoid adding a separator,
            // to match the original `print`
            print!("> ");
            for arg in args {
                print!("{}", arg.display_evaluated(context))
            }
            println!();
        }

        Ok(NodeValue::HANDLED)
    }
}
