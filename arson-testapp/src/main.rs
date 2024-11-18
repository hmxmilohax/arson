// SPDX-License-Identifier: LGPL-3.0-or-later

use std::path::Path;

use arson::fs::BasicFileSystemDriver;
use arson::{Context, LoadOptions, NodeValue};

fn main() -> arson::Result<()> {
    println!("> Hello from native!");

    let mount_dir = Path::new(file!()).join("../../run");
    let driver = BasicFileSystemDriver::new(&mount_dir)?;
    let mut context = Context::with_file_driver(driver);
    println!("Created context.");

    context.register_func_by_name("print", |context, args| {
        let message = args.string(context, 0)?;
        println!("> {message}");
        Ok(NodeValue::HANDLED)
    });

    let options = LoadOptions { allow_include: true };
    let file = context.load_path(options, "/main.dta")?;
    println!("Loaded main.dta");

    let command = file.find_array(&context.add_symbol("main"))?.command(1)?;
    context.execute(&command)?;
    println!("Ran main.dta!");

    Ok(())
}
