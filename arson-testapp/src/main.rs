// SPDX-License-Identifier: LGPL-3.0-or-later

use std::path::Path;

use arson::fs::drivers::BasicFileSystemDriver;
use arson::{stdlib, Context, LoadOptions};

fn main() -> arson::Result<()> {
    println!("> Hello from native!");

    // Mount `run` directory for scripts
    let mount_dir = Path::new(file!()).join("../../run");
    let driver = BasicFileSystemDriver::new(&mount_dir)?;

    // Make context
    let mut context = Context::with_file_driver(driver);
    stdlib::register_funcs(&mut context);
    println!("Created context.");

    // Load main.dta file
    let options = LoadOptions { allow_include: true };
    let file = context.load_path(options, "/main.dta")?;
    println!("Loaded main.dta");

    // Execute (main {...}) script
    let script = file.find_array(context.add_symbol("main"))?;
    context.execute_block(script.borrow().slice(1..)?)?;
    println!("Ran main.dta!");

    Ok(())
}
