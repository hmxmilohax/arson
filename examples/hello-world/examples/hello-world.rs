// SPDX-License-Identifier: MIT OR Apache-2.0

use std::path::Path;

use arson::fs::drivers::BasicFileSystemDriver;
use arson::prelude::*;

fn main() -> arson::Result {
    println!("> Hello from native!");

    // Mount `run` directory for scripts
    let mount_dir = Path::new(file!()).join("../../run");
    let driver = BasicFileSystemDriver::new(mount_dir)?;

    // Make context
    let mut context = Context::new().with_filesystem_driver(driver);
    context.register_state(StdlibOptions {
        file_load_options: LoadOptions { allow_include: true, allow_autorun: true },
    });
    arson::stdlib::register_funcs(&mut context);
    println!("Created context.");

    // Load main.dta file
    let options = LoadOptions { allow_include: true, allow_autorun: true };
    let file = context.load_path(options, "/main.dta")?;
    println!("Loaded main.dta");

    // Execute (main {...}) script
    let script = file.find_tag((&mut context, "main"))?;
    context.execute_block(script.borrow()?.slice(1..)?)?;
    println!("Ran main.dta!");

    Ok(())
}
