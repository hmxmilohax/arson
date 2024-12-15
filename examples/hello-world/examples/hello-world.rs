// SPDX-License-Identifier: MIT OR Apache-2.0

use std::path::Path;

use arson::prelude::*;

struct State {
    file_system: FileSystem,
}

impl FileSystemState for State {
    fn file_system(&self) -> &FileSystem {
        &self.file_system
    }

    fn file_system_mut(&mut self) -> &mut FileSystem {
        &mut self.file_system
    }
}

impl StdlibState for State {
    fn file_load_options(&self) -> LoadOptions {
        LoadOptions { allow_include: true, allow_autorun: true }
    }
}

fn main() -> arson::Result {
    println!("> Hello from native!");

    // Mount `run` directory for scripts
    let mount_dir = Path::new(file!()).join("../../run");
    let driver = BasicFileSystemDriver::new(&mount_dir)?;

    // Make context
    let mut context = Context::new(State { file_system: FileSystem::new(driver) });
    arson::stdlib::register_funcs(&mut context);
    println!("Created context.");

    // Load main.dta file
    let file = context.load_path("/main.dta")?;
    println!("Loaded main.dta");

    // Execute (main {...}) script
    let script = file.find_tag((&mut context, "main"))?;
    context.execute_block(script.borrow()?.slice(1..)?)?;
    println!("Ran main.dta!");

    Ok(())
}
