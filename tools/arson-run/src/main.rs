// SPDX-License-Identifier: LGPL-3.0-or-later

use std::path::{Path, PathBuf};

use anyhow::{bail, Context};
use arson::fs::drivers::BasicFileSystemDriver;
use arson::stdlib::StdlibOptions;
use arson::LoadOptions;
use clap::Parser;

/// A basic runtime for tool scripts written in DTA..
#[derive(Debug, clap::Parser)]
struct Arguments {
    /// The script to run.
    path: PathBuf,

    /// The directory to mount as the base scripting directory.
    #[arg(long, short)]
    mount_dir: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();

    fn canon_fail(path: &Path) -> String {
        format!("failed to resolve path {}", path.display())
    }

    let path = args.path.canonicalize().with_context(|| canon_fail(&args.path))?;
    let mount_dir = match args.mount_dir {
        Some(mount_dir) => mount_dir.canonicalize().with_context(|| canon_fail(&mount_dir))?,
        None => path.parent().unwrap().to_path_buf(),
    };

    let Ok(file_path) = path.strip_prefix(&mount_dir) else {
        // TODO: find a way to remove this limitation?
        bail!("script is not contained within mount directory");
    };

    let driver = BasicFileSystemDriver::new(mount_dir)?;

    // Make context
    let mut context = arson::Context::new().with_filesystem_driver(driver);
    context.register_state(StdlibOptions {
        file_load_options: LoadOptions { allow_include: true, allow_autorun: true },
    });
    arson::stdlib::register_funcs(&mut context);

    // Load script file
    let options = LoadOptions { allow_include: true, allow_autorun: true };
    let file = context.load_path(options, file_path.to_string_lossy().as_ref())?;

    // Execute (main {...}) script
    let script = file.find_tag((&mut context, "main"))?;
    context.execute_block(script.borrow()?.slice(1..)?)?;

    Ok(())
}
