// SPDX-License-Identifier: MIT OR Apache-2.0

use std::path::Path;

use arson::parse::reporting as codespan_reporting;
use arson::prolog::*;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::Chars;

fn main() {
    let mount_dir = Path::new(file!()).join("../../run");
    let driver = BasicFileSystemDriver::new(&mount_dir).expect("provided path is a directory");
    let file_system = FileSystem::new(driver);

    let files = ["success.dta", "fail.dta"];
    for file in files {
        println!("Parsing file {file}");

        print_parsed(&file_system, file);

        println!();
    }
}

fn print_parsed(fs: &FileSystem, path: impl AsRef<VirtualPath>) {
    let path = path.as_ref();
    let text = {
        let file = fs.open(path).expect("failed to open file");
        std::io::read_to_string(file).expect("failed to read file")
    };

    match arson::parse::parse_text(&text) {
        Ok(parsed) => {
            println!("File {path}:");
            for expr in parsed {
                println!("- ({:?}) {:?}", expr.location, expr.value);
            }
        },
        Err(diagnostics) => {
            let db = SimpleFile::new(path, text);
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = codespan_reporting::term::Config { chars: Chars::ascii(), ..Default::default() };

            for diagnostic in diagnostics {
                term::emit(&mut writer.lock(), &config, &db, &diagnostic.to_codespan(()))
                    .expect("failed to emit diagnostic");
            }
        },
    }
}
