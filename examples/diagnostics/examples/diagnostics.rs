// SPDX-License-Identifier: MIT OR Apache-2.0

use std::fs::File;
use std::path::Path;

use arson_parse::reporting as codespan_reporting;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::Chars;

fn main() {
    let mount_dir = Path::new(file!()).join("../../run");

    let files = ["success.dta", "fail.dta"];
    for file in files {
        println!("Parsing file {file}");

        print_parsed(&mount_dir, file);

        println!();
    }
}

fn print_parsed(mount_dir: &Path, file: &str) {
    let text = {
        let file = File::open(mount_dir.join(file)).expect("failed to open file");
        std::io::read_to_string(file).expect("failed to read file")
    };

    match arson_parse::parse_text(&text) {
        Ok(parsed) => {
            println!("File {file}:");
            for expr in parsed {
                println!("- ({:?}) {:?}", expr.location, expr.value);
            }
        },
        Err(diagnostics) => {
            let db = SimpleFile::new(file, text);
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = codespan_reporting::term::Config { chars: Chars::ascii(), ..Default::default() };

            for diagnostic in diagnostics {
                term::emit(&mut writer.lock(), &config, &db, &diagnostic.to_codespan(()))
                    .expect("failed to emit diagnostic");
            }
        },
    }
}
