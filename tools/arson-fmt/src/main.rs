// SPDX-License-Identifier: LGPL-3.0-or-later

use std::fs::File;
use std::io::{BufWriter, Write};
use std::process::ExitCode;

use arson_fmt::{Formatter, Options};
use arson_parse::reporting::files::SimpleFile;
use arson_parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson_parse::reporting::term::{self, Chars};
use clap::Parser;

/// A formatter for DTA files.
#[derive(clap::Parser, Debug)]
struct Arguments {
    /// the input file to be formatted
    input_file: String,

    /// the text editing mode to use
    #[arg(long, short)]
    output_file: Option<String>,
}

/*
/// A formatter for DTA files.
#[derive(clap::Parser, Debug)]
struct Arguments {
    /// the input file to be formatted
    #[command(subcommand)]
    mode: InputMode,
}

#[derive(clap::Parser, Debug)]
enum InputMode {
    File {
        /// the input file to be formatted
        input_file: String,

        /// the text editing mode to use
        #[arg(long = "output", short)]
        output_file: Option<String>,
    },
    Stdio,
}

#[derive(clap::Parser, Debug)]
enum IndentMode {

}
*/

fn main() -> ExitCode {
    let args = Arguments::parse();
    let file_text = std::fs::read_to_string(&args.input_file).unwrap();

    let options = Options::default(); // TODO
    match Formatter::new(&file_text, options) {
        Ok(formatter) => {
            let output_file = args.output_file.unwrap_or_else(|| args.input_file.clone());
            let output_file = File::create(output_file).unwrap();
            let mut output_file = BufWriter::new(output_file);

            write!(output_file, "{}", formatter).unwrap();

            ExitCode::SUCCESS
        },
        Err(error) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config { chars: Chars::ascii(), ..Default::default() };

            let file = SimpleFile::new(args.input_file, &file_text);
            for error in error.diagnostics {
                let _ = term::emit(&mut writer.lock(), &config, &file, &error.to_codespan(()));
            }

            ExitCode::FAILURE
        },
    }
}
