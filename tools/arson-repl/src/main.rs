// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io;

use arson::fs::drivers::BasicFileSystemDriver;
use arson::parse::reporting::files::SimpleFile;
use arson::parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson::parse::reporting::term::{self, Chars};
use arson::parse::DiagnosticKind;
use arson::prelude::*;

mod util;

use util::*;

struct State;

impl StdlibState for State {
    fn file_load_options(&self) -> LoadOptions {
        LoadOptions { allow_include: true, allow_autorun: true }
    }
}

fn main() {
    let mut context = setup();
    run(&mut context)
}

fn setup() -> Context<State> {
    let mut context = Context::new(State);
    arson::stdlib::register_funcs(&mut context);

    if prompt_question("Mount a directory to be accessible from scripts?") {
        let driver = loop {
            println!("Note that all paths used in scripts will be relative to this directory.");
            let directory = prompt_str("Enter a directory");
            match BasicFileSystemDriver::new(&directory) {
                Ok(driver) => break driver,
                Err(error) => eprintln!("Could not create file driver: {error}"),
            }
        };
        context = context.with_filesystem_driver(driver);
    }

    context
}

fn run(context: &mut Context<State>) {
    loop {
        let array = match read_input(context) {
            Ok(array) => array,
            Err((text, error)) => {
                emit_load_error(text, error);
                continue;
            },
        };

        if array.len() != 1 {
            continue;
        }

        match array.evaluate(context, 0) {
            Ok(evaluated) => println!("{evaluated}"),
            Err(error) => {
                eprintln!("Evaluation error: {error}\n{}", error.backtrace())
            },
        };
    }
}

fn read_input(context: &mut Context<State>) -> Result<NodeArray, (String, LoadError)> {
    // Read input
    print_flush!(">>> ");

    let mut text = String::new();
    loop {
        io::stdin().read_line(&mut text).expect("failed to read line");

        let options = context.state.file_load_options();
        match context.load_text(options, &text) {
            Ok(array) => return Ok(array),
            Err(error) => {
                if let LoadError::Parse(ref error) = error {
                    // Allow multi-line input while unmatched braces are present
                    if error.unclosed_array_count > 0
                        || error.diagnostics.iter().any(|e| {
                            matches!(
                                e.kind(),
                                DiagnosticKind::UnmatchedConditional | DiagnosticKind::UnclosedBlockComment
                            )
                        })
                    {
                        print_flush!("... ");
                        continue;
                    }
                }

                return Err((text, error));
            },
        }
    }
}

fn emit_load_error(text: String, error: LoadError) {
    match error {
        LoadError::Parse(error) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config { chars: Chars::ascii(), ..Default::default() };

            let file = SimpleFile::new("<input>", text);
            for error in error.diagnostics {
                let _ = term::emit(&mut writer.lock(), &config, &file, &error.to_codespan(()));
            }
        },
        LoadError::Inner(errors) => {
            // TODO: use codespan_diagnostics for this
            eprintln!("Errors while loading input:");
            for error in errors {
                eprintln!("- {error}");
            }
        },
        _ => eprintln!("Error while loading input: {error}"),
    }
}
