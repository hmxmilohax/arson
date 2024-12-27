// SPDX-License-Identifier: LGPL-3.0-or-later

use arson::fs::drivers::BasicFileSystemDriver;
use arson::parse::reporting::files::SimpleFile;
use arson::parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson::parse::reporting::term::{self, Chars};
use arson::parse::DiagnosticKind;
use arson::prelude::*;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

mod terminal;

struct State;

impl StdlibState for State {
    fn file_load_options(&self) -> LoadOptions {
        LoadOptions { allow_include: true, allow_autorun: true }
    }
}

fn main() {
    let mut context = make_context();
    let mut editor = make_editor();
    run(&mut context, &mut editor);
}

fn make_context() -> Context<State> {
    let mut context = Context::new(State);
    arson::stdlib::register_funcs(&mut context);

    if terminal::prompt_question("Mount a directory to be accessible from scripts?") {
        println!("Note that all paths used in scripts will be relative to this directory.");

        let driver = loop {
            let directory = terminal::prompt_str("Enter a directory");
            match BasicFileSystemDriver::new(&directory) {
                Ok(driver) => break driver,
                Err(error) => eprintln!("Could not create file driver: {error}"),
            }
        };

        context = context.with_filesystem_driver(driver);
    }

    context
}

fn make_editor() -> DefaultEditor {
    use rustyline::Config;

    #[rustfmt::skip] // do not pack into the same line
    let config = Config::builder()
        .auto_add_history(true)
        // .edit_mode(EditMode) // TODO
        .indent_size(3)
        .tab_stop(3)
        .build();

    DefaultEditor::with_config(config).expect("failed to create text reader")
}

fn run(context: &mut Context<State>, editor: &mut DefaultEditor) {
    loop {
        let array = match read_input(context, editor) {
            Some(array) => array,
            None => return,
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

fn read_input(context: &mut Context<State>, editor: &mut DefaultEditor) -> Option<NodeArray> {
    let mut prompt = ">>> ";
    let mut text = String::new();
    loop {
        let line = match editor.readline(prompt) {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) => return Some(NodeArray::new()),
            Err(ReadlineError::Eof) => return None,
            Err(ReadlineError::WindowResized) => todo!("window resized"),
            Err(error) => {
                eprintln!("Error while reading line: {error}");
                return Some(NodeArray::new());
            },
        };

        text.push_str(&line);

        let options = context.state.file_load_options();
        match context.load_text(options, &text) {
            Ok(array) => return Some(array),
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
                        prompt = "... ";
                        continue;
                    }
                }

                emit_load_error(&text, error);
                continue;
            },
        }
    }
}

fn emit_load_error(text: &str, error: LoadError) {
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
