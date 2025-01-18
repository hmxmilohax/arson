// SPDX-License-Identifier: LGPL-3.0-or-later

use anyhow::{bail, Context};
use arson::fs::drivers::BasicFileSystemDriver;
use arson::parse::reporting::files::SimpleFile;
use arson::parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson::parse::reporting::term::{self, Chars};
use arson::parse::DiagnosticKind;
use arson::prelude::*;
use arson::stdlib::process::ExitError;
use clap::Parser;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

mod terminal;

/// The REPL for Arson's DTA implementation.
#[derive(Debug, clap::Parser)]
struct Arguments {
    /// skip configuration prompts and use default settings
    #[arg(long, short)]
    skip_prompt: bool,

    /// the directory to mount for scripting access
    #[arg(long, short)]
    mount_dir: Option<String>,

    /// the text editing mode to use
    #[arg(long, short)]
    editor_mode: Option<EditorModeArgument>,
}

#[derive(Debug, Clone, Copy, clap::ValueEnum)]
enum EditorModeArgument {
    Emacs,
    Vi,
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();

    let mut context = make_context(&args)?;
    let mut editor = make_editor(&args)?;

    run(&mut context, &mut editor)
}

fn make_context(args: &Arguments) -> anyhow::Result<arson::Context> {
    let mut context = arson::Context::new();
    context.register_state(StdlibOptions {
        file_load_options: LoadOptions { allow_include: true, allow_autorun: true },
    });
    arson::stdlib::register_funcs(&mut context);

    if let Some(ref directory) = args.mount_dir {
        let driver = BasicFileSystemDriver::new(directory).context("failed to create file driver")?;
        context = context.with_filesystem_driver(driver);
    } else if !args.skip_prompt {
        loop {
            let directory = terminal::prompt_str("Script mount directory? (leave empty to skip)");
            if directory.is_empty() {
                break;
            }

            let driver = match BasicFileSystemDriver::new(&directory) {
                Ok(driver) => driver,
                Err(error) => {
                    eprintln!("Could not create file driver: {error}");
                    continue;
                },
            };
            context = context.with_filesystem_driver(driver);

            println!("Note that all paths used in scripts will be relative to this directory.");
            break;
        }
    }

    Ok(context)
}

fn make_editor(args: &Arguments) -> anyhow::Result<DefaultEditor> {
    use rustyline::config::Configurer;
    use rustyline::{Config, EditMode};

    #[rustfmt::skip] // do not pack into the same line
    let config = Config::builder()
        .auto_add_history(true)
        .indent_size(3)
        .tab_stop(3)
        .build();

    let mut editor = DefaultEditor::with_config(config).context("failed to create text reader")?;

    if let Some(ref mode) = args.editor_mode {
        let mode = match mode {
            EditorModeArgument::Emacs => EditMode::Emacs,
            EditorModeArgument::Vi => EditMode::Vi,
        };
        editor.set_edit_mode(mode);
    } else if !args.skip_prompt {
        let mode =
            terminal::prompt_option("Text editor mode?", [("emacs", EditMode::Emacs), ("vi", EditMode::Vi)]);
        if let Some(mode) = mode {
            editor.set_edit_mode(mode);
        }
    }

    Ok(editor)
}

fn run(context: &mut arson::Context, editor: &mut DefaultEditor) -> anyhow::Result<()> {
    loop {
        let array = match read_input(context, editor) {
            Some(array) => array,
            None => return Ok(()),
        };

        for node in array {
            match node.evaluate(context) {
                Ok(evaluated) => println!("{evaluated}"),
                Err(error) => {
                    if let Some(message) = ExitError::is_exit(&error) {
                        match message {
                            Some(message) => bail!("exit error: {message}"),
                            None => return Ok(()),
                        }
                    }

                    eprintln!("Evaluation error: {error}\n{}", error.backtrace())
                },
            };
        }
    }
}

fn read_input(context: &mut arson::Context, editor: &mut DefaultEditor) -> Option<NodeArray> {
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

        let options = LoadOptions { allow_include: true, allow_autorun: true };
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
                return Some(NodeArray::new());
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
