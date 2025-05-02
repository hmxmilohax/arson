// SPDX-License-Identifier: LGPL-3.0-or-later

use std::path::{Path, PathBuf};

use anyhow::{bail, Context};
use arson_fmtlib::{expr, token, Formatter, Options};
use arson_parse::reporting::files::SimpleFile;
use arson_parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson_parse::reporting::term::{self, Chars};
use arson_parse::ParseError;
use clap::Parser;

/// A formatter for DTA files.
#[derive(clap::Parser, Debug)]
struct Arguments {
    /// The formatter mode to use.
    #[arg(short, long)]
    mode: Option<FormatMode>,

    /// The maximum width of arrays in the output.
    #[arg(short = 'a', long)]
    max_array_width: Option<usize>,

    /// Suppress parsing errors that occur as part of formatting the output file.
    #[arg(short, long)]
    suppress_errors: bool,

    /// The input file to be formatted.
    input_path: PathBuf,

    /// The file to output to.
    ///
    /// Defaults to modifying the input file.
    output_path: Option<PathBuf>,
}

/// The formatter mode to use.
#[derive(clap::ValueEnum, Debug, Clone, Copy)]
enum FormatMode {
    /// Richer expression-based formatting.
    /// Requires text to be fully parsable.
    Expression,
    /// Less capable token-based formatting.
    /// Formats text regardless of parsability.
    Token,
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();
    let file_bytes = std::fs::read(&args.input_path).context("failed to open input file")?;

    let (file_text, encoding) =
        arson_parse::encoding::decode_default(&file_bytes).context("failed to decode input file")?;
    let file_text = file_text.into_owned();
    drop(file_bytes); // conserve memory

    let mut options = Options::default();
    args.max_array_width.inspect(|value| options.max_array_width = *value);
    // args.max_line_width.inspect(|value| options.max_line_width = *value);

    let formatter = match args.mode {
        Some(FormatMode::Expression) => match expr::Formatter::new(&file_text, options) {
            Ok(formatter) => Formatter::Expression(formatter),
            Err(error) => {
                if !args.suppress_errors {
                    write_parse_errors(error, &args.input_path, &file_text);
                }
                bail!("failed to parse file")
            },
        },
        Some(FormatMode::Token) => Formatter::Token(token::Formatter::new(&file_text, options)),
        None => match Formatter::new(&file_text, options) {
            (formatter, None) => formatter,
            (formatter, Some(error)) => {
                if !args.suppress_errors {
                    write_parse_errors(error, &args.input_path, &file_text);
                }
                formatter
            },
        },
    };

    let output_text = formatter.to_string();
    let output_path = args.output_path.unwrap_or_else(|| args.input_path.clone());
    let output_bytes = arson_parse::encoding::encode(&output_text, encoding)?;
    std::fs::write(&output_path, &output_bytes).context("failed to write output file")?;

    Ok(())
}

fn write_parse_errors(error: ParseError, input_path: &Path, input_text: &str) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config { chars: Chars::ascii(), ..Default::default() };

    let file = SimpleFile::new(input_path.to_string_lossy(), input_text);
    for error in error.diagnostics {
        _ = term::emit(&mut writer.lock(), &config, &file, &error.to_codespan(()));
    }
}
