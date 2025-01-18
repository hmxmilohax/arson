// SPDX-License-Identifier: LGPL-3.0-or-later

use std::path::PathBuf;

use anyhow::{bail, Context};
use arson_fmtlib::{Formatter, Options};
use arson_parse::reporting::files::SimpleFile;
use arson_parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson_parse::reporting::term::{self, Chars};
use clap::Parser;
use encoding_rs::{UTF_8, WINDOWS_1252};

/// A formatter for DTA files.
#[derive(clap::Parser, Debug)]
struct Arguments {
    /// The input file to be formatted.
    input_path: PathBuf,

    /// The file to output to.
    ///
    /// Defaults to modifying the input file.
    #[arg(long, short)]
    output_path: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();
    let file_bytes = std::fs::read(&args.input_path).context("failed to open input file")?;

    // Attempt to decode as UTF-8 first, it will more reliably result in a
    // decoding error if it's not the right encoding due to the format details.
    let (file_text, encoding) = match UTF_8.decode_without_bom_handling_and_without_replacement(&file_bytes) {
        Some(text) => (text, UTF_8),
        // Attempt Latin-1 next, specifically Windows-1252 because it has more
        // printable characters which are more likely intended in this context
        None => (WINDOWS_1252.decode(&file_bytes).0, WINDOWS_1252),
    };
    let file_text = file_text.into_owned();
    drop(file_bytes); // conserve memory

    let options = Options::default(); // TODO
    let output_text = match Formatter::new(&file_text, options).map(|f| f.to_string()) {
        Ok(text) => text,
        Err(error) => {
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = term::Config { chars: Chars::ascii(), ..Default::default() };

            let file = SimpleFile::new(args.input_path.to_string_lossy(), &file_text);
            for error in error.diagnostics {
                let _ = term::emit(&mut writer.lock(), &config, &file, &error.to_codespan(()));
            }

            bail!("failed to parse file")
        },
    };

    let output_path = args.output_path.unwrap_or_else(|| args.input_path.clone());
    let output_bytes = encoding.encode(&output_text).0;
    std::fs::write(&output_path, &output_bytes).context("failed to write output file")?;

    Ok(())
}
