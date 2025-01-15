// SPDX-License-Identifier: LGPL-3.0-or-later

use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

use anyhow::{bail, ensure, Context};
use arson_dtb::prelude::*;
use arson_parse::reporting::files::SimpleFile;
use arson_parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson_parse::reporting::term::{self, Chars};
use clap::Parser;

/// The Arson DTA<->DTB compiler/decompiler.
#[derive(clap::Parser, Debug)]
struct Arguments {
    /// The mode to run the compiler in.
    #[command(subcommand)]
    mode: CompilerMode,
}

/// The mode to run the compiler in.
#[derive(clap::Subcommand, Debug)]
enum CompilerMode {
    /// Compile a script file (.dta) into binary form (.dtb).
    Compile {
        /// The encryption method to use for the output file.
        #[arg(short = 'c', long)]
        encryption: EncryptionMode,
        /// The key to use for encryption.
        ///
        /// Defaults to a randomly-generated key.
        #[arg(short = 'k', long, value_parser = parse_key)]
        key: Option<u32>,
        /// The encoding to use for text in the output file.
        #[arg(short = 'e', long)]
        encoding: Encoding,
        /// The file to compile.
        input_path: PathBuf,
        /// The path to output the compiled file to.
        ///
        /// Defaults to the input path with the extension changed to .dtb.
        output_path: Option<PathBuf>,
    },
    /// Decompile a compiled script file (.dtb) into textual form (.dta).
    Decompile {
        /// The decryption method to use for the input file.
        ///
        /// Leave unspecified to automatically detect the encryption method.
        #[arg(short, long)]
        decryption: Option<EncryptionMode>,
        /// The file to decompile.
        input_path: PathBuf,
        /// The path to output the decompiled file to.
        ///
        /// Defaults to the input path with the extension changed to .dta.
        output_path: Option<PathBuf>,
    },
}

/// The encryption/decryption mode to use.
#[derive(clap::ValueEnum, Debug, Clone, Copy)]
enum EncryptionMode {
    /// No encryption.
    None,
    /// Old-style encryption (pre-GH2).
    Old,
    /// New-style encryption (GH2 onwards).
    New,
}

/// The encoding to use for .dtb files.
#[derive(clap::ValueEnum, Debug, Clone, Copy)]
enum Encoding {
    UTF8,
    Latin1,
}

fn parse_key(text: &str) -> anyhow::Result<u32> {
    if text.starts_with("0x") {
        let text = text.get(2..).context("hex input too small")?;
        Ok(u32::from_str_radix(text, 16)?)
    } else {
        Ok(text.parse()?)
    }
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();

    fn validate_paths(input: &Path, output: &Path, input_ext: &str, output_ext: &str) -> anyhow::Result<()> {
        fn check_extension(path: &Path, extension: &str) -> bool {
            path.extension().map_or(false, |ext| ext.eq_ignore_ascii_case(extension))
        }

        ensure!(
            check_extension(input, input_ext),
            "invalid {input_ext} input path {}",
            input.display()
        );
        ensure!(
            check_extension(output, output_ext),
            "invalid {output_ext} output path {}",
            output.display()
        );

        Ok(())
    }

    match args.mode {
        CompilerMode::Compile { encryption, key, encoding, input_path, output_path } => {
            let output_path = output_path.unwrap_or_else(|| input_path.with_extension("dtb"));
            validate_paths(&input_path, &output_path, "DTA", "DTB")?;
            compile(encryption, key, encoding, input_path, output_path)
        },
        CompilerMode::Decompile { decryption, input_path, output_path } => {
            let output_path = output_path.unwrap_or_else(|| input_path.with_extension("dta"));
            validate_paths(&input_path, &output_path, "DTB", "DTA")?;
            decompile(decryption, input_path, output_path)
        },
    }
}

fn compile(
    encryption: EncryptionMode,
    key: Option<u32>,
    encoding: Encoding,
    input_path: PathBuf,
    output_path: PathBuf,
) -> anyhow::Result<()> {
    let input_file = File::open(&input_path)
        .and_then(std::io::read_to_string)
        .context("couldn't read input file")?;

    let array = match DataArray::parse(&input_file) {
        Ok(ast) => ast,
        Err(error) => match error {
            arson_dtb::DataParseError::Parse(error) => {
                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = term::Config { chars: Chars::ascii(), ..Default::default() };

                let file = SimpleFile::new(input_path.to_string_lossy(), &input_file);
                for error in error.diagnostics {
                    _ = term::emit(&mut writer.lock(), &config, &file, &error.to_codespan(()));
                }

                std::process::abort();
            },
            _ => bail!("couldn't load input file: {error}"),
        },
    };

    let mut output_file = File::create_new(&output_path).context("couldn't create output file")?;
    let settings = WriteSettings {
        encoding: match encoding {
            Encoding::UTF8 => WriteEncoding::UTF8,
            Encoding::Latin1 => WriteEncoding::Latin1,
        },
    };
    let result = match encryption {
        EncryptionMode::None => arson_dtb::write_unencrypted(&array, &mut output_file, settings),
        EncryptionMode::Old => match key {
            Some(key) => arson_dtb::write_oldstyle_seeded(&array, &mut output_file, settings, key),
            None => arson_dtb::write_oldstyle(&array, &mut output_file, settings),
        },
        EncryptionMode::New => match key {
            Some(key) => arson_dtb::write_newstyle_seeded(&array, &mut output_file, settings, key as i32),
            None => arson_dtb::write_newstyle(&array, &mut output_file, settings),
        },
    };

    result.with_context(|| {
        _ = std::fs::remove_file(output_path);
        "couldn't write output file"
    })
}

fn decompile(
    decryption: Option<EncryptionMode>,
    input_path: PathBuf,
    output_path: PathBuf,
) -> anyhow::Result<()> {
    let array = File::open(input_path)
        .map_err(|e| arson_dtb::ReadError::IO(e))
        .and_then(|mut f| match decryption {
            Some(EncryptionMode::None) => arson_dtb::read_unencrypted(&mut f),
            Some(EncryptionMode::Old) => arson_dtb::read_oldstyle(&mut f).map(|(arr, _)| arr),
            Some(EncryptionMode::New) => arson_dtb::read_newstyle(&mut f).map(|(arr, _)| arr),
            None => arson_dtb::read(&mut f),
        })
        .expect("couldn't read input file");

    let tokens = array.to_tokens();

    let mut output_file = File::create_new(&output_path).expect("couldn't create output file");
    let result = tokens.iter().try_for_each(|token| write!(output_file, "{token} "));

    result.with_context(|| {
        _ = std::fs::remove_file(output_path);
        "couldn't write output file"
    })
}
