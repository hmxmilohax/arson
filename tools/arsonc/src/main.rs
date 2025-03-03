// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ffi::OsString;
use std::fs::File;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::{Path, PathBuf};

use anyhow::{bail, ensure, Context};
use arson_dtb::prelude::*;
use arson_dtb::ReadError;
use arson_parse::reporting::files::SimpleFile;
use arson_parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson_parse::reporting::term::{self, Chars};
use arson_parse::{ParseError, TokenValue};
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
    Compile(CompileArgs),
    /// Decompile a compiled script file (.dtb) into textual form (.dta).
    Decompile(DecompileArgs),
    /// Changes the encryption of a compiled script file (.dtb).
    CrossCrypt(CrossCryptArgs),
}

/// Compile a script file (.dta) into binary form (.dtb).
#[derive(clap::Args, Debug)]
struct CompileArgs {
    /// The encryption method to use for the output file.
    #[arg(short, long)]
    encryption: EncryptionMode,
    /// The key to use for encryption.
    ///
    /// Leave unspecified to use a default key.
    #[arg(short, long, value_parser = parse_key)]
    key: Option<u32>,

    /// The encoding to use for text in the output file.
    #[arg(short = 'c', long)]
    output_encoding: Encoding,
    /// Allow output path to overwrite an existing file.
    #[arg(short = 'o', long)]
    allow_overwrite: bool,

    /// The file to compile.
    input_path: PathBuf,
    /// The path to output the compiled file to.
    ///
    /// Defaults to the input path with the extension changed to .dtb.
    output_path: Option<PathBuf>,
}

/// Decompile a compiled script file (.dtb) into textual form (.dta).
#[derive(clap::Args, Debug)]
struct DecompileArgs {
    /// The decryption method to use for the input file.
    ///
    /// Leave unspecified to automatically detect the encryption method.
    #[arg(short, long)]
    decryption: Option<EncryptionMode>,

    /// Suppress parsing errors that occur as part of formatting the output file.
    #[arg(short, long)]
    print_format_errors: bool,

    /// Allow output path to overwrite an existing file.
    #[arg(short = 'o', long)]
    allow_overwrite: bool,

    /// The file to decompile.
    input_path: PathBuf,
    /// The path to output the decompiled file to.
    ///
    /// Defaults to the input path with the extension changed to .dta.
    output_path: Option<PathBuf>,
}

/// Changes the encryption of a compiled script file (.dtb).
#[derive(clap::Args, Debug)]
struct CrossCryptArgs {
    /// The decryption method to use for the input file.
    ///
    /// Leave unspecified to automatically detect the encryption method.
    #[arg(short, long)]
    decryption: Option<EncryptionMode>,
    /// The encryption method to use for the output file.
    #[arg(short, long)]
    encryption: EncryptionMode,
    /// The key to use for encryption.
    ///
    /// Leave unspecified to use a default key.
    #[arg(short, long, value_parser = parse_key)]
    key: Option<u32>,
    /// The file to encrypt.
    input_path: PathBuf,
    /// The path to output the encrypted file to.
    ///
    /// Defaults to appending `_encrypted` to the file name.
    output_path: Option<PathBuf>,
    /// Allow output path to overwrite an existing file.
    #[arg(short = 'o', long)]
    allow_overwrite: bool,
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
        u32::from_str_radix(text, 16).context("failed to parse key value")
    } else {
        text.parse().context("failed to parse key value")
    }
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();

    match args.mode {
        CompilerMode::Compile(args) => compile(args),
        CompilerMode::Decompile(args) => decompile(args),
        CompilerMode::CrossCrypt(args) => cross_crypt(args),
    }
}

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

fn compile(args: CompileArgs) -> anyhow::Result<()> {
    let output_path = args.output_path.unwrap_or_else(|| args.input_path.with_extension("dtb"));
    validate_paths(&args.input_path, &output_path, "DTA", "DTB")?;

    let input_file = File::open(&args.input_path)
        .and_then(std::io::read_to_string)
        .context("couldn't read input file")?;

    let array = match DataArray::parse(&input_file) {
        Ok(ast) => ast,
        Err(error) => match error {
            arson_dtb::DataParseError::Parse(error) => {
                write_parse_errors(error, &args.input_path, &input_file);
                bail!("failed to parse input file");
            },
            _ => bail!("couldn't load input file: {error}"),
        },
    };

    let mut output_file = create_file(&output_path, args.allow_overwrite)?;
    let settings = WriteSettings {
        encoding: match args.output_encoding {
            Encoding::UTF8 => WriteEncoding::UTF8,
            Encoding::Latin1 => WriteEncoding::Latin1,
        },
    };
    let result = match args.encryption {
        EncryptionMode::None => arson_dtb::write_unencrypted(&array, &mut output_file, settings),
        EncryptionMode::Old => match args.key {
            Some(key) => arson_dtb::write_oldstyle_seeded(&array, &mut output_file, settings, key),
            None => arson_dtb::write_oldstyle(&array, &mut output_file, settings),
        },
        EncryptionMode::New => match args.key {
            Some(key) => arson_dtb::write_newstyle_seeded(&array, &mut output_file, settings, key as i32),
            None => arson_dtb::write_newstyle(&array, &mut output_file, settings),
        },
    };

    result.with_context(|| {
        _ = std::fs::remove_file(output_path);
        "couldn't write output file"
    })
}

fn decompile(args: DecompileArgs) -> anyhow::Result<()> {
    let output_path = args.output_path.unwrap_or_else(|| args.input_path.with_extension("dta"));
    validate_paths(&args.input_path, &output_path, "DTB", "DTA")?;

    let mut file = File::open(&args.input_path)
        .map(BufReader::new)
        .context("couldn't open file")?;

    let result = match args.decryption {
        Some(EncryptionMode::None) => arson_dtb::read_unencrypted(&mut file),
        Some(EncryptionMode::Old) => arson_dtb::read_oldstyle(&mut file).map(|(arr, _)| arr),
        Some(EncryptionMode::New) => arson_dtb::read_newstyle(&mut file).map(|(arr, _)| arr),
        None => arson_dtb::read(&mut file),
    };
    let array = result.context("couldn't read input file")?;
    let tokens = array.to_tokens();

    let mut unformatted = String::with_capacity(tokens.len() * 5);
    for token in tokens {
        use std::fmt::Write;

        if let TokenValue::Error(error) = token {
            bail!("encountered error when tokenizing decompiled file: {error}");
        }

        write!(unformatted, "{token}").context("couldn't format token buffer")?;
        unformatted.push(' ');
    }

    let options = arson_fmtlib::Options::default();
    let formatter = match arson_fmtlib::Formatter::new(&unformatted, options) {
        (formatter, None) => formatter,
        (formatter, Some(error)) => {
            if args.print_format_errors {
                write_parse_errors(error, &args.input_path, &unformatted);
            }
            formatter
        },
    };

    let mut output_file = create_file(&output_path, args.allow_overwrite)?;
    write!(output_file, "{formatter}").with_context(|| {
        _ = std::fs::remove_file(output_path);
        "couldn't write output file"
    })
}

fn cross_crypt(args: CrossCryptArgs) -> anyhow::Result<()> {
    let output_path = args.output_path.unwrap_or_else(|| {
        let suffix = match args.encryption {
            EncryptionMode::None => "decrypted",
            _ => "encrypted",
        };

        let name = args
            .input_path
            .file_stem()
            .map(|name| {
                let mut name = name.to_os_string();
                name.push("_");
                name.push(suffix);
                name
            })
            .unwrap_or_else(|| OsString::from(suffix));

        args.input_path.with_file_name(name).with_extension("dtb")
    });
    validate_paths(&args.input_path, &output_path, "DTB", "DTB")?;

    let mut file = File::open(args.input_path)
        .map(BufReader::new)
        .context("couldn't open file")?;

    let result = match args.decryption {
        Some(EncryptionMode::None) => {
            let mut bytes = Vec::new();
            file.read_to_end(&mut bytes).map(|_| bytes).map_err(ReadError::IO)
        },
        Some(EncryptionMode::Old) => arson_dtb::decrypt_oldstyle(&mut file)
            .map(|(bytes, _)| bytes)
            .map_err(ReadError::IO),
        Some(EncryptionMode::New) => arson_dtb::decrypt_newstyle(&mut file)
            .map(|(bytes, _)| bytes)
            .map_err(ReadError::IO),
        None => arson_dtb::decrypt(&mut file),
    };
    let bytes = result.context("couldn't read input file")?;

    let mut output_file = create_file(&output_path, args.allow_overwrite)?;
    let result = match args.encryption {
        EncryptionMode::None => output_file.write_all(&bytes),
        EncryptionMode::Old => match args.key {
            Some(key) => arson_dtb::encrypt_oldstyle_seeded(&bytes, &mut output_file, key),
            None => arson_dtb::encrypt_oldstyle(&bytes, &mut output_file),
        },
        EncryptionMode::New => match args.key {
            Some(key) => arson_dtb::encrypt_newstyle_seeded(&bytes, &mut output_file, key as i32),
            None => arson_dtb::encrypt_newstyle(&bytes, &mut output_file),
        },
    };

    result.with_context(|| {
        _ = std::fs::remove_file(output_path);
        "couldn't write output file"
    })
}

fn create_file(path: &Path, allow_overwrite: bool) -> anyhow::Result<BufWriter<File>> {
    let file = match allow_overwrite {
        true => File::create(path),
        false => File::create_new(path),
    };
    file.map(BufWriter::new).context("couldn't create output file")
}

fn write_parse_errors(error: ParseError, input_path: &Path, input_text: &str) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config { chars: Chars::ascii(), ..Default::default() };

    let file = SimpleFile::new(input_path.to_string_lossy(), input_text);
    for error in error.diagnostics {
        _ = term::emit(&mut writer.lock(), &config, &file, &error.to_codespan(()));
    }
}
