// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ffi::OsString;
use std::fs::File;
use std::io::{BufReader, BufWriter, Cursor, Write};
use std::path::{Path, PathBuf};

use anyhow::{bail, Context};
use arson_dtb::prelude::*;
use arson_dtb::{EncryptionMode, TokenizeOptions};
use arson_parse::encoding::DtaEncoding;
use arson_parse::reporting::files::SimpleFile;
use arson_parse::reporting::term::termcolor::{ColorChoice, StandardStream};
use arson_parse::reporting::term::{self, Chars};
use arson_parse::{ParseError, TokenValue};
use clap::Parser;

/// The Arson DTA<->DTB compiler/decompiler.
///
/// Based on dtab and other earlier work by Onyxite, xorloser, Deimos, and Maxton.
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

    /// (dtab compatibility) Compile a script file (.dta) into binary form (.dtb) (v1 format).
    #[command(name = "-b")]
    DtabCompileMilo(DtabArgumentsNoKey),
    /// (dtab compatibility) Decompile a compiled script file (.dtb) into textual form (.dta) (v1 format).
    #[command(name = "-a")]
    DtabDecompileMilo(DtabArgumentsNoKey),
    /// (dtab compatibility) Decompile a compiled script file (.dtb) into textual form (.dta) (v2 format).
    #[command(name = "-A")]
    DtabDecompileForge(DtabArgumentsNoKey),

    /// (dtab compatibility) Decrypt a binary script file (.dtb) using new-style encryption.
    #[command(name = "-d")]
    DtabDecryptNewstyle(DtabArgumentsNoKey),
    /// (dtab compatibility) Decrypt a binary script file (.dtb) using old-style encryption.
    #[command(name = "-D")]
    DtabDecryptOldstyle(DtabArgumentsNoKey),
    /// (dtab compatibility) Encrypt a binary script file (.dtb) using new-style encryption.
    #[command(name = "-e")]
    DtabEncryptNewstyle(DtabArgumentsKey),
    /// (dtab compatibility) Encrypt a binary script file (.dtb) using old-style encryption.
    #[command(name = "-E")]
    DtabEncryptOldstyle(DtabArgumentsKey),
}

/// Compile a script file (.dta) into binary form (.dtb).
#[derive(clap::Args, Debug)]
struct CompileArgs {
    /// The encoding to use for text in the input file.
    ///
    /// Leave empty to automatically detect the encoding.
    #[arg(long)]
    input_encoding: Option<EncodingArg>,

    /// The format version to use for the output file.
    #[arg(short = 'f', long)]
    output_format: FormatVersionArg,
    /// The encoding to use for text in the output file.
    ///
    /// Leave empty to match the encoding of the input file.
    #[arg(long)]
    output_encoding: Option<EncodingArg>,
    /// The encryption method to use for the output file.
    ///
    /// Leave unspecified for no encryption.
    #[arg(short = 'e', long)]
    output_encryption: EncryptionModeArg,
    /// The key to use for encryption.
    ///
    /// Leave unspecified to use a default key.
    #[arg(short = 'k', long, value_parser = parse_key)]
    output_key: Option<u32>,
    /// Use time-based entropy to scramble the encryption seed.
    #[arg(long)]
    output_entropy: bool,

    /// Allow output path to overwrite an existing file.
    #[arg(short = 'o', long)]
    allow_overwrite: bool,

    /// The file to compile.
    ///
    /// Use `-` to read from stdin.
    input_path: PathBuf,
    /// The path to output the compiled file to.
    ///
    /// Defaults to the input path with the extension changed to .dtb.
    /// Use `-` to write to stdout.
    output_path: Option<PathBuf>,
}

/// Decompile a compiled script file (.dtb) into textual form (.dta).
#[derive(clap::Args, Debug)]
struct DecompileArgs {
    /// The format version to use for the input file.
    ///
    /// Leave unspecified to automatically detect.
    #[arg(short = 'f', long)]
    input_format: Option<FormatVersionArg>,
    /// The encoding to use for text in the input file.
    ///
    /// Leave empty to automatically detect the encoding.
    #[arg(long)]
    input_encoding: Option<EncodingArg>,
    /// The decryption method to use for the input file.
    ///
    /// Leave unspecified to automatically detect, or to signify that the file is not encrypted.
    #[arg(short = 'd', long, default_value = "unknown")]
    input_decryption: DecryptionModeArg,
    /// The key to use for decryption.
    ///
    /// Leave unspecified to determine from the file.
    #[arg(short = 'k', long, value_parser = parse_key)]
    input_key: Option<u32>,

    /// The encoding to use for text in the output file.
    #[arg(long)]
    output_encoding: EncodingArg,
    /// Output file ID information to the file.
    #[arg(long)]
    output_file_ids: bool,
    /// Output line number information to the file.
    #[arg(long)]
    output_line_numbers: bool,

    /// Skip file extension checks/errors.
    #[arg(long)]
    ignore_extension: bool,
    /// Output parsing errors that occur as part of formatting the output file.
    #[arg(long)]
    suppress_format_errors: bool,
    /// Allow output path to overwrite an existing file.
    #[arg(short = 'o', long)]
    allow_overwrite: bool,

    /// The file to decompile.
    ///
    /// Use `-` to read from stdin.
    input_path: PathBuf,
    /// The path to output the decompiled file to.
    ///
    /// Defaults to the input path with the extension changed to .dta.
    /// Use `-` to write to stdout.
    output_path: Option<PathBuf>,
}

/// Changes the encryption of a compiled script file (.dtb).
#[derive(clap::Args, Debug)]
struct CrossCryptArgs {
    /// The decryption method to use for the input file.
    ///
    /// Leave unspecified to automatically detect, or to signify that the file is not encrypted.
    #[arg(long, default_value = "unknown")]
    input_decryption: DecryptionModeArg,
    /// The key to use for decrypting the input file.
    ///
    /// Leave unspecified to determine from the file.
    #[arg(long, value_parser = parse_key)]
    input_key: Option<u32>,

    /// The encryption method to use for the output file.
    ///
    /// Leave unspecified for no encryption.
    #[arg(long)]
    output_encryption: EncryptionModeArg,
    /// The key to use for encrypting the output file.
    ///
    /// Leave unspecified to use a default key.
    #[arg(long, value_parser = parse_key)]
    output_key: Option<u32>,
    /// Use time-based entropy to scramble the encryption seed.
    #[arg(long)]
    output_entropy: bool,

    /// Skip file extension checks/errors.
    #[arg(long)]
    ignore_extension: bool,
    /// Allow output path to overwrite an existing file.
    #[arg(short = 'o', long)]
    allow_overwrite: bool,

    /// The file to encrypt.
    ///
    /// Use `-` to read from stdin.
    input_path: PathBuf,
    /// The path to output the encrypted file to.
    ///
    /// Defaults to appending `_encrypted` to the file name.
    /// Use `-` to write to stdout.
    output_path: Option<PathBuf>,
}

/// Arguments for dtab commands.
#[derive(clap::Args, Debug)]
struct DtabArgumentsNoKey {
    /// The path to read from.
    input_path: PathBuf,
    /// The path to output to.
    output_path: PathBuf,
}

/// Arguments for dtab commands.
#[derive(clap::Args, Debug)]
struct DtabArgumentsKey {
    /// The path to read from.
    ///
    /// Use `-` to read from stdin.
    input_path: PathBuf,
    /// The path to output to.
    ///
    /// Use `-` to write to stdout.
    output_path: PathBuf,
    /// The key to use for encryption.
    ///
    /// Leave unspecified to determine from the file.
    #[arg(long, value_parser = parse_key)]
    output_key: Option<u32>,
}

/// The format version to use for .dtb files.
#[derive(clap::ValueEnum, Debug, Clone, Copy)]
enum FormatVersionArg {
    // TODO
    // /// The format used for Rnd-era games (Amplitude and earlier).
    // Rnd,
    /// The format used for Milo (Rock Band 3 and earlier).
    Milo,
    /// The format used for Forge (Fantasia and later).
    Forge,
}

impl FormatVersionArg {
    fn to_arson(self) -> FormatVersion {
        match self {
            Self::Milo => FormatVersion::Milo,
            Self::Forge => FormatVersion::Forge,
        }
    }
}

/// The encoding to use for text.
#[derive(clap::ValueEnum, Debug, Clone, Copy)]
enum EncodingArg {
    /// UTF-8.
    UTF8,
    /// Latin-1 (Windows-1252).
    Latin1,
}

impl EncodingArg {
    fn to_arson(self) -> DtaEncoding {
        match self {
            Self::UTF8 => DtaEncoding::Utf8,
            Self::Latin1 => DtaEncoding::Latin1,
        }
    }
}

/// The decryption to use.
#[derive(clap::ValueEnum, Debug, Default, Clone, Copy)]
enum DecryptionModeArg {
    /// Guess the encryption (default).
    #[default]
    Unknown,
    /// No encryption.
    None,
    /// Old-style encryption (pre-GH2).
    Old,
    /// New-style encryption (GH2 onwards).
    New,
}

impl DecryptionModeArg {
    fn to_arson(self) -> Option<EncryptionMode> {
        match self {
            Self::Unknown => None,
            Self::None => Some(EncryptionMode::None),
            Self::Old => Some(EncryptionMode::Old),
            Self::New => Some(EncryptionMode::New),
        }
    }
}

/// The encryption to use.
#[derive(clap::ValueEnum, Debug, Default, Clone, Copy)]
enum EncryptionModeArg {
    /// No encryption.
    #[default]
    None,
    /// Old-style encryption (pre-GH2).
    Old,
    /// New-style encryption (GH2 onwards).
    New,
}

impl EncryptionModeArg {
    fn to_arson(self) -> EncryptionMode {
        match self {
            Self::None => EncryptionMode::None,
            Self::Old => EncryptionMode::Old,
            Self::New => EncryptionMode::New,
        }
    }
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
        CompilerMode::DtabCompileMilo(args) => compile(CompileArgs {
            input_encoding: None,
            output_format: FormatVersionArg::Milo,
            output_encoding: None,
            output_encryption: EncryptionModeArg::None,
            output_key: None,
            output_entropy: false,
            allow_overwrite: true,
            input_path: args.input_path,
            output_path: Some(args.output_path),
        }),
        CompilerMode::DtabDecompileMilo(args) => decompile(DecompileArgs {
            input_format: Some(FormatVersionArg::Milo),
            input_encoding: None,
            input_decryption: DecryptionModeArg::None,
            input_key: None,
            output_encoding: EncodingArg::UTF8,
            output_file_ids: false,
            output_line_numbers: false,
            ignore_extension: true,
            suppress_format_errors: true,
            allow_overwrite: true,
            input_path: args.input_path,
            output_path: Some(args.output_path),
        }),
        CompilerMode::DtabDecompileForge(args) => decompile(DecompileArgs {
            input_format: Some(FormatVersionArg::Forge),
            input_encoding: None,
            input_decryption: DecryptionModeArg::None,
            input_key: None,
            output_encoding: EncodingArg::UTF8,
            output_file_ids: false,
            output_line_numbers: false,
            ignore_extension: true,
            suppress_format_errors: true,
            allow_overwrite: true,
            input_path: args.input_path,
            output_path: Some(args.output_path),
        }),
        CompilerMode::DtabDecryptNewstyle(args) => cross_crypt(CrossCryptArgs {
            input_decryption: DecryptionModeArg::New,
            input_key: None,
            output_encryption: EncryptionModeArg::None,
            output_key: None,
            output_entropy: false,
            ignore_extension: true,
            allow_overwrite: true,
            input_path: args.input_path,
            output_path: Some(args.output_path),
        }),
        CompilerMode::DtabDecryptOldstyle(args) => cross_crypt(CrossCryptArgs {
            input_decryption: DecryptionModeArg::Old,
            input_key: None,
            output_encryption: EncryptionModeArg::None,
            output_key: None,
            output_entropy: false,
            ignore_extension: true,
            allow_overwrite: true,
            input_path: args.input_path,
            output_path: Some(args.output_path),
        }),
        CompilerMode::DtabEncryptNewstyle(args) => cross_crypt(CrossCryptArgs {
            input_decryption: DecryptionModeArg::None,
            input_key: None,
            output_encryption: EncryptionModeArg::New,
            output_key: args.output_key,
            output_entropy: false,
            ignore_extension: true,
            allow_overwrite: true,
            input_path: args.input_path,
            output_path: Some(args.output_path),
        }),
        CompilerMode::DtabEncryptOldstyle(args) => cross_crypt(CrossCryptArgs {
            input_decryption: DecryptionModeArg::None,
            input_key: None,
            output_encryption: EncryptionModeArg::Old,
            output_key: args.output_key,
            output_entropy: false,
            ignore_extension: true,
            allow_overwrite: true,
            input_path: args.input_path,
            output_path: Some(args.output_path),
        }),
    }
}

fn compile(args: CompileArgs) -> anyhow::Result<()> {
    let output_path = args.output_path.unwrap_or_else(|| args.input_path.with_extension("dtb"));

    let input_bytes = read_file(&args.input_path)?;
    let (input_file, input_encoding) =
        arson_parse::encoding::decode(&input_bytes, args.input_encoding.map(EncodingArg::to_arson))
            .context("couldn't decode input .dta file text")?;

    let array = match DataArray::parse(&input_file) {
        Ok(ast) => ast,
        Err(error) => match error {
            arson_dtb::DataParseError::Parse(error) => {
                write_parse_errors(error, &args.input_path, &input_file);
                bail!("couldn't parse input .dta file");
            },
            _ => bail!("couldn't load input .dta file: {error}"),
        },
    };

    let mut output_bytes = Cursor::new(Vec::new());
    let settings = WriteSettings {
        format: args.output_format.to_arson(),
        encoding: args.output_encoding.map(EncodingArg::to_arson).unwrap_or(input_encoding),
        encryption: EncryptionSettings {
            mode: args.output_encryption.to_arson(),
            key: args.output_key,
            time_entropy: args.output_entropy,
        },
    };
    arson_dtb::write(&array, &mut output_bytes, &settings)
        .context("couldn't write output .dtb file bytes")?;

    write_file(&output_path, &output_bytes.into_inner(), args.allow_overwrite)
}

fn decompile(args: DecompileArgs) -> anyhow::Result<()> {
    let output_path = args.output_path.unwrap_or_else(|| args.input_path.with_extension("dta"));

    let input_bytes = Cursor::new(read_file(&args.input_path)?);

    let settings = ReadSettings {
        format: args.input_format.map(FormatVersionArg::to_arson),
        encoding: args.input_encoding.map(EncodingArg::to_arson),
        decryption: DecryptionSettings {
            mode: args.input_decryption.to_arson(),
            key: args.input_key,
        },
    };
    let result = arson_dtb::read(input_bytes, &settings).context("couldn't parse input .dtb file")?;
    let tokens = result.value.to_tokens(TokenizeOptions {
        file_ids: args.output_file_ids,
        line_numbers: args.output_line_numbers,
    });

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
    let output_text = match arson_fmtlib::Formatter::new(&unformatted, options) {
        (formatter, None) => formatter.to_string(),
        (formatter, Some(error)) => {
            if !args.suppress_format_errors {
                write_parse_errors(error, &args.input_path, &unformatted);
            }
            formatter.to_string()
        },
    };
    let output_encoded = arson_parse::encoding::encode(&output_text, args.output_encoding.to_arson())
        .context("couldn't encode output .dta file text")?;

    write_file(&output_path, &output_encoded, args.allow_overwrite)
}

fn cross_crypt(args: CrossCryptArgs) -> anyhow::Result<()> {
    let output_path = args.output_path.unwrap_or_else(|| {
        let suffix = match args.output_encryption {
            EncryptionModeArg::None => "decrypted",
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

    let input_bytes = Cursor::new(read_file(&args.input_path)?);

    let settings = DecryptionSettings {
        mode: args.input_decryption.to_arson(),
        key: args.input_key,
    };
    let result = arson_dtb::decrypt(input_bytes, &settings).context("couldn't decrypt input .dtb file")?;

    let mut output_bytes = Cursor::new(Vec::new());
    let settings = EncryptionSettings {
        mode: args.output_encryption.to_arson(),
        key: args.output_key,
        time_entropy: args.output_entropy,
    };
    arson_dtb::encrypt(&result.value, &mut output_bytes, settings)
        .context("couldn't encrypt output .dtb file")?;

    write_file(&output_path, &output_bytes.into_inner(), args.allow_overwrite)
}

fn read_file(path: &Path) -> anyhow::Result<Vec<u8>> {
    fn read_to_end(mut reader: impl std::io::Read) -> anyhow::Result<Vec<u8>> {
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).context("couldn't read input file")?;
        Ok(bytes)
    }

    if path == Path::new("-") {
        read_to_end(std::io::stdin())
    } else {
        let file = File::open(path).map(BufReader::new).context("couldn't open input file")?;
        read_to_end(file)
    }
}

fn write_file(path: &Path, bytes: &[u8], allow_overwrite: bool) -> anyhow::Result<()> {
    if path == Path::new("-") {
        std::io::stdout().write_all(bytes).context("couldn't write output file")?;
    } else {
        let file = match allow_overwrite {
            true => File::create(path),
            false => File::create_new(path),
        };
        let mut file = file.map(BufWriter::new).context("couldn't create output file")?;
        file.write_all(bytes).with_context(|| {
            _ = std::fs::remove_file(path);
            "couldn't write output file"
        })?;
    }

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
