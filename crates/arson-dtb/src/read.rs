// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Read, Seek};

use arson_parse::encoding::{DecodeError, DtaEncoding};
use byteorder::{LittleEndian, ReadBytesExt};

use crate::crypt::{CryptAlgorithm, CryptReader, NewRandom, NoopCrypt, OldRandom};
use crate::{DataArray, DataKind, DataNode, EncryptionMode, FormatVersion};

#[derive(Debug, Clone, Default)]
pub struct ReadSettings {
    pub format: Option<FormatVersion>,
    pub encoding: Option<DtaEncoding>,
    pub decryption: DecryptionSettings,
}

impl ReadSettings {
    pub fn with_format(mut self, format: Option<FormatVersion>) -> Self {
        self.format = format;
        self
    }

    pub fn with_encoding(mut self, encoding: Option<DtaEncoding>) -> Self {
        self.encoding = encoding;
        self
    }

    pub fn with_decryption_mode(mut self, mode: Option<EncryptionMode>) -> Self {
        self.decryption = self.decryption.with_mode(mode);
        self
    }

    pub fn with_decryption_key(mut self, key: Option<u32>) -> Self {
        self.decryption = self.decryption.with_key(key);
        self
    }
}

#[derive(Debug, Clone, Default)]
pub struct DecryptionSettings {
    pub mode: Option<EncryptionMode>,
    pub key: Option<u32>,
}

impl DecryptionSettings {
    pub fn with_mode(mut self, mode: Option<EncryptionMode>) -> Self {
        self.mode = mode;
        self
    }

    pub fn with_key(mut self, key: Option<u32>) -> Self {
        self.key = key;
        self
    }
}

#[derive(Debug)]
pub struct ReadValue<T> {
    pub value: T,
    pub format: FormatVersion,
    pub encryption: EncryptionMode,
    pub key: u32,
    pub encoding: Option<DtaEncoding>,
}

#[derive(Debug)]
pub struct DecryptValue<T> {
    pub value: T,
    pub encryption: EncryptionMode,
    pub key: u32,
}

#[derive(thiserror::Error, Debug)]
pub enum ReadError {
    #[error("position {0}: malformed data")]
    MalformedData(u64),

    #[error("position {0}: value {1} cannot be negative")]
    InvalidArrayInteger(u64, i32),

    #[error("position {0}: invalid node kind {1}")]
    InvalidKind(u64, u32),

    #[error("position {0}: symbol '{1}...' exceeds length safety limit")]
    SymbolTooLong(u64, String),

    #[error("position {0}: {1}")]
    DecodingFailed(u64, DecodeError),

    #[error(
        "position {position}: {expecting:?} was expecting {expected:?} argument, but got {actual:?} instead"
    )]
    IncorrectNodeArgument {
        position: u64,
        expecting: DataKind,
        expected: DataKind,
        actual: DataKind,
    },

    #[error("{}", format_probe_failure(.0))]
    ProbeFailure(Vec<ProbeError>),

    #[error(transparent)]
    IO(#[from] io::Error),
}

#[derive(thiserror::Error, Debug)]
#[error("{context}: {inner}")]
pub struct ProbeError {
    inner: ReadError,
    context: String,
}

fn format_probe_failure(errors: &Vec<ProbeError>) -> String {
    use std::fmt::Write;

    let mut text = String::from("failed to probe file for decoding details");
    for error in errors {
        write!(text, "\nwhile probing {}: {}", error.context, error.inner).unwrap();
    }

    text
}

struct Reader<'settings, Reader: io::Read + io::Seek, Crypt: CryptAlgorithm> {
    reader: CryptReader<Reader, Crypt>,
    settings: &'settings ReadSettings,

    probing_encoding: bool,
    probed_encoding: Option<DtaEncoding>,
}

impl<'s, R: io::Read + io::Seek, C: CryptAlgorithm> Reader<'s, R, C> {
    fn new(reader: CryptReader<R, C>, settings: &'s ReadSettings) -> Self {
        Self {
            reader,
            settings,

            probing_encoding: settings.encoding.is_none(),
            probed_encoding: settings.encoding,
        }
    }

    fn read_file(
        mut reader: CryptReader<R, C>,
        settings: &ReadSettings,
    ) -> Result<(DataArray, Option<DtaEncoding>), ReadError> {
        let exists = reader.read_u8()?;
        match exists {
            0 => Ok((DataArray::new(1, 1, 0), None)),
            1 => {
                let mut reader = Reader::new(reader, settings);
                let array = reader.read_array()?;
                Ok((array, reader.probed_encoding))
            },
            _ => Err(ReadError::MalformedData(0)),
        }
    }

    fn read_array(&mut self) -> Result<DataArray, ReadError> {
        macro_rules! read_size {
            ($self:ident, $read:ident) => {{
                let position = $self.reader.stream_position()?;
                let value = $self.reader.$read::<LittleEndian>()?;
                usize::try_from(value).map_err(|_| ReadError::InvalidArrayInteger(position, value as i32))?
            }};
        }

        let length;
        let line;
        // This field is referred to as 'mDeprecated' according to the decomp. Some extrapolation
        // regarding how scripts are structured in early HMX games leads to the conclusion that
        // this may be a file ID/index: Frequency has all of its script files pre-processed,
        // meaning all includes and merges are performed at compile time instead of load time. The
        // scripts include a list of files, which is what the file IDs refer to.
        let file_id;

        match self.settings.format {
            Some(FormatVersion::Milo) => {
                length = read_size!(self, read_i16);
                line = read_size!(self, read_i16);
                file_id = read_size!(self, read_i16);
            },
            Some(FormatVersion::Forge) => {
                file_id = read_size!(self, read_i32);
                length = read_size!(self, read_i32);
                line = read_size!(self, read_i16);
            },
            None => unreachable!("format is guaranteed to be set by prior code"),
        }

        let mut array = DataArray::with_capacity(line, 0, file_id, length);
        for _i in 0..length {
            // Some files have arrays which list a longer length than is actually contained in the file,
            // so an exception is made for an EOF condition encountered on a node boundary
            let node_start = self.reader.stream_position()?;

            let node = match self.read_node() {
                Ok(node) => node,
                Err(ReadError::IO(err)) => match err.kind() {
                    io::ErrorKind::UnexpectedEof if self.reader.stream_position()? == node_start => break,
                    _ => return Err(err.into()),
                },
                Err(err) => return Err(err),
            };
            array.push(node);
        }

        Ok(array)
    }

    fn read_node(&mut self) -> Result<DataNode, ReadError> {
        let kind = self.reader.read_u32::<LittleEndian>()?;
        let data = match kind {
            0 => DataNode::Integer(self.reader.read_i32::<LittleEndian>()?),
            1 => DataNode::Float(self.reader.read_f32::<LittleEndian>()?),
            2 => DataNode::Variable(self.read_string()?),
            3 => DataNode::Function(self.read_string()?),
            4 => DataNode::Object(self.read_string()?),
            5 => {
                let symbol = self.read_string()?;
                if symbol.len() > arson_parse::MAX_SYMBOL_LENGTH {
                    return Err(ReadError::SymbolTooLong(
                        self.reader.stream_position()?,
                        symbol.chars().take(arson_parse::MAX_SYMBOL_LENGTH).collect::<String>(),
                    ));
                }
                DataNode::Symbol(symbol)
            },
            6 => {
                _ = self.reader.read_i32::<LittleEndian>()?;
                DataNode::Unhandled
            },

            7 => DataNode::Ifdef(self.read_string()?),
            8 => {
                _ = self.reader.read_i32::<LittleEndian>()?;
                DataNode::Else
            },
            9 => {
                _ = self.reader.read_i32::<LittleEndian>()?;
                DataNode::Endif
            },

            16 => DataNode::Array(self.read_array()?),
            17 => DataNode::Command(self.read_array()?),
            18 => DataNode::String(self.read_string()?),
            19 => DataNode::Property(self.read_array()?),
            20 => DataNode::Glob(self.read_glob()?),

            32 => {
                let name = self.read_string()?;

                let position = self.reader.stream_position()?;
                let body = match self.read_node()? {
                    DataNode::Array(array) => array,
                    node => {
                        return Err(ReadError::IncorrectNodeArgument {
                            position,
                            expecting: DataKind::Define,
                            expected: DataKind::Array,
                            actual: node.get_kind(),
                        })
                    },
                };
                DataNode::Define(name, body)
            },
            33 => DataNode::Include(self.read_string()?),
            34 => DataNode::Merge(self.read_string()?),
            35 => DataNode::Ifndef(self.read_string()?),
            36 => {
                _ = self.reader.read_i32::<LittleEndian>()?;

                let position = self.reader.stream_position()?;
                let body = match self.read_node()? {
                    DataNode::Command(array) => array,
                    node => {
                        return Err(ReadError::IncorrectNodeArgument {
                            position,
                            expecting: DataKind::Autorun,
                            expected: DataKind::Command,
                            actual: node.get_kind(),
                        })
                    },
                };
                DataNode::Autorun(body)
            },
            37 => DataNode::Undefine(self.read_string()?),

            _ => {
                let position = self.reader.stream_position()? - 4;

                // A kind value greater than 255 is most likely improperly-decrypted data
                if kind > 0xFF {
                    return Err(ReadError::MalformedData(position));
                } else {
                    return Err(ReadError::InvalidKind(position, kind));
                }
            },
        };

        Ok(data)
    }

    fn read_glob(&mut self) -> Result<Vec<u8>, ReadError> {
        let length = self.reader.read_u32::<LittleEndian>()?;

        let mut bytes = vec![0u8; length as usize];
        self.reader.read_exact(&mut bytes)?;

        Ok(bytes)
    }

    fn read_string(&mut self) -> Result<String, ReadError> {
        let position = self.reader.stream_position()?;

        let bytes = self.read_glob()?;
        let decoded = match arson_parse::encoding::decode(&bytes, self.settings.encoding) {
            Ok((decoded, encoding)) => {
                // Check encoding
                if self.probing_encoding {
                    match self.probed_encoding {
                        Some(probed_encoding) => {
                            if probed_encoding != encoding {
                                self.probed_encoding = None;
                                self.probing_encoding = false;
                            }
                        },
                        None => self.probed_encoding = Some(encoding),
                    }
                }

                decoded
            },
            Err(error) => {
                return Err(ReadError::DecodingFailed(position, error));
            },
        };
        Ok(decoded.into_owned())
    }
}

macro_rules! match_encryption {
    ($reader:ident, $settings:ident, $decryption:expr, $func:path) => {
        match $decryption.mode {
            Some(EncryptionMode::New) => {
                // Note: the key is always present in the file when encrypted and must always be read,
                // unwrap_or instead of unwrap_or_else is deliberate here
                let key = $decryption.key.unwrap_or($reader.read_u32::<LittleEndian>()?);
                let reader = CryptReader::new($reader, NewRandom::new(key));
                ($func(reader, $settings)?, key)
            },
            Some(EncryptionMode::Old) => {
                // Same note as above
                let key = $decryption.key.unwrap_or($reader.read_u32::<LittleEndian>()?);
                let reader = CryptReader::new($reader, OldRandom::new(key));
                ($func(reader, $settings)?, key)
            },
            Some(EncryptionMode::None) | None => {
                let reader = CryptReader::new($reader, NoopCrypt);
                ($func(reader, $settings)?, 0)
            },
        }
    };
}

pub fn read(
    mut reader: impl io::Seek + io::Read,
    settings: &ReadSettings,
) -> Result<ReadValue<DataArray>, ReadError> {
    let position = reader.stream_position()?;

    match settings.format {
        Some(format) => {
            let (array, encryption, key, encoding) = probe_encryption(&mut reader, settings)?;
            Ok(ReadValue { value: array, format, encryption, key, encoding })
        },
        None => {
            let mut errors = Vec::new();

            for format in [/*FormatVersion::Rnd,*/ FormatVersion::Milo, FormatVersion::Forge] {
                reader.seek(io::SeekFrom::Start(position))?;

                let settings = ReadSettings { format: Some(format), ..settings.clone() };
                match probe_encryption(&mut reader, &settings) {
                    Ok((array, encryption, key, encoding)) => {
                        return Ok(ReadValue { value: array, format, encryption, key, encoding });
                    },
                    Err(error) => match error {
                        ReadError::ProbeFailure(inner) => {
                            for error in inner {
                                errors.push(ProbeError {
                                    context: format!("format type {format:?}, {}", error.context),
                                    ..error
                                })
                            }
                        },
                        _ => errors.push(ProbeError {
                            inner: error,
                            context: format!("format type {format:?}"),
                        }),
                    },
                }
            }

            Err(ReadError::ProbeFailure(errors))
        },
    }
}

fn probe_encryption(
    mut reader: impl io::Seek + io::Read,
    settings: &ReadSettings,
) -> Result<(DataArray, EncryptionMode, u32, Option<DtaEncoding>), ReadError> {
    match settings.decryption.mode {
        Some(encryption) => {
            let (array, key, encoding) = read_impl(&mut reader, settings)?;
            Ok((array, encryption, key, encoding))
        },
        None => {
            let position = reader.stream_position()?;
            let mut errors = Vec::new();

            for encryption in [EncryptionMode::New, EncryptionMode::Old, EncryptionMode::None] {
                reader.seek(io::SeekFrom::Start(position))?;

                let settings = ReadSettings {
                    decryption: DecryptionSettings { mode: Some(encryption), ..settings.decryption },
                    ..settings.clone()
                };
                match read_impl(&mut reader, &settings) {
                    Ok((array, key, encoding)) => {
                        return Ok((array, encryption, key, encoding));
                    },
                    Err(error) => match error {
                        ReadError::ProbeFailure(mut inner) => errors.append(&mut inner),
                        _ => errors.push(ProbeError {
                            inner: error,
                            context: format!("encryption type {encryption:?}"),
                        }),
                    },
                };
            }

            Err(ReadError::ProbeFailure(errors))
        },
    }
}

fn read_impl(
    mut reader: impl io::Seek + io::Read,
    settings: &ReadSettings,
) -> Result<(DataArray, u32, Option<DtaEncoding>), ReadError> {
    let ((array, encoding), key) =
        match_encryption!(reader, settings, settings.decryption, Reader::read_file);
    Ok((array, key, encoding))
}

pub fn decrypt(
    mut reader: impl io::Read + io::Seek,
    settings: &DecryptionSettings,
) -> Result<DecryptValue<Vec<u8>>, ReadError> {
    let position = reader.stream_position()?;

    // Combination sanity check for specified decryption,
    // and probing for unspecified decryption
    let read_settings = ReadSettings { decryption: settings.clone(), ..Default::default() };
    let value = read(&mut reader, &read_settings)?;
    let encryption = settings.mode.unwrap_or(value.encryption);

    reader.seek(io::SeekFrom::Start(position))?;

    let (bytes, key) = match_encryption!(reader, settings, settings, decrypt_impl);
    Ok(DecryptValue { value: bytes, encryption, key })
}

fn decrypt_impl<R: io::Read, C: CryptAlgorithm>(
    mut reader: CryptReader<R, C>,
    _settings: &DecryptionSettings,
) -> io::Result<Vec<u8>> {
    let mut bytes = Vec::new();
    reader.read_to_end(&mut bytes)?;
    Ok(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_node_bytes(bytes: &[u8]) -> DataNode {
        let reader = CryptReader::new(io::Cursor::new(bytes), NoopCrypt);
        let mut settings = ReadSettings {
            format: Some(FormatVersion::Milo),
            ..Default::default()
        };

        let mut reader = Reader::new(reader, &mut settings);
        let node = reader.read_node().expect("failed to read node bytes");

        assert_eq!(
            reader.reader.stream_position().unwrap() as usize,
            bytes.len(),
            "not all node bytes were read"
        );

        node
    }

    fn assert_node_bytes(bytes: &[u8], expected: DataNode) {
        assert_eq!(read_node_bytes(&bytes), expected);
    }

    #[test]
    fn values() {
        assert_node_bytes(&[0, 0, 0, 0, 10, 0, 0, 0], DataNode::Integer(10));
        assert_node_bytes(&[1, 0, 0, 0, 0x00, 0x00, 0x80, 0x3F], DataNode::Float(1.0));
        assert_node_bytes(
            &[2, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Variable("foo".to_owned()),
        );
        assert_node_bytes(
            &[3, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Function("foo".to_owned()),
        );
        assert_node_bytes(
            &[4, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Object("foo".to_owned()),
        );
        assert_node_bytes(
            &[5, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Symbol("foo".to_owned()),
        );
        assert_node_bytes(&[6, 0, 0, 0, 0xCC, 0xCC, 0xCC, 0xCC], DataNode::Unhandled);

        assert_node_bytes(
            &[7, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Ifdef("foo".to_owned()),
        );
        assert_node_bytes(&[8, 0, 0, 0, 0xCC, 0xCC, 0xCC, 0xCC], DataNode::Else);
        assert_node_bytes(&[9, 0, 0, 0, 0xCC, 0xCC, 0xCC, 0xCC], DataNode::Endif);

        #[rustfmt::skip]
        const fn array_bytes<const KIND: u8, const LINE: u8, const FILE_ID: u8>() -> &'static [u8] {
            &[
                KIND, 0, 0, 0,
                3, 0, // size
                LINE, 0, // line
                FILE_ID, 0, // file id
                0, 0, 0, 0, 10, 0, 0, 0, // DataNode::Integer(10)
                1, 0, 0, 0, 0x00, 0x00, 0x80, 0x3F, // DataNode::Float(1.0)
                5, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o', // DataNode::Symbol("foo")
            ]
        }

        assert_node_bytes(
            array_bytes::<16, 13, 0>(),
            DataNode::Array(DataArray::from_nodes(13, 0, 0, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
        );
        assert_node_bytes(
            array_bytes::<17, 14, 1>(),
            DataNode::Command(DataArray::from_nodes(14, 0, 1, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
        );
        assert_node_bytes(
            &[18, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::String("foo".to_owned()),
        );
        assert_node_bytes(
            array_bytes::<19, 16, 2>(),
            DataNode::Property(DataArray::from_nodes(16, 0, 2, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
        );
        assert_node_bytes(
            &[20, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Glob(vec![b'f', b'o', b'o']),
        );

        let mut bytes = [32, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'].to_vec();
        bytes.extend_from_slice(array_bytes::<16, 19, 3>());
        assert_node_bytes(
            &bytes,
            DataNode::Define(
                "foo".to_owned(),
                DataArray::from_nodes(19, 0, 3, vec![
                    DataNode::Integer(10),
                    DataNode::Float(1.0),
                    DataNode::Symbol("foo".to_owned()),
                ]),
            ),
        );
        assert_node_bytes(
            &[33, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Include("foo".to_owned()),
        );
        assert_node_bytes(
            &[34, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Merge("foo".to_owned()),
        );
        assert_node_bytes(
            &[35, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Ifndef("foo".to_owned()),
        );
        let mut bytes = [36, 0, 0, 0, 0xCC, 0xCC, 0xCC, 0xCC].to_vec();
        bytes.extend_from_slice(array_bytes::<17, 23, 4>());
        assert_node_bytes(
            &bytes,
            DataNode::Autorun(DataArray::from_nodes(23, 0, 4, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
        );
        assert_node_bytes(
            &[37, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'],
            DataNode::Undefine("foo".to_owned()),
        );
    }
}
