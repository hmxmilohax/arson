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

    #[error(transparent)]
    IO(#[from] io::Error),
}

struct Reader<'settings, Reader: io::Read + io::Seek, Crypt: CryptAlgorithm> {
    reader: CryptReader<Reader, Crypt>,
    settings: &'settings mut ReadSettings,

    probing_encoding: bool,
}

impl<'s, R: io::Read + io::Seek, C: CryptAlgorithm> Reader<'s, R, C> {
    fn new(reader: CryptReader<R, C>, settings: &'s mut ReadSettings) -> Self {
        let probing_encoding = settings.encoding.is_none();
        Self { reader, settings, probing_encoding }
    }

    fn read_file(mut reader: CryptReader<R, C>, settings: &mut ReadSettings) -> Result<DataArray, ReadError> {
        let exists = reader.read_u8()?;
        match exists {
            0 => Ok(DataArray::new(1, 0)),
            1 => {
                let mut reader = Reader::new(reader, settings);
                reader.read_array()
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
        let id;

        match self.settings.format {
            Some(FormatVersion::Milo) => {
                length = read_size!(self, read_i16);
                line = read_size!(self, read_i16);
                id = read_size!(self, read_i16);
            },
            Some(FormatVersion::Forge) => {
                id = read_size!(self, read_i32);
                length = read_size!(self, read_i32);
                line = read_size!(self, read_i16);
            },
            None => unreachable!("format is guaranteed to be set by prior code"),
        }

        let mut array = DataArray::with_capacity(line, id, length);
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
                if self.probing_encoding {
                    match self.settings.encoding {
                        Some(probed_encoding) => {
                            if probed_encoding != encoding {
                                self.settings.encoding = None;
                                self.probing_encoding = false;
                            }
                        },
                        None => self.settings.encoding = Some(encoding),
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
                $decryption.key = Some(key);

                let reader = CryptReader::new($reader, NewRandom::new(key));
                $func(reader, $settings)
            },
            Some(EncryptionMode::Old) => {
                // Same note as above
                let key = $decryption.key.unwrap_or($reader.read_u32::<LittleEndian>()?);
                $decryption.key = Some(key);

                let reader = CryptReader::new($reader, OldRandom::new(key));
                $func(reader, $settings)
            },
            Some(EncryptionMode::None) | None => {
                let reader = CryptReader::new($reader, NoopCrypt);
                $func(reader, $settings)
            },
        }
    };
}

pub fn read(
    mut reader: impl io::Seek + io::Read,
    settings: &mut ReadSettings,
) -> Result<DataArray, ReadError> {
    let position = reader.stream_position()?;

    match settings.format {
        Some(_) => probe_encryption(&mut reader, settings),
        None => {
            for format in [/*FormatVersion::Rnd,*/ FormatVersion::Milo, FormatVersion::Forge] {
                reader.seek(io::SeekFrom::Start(position))?;

                settings.format = Some(format);
                if let Ok(array) = probe_encryption(&mut reader, settings) {
                    return Ok(array);
                }
            }

            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "input data could not be decoded through any known format",
            )
            .into())
        },
    }
}

fn probe_encryption(
    mut reader: impl io::Seek + io::Read,
    settings: &mut ReadSettings,
) -> Result<DataArray, ReadError> {
    match settings.decryption.mode {
        Some(_) => read_impl(&mut reader, settings),
        None => {
            let position = reader.stream_position()?;

            for mode in [Some(EncryptionMode::New), Some(EncryptionMode::Old), None] {
                reader.seek(io::SeekFrom::Start(position))?;

                settings.decryption.mode = mode;
                if let Ok(array) = read_impl(&mut reader, settings) {
                    return Ok(array);
                };
            }

            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "input data could not be decoded through any known encryption",
            )
            .into())
        },
    }
}

fn read_impl(
    mut reader: impl io::Seek + io::Read,
    settings: &mut ReadSettings,
) -> Result<DataArray, ReadError> {
    match_encryption!(reader, settings, settings.decryption, Reader::read_file)
}

pub fn decrypt(
    mut reader: impl io::Read + io::Seek,
    settings: &mut DecryptionSettings,
) -> io::Result<Vec<u8>> {
    if settings.mode.is_none() {
        let position = reader.stream_position()?;

        let mut read_settings = ReadSettings {
            format: None,
            decryption: settings.clone(),
            ..Default::default()
        };
        let Ok(_) = read(&mut reader, &mut read_settings) else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "couldn't probe file for format/encryption properties",
            ));
        };
        *settings = read_settings.decryption;

        reader.seek(io::SeekFrom::Start(position))?;
    }

    match_encryption!(reader, settings, settings, decrypt_impl)
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
        const fn array_bytes<const KIND: u8, const LINE: u8, const ID: u8>() -> &'static [u8] {
            &[
                KIND, 0, 0, 0,
                3, 0, // size
                LINE, 0, // line
                ID, 0, // id
                0, 0, 0, 0, 10, 0, 0, 0, // DataNode::Integer(10)
                1, 0, 0, 0, 0x00, 0x00, 0x80, 0x3F, // DataNode::Float(1.0)
                5, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o', // DataNode::Symbol("foo")
            ]
        }

        assert_node_bytes(
            array_bytes::<16, 13, 0>(),
            DataNode::Array(DataArray::from_nodes(13, 0, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
        );
        assert_node_bytes(
            array_bytes::<17, 14, 1>(),
            DataNode::Command(DataArray::from_nodes(14, 1, vec![
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
            DataNode::Property(DataArray::from_nodes(16, 2, vec![
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
                DataArray::from_nodes(19, 3, vec![
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
            DataNode::Autorun(DataArray::from_nodes(23, 4, vec![
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
