// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Write};
use std::sync::Mutex;

use arson_parse::encoding::{DtaEncoding, EncodeError};
use byteorder::{LittleEndian, WriteBytesExt};

use crate::crypt::{CryptAlgorithm, CryptWriter, NewRandom, NoopCrypt, OldRandom};
use crate::{DataArray, DataKind, DataNode, EncryptionMode, FormatVersion};

#[derive(Debug, Clone)]
pub struct WriteSettings {
    pub format: FormatVersion,
    pub encoding: DtaEncoding,
    pub encryption: EncryptionSettings,
}

impl WriteSettings {
    pub fn with_format(mut self, format: FormatVersion) -> Self {
        self.format = format;
        self
    }

    pub fn with_encoding(mut self, encoding: DtaEncoding) -> Self {
        self.encoding = encoding;
        self
    }

    pub fn with_encryption_mode(mut self, mode: EncryptionMode) -> Self {
        self.encryption = self.encryption.with_mode(mode);
        self
    }

    pub fn with_encryption_key(mut self, key: Option<u32>) -> Self {
        self.encryption = self.encryption.with_key(key);
        self
    }
}

#[derive(Debug, Clone, Default)]
pub struct EncryptionSettings {
    pub mode: EncryptionMode,
    pub key: Option<u32>,
    pub time_entropy: bool,
}

impl EncryptionSettings {
    pub fn with_mode(mut self, mode: EncryptionMode) -> Self {
        self.mode = mode;
        self
    }

    pub fn with_key(mut self, key: Option<u32>) -> Self {
        self.key = key;
        self
    }

    pub fn with_time_entropy(mut self, time_entropy: bool) -> Self {
        self.time_entropy = time_entropy;
        self
    }
}

#[derive(thiserror::Error, Debug)]
pub enum WriteError {
    #[error("length {0} is too large to be written")]
    LengthTooLarge(usize),

    #[error("{0}: {1}")]
    EncodingFailed(EncodeError, String),

    #[error(transparent)]
    IO(#[from] io::Error),
}

struct Writer<'settings, Writer: io::Write, Crypt: CryptAlgorithm> {
    settings: &'settings WriteSettings,

    writer: CryptWriter<Writer, Crypt>,
}

impl<'s, W: io::Write, C: CryptAlgorithm> Writer<'s, W, C> {
    fn write_file(
        array: &DataArray,
        mut writer: CryptWriter<W, C>,
        settings: &'s WriteSettings,
    ) -> Result<(), WriteError> {
        writer.write_u8(1)?; // Array always exists under our model

        let mut writer = Writer { settings, writer };
        writer.write_array(array)
    }

    fn write_array(&mut self, array: &DataArray) -> Result<(), WriteError> {
        macro_rules! write_length {
            ($self:ident, $write:ident, $size:expr) => {{
                let size = $size;
                let value = size.try_into().map_err(|_| WriteError::LengthTooLarge(size))?;
                $self.writer.$write::<LittleEndian>(value)?;
            }};
        }

        macro_rules! write_id {
            ($self:ident, $write:ident, $inttype:ty, $value:expr) => {{
                let value = $value.min(<$inttype>::MAX as usize);
                $self.writer.$write::<LittleEndian>(value as $inttype)?;
            }};
        }

        match self.settings.format {
            FormatVersion::Milo => {
                write_length!(self, write_i16, array.len());
                write_id!(self, write_i16, i16, array.line());
                write_id!(self, write_i16, i16, array.file_id());
            },
            FormatVersion::Forge => {
                write_id!(self, write_i32, i32, array.file_id());
                write_length!(self, write_i32, array.len());
                write_id!(self, write_i16, i16, array.line());
            },
        }

        for node in array {
            self.write_node(node)?;
        }

        Ok(())
    }

    fn write_node(&mut self, node: &DataNode) -> Result<(), WriteError> {
        self.writer.write_u32::<LittleEndian>(node.get_kind() as u32)?;

        match node {
            DataNode::Integer(value) => self.writer.write_i32::<LittleEndian>(*value)?,
            DataNode::Float(value) => self.writer.write_f32::<LittleEndian>(*value)?,
            DataNode::Variable(name) => self.write_string(name)?,
            DataNode::Function(name) => self.write_string(name)?,
            DataNode::Object(name) => self.write_string(name)?,
            DataNode::Symbol(name) => self.write_string(name)?,
            DataNode::Unhandled => self.writer.write_i32::<LittleEndian>(0)?,

            DataNode::Ifdef(name) => self.write_string(name)?,
            DataNode::Else => self.writer.write_i32::<LittleEndian>(0)?,
            DataNode::Endif => self.writer.write_i32::<LittleEndian>(0)?,

            DataNode::Array(array) => self.write_array(array)?,
            DataNode::Command(array) => self.write_array(array)?,
            DataNode::String(value) => self.write_string(value)?,
            DataNode::Property(array) => self.write_array(array)?,
            DataNode::Glob(value) => self.write_glob(value)?,

            DataNode::Define(name, body) => {
                self.write_string(name)?;
                self.writer.write_u32::<LittleEndian>(DataKind::Array as u32)?;
                self.write_array(body)?;
            },
            DataNode::Include(path) => self.write_string(path)?,
            DataNode::Merge(path) => self.write_string(path)?,
            DataNode::Ifndef(name) => self.write_string(name)?,
            DataNode::Autorun(body) => {
                self.writer.write_i32::<LittleEndian>(0)?;
                self.writer.write_u32::<LittleEndian>(DataKind::Command as u32)?;
                self.write_array(body)?;
            },
            DataNode::Undefine(name) => self.write_string(name)?,
        }

        Ok(())
    }

    fn write_glob(&mut self, bytes: &[u8]) -> Result<(), WriteError> {
        let length = u32::try_from(bytes.len()).map_err(|_| WriteError::LengthTooLarge(bytes.len()))?;
        self.writer.write_u32::<LittleEndian>(length)?;
        Ok(self.writer.write_all(bytes)?)
    }

    fn write_string(&mut self, text: &str) -> Result<(), WriteError> {
        match arson_parse::encoding::encode(text, self.settings.encoding) {
            Ok(bytes) => self.write_glob(&bytes),
            Err(error) => Err(WriteError::EncodingFailed(error, text.to_owned())),
        }
    }
}

static NEWSTYLE_SEEDER: Mutex<NewRandom> = Mutex::new(NewRandom::default());
static OLDSTYLE_SEEDER: Mutex<OldRandom> = Mutex::new(OldRandom::default());

fn make_key<C: CryptAlgorithm>(encryption: &EncryptionSettings, seeder: &Mutex<C>) -> u32 {
    use std::time::SystemTime;

    let key = encryption
        .key
        .unwrap_or_else(|| seeder.lock().map_or(C::DEFAULT_SEED, |mut g| g.next()));
    if !encryption.time_entropy {
        return key;
    }

    let salt = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .map(|duration| duration.as_micros() as u32)
        .unwrap_or(0);
    key.wrapping_mul(salt)
}

macro_rules! match_encryption {
    ($data:ident, $writer:ident, $settings:ident, $encryption:expr, $func:path) => {
        match $encryption.mode {
            EncryptionMode::New => {
                let key = make_key(&$encryption, &NEWSTYLE_SEEDER);
                $writer.write_u32::<LittleEndian>(key)?;

                let writer = CryptWriter::new($writer, NewRandom::new(key));
                $func($data, writer, $settings)
            },
            EncryptionMode::Old => {
                let key = make_key(&$encryption, &OLDSTYLE_SEEDER);
                $writer.write_u32::<LittleEndian>(key)?;

                let writer = CryptWriter::new($writer, OldRandom::new(key));
                $func($data, writer, $settings)
            },
            EncryptionMode::None => {
                let writer = CryptWriter::new($writer, NoopCrypt);
                $func($data, writer, $settings)
            },
        }
    };
}

pub fn write(
    array: &DataArray,
    mut writer: impl io::Write,
    settings: &WriteSettings,
) -> Result<(), WriteError> {
    match_encryption!(array, writer, settings, settings.encryption, Writer::write_file)
}

pub fn encrypt(bytes: &[u8], mut writer: impl io::Write, settings: EncryptionSettings) -> io::Result<()> {
    match_encryption!(bytes, writer, settings, settings, encrypt_impl)
}

fn encrypt_impl<W: io::Write, C: CryptAlgorithm>(
    bytes: &[u8],
    mut writer: CryptWriter<W, C>,
    _settings: EncryptionSettings,
) -> io::Result<()> {
    writer.write_all(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_node_bytes(node: DataNode) -> Vec<u8> {
        let mut bytes = Vec::new();
        let mut writer = io::Cursor::new(&mut bytes);

        let mut crypt_writer = Writer {
            settings: &WriteSettings {
                format: FormatVersion::Milo,
                encoding: DtaEncoding::Utf8,
                encryption: EncryptionSettings::default(),
            },
            writer: CryptWriter::new(&mut writer, NoopCrypt),
        };

        crypt_writer.write_node(&node).expect("failed to write node bytes");

        bytes
    }

    fn assert_node_bytes(node: DataNode, expected: &[u8]) {
        assert_eq!(write_node_bytes(node), expected);
    }

    #[test]
    fn values() {
        assert_node_bytes(DataNode::Integer(10), &[0, 0, 0, 0, 10, 0, 0, 0]);
        assert_node_bytes(DataNode::Float(1.0), &[1, 0, 0, 0, 0x00, 0x00, 0x80, 0x3F]);
        assert_node_bytes(DataNode::Variable("foo".to_owned()), &[
            2, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(DataNode::Function("foo".to_owned()), &[
            3, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(DataNode::Object("foo".to_owned()), &[
            4, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(DataNode::Symbol("foo".to_owned()), &[
            5, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(DataNode::Unhandled, &[6, 0, 0, 0, 0, 0, 0, 0]);

        assert_node_bytes(DataNode::Ifdef("foo".to_owned()), &[
            7, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(DataNode::Else, &[8, 0, 0, 0, 0, 0, 0, 0]);
        assert_node_bytes(DataNode::Endif, &[9, 0, 0, 0, 0, 0, 0, 0]);

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
            DataNode::Array(DataArray::from_nodes(13, 0, 0, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
            array_bytes::<16, 13, 0>(),
        );
        assert_node_bytes(
            DataNode::Command(DataArray::from_nodes(14, 0, 1, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
            array_bytes::<17, 14, 1>(),
        );
        assert_node_bytes(DataNode::String("foo".to_owned()), &[
            18, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(
            DataNode::Property(DataArray::from_nodes(16, 0, 2, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
            array_bytes::<19, 16, 2>(),
        );
        assert_node_bytes(DataNode::Glob(vec![b'f', b'o', b'o']), &[
            20, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);

        let mut bytes = [32, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'].to_vec();
        bytes.extend_from_slice(array_bytes::<16, 19, 3>());
        assert_node_bytes(
            DataNode::Define(
                "foo".to_owned(),
                DataArray::from_nodes(19, 0, 3, vec![
                    DataNode::Integer(10),
                    DataNode::Float(1.0),
                    DataNode::Symbol("foo".to_owned()),
                ]),
            ),
            &bytes,
        );
        assert_node_bytes(DataNode::Include("foo".to_owned()), &[
            33, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(DataNode::Merge("foo".to_owned()), &[
            34, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(DataNode::Ifndef("foo".to_owned()), &[
            35, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        let mut bytes = [36, 0, 0, 0, 0, 0, 0, 0].to_vec();
        bytes.extend_from_slice(array_bytes::<17, 23, 4>());
        assert_node_bytes(
            DataNode::Autorun(DataArray::from_nodes(23, 0, 4, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
            &bytes,
        );
        assert_node_bytes(DataNode::Undefine("foo".to_owned()), &[
            37, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
    }
}
