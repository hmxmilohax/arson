// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Write};
use std::sync::Mutex;

use byteorder::{LittleEndian, WriteBytesExt};

use crate::crypt::{CryptAlgorithm, CryptWriter, NewRandom, NoopCrypt, OldRandom};
use crate::{DataArray, DataKind, DataNode};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum WriteEncoding {
    UTF8,
    Latin1,
}

#[derive(Debug, Clone)]
pub struct WriteSettings {
    pub encoding: WriteEncoding,
}

#[derive(thiserror::Error, Debug)]
pub enum WriteError {
    #[error("length/line {0} is too large to be written")]
    LengthTooLarge(usize),

    #[error("could not encode text '{0}' into the desired encoding")]
    EncodeError(String),

    #[error("{0}")]
    IO(#[from] io::Error),
}

struct Writer<'read, 'crypt> {
    #[allow(dead_code, reason = "stored for potential usage in the future")]
    settings: WriteSettings,
    encoding: &'static encoding_rs::Encoding,

    writer: CryptWriter<'read, 'crypt>,
}

impl Writer<'_, '_> {
    fn write_array(&mut self, array: &DataArray) -> Result<(), WriteError> {
        fn shorten(value: usize) -> Result<i16, WriteError> {
            value.try_into().map_err(|_| WriteError::LengthTooLarge(value))
        }

        self.writer.write_i16::<LittleEndian>(shorten(array.len())?)?;
        self.writer.write_i16::<LittleEndian>(shorten(array.line())?)?;
        self.writer.write_i16::<LittleEndian>(0)?;

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

    fn write_string(&mut self, text: &String) -> Result<(), WriteError> {
        let (encoded, actual_encoding, remapped) = self.encoding.encode(text);
        if remapped || !std::ptr::addr_eq(self.encoding, actual_encoding) {
            return Err(WriteError::EncodeError(text.clone()));
        }

        self.write_glob(&encoded)?;
        Ok(())
    }
}

fn write_encrypted(
    array: &DataArray,
    writer: &mut impl io::Write,
    crypt: &mut impl CryptAlgorithm,
    settings: WriteSettings,
) -> Result<(), WriteError> {
    let encoding = match settings.encoding {
        WriteEncoding::UTF8 => encoding_rs::UTF_8,
        WriteEncoding::Latin1 => encoding_rs::WINDOWS_1252,
    };

    let mut writer = Writer {
        settings,
        encoding,
        writer: CryptWriter::new(writer, crypt),
    };

    // Array always exists under our model
    writer.writer.write_u8(1)?;
    writer.write_array(array)?;

    Ok(())
}

static NEWSTYLE_SEEDER: Mutex<NewRandom> = Mutex::new(NewRandom::new(NewRandom::DEFAULT_SEED));
static OLDSTYLE_SEEDER: Mutex<OldRandom> = Mutex::new(OldRandom::new(OldRandom::DEFAULT_SEED));

pub fn write_newstyle(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
) -> Result<(), WriteError> {
    let seed = NEWSTYLE_SEEDER.lock().map_or(NewRandom::DEFAULT_SEED, |mut g| g.next());
    write_newstyle_seeded(array, writer, settings, seed)
}

pub fn write_oldstyle(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
) -> Result<(), WriteError> {
    let seed = OLDSTYLE_SEEDER.lock().map_or(OldRandom::DEFAULT_SEED, |mut g| g.next());
    write_oldstyle_seeded(array, writer, settings, seed)
}

pub fn write_newstyle_seeded(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
    seed: i32,
) -> Result<(), WriteError> {
    writer.write_i32::<LittleEndian>(seed)?;
    write_encrypted(array, writer, &mut NewRandom::new(seed), settings)
}

pub fn write_oldstyle_seeded(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
    seed: u32,
) -> Result<(), WriteError> {
    writer.write_u32::<LittleEndian>(seed)?;
    write_encrypted(array, writer, &mut OldRandom::new(seed), settings)
}

pub fn write_unencrypted(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
) -> Result<(), WriteError> {
    write_encrypted(array, writer, &mut NoopCrypt, settings)
}

pub fn encrypt_newstyle(bytes: &Vec<u8>, writer: &mut impl io::Write) -> io::Result<()> {
    let seed = NEWSTYLE_SEEDER.lock().map_or(NewRandom::DEFAULT_SEED, |mut g| g.next());
    encrypt_newstyle_seeded(bytes, writer, seed)
}

pub fn encrypt_oldstyle(bytes: &Vec<u8>, writer: &mut impl io::Write) -> io::Result<()> {
    let seed = OLDSTYLE_SEEDER.lock().map_or(OldRandom::DEFAULT_SEED, |mut g| g.next());
    encrypt_oldstyle_seeded(bytes, writer, seed)
}

pub fn encrypt_newstyle_seeded(bytes: &Vec<u8>, writer: &mut impl io::Write, seed: i32) -> io::Result<()> {
    writer.write_i32::<LittleEndian>(seed)?;
    Ok(encrypt_impl(bytes, writer, &mut NewRandom::new(seed))?)
}

pub fn encrypt_oldstyle_seeded(bytes: &Vec<u8>, writer: &mut impl io::Write, seed: u32) -> io::Result<()> {
    writer.write_u32::<LittleEndian>(seed)?;
    Ok(encrypt_impl(bytes, writer, &mut OldRandom::new(seed))?)
}

fn encrypt_impl(
    bytes: &Vec<u8>,
    writer: &mut impl io::Write,
    crypt: &mut impl CryptAlgorithm,
) -> io::Result<()> {
    CryptWriter::new(writer, crypt).write_all(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_node_bytes(node: DataNode) -> Vec<u8> {
        let mut bytes = Vec::new();
        let mut writer = io::Cursor::new(&mut bytes);
        let mut crypt = NoopCrypt;

        let mut crypt_writer = Writer {
            settings: WriteSettings { encoding: WriteEncoding::UTF8 },
            encoding: encoding_rs::UTF_8,
            writer: CryptWriter::new(&mut writer, &mut crypt),
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
        const fn array_bytes<const KIND: u8>() -> &'static [u8] {
            &[
                KIND, 0, 0, 0,
                3, 0, // size
                1, 0, // line
                0, 0, // deprecated field
                0, 0, 0, 0, 10, 0, 0, 0, // DataNode::Integer(10)
                1, 0, 0, 0, 0x00, 0x00, 0x80, 0x3F, // DataNode::Float(1.0)
                5, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o', // DataNode::Symbol("foo")
            ]
        }

        assert_node_bytes(
            DataNode::Array(DataArray::from_nodes(1, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
            array_bytes::<16>(),
        );
        assert_node_bytes(
            DataNode::Command(DataArray::from_nodes(1, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
            array_bytes::<17>(),
        );
        assert_node_bytes(DataNode::String("foo".to_owned()), &[
            18, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);
        assert_node_bytes(
            DataNode::Property(DataArray::from_nodes(1, vec![
                DataNode::Integer(10),
                DataNode::Float(1.0),
                DataNode::Symbol("foo".to_owned()),
            ])),
            array_bytes::<19>(),
        );
        assert_node_bytes(DataNode::Glob(vec![b'f', b'o', b'o']), &[
            20, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o',
        ]);

        let mut bytes = [32, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o'].to_vec();
        bytes.extend_from_slice(array_bytes::<16>());
        assert_node_bytes(
            DataNode::Define(
                "foo".to_owned(),
                DataArray::from_nodes(1, vec![
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
        bytes.extend_from_slice(array_bytes::<17>());
        assert_node_bytes(
            DataNode::Autorun(DataArray::from_nodes(1, vec![
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
