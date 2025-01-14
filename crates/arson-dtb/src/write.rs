// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Write};

use byteorder::{LittleEndian, WriteBytesExt};

use crate::crypt::{CryptAlgorithm, CryptWriter, NewRandom, NoopCrypt, OldRandom};
use crate::{DataArray, DataNode};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum WriteEncoding {
    UTF8,
    Latin1,
}

#[derive(Debug, Clone)]
pub struct WriteSettings {
    encoding: WriteEncoding,
}

#[derive(thiserror::Error, Debug)]
pub enum WriteError {
    #[error("length/line {0} is too large to be written")]
    LengthTooLarge(usize),

    #[error("could not encode text {0} into the desired encoding")]
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
            DataNode::Var(name) => self.write_string(name)?,
            DataNode::Func(name) => self.write_string(name)?,
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

            DataNode::Define(name) => self.write_string(name)?,
            DataNode::Include(path) => self.write_string(path)?,
            DataNode::Merge(path) => self.write_string(path)?,
            DataNode::Ifndef(name) => self.write_string(name)?,
            DataNode::Autorun(array) => {
                self.writer.write_i32::<LittleEndian>(0)?;
                self.write_array(array)?;
            },
            DataNode::Undef(name) => self.write_string(name)?,
        }

        Ok(())
    }

    fn write_glob(&mut self, bytes: &[u8]) -> Result<(), WriteError> {
        let length = u32::try_from(bytes.len()).map_err(|_| WriteError::LengthTooLarge(bytes.len()))?;
        self.writer.write_u32::<LittleEndian>(length)?;

        let written = self.writer.write(bytes)?;
        if written < bytes.len() {
            // Attempt once more to make sure it's not a fluke
            let bytes = &bytes[written..];
            let written = self.writer.write(bytes)?;
            if written < bytes.len() {
                return Err(io::Error::from(io::ErrorKind::WriteZero).into());
            }
        }

        Ok(())
    }

    fn write_string(&mut self, text: &String) -> Result<(), WriteError> {
        let (encoded, actual_encoding, success) = self.encoding.encode(text);
        if !success || std::ptr::addr_eq(self.encoding, actual_encoding) {
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

pub fn write_newstyle(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
) -> Result<(), WriteError> {
    const DEFAULT_SEED: i32 = 0x30171609; // seed used by dtab
    write_newstyle_seeded(array, writer, settings, DEFAULT_SEED)
}

pub fn write_oldstyle(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
) -> Result<(), WriteError> {
    const DEFAULT_SEED: u32 = 0x52534F4C; // seed used by DtbCrypt
    write_oldstyle_seeded(array, writer, settings, DEFAULT_SEED)
}

pub fn write_newstyle_seeded(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
    seed: i32,
) -> Result<(), WriteError> {
    write_encrypted(array, writer, &mut NewRandom::new(seed), settings)
}

pub fn write_oldstyle_seeded(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
    seed: u32,
) -> Result<(), WriteError> {
    write_encrypted(array, writer, &mut OldRandom::new(seed), settings)
}

pub fn write_unencrypted(
    array: &DataArray,
    writer: &mut impl io::Write,
    settings: WriteSettings,
) -> Result<(), WriteError> {
    write_encrypted(array, writer, &mut NoopCrypt, settings)
}
