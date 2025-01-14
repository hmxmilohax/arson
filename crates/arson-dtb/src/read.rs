// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Read};

use byteorder::{LittleEndian, ReadBytesExt};

use crate::crypt::{CryptAlgorithm, CryptReader, NewRandom, NoopCrypt, OldRandom};
use crate::{DataArray, DataKind, DataNode};

#[derive(thiserror::Error, Debug)]
pub enum ReadError {
    #[error("malformed data at offset {0}")]
    Malformed(u64),

    #[error("invalid node kind {0}")]
    InvalidKind(u32),

    #[error("length/line {0} cannot be negative")]
    InvalidLength(i16),

    #[error("{expecting:?} was expecting {expected:?} argument, but got {actual:?} instead")]
    IncorrectNodeArgument {
        expecting: DataKind,
        expected: DataKind,
        actual: DataKind,
    },

    #[error("unexpected end of file")]
    UnexpectedEof,

    #[error("{0}")]
    IO(#[from] io::Error),
}

fn read_array(reader: &mut CryptReader<'_, '_>) -> Result<DataArray, ReadError> {
    fn lengthen(value: i16) -> Result<usize, ReadError> {
        value.try_into().map_err(|_| ReadError::InvalidLength(value))
    }

    let size = lengthen(reader.read_i16::<LittleEndian>()?)?;
    let line = lengthen(reader.read_i16::<LittleEndian>()?)?;
    let _deprecated = reader.read_i16::<LittleEndian>()?;

    let mut array = DataArray::with_capacity(size, line);
    for _i in 0..size {
        let data = read_node(reader)?;
        array.push(data);
    }

    Ok(array)
}

fn read_node(reader: &mut CryptReader<'_, '_>) -> Result<DataNode, ReadError> {
    let kind = reader.read_u32::<LittleEndian>()?;
    let data = match kind {
        0 => DataNode::Integer(reader.read_i32::<LittleEndian>()?),
        1 => DataNode::Float(reader.read_f32::<LittleEndian>()?),
        2 => DataNode::Variable(read_string(reader)?),
        3 => DataNode::Function(read_string(reader)?),
        4 => DataNode::Object(read_string(reader)?),
        5 => DataNode::Symbol(read_string(reader)?),
        6 => {
            _ = reader.read_i32::<LittleEndian>()?;
            DataNode::Unhandled
        },

        7 => DataNode::Ifdef(read_string(reader)?),
        8 => {
            _ = reader.read_i32::<LittleEndian>()?;
            DataNode::Else
        },
        9 => {
            _ = reader.read_i32::<LittleEndian>()?;
            DataNode::Endif
        },

        16 => DataNode::Array(read_array(reader)?),
        17 => DataNode::Command(read_array(reader)?),
        18 => DataNode::String(read_string(reader)?),
        19 => DataNode::Property(read_array(reader)?),
        20 => DataNode::Glob(read_glob(reader)?),

        32 => {
            let name = read_string(reader)?;
            let body = match read_node(reader)? {
                DataNode::Array(array) => array,
                node => {
                    return Err(ReadError::IncorrectNodeArgument {
                        expecting: DataKind::Define,
                        expected: DataKind::Array,
                        actual: node.get_kind(),
                    })
                },
            };
            DataNode::Define(name, body)
        },
        33 => DataNode::Include(read_string(reader)?),
        34 => DataNode::Merge(read_string(reader)?),
        35 => DataNode::Ifndef(read_string(reader)?),
        36 => {
            _ = reader.read_i32::<LittleEndian>()?;
            let body = match read_node(reader)? {
                DataNode::Command(array) => array,
                node => {
                    return Err(ReadError::IncorrectNodeArgument {
                        expecting: DataKind::Autorun,
                        expected: DataKind::Command,
                        actual: node.get_kind(),
                    })
                },
            };
            DataNode::Autorun(body)
        },
        37 => DataNode::Undef(read_string(reader)?),

        _ => {
            // A kind value greater than 255 is most likely improperly-decrypted data
            if kind > 0xFF {
                return Err(ReadError::Malformed(
                    4, /* backwards position offset, will be corrected later */
                ));
            } else {
                return Err(ReadError::InvalidKind(kind));
            }
        },
    };

    Ok(data)
}

fn read_glob(reader: &mut CryptReader<'_, '_>) -> Result<Vec<u8>, ReadError> {
    let length = reader.read_u32::<LittleEndian>()?;

    let mut bytes = vec![0u8; length as usize];
    let bytes_read = reader.read(&mut bytes)?;
    bytes.truncate(bytes_read);

    Ok(bytes)
}

fn read_string(reader: &mut CryptReader<'_, '_>) -> Result<String, ReadError> {
    use encoding_rs::{UTF_8, WINDOWS_1252};
    let bytes = read_glob(reader)?;

    // Attempt to decode as UTF-8 first, it will more reliably result in a
    // decoding error if it's not the right encoding due to the format details.
    //
    // 0x80-0xFF are only used for multi-byte sequences, and these sequences follow a
    // specific format that real Latin-1 text is pretty much guaranteed to break.
    // Almost all Latin-1 special characters are in the 0xC0-0xFF range, which UTF-8
    // uses exclusively as the first byte for multi-byte code points, so consecutive
    // special characters will not result in a valid byte sequence.
    let decoded = match UTF_8.decode_without_bom_handling_and_without_replacement(&bytes) {
        Some(text) => text,
        // Attempt Latin-1 next, specifically Windows-1252 because it has more
        // printable characters which are more likely intended in this context
        None => WINDOWS_1252.decode(&bytes).0,
    };

    Ok(decoded.into_owned())
}

pub trait SeekRead: io::Read + io::Seek {}
impl<T: io::Read + io::Seek> SeekRead for T {}

fn read_encrypted(
    reader: &mut impl SeekRead,
    crypt: &mut impl CryptAlgorithm,
) -> Result<DataArray, ReadError> {
    let mut crypt_reader = CryptReader::new(reader, crypt);

    let exists = crypt_reader.read_u8()?;
    match exists {
        0 => Ok(DataArray::default()),
        1 => match read_array(&mut crypt_reader) {
            Ok(value) => Ok(value),
            Err(err) => match err {
                ReadError::Malformed(offset) => {
                    // Insert proper position into error
                    let position = reader.stream_position()?.saturating_sub(offset);
                    Err(ReadError::Malformed(position))
                },
                ReadError::IO(error) => match error.kind() {
                    io::ErrorKind::UnexpectedEof => Err(ReadError::UnexpectedEof),
                    _ => Err(ReadError::IO(error)),
                },
                _ => Err(err),
            },
        },
        _ => Err(ReadError::Malformed(0)),
    }
}

pub fn read(reader: &mut impl SeekRead) -> Result<DataArray, ReadError> {
    // Attempt new-style decryption first
    if let Ok(result) = read_newstyle(reader) {
        return Ok(result);
    };

    // If that fails, try old-style
    reader.seek(io::SeekFrom::Start(0))?;
    if let Ok(result) = read_oldstyle(reader) {
        return Ok(result);
    };

    // Finally, try unencrypted
    reader.seek(io::SeekFrom::Start(0))?;
    if let Ok(result) = read_unencrypted(reader) {
        return Ok(result);
    };

    Err(ReadError::IO(io::Error::new(
        io::ErrorKind::InvalidData,
        "input data could not be decoded; tried newstyle, oldstyle, and unencrypted",
    )))
}

pub fn read_newstyle(reader: &mut impl SeekRead) -> Result<DataArray, ReadError> {
    let seed = reader.read_i32::<LittleEndian>()?;
    read_newstyle_seeded(reader, seed)
}

pub fn read_oldstyle(reader: &mut impl SeekRead) -> Result<DataArray, ReadError> {
    let seed = reader.read_u32::<LittleEndian>()?;
    read_oldstyle_seeded(reader, seed)
}

pub fn read_newstyle_seeded(reader: &mut impl SeekRead, seed: i32) -> Result<DataArray, ReadError> {
    read_encrypted(reader, &mut NewRandom::new(seed))
}

pub fn read_oldstyle_seeded(reader: &mut impl SeekRead, seed: u32) -> Result<DataArray, ReadError> {
    read_encrypted(reader, &mut OldRandom::new(seed))
}

pub fn read_unencrypted(reader: &mut impl SeekRead) -> Result<DataArray, ReadError> {
    read_encrypted(reader, &mut NoopCrypt)
}
