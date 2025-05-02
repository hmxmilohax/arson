// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Read};

use arson_parse::encoding::{DecodeError, DtaEncoding};
use byteorder::{LittleEndian, ReadBytesExt};

use crate::crypt::{CryptAlgorithm, CryptReader, NewRandom, NoopCrypt, OldRandom};
use crate::{DataArray, DataKind, DataNode};

#[derive(Debug, Clone)]
pub struct ReadSettings {
    pub encoding: Option<DtaEncoding>,
}

#[derive(thiserror::Error, Debug)]
pub enum ReadError {
    #[error("malformed data at offset {0}")]
    Malformed(u64),

    #[error("{0} at offset {1}")]
    DecodingFailed(DecodeError, u64),

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

    #[error(transparent)]
    IO(#[from] io::Error),
}

struct Reader<Reader: io::Read + io::Seek, Crypt: CryptAlgorithm> {
    settings: ReadSettings,

    reader: CryptReader<Reader, Crypt>,
}

impl<R: io::Read + io::Seek, C: CryptAlgorithm> Reader<R, C> {
    fn read_file(mut self) -> Result<DataArray, ReadError> {
        let exists = self.reader.read_u8()?;
        match exists {
            0 => Ok(DataArray::new(1, 0)),
            1 => match self.read_array() {
                Ok(value) => Ok(value),
                Err(err) => match err {
                    ReadError::Malformed(offset) => {
                        // Insert proper position into error
                        let mut reader = self.reader.into_inner();
                        let position = reader.stream_position()?.saturating_sub(offset);
                        Err(ReadError::Malformed(position))
                    },
                    ReadError::DecodingFailed(error, offset) => {
                        // Insert proper position into error
                        let mut reader = self.reader.into_inner();
                        let position = reader.stream_position()?.saturating_sub(offset);
                        Err(ReadError::DecodingFailed(error, position))
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

    fn read_array(&mut self) -> Result<DataArray, ReadError> {
        fn lengthen(value: i16) -> Result<usize, ReadError> {
            value.try_into().map_err(|_| ReadError::InvalidLength(value))
        }

        let size = lengthen(self.reader.read_i16::<LittleEndian>()?)?;
        let line = lengthen(self.reader.read_i16::<LittleEndian>()?)?;
        let id = lengthen(self.reader.read_i16::<LittleEndian>()?)?;

        let mut array = DataArray::with_capacity(line, id, size);
        for _i in 0..size {
            let data = self.read_node()?;
            array.push(data);
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
            5 => DataNode::Symbol(self.read_string()?),
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
                let body = match self.read_node()? {
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
            33 => DataNode::Include(self.read_string()?),
            34 => DataNode::Merge(self.read_string()?),
            35 => DataNode::Ifndef(self.read_string()?),
            36 => {
                _ = self.reader.read_i32::<LittleEndian>()?;
                let body = match self.read_node()? {
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
            37 => DataNode::Undefine(self.read_string()?),

            _ => {
                // A kind value greater than 255 is most likely improperly-decrypted data
                if kind > 0xFF {
                    // backwards position offset, will be corrected later
                    return Err(ReadError::Malformed(4));
                } else {
                    return Err(ReadError::InvalidKind(kind));
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
        let bytes = self.read_glob()?;
        let decoded = match arson_parse::encoding::decode(&bytes, self.settings.encoding) {
            Ok((decoded, _)) => decoded,
            Err(error) => {
                // backwards position offset, will be corrected later
                return Err(ReadError::DecodingFailed(error, bytes.len() as u64));
            },
        };
        Ok(decoded.into_owned())
    }
}

fn read_encrypted(
    reader: impl io::Seek + io::Read,
    settings: ReadSettings,
    crypt: impl CryptAlgorithm,
) -> Result<DataArray, ReadError> {
    let reader = Reader { settings, reader: CryptReader::new(reader, crypt) };
    reader.read_file()
}

pub fn read(mut reader: impl io::Seek + io::Read, settings: ReadSettings) -> Result<DataArray, ReadError> {
    let position = reader.stream_position()?;

    // Attempt new-style decryption first
    if let Ok((array, _seed)) = read_newstyle(&mut reader, settings.clone()) {
        return Ok(array);
    };

    // If that fails, try old-style
    reader.seek(io::SeekFrom::Start(position))?;
    if let Ok((array, _seed)) = read_oldstyle(&mut reader, settings.clone()) {
        return Ok(array);
    };

    // Finally, try unencrypted
    reader.seek(io::SeekFrom::Start(position))?;
    if let Ok(array) = read_unencrypted(reader, settings.clone()) {
        return Ok(array);
    };

    Err(ReadError::IO(io::Error::new(
        io::ErrorKind::InvalidData,
        "input data could not be decoded; tried newstyle, oldstyle, and unencrypted",
    )))
}

pub fn read_newstyle(
    mut reader: impl io::Read + io::Seek,
    settings: ReadSettings,
) -> Result<(DataArray, i32), ReadError> {
    let seed = reader.read_i32::<LittleEndian>()?;
    read_encrypted(reader, settings, NewRandom::new(seed)).map(|array| (array, seed))
}

pub fn read_oldstyle(
    mut reader: impl io::Read + io::Seek,
    settings: ReadSettings,
) -> Result<(DataArray, u32), ReadError> {
    let seed = reader.read_u32::<LittleEndian>()?;
    read_encrypted(reader, settings, OldRandom::new(seed)).map(|array| (array, seed))
}

pub fn read_unencrypted(
    reader: impl io::Read + io::Seek,
    settings: ReadSettings,
) -> Result<DataArray, ReadError> {
    read_encrypted(reader, settings, NoopCrypt)
}

pub fn decrypt(mut reader: impl io::Read + io::Seek, settings: ReadSettings) -> Result<Vec<u8>, ReadError> {
    let position = reader.stream_position()?;

    // Attempt new-style decryption first
    if read_newstyle(&mut reader, settings.clone()).is_ok() {
        reader.seek(io::SeekFrom::Start(position))?;
        return Ok(decrypt_newstyle(reader).map(|(bytes, _)| bytes)?);
    };

    // If that fails, try old-style
    reader.seek(io::SeekFrom::Start(position))?;
    if read_oldstyle(&mut reader, settings.clone()).is_ok() {
        reader.seek(io::SeekFrom::Start(position))?;
        return Ok(decrypt_oldstyle(reader).map(|(bytes, _)| bytes)?);
    };

    // Finally, try unencrypted
    reader.seek(io::SeekFrom::Start(position))?;
    if read_unencrypted(&mut reader, settings.clone()).is_ok() {
        reader.seek(io::SeekFrom::Start(position))?;
        return Ok(decrypt_unencrypted(reader)?);
    };

    Err(ReadError::IO(io::Error::new(
        io::ErrorKind::InvalidData,
        "input data could not be decoded; tried newstyle, oldstyle, and unencrypted",
    )))
}

pub fn decrypt_newstyle(mut reader: impl io::Read) -> io::Result<(Vec<u8>, i32)> {
    let seed = reader.read_i32::<LittleEndian>()?;
    decrypt_impl(reader, NewRandom::new(seed)).map(|bytes| (bytes, seed))
}

pub fn decrypt_oldstyle(mut reader: impl io::Read) -> io::Result<(Vec<u8>, u32)> {
    let seed = reader.read_u32::<LittleEndian>()?;
    decrypt_impl(reader, OldRandom::new(seed)).map(|bytes| (bytes, seed))
}

fn decrypt_unencrypted(reader: impl io::Read) -> io::Result<Vec<u8>> {
    decrypt_impl(reader, NoopCrypt)
}

fn decrypt_impl(reader: impl io::Read, crypt: impl CryptAlgorithm) -> io::Result<Vec<u8>> {
    let mut bytes = Vec::new();
    CryptReader::new(reader, crypt).read_to_end(&mut bytes)?;
    Ok(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read_node_bytes(bytes: &[u8]) -> DataNode {
        let mut reader = io::Cursor::new(bytes);

        let mut crypt_reader = Reader {
            settings: ReadSettings { encoding: None },
            reader: CryptReader::new(&mut reader, NoopCrypt),
        };
        let node = crypt_reader.read_node().expect("failed to read node bytes");

        assert_eq!(reader.position() as usize, bytes.len(), "not all node bytes were read");

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
