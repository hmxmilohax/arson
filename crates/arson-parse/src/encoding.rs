// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Cow;

/// Supported encodings for DTA text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DtaEncoding {
    Utf8,
    Utf16BigEndian,
    Utf16LittleEndian,
    Latin1,
}

#[derive(thiserror::Error, Debug)]
#[error("could not encode the text to {0:?}")]
pub struct EncodeError(pub DtaEncoding);

#[derive(thiserror::Error, Debug)]
pub struct DecodeError(pub Option<DtaEncoding>);

impl std::fmt::Display for DecodeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Some(encoding) => write!(f, "could not decode the bytes from {encoding:?}"),
            None => f.write_str("could not decode the bytes via default encoding detection"),
        }
    }
}

macro_rules! decode_default_impl {
    ($bytes:ident, $encoding_object:ident, $encoding_variant:ident) => {{
        match encoding_rs::$encoding_object.decode_without_bom_handling_and_without_replacement($bytes) {
            Some(decoded) => Ok(decoded),
            None => Err(DecodeError(Some(DtaEncoding::$encoding_variant))),
        }
    }};
}

/// Decodes the given bytes according to the provided encoding.
pub fn decode(
    bytes: &[u8],
    encoding: Option<DtaEncoding>,
) -> Result<(Cow<'_, str>, DtaEncoding), DecodeError> {
    match encoding {
        Some(DtaEncoding::Utf8) => Ok((decode_utf8(bytes)?, DtaEncoding::Utf8)),
        Some(DtaEncoding::Utf16BigEndian) => Ok((decode_utf16_be(bytes)?, DtaEncoding::Utf16BigEndian)),
        Some(DtaEncoding::Utf16LittleEndian) => Ok((decode_utf16_le(bytes)?, DtaEncoding::Utf16LittleEndian)),
        Some(DtaEncoding::Latin1) => Ok((decode_latin1(bytes)?, DtaEncoding::Latin1)),
        None => decode_default(bytes),
    }
}

pub fn decode_default(bytes: &[u8]) -> Result<(Cow<'_, str>, DtaEncoding), DecodeError> {
    // Attempt to decode as UTF first, it will more reliably result in a
    // decoding error if it's not the right encoding due to the format details.
    //
    // 0x80-0xFF are only used for multi-byte sequences, and these sequences follow a
    // specific format that real Latin-1 text is pretty much guaranteed to break.
    // Almost all Latin-1 special characters are in the 0xC0-0xFF range, which UTF-8
    // uses exclusively as the first byte for multi-byte code points, so consecutive
    // special characters will not result in a valid byte sequence.
    let (decoded, actual_encoding, malformed) = encoding_rs::UTF_8.decode(bytes);
    if !malformed && std::ptr::addr_eq(actual_encoding, encoding_rs::UTF_8) {
        return Ok((decoded, DtaEncoding::Utf8));
    }

    // Attempt Latin-1 next (specifically Windows-1252, because it has more
    // printable characters which are more likely intended)
    let (decoded, actual_encoding, malformed) = encoding_rs::WINDOWS_1252.decode(bytes);
    if !malformed && std::ptr::addr_eq(actual_encoding, encoding_rs::WINDOWS_1252) {
        return Ok((decoded, DtaEncoding::Latin1));
    }

    Err(DecodeError(None))
}

pub fn decode_utf8(bytes: &[u8]) -> Result<Cow<'_, str>, DecodeError> {
    decode_default_impl!(bytes, UTF_8, Utf8)
}

pub fn decode_utf16_be(bytes: &[u8]) -> Result<Cow<'_, str>, DecodeError> {
    decode_default_impl!(bytes, UTF_16BE, Utf16BigEndian)
}

pub fn decode_utf16_le(bytes: &[u8]) -> Result<Cow<'_, str>, DecodeError> {
    decode_default_impl!(bytes, UTF_16LE, Utf16LittleEndian)
}

pub fn decode_latin1(bytes: &[u8]) -> Result<Cow<'_, str>, DecodeError> {
    decode_default_impl!(bytes, WINDOWS_1252, Latin1)
}

macro_rules! encode_default_impl {
    ($text:ident, $encoding_object:ident, $encoding_variant:ident) => {{
        let (encoded, actual_encoding, unmappable) = encoding_rs::$encoding_object.encode($text);
        if !unmappable && std::ptr::addr_eq(actual_encoding, encoding_rs::$encoding_object) {
            return Ok(encoded);
        }

        Err(EncodeError(DtaEncoding::$encoding_variant))
    }};
}

pub fn encode(text: &str, encoding: DtaEncoding) -> Result<Cow<'_, [u8]>, EncodeError> {
    match encoding {
        DtaEncoding::Utf8 => encode_utf8(text),
        DtaEncoding::Utf16BigEndian => encode_utf16_be(text),
        DtaEncoding::Utf16LittleEndian => encode_utf16_le(text),
        DtaEncoding::Latin1 => encode_latin1(text),
    }
}

pub fn encode_utf8(text: &str) -> Result<Cow<'_, [u8]>, EncodeError> {
    encode_default_impl!(text, UTF_8, Utf8)
}

pub fn encode_utf16_be(text: &str) -> Result<Cow<'_, [u8]>, EncodeError> {
    encode_default_impl!(text, UTF_16BE, Utf16BigEndian)
}

pub fn encode_utf16_le(text: &str) -> Result<Cow<'_, [u8]>, EncodeError> {
    encode_default_impl!(text, UTF_16LE, Utf16LittleEndian)
}

pub fn encode_latin1(text: &str) -> Result<Cow<'_, [u8]>, EncodeError> {
    encode_default_impl!(text, WINDOWS_1252, Latin1)
}
