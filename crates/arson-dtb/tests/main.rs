// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::Cursor;
use std::sync::LazyLock;

use arson_dtb::{
    DataArray,
    DataNode,
    DecryptionSettings,
    EncryptionMode,
    EncryptionSettings,
    FormatVersion,
    ReadSettings,
    WriteSettings,
};
use arson_parse::encoding::DtaEncoding;

static TEST_ARRAY: LazyLock<DataArray> = LazyLock::new(|| {
    DataArray::from_nodes(1, 0, vec![
        DataNode::Integer(10),
        DataNode::Float(1.0),
        DataNode::Variable("foo".to_owned()),
        DataNode::Function("foo".to_owned()),
        DataNode::Object("foo".to_owned()),
        DataNode::Symbol("foo".to_owned()),
        DataNode::Unhandled,
    ])
});

#[rustfmt::skip]
static TEST_DATA_UNENCRYPTED: &[u8] = &[
    1, // "exists" flag
    7, 0, // size
    1, 0, // line
    0, 0, // id

    0, 0, 0, 0, 10, 0, 0, 0, // DataNode::Integer(10)
    1, 0, 0, 0, 0x00, 0x00, 0x80, 0x3F, // DataNode::Float(1.0)
    2, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o', // DataNode::Variable("foo")
    3, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o', // DataNode::Function("foo")
    4, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o', // DataNode::Object("foo")
    5, 0, 0, 0, 3, 0, 0, 0, b'f', b'o', b'o', // DataNode::Symbol("foo")
    6, 0, 0, 0, 0, 0, 0, 0, // DataNode::Unhandled
];

static TEST_DATA_OLDSTYLE: &[u8] = &[
    76, 79, 83, 82, 223, 88, 103, 182, 96, 249, 200, 226, 38, 138, 78, 137, 187, 220, 63, 172, 191, 112, 180,
    75, 225, 175, 24, 211, 47, 93, 200, 101, 131, 26, 51, 76, 142, 56, 253, 40, 96, 98, 230, 23, 159, 25,
    247, 22, 163, 58, 239, 123, 115, 144, 86, 240, 162, 51, 172, 0, 18, 166, 115, 134, 75, 68, 203, 53, 177,
    142, 201, 180, 154, 231, 28, 47, 2, 16, 200,
];

static TEST_DATA_NEWSTYLE: &[u8] = &[
    9, 22, 23, 48, 136, 255, 26, 172, 15, 33, 50, 202, 190, 99, 19, 107, 233, 185, 3, 196, 188, 126, 177,
    125, 84, 25, 250, 58, 217, 99, 207, 102, 45, 49, 33, 17, 49, 173, 31, 193, 80, 131, 90, 77, 124, 138, 60,
    128, 98, 4, 108, 69, 44, 166, 25, 34, 150, 52, 143, 112, 41, 5, 255, 117, 145, 165, 112, 226, 153, 205,
    240, 104, 191, 32, 96, 14, 253, 131, 9,
];

const READ_SETTINGS: ReadSettings = ReadSettings {
    format: Some(FormatVersion::Milo),
    encoding: Some(DtaEncoding::Utf8),
    decryption: DecryptionSettings { mode: None, key: None },
};

const WRITE_SETTINGS: WriteSettings = WriteSettings {
    format: FormatVersion::Milo,
    encoding: DtaEncoding::Utf8,
    encryption: EncryptionSettings { mode: EncryptionMode::None, key: None, time_entropy: false },
};

fn test_encryption(source_array: &DataArray, source_bytes: &[u8], mode: Option<EncryptionMode>) {
    // read
    let read_settings = READ_SETTINGS.with_decryption_mode(mode);
    let read_result = arson_dtb::read(Cursor::new(source_bytes), &read_settings).unwrap();
    assert_eq!(read_result.value, *source_array);

    // write
    let mut written_bytes = Vec::new();
    let write_settings = WRITE_SETTINGS
        .with_encryption_mode(read_result.encryption)
        .with_encryption_key(Some(read_result.key));
    arson_dtb::write(&read_result.value, Cursor::new(&mut written_bytes), &write_settings).unwrap();
    assert_eq!(written_bytes, source_bytes);

    // cycle
    for _i in 0..25 {
        let mut read_settings = READ_SETTINGS.with_decryption_mode(mode);
        let read_result = arson_dtb::read(Cursor::new(&written_bytes), &read_settings).unwrap();
        assert_eq!(read_result.value, *source_array);

        if let Some(key) = read_settings.decryption.key {
            let a = key.wrapping_rem(0x1F31D).wrapping_mul(0x41A7);
            let b = key.wrapping_div(0x1F31D).wrapping_mul(0xB14);
            let key = match a - b {
                c if c <= 0 => c + 0x7FFFFFFF,
                c => c,
            };
            read_settings.decryption.key = Some(key);
        }

        let write_settings = WRITE_SETTINGS
            .with_encryption_mode(read_result.encryption)
            .with_encryption_key(Some(read_result.key));

        written_bytes.clear();
        arson_dtb::write(&read_result.value, Cursor::new(&mut written_bytes), &write_settings).unwrap();
    }
}

#[test]
fn unencrypted() {
    test_encryption(&*TEST_ARRAY, TEST_DATA_UNENCRYPTED, None);
}

#[test]
fn oldstyle() {
    test_encryption(&*TEST_ARRAY, TEST_DATA_OLDSTYLE, Some(EncryptionMode::Old));
}

#[test]
fn newstyle() {
    test_encryption(&*TEST_ARRAY, TEST_DATA_NEWSTYLE, Some(EncryptionMode::New));
}
