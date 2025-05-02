// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::Cursor;
use std::sync::LazyLock;

use arson_dtb::{DataArray, DataNode, ReadSettings, WriteSettings};
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

const READ_SETTINGS: ReadSettings = ReadSettings { encoding: Some(DtaEncoding::Utf8) };
const WRITE_SETTINGS: WriteSettings = WriteSettings { encoding: DtaEncoding::Utf8 };

#[test]
fn unencrypted() {
    // read
    let array = arson_dtb::read_unencrypted(Cursor::new(TEST_DATA_UNENCRYPTED), READ_SETTINGS).unwrap();
    assert_eq!(array, *TEST_ARRAY);

    // write
    let mut bytes = Vec::new();
    arson_dtb::write_unencrypted(&array, Cursor::new(&mut bytes), WRITE_SETTINGS).unwrap();
    assert_eq!(bytes, TEST_DATA_UNENCRYPTED);

    // cycle
    for _i in 0..25 {
        let array = arson_dtb::read_unencrypted(Cursor::new(&bytes), READ_SETTINGS).unwrap();
        assert_eq!(array, array);

        bytes.clear();
        arson_dtb::write_unencrypted(&array, Cursor::new(&mut bytes), WRITE_SETTINGS).unwrap();
        assert_eq!(bytes, TEST_DATA_UNENCRYPTED);
    }
}

#[test]
fn oldstyle() {
    // read
    let (array, seed) = arson_dtb::read_oldstyle(Cursor::new(TEST_DATA_OLDSTYLE), READ_SETTINGS).unwrap();
    assert_eq!(array, *TEST_ARRAY);

    // write
    let mut bytes = Vec::new();
    arson_dtb::write_oldstyle(&array, Cursor::new(&mut bytes), WRITE_SETTINGS, Some(seed)).unwrap();
    assert_eq!(bytes, TEST_DATA_OLDSTYLE);

    // cycle
    for _i in 0..25 {
        let (array, seed) = arson_dtb::read_oldstyle(Cursor::new(&bytes), READ_SETTINGS).unwrap();
        assert_eq!(array, array);

        let a = seed.wrapping_rem(0x1F31D).wrapping_mul(0x41A7);
        let b = seed.wrapping_div(0x1F31D).wrapping_mul(0xB14);
        let seed = match a - b {
            c if c <= 0 => c + 0x7FFFFFFF,
            c => c,
        };

        bytes.clear();
        arson_dtb::write_oldstyle(&array, Cursor::new(&mut bytes), WRITE_SETTINGS, Some(seed)).unwrap();
    }
}

#[test]
fn newstyle() {
    // read
    let (array, seed) = arson_dtb::read_newstyle(Cursor::new(TEST_DATA_NEWSTYLE), READ_SETTINGS).unwrap();
    assert_eq!(array, *TEST_ARRAY);

    // write
    let mut bytes = Vec::new();
    arson_dtb::write_newstyle(&array, Cursor::new(&mut bytes), WRITE_SETTINGS, Some(seed)).unwrap();
    assert_eq!(bytes, TEST_DATA_NEWSTYLE);

    // cycle
    for _i in 0..25 {
        let (array, seed) = arson_dtb::read_newstyle(Cursor::new(&bytes), READ_SETTINGS).unwrap();
        assert_eq!(array, array);

        let a = seed.wrapping_rem(0x1F31D).wrapping_mul(0x41A7);
        let b = seed.wrapping_div(0x1F31D).wrapping_mul(0xB14);
        let seed = match a - b {
            c if c <= 0 => c + 0x7FFFFFFF,
            c => c,
        };

        bytes.clear();
        arson_dtb::write_newstyle(&array, Cursor::new(&mut bytes), WRITE_SETTINGS, Some(seed)).unwrap();
    }
}
