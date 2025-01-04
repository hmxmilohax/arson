// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_parse::{DiagnosticKind, IntegerValue, Token, TokenValue, Tokenizer};

#[test]
fn thorough() {
    use TokenValue::*;

    let text = include_str!("../test_files/thorough.dta").replace("\r\n", "\n");

    // Format for this behemoth:
    // - each line is its own inner vec
    // - each token is stored as the value + location
    // - token location is relative to the current line, not the whole file
    #[rustfmt::skip] // structure/formatting matches the test file
    let tokens = vec![
        (1, vec![(Comment(" SPDX-License-Identifier: LGPL-3.0-or-later"), 0..44)]),

        (3, vec![(Comment(" integers"), 0..10)]),
        (4, vec![(Integer(1), 0..1), (Integer(2), 2..4), (Integer(-3), 5..7)]),
        (5, vec![(Integer(1), 0..2), (Integer(2), 3..6), (Integer(-3), 7..10)]),

        (7, vec![(Comment(" hex numbers"), 0..13)]),
        (8, vec![(Integer(0x1), 0..3), (Integer(0xA), 4..7), (Integer(0xa), 8..11)]),
        (9, vec![(Integer(0xFFFFFFFF), 0..10)]),
        (10, vec![(Integer(0xFFFFFFFFFFFFFFFFu64 as IntegerValue), 0..18)]),
        (11, vec![(Comment(" invalid (too big for 64 bits)"), 0..31)]),
        (12, vec![(
            TokenValue::Error(DiagnosticKind::IntegerParseError(
                IntegerValue::from_str_radix("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).unwrap_err(),
            )),
            0..34,
        )]),
        (13, vec![(Comment(" invalid (lexed as symbols)"), 0..28)]),
        (14, vec![(Symbol("0x"), 0..2), (Symbol("x1"), 5..7)]),
        (15, vec![(Symbol("+0x2"), 0..4), (Symbol("-0x3"), 5..9)]),
        (16, vec![(Symbol("+0xB"), 0..4), (Symbol("-0xC"), 5..9)]),
        (17, vec![(Symbol("+0xb"), 0..4), (Symbol("-0xc"), 5..9)]),

        (19, vec![(Comment(" floats"), 0..8)]),
        (20, vec![(Float(1.0), 0..3), (Float(2.0), 7..11), (Float(-3.0), 15..19)]),
        (21, vec![(Float(1.0), 0..2), (Float(2.0), 7..10), (Float(-3.0), 15..18)]),
        (22, vec![(Float(0.1), 0..2), (Float(0.2), 7..10), (Float(-0.3), 15..18)]),
        (23, vec![(Comment(" these are valid"), 0..17)]),
        (24, vec![(Float(0.0), 0..1), (Float(0.0), 7..9), (Float(-0.0), 15..17)]),

        (26, vec![(Comment(" floats with exponents"), 0..23)]),
        (27, vec![(Comment(" valid                 -  invalid"), 0..34)]),
        (28, vec![(Float(1.0E1), 0..5),  (Float(2.0E1), 7..13),  (Float(-3.0E1), 15..21),      (Symbol("1.0-E1"), 27..33),  (Symbol("+2.0-E1"), 35..42),  (Symbol("-3.0-E1"), 44..51)]),
        (29, vec![(Float(1.0E+1), 0..6), (Float(2.0E+1), 7..14), (Float(-3.0E+1), 15..22),     (Symbol("1.0-E+1"), 27..34), (Symbol("+2.0-E+1"), 35..43), (Symbol("-3.0-E+1"), 44..52)]),
        (30, vec![(Float(1.0E-1), 0..6), (Float(2.0E-1), 7..14), (Float(-3.0E-1), 15..22),     (Symbol("1.0-E-1"), 27..34), (Symbol("+2.0-E-1"), 35..43), (Symbol("-3.0-E-1"), 44..52)]),

        (32, vec![(Float(1.0E1), 0..4),  (Float(2.0E1), 7..12),  (Float(-3.0E1), 15..20),      (Symbol("1.-E1"), 27..32),   (Symbol("+2.-E1"), 35..41),   (Symbol("-3.-E1"), 44..50)]),
        (33, vec![(Float(1.0E+1), 0..5), (Float(2.0E+1), 7..13), (Float(-3.0E+1), 15..21),     (Symbol("1.-E+1"), 27..33),  (Symbol("+2.-E+1"), 35..42),  (Symbol("-3.-E+1"), 44..51)]),
        (34, vec![(Float(1.0E-1), 0..5), (Float(2.0E-1), 7..13), (Float(-3.0E-1), 15..21),     (Symbol("1.-E-1"), 27..33),  (Symbol("+2.-E-1"), 35..42),  (Symbol("-3.-E-1"), 44..51)]),

        (36, vec![(Float(0.1E1), 0..4),  (Float(0.2E1), 7..12),  (Float(-0.3E1), 15..20),      (Symbol(".1-E1"), 27..32),   (Symbol("+.2-E1"), 35..41),   (Symbol("-.3-E1"), 44..50)]),
        (37, vec![(Float(0.1E+1), 0..5), (Float(0.2E+1), 7..13), (Float(-0.3E+1), 15..21),     (Symbol(".1-E+1"), 27..33),  (Symbol("+.2-E+1"), 35..42),  (Symbol("-.3-E+1"), 44..51)]),
        (38, vec![(Float(0.1E-1), 0..5), (Float(0.2E-1), 7..13), (Float(-0.3E-1), 15..21),     (Symbol(".1-E-1"), 27..33),  (Symbol("+.2-E-1"), 35..42),  (Symbol("-.3-E-1"), 44..51)]),

        (40, vec![(Float(0.0E1), 0..3),  (Float(0.0E1), 7..11),  (Float(-0.0E1), 15..19),      (Symbol(".-E1"), 27..31),    (Symbol("+.-E1"), 35..40),    (Symbol("-.-E1"), 44..49)]),
        (41, vec![(Float(0.0E+1), 0..4), (Float(0.0E+1), 7..12), (Float(-0.0E+1), 15..20),     (Symbol(".-E+1"), 27..32),   (Symbol("+.-E+1"), 35..41),   (Symbol("-.-E+1"), 44..50)]),
        (42, vec![(Float(0.0E-1), 0..4), (Float(0.0E-1), 7..12), (Float(-0.0E-1), 15..20),     (Symbol(".-E-1"), 27..32),   (Symbol("+.-E-1"), 35..41),   (Symbol("-.-E-1"), 44..50)]),

        (44, vec![(Comment(" strings"), 0..9)]),
        (45, vec![(String("asdf"), 0..6)]),
        (46, vec![(String(""), 0..2), (String(""), 3..5)]),

        (48, vec![(String(
            "\n\
            asdf\n\
            jkl\n\
            qwerty\
            \n"
        ), 0..19)]),


        (55, vec![(Comment(" symbols"), 0..9)]),
        (56, vec![(Symbol("asdf"), 0..4)]),
        (57, vec![(Symbol("jkl"), 0..3)]),
        (58, vec![(Symbol("qwerty"), 0..6)]),

        (60, vec![(Comment(" quoted symbols"), 0..16)]),
        (61, vec![(Symbol("asdf"), 0..6)]),
        (62, vec![(Symbol(""), 0..2), (Symbol(""), 3..5)]),

        (64, vec![(Symbol(
            "\n\
            asdf\n\
            jkl\n\
            qwerty\
            \n"
        ), 0..19)]),

        (70, vec![(Comment(" variables"), 0..11)]),
        (71, vec![(Variable("asdf"), 0..5)]),
        (72, vec![(Variable("jkl"), 0..4)]),
        (73, vec![(Variable("qwerty"), 0..7)]),

        (75, vec![(Comment(" kDataUnhandled is its own token"), 0..33)]),
        (76, vec![(Unhandled, 0..14)]),

        (79, vec![(Comment(" arrays"), 0..8)]),
        (80, vec![(ArrayOpen, 0..1), (Symbol("array"), 1..6), (Integer(1), 7..8), (Integer(2), 9..10), (ArrayClose, 10..11), (Comment(" array"), 13..20)]),
        (81, vec![(CommandOpen, 0..1), (Symbol("+"), 1..2), (Integer(1), 3..4), (Integer(2), 5..6), (CommandClose, 6..7), (Comment(" command"), 13..22)]),
        (82, vec![(PropertyOpen, 0..1), (Symbol("property"), 1..9), (PropertyClose, 9..10), (Comment(" property"), 13..23)]),

        (85, vec![(Comment(" directives"), 0..12)]),
        (86, vec![(IncludeOptional, 0..12), (Symbol("../file.dta"), 13..24)]),
        (87, vec![(Include, 0..8), (Symbol("../file.dta"), 9..20)]),
        (88, vec![(Merge, 0..6), (Symbol("../file.dta"), 7..18)]),
        (89, vec![(Ifdef, 0..6), (Symbol("kDefine"), 7..14)]),
        (90, vec![(Undefine, 0..6), (Symbol("kDefine"), 7..14)]),
        (91, vec![(Endif, 0..6)]),
        (92, vec![(Ifndef, 0..7), (Symbol("kDefine"), 8..15)]),
        (93, vec![(Define, 0..7), (Symbol("kDefine"), 8..15), (ArrayOpen, 16..17), (Integer(1), 17..18), (ArrayClose, 18..19)]),
        (94, vec![(Else, 0..5)]),
        (95, vec![(Autorun, 0..8), (CommandOpen, 9..10), (Symbol("action"), 10..16), (CommandClose, 16..17)]),
        (96, vec![(Endif, 0..6)]),
        (97, vec![(Comment(" invalid"), 0..9)]),
        (98, vec![(Error(DiagnosticKind::BadDirective), 0..4)]),
        (99, vec![(Error(DiagnosticKind::BadDirective), 0..2)]),

        (101, vec![(Comment(" *not* directives, these are lexed as symbols"), 0..46)]),
        (102, vec![(Symbol("#"), 0..1)]),
        (103, vec![(Symbol("#"), 0..1), (Symbol("#"), 2..3), (Comment(" space-separated"), 4..21)]),
        (104, vec![(Symbol("#"), 0..1), (Symbol("#"), 2..3), (Comment(" tab-separated"), 4..19)]),
        (105, vec![(Comment(" lexed as symbols and arrays"), 0..29)]),
        (106, vec![(Symbol("#"), 0..1), (ArrayOpen, 1..2), (Symbol("#"), 2..3), (ArrayClose, 3..4), (Comment(" '#', '(', '#', ')'"), 5..25)]),
        (107, vec![(Symbol("#"), 0..1), (CommandOpen, 1..2), (Symbol("#"), 2..3), (CommandClose, 3..4), (Comment(" '#', '{', '#', '}'"), 5..25)]),
        (108, vec![(Symbol("#"), 0..1), (PropertyOpen, 1..2), (Symbol("#"), 2..3), (PropertyClose, 3..4), (Comment(" '#', '[', '#', ']'"), 5..25)]),

        (111, vec![(Comment(" line comment"), 0..14)]),
        (112, vec![(Comment(";"), 0..2)]),
        (113, vec![(Comment(" ;"), 0..3)]),
        (114, vec![(Comment("	;"), 0..3)]),
        (115, vec![(Comment(";;;;;;;"), 0..8)]),
        (116, vec![(Comment("nospace"), 0..8)]),
        (117, vec![(Symbol("asdf;jkl"), 0..8), (Comment(" invalid, lexed as part of the symbol"), 9..47)]),

        (119, vec![(BlockComment(
            "/*\n\
            block comment\n\
            */"
        ), 0..19)]),

        (123, vec![(Symbol("/*asdf*/"), 0..8), (Comment(" invalid, lexed as a symbol"), 9..37)]),
        (124, vec![(BlockComment("/*jkl */"), 0..8)]),

        (126, vec![(Symbol("/**/"), 0..4), (Comment(" invalid, lexed as a symbol"), 5..33)]),
        (127, vec![(BlockComment("/* */"), 0..5)]),
        (128, vec![(BlockComment("/*\t*/"), 0..5)]),

        (130, vec![(Comment(" stray block-comment close, lexed as a symbol"), 0..46)]),
        (131, vec![(Symbol("*/"), 0..2)]),

        (133, vec![(Symbol("/*****/"), 0..7), (Comment(" invalid, lexed as a symbol"), 8..36)]),

        (135, vec![(Symbol("/*****"), 0..6), (Comment(" invalid, lexed as a symbol"), 7..35)]),
        (137, vec![(BlockComment(
            "/*\
            \n\
            \n    *****\n\
            \n\
            ***/"
        ), 4..23)]),

        (143, vec![(Comment(" comments between directives"), 0..29)]),
        (144, vec![(IncludeOptional, 0..12), (Comment(" asdf"), 13..19)]),
        (145, vec![(Symbol("../file.dta"), 4..15), (Comment(" asdf"), 16..22)]),
        (146, vec![(Include, 0..8), (Comment(" asdf"), 9..15)]),
        (147, vec![(Symbol("../file.dta"), 4..15), (Comment(" asdf"), 16..22)]),
        (148, vec![(Merge, 0..6), (Comment(" asdf"), 7..13)]),
        (149, vec![(Symbol("../file.dta"), 4..15), (Comment(" asdf"), 16..22)]),
        (150, vec![(Ifdef, 0..6), (Comment(" asdf"), 7..13)]),
        (151, vec![(Symbol("kDefine"), 4..11), (Comment(" asdf"), 12..18)]),
        (152, vec![(Undefine, 0..6), (Comment(" asdf"), 7..13)]),
        (153, vec![(Symbol("kDefine"), 4..11), (Comment(" asdf"), 12..18)]),
        (154, vec![(Endif, 0..6), (Comment(" asdf"), 7..13)]),
        (155, vec![(Ifndef, 0..7), (Comment(" asdf"), 8..14)]),
        (156, vec![(Symbol("kDefine"), 4..11), (Comment(" asdf"), 12..18)]),
        (157, vec![(Define, 0..7), (Comment(" asdf"), 8..14)]),
        (158, vec![(Symbol("kDefine"), 4..11), (Comment(" asdf"), 12..18)]),
        (159, vec![(ArrayOpen, 4..5), (Integer(1), 5..6), (ArrayClose, 6..7), (Comment(" asdf"), 8..14)]),
        (160, vec![(Else, 0..5), (Comment(" asdf"), 6..12)]),
        (161, vec![(Autorun, 0..8), (Comment(" asdf"), 9..15)]),
        (162, vec![(CommandOpen, 4..5), (Symbol("action"), 5..11), (CommandClose, 11..12), (Comment(" asdf"), 13..19)]),
        (163, vec![(Endif, 0..6), (Comment(" asdf"), 7..13)]),

        (165, vec![(Comment(" block comments between directives"), 0..35)]),
        (166, vec![(BlockComment("/* asdf */"), 0..10), (IncludeOptional, 11..23), (BlockComment("/* asdf */"), 24..34), (Symbol("../file.dta"), 35..46), (BlockComment("/* asdf */"), 47..57)]),
        (167, vec![(BlockComment("/* asdf */"), 0..10), (Include, 11..19), (BlockComment("/* asdf */"), 20..30), (Symbol("../file.dta"), 31..42), (BlockComment("/* asdf */"), 43..53)]),
        (168, vec![(BlockComment("/* asdf */"), 0..10), (Merge, 11..17), (BlockComment("/* asdf */"), 18..28), (Symbol("../file.dta"), 29..40), (BlockComment("/* asdf */"), 41..51)]),
        (169, vec![(BlockComment("/* asdf */"), 0..10), (Ifdef, 11..17), (BlockComment("/* asdf */"), 18..28), (Symbol("kDefine"), 29..36), (BlockComment("/* asdf */"), 37..47)]),
        (170, vec![(BlockComment("/* asdf */"), 0..10), (Undefine, 11..17), (BlockComment("/* asdf */"), 18..28), (Symbol("kDefine"), 29..36), (BlockComment("/* asdf */"), 37..47)]),
        (171, vec![(BlockComment("/* asdf */"), 0..10), (Endif, 11..17), (BlockComment("/* asdf */"), 18..28)]),
        (172, vec![(BlockComment("/* asdf */"), 0..10), (Ifndef, 11..18), (BlockComment("/* asdf */"), 19..29), (Symbol("kDefine"), 30..37), (BlockComment("/* asdf */"), 38..48)]),
        (173, vec![(BlockComment("/* asdf */"), 0..10), (Define, 11..18), (BlockComment("/* asdf */"), 19..29), (Symbol("kDefine"), 30..37), (BlockComment("/* asdf */"), 38..48), (ArrayOpen, 49..50), (Integer(1), 50..51), (ArrayClose, 51..52), (BlockComment("/* asdf */"), 53..63)]),
        (174, vec![(BlockComment("/* asdf */"), 0..10), (Else, 11..16), (BlockComment("/* asdf */"), 17..27)]),
        (175, vec![(BlockComment("/* asdf */"), 0..10), (Autorun, 11..19), (BlockComment("/* asdf */"), 20..30), (CommandOpen, 31..32), (Symbol("action"), 32..38), (CommandClose, 38..39), (BlockComment("/* asdf */"), 40..50)]),
        (176, vec![(BlockComment("/* asdf */"), 0..10), (Endif, 11..17), (BlockComment("/* asdf */"), 18..28)]),
    ];

    // Apply proper locations to tokens
    let tokens = {
        // Collect lines by index and their locations in the original text
        let line_locations = {
            let mut lines = vec![];
            let mut location = 0;
            for line in text.lines() {
                lines.push(location);
                location += line.len() + 1; // + 1 for \n
            }

            lines
        };

        let mut adjusted_tokens = vec![];
        for (line_number, token_line) in tokens {
            let location = line_locations[line_number - 1];
            for token in token_line {
                let new_start = location + token.1.start;
                let new_end = location + token.1.end;
                adjusted_tokens.push((token.0, new_start..new_end))
            }
        }

        adjusted_tokens
    };

    let expected = Vec::from_iter(tokens.into_iter().map(|t| Token { value: t.0, location: t.1 }));
    let actual = Tokenizer::new(&text).collect::<Vec<_>>();
    assert_eq!(actual, expected);
}
