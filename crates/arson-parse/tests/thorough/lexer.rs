// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_parse::{IntegerValue, Token, TokenValue, Tokenizer};

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
        (11, vec![(Comment(" invalid (lexed as symbols)"), 0..28)]),
        (12, vec![(Symbol("0x"), 0..2), (Symbol("x1"), 5..7)]),
        (13, vec![(Symbol("+0x2"), 0..4), (Symbol("-0x3"), 5..9)]),
        (14, vec![(Symbol("+0xB"), 0..4), (Symbol("-0xC"), 5..9)]),
        (15, vec![(Symbol("+0xb"), 0..4), (Symbol("-0xc"), 5..9)]),

        (17, vec![(Comment(" floats"), 0..8)]),
        (18, vec![(Float(1.0), 0..3), (Float(2.0), 7..11), (Float(-3.0), 15..19)]),
        (19, vec![(Float(1.0), 0..2), (Float(2.0), 7..10), (Float(-3.0), 15..18)]),
        (20, vec![(Float(0.1), 0..2), (Float(0.2), 7..10), (Float(-0.3), 15..18)]),
        (21, vec![(Comment(" these are valid"), 0..17)]),
        (22, vec![(Float(0.0), 0..1), (Float(0.0), 7..9), (Float(-0.0), 15..17)]),

        (24, vec![(Comment(" floats with exponents"), 0..23)]),
        (25, vec![(Comment(" valid                 -  invalid"), 0..34)]),
        (26, vec![(Float(1.0E1), 0..5),  (Float(2.0E1), 7..13),  (Float(-3.0E1), 15..21),      (Symbol("1.0-E1"), 27..33),  (Symbol("+2.0-E1"), 35..42),  (Symbol("-3.0-E1"), 44..51)]),
        (27, vec![(Float(1.0E+1), 0..6), (Float(2.0E+1), 7..14), (Float(-3.0E+1), 15..22),     (Symbol("1.0-E+1"), 27..34), (Symbol("+2.0-E+1"), 35..43), (Symbol("-3.0-E+1"), 44..52)]),
        (28, vec![(Float(1.0E-1), 0..6), (Float(2.0E-1), 7..14), (Float(-3.0E-1), 15..22),     (Symbol("1.0-E-1"), 27..34), (Symbol("+2.0-E-1"), 35..43), (Symbol("-3.0-E-1"), 44..52)]),

        (30, vec![(Float(1.0E1), 0..4),  (Float(2.0E1), 7..12),  (Float(-3.0E1), 15..20),      (Symbol("1.-E1"), 27..32),   (Symbol("+2.-E1"), 35..41),   (Symbol("-3.-E1"), 44..50)]),
        (31, vec![(Float(1.0E+1), 0..5), (Float(2.0E+1), 7..13), (Float(-3.0E+1), 15..21),     (Symbol("1.-E+1"), 27..33),  (Symbol("+2.-E+1"), 35..42),  (Symbol("-3.-E+1"), 44..51)]),
        (32, vec![(Float(1.0E-1), 0..5), (Float(2.0E-1), 7..13), (Float(-3.0E-1), 15..21),     (Symbol("1.-E-1"), 27..33),  (Symbol("+2.-E-1"), 35..42),  (Symbol("-3.-E-1"), 44..51)]),

        (34, vec![(Float(0.1E1), 0..4),  (Float(0.2E1), 7..12),  (Float(-0.3E1), 15..20),      (Symbol(".1-E1"), 27..32),   (Symbol("+.2-E1"), 35..41),   (Symbol("-.3-E1"), 44..50)]),
        (35, vec![(Float(0.1E+1), 0..5), (Float(0.2E+1), 7..13), (Float(-0.3E+1), 15..21),     (Symbol(".1-E+1"), 27..33),  (Symbol("+.2-E+1"), 35..42),  (Symbol("-.3-E+1"), 44..51)]),
        (36, vec![(Float(0.1E-1), 0..5), (Float(0.2E-1), 7..13), (Float(-0.3E-1), 15..21),     (Symbol(".1-E-1"), 27..33),  (Symbol("+.2-E-1"), 35..42),  (Symbol("-.3-E-1"), 44..51)]),

        (38, vec![(Float(0.0E1), 0..3),  (Float(0.0E1), 7..11),  (Float(-0.0E1), 15..19),      (Symbol(".-E1"), 27..31),    (Symbol("+.-E1"), 35..40),    (Symbol("-.-E1"), 44..49)]),
        (39, vec![(Float(0.0E+1), 0..4), (Float(0.0E+1), 7..12), (Float(-0.0E+1), 15..20),     (Symbol(".-E+1"), 27..32),   (Symbol("+.-E+1"), 35..41),   (Symbol("-.-E+1"), 44..50)]),
        (40, vec![(Float(0.0E-1), 0..4), (Float(0.0E-1), 7..12), (Float(-0.0E-1), 15..20),     (Symbol(".-E-1"), 27..32),   (Symbol("+.-E-1"), 35..41),   (Symbol("-.-E-1"), 44..50)]),

        (42, vec![(Comment(" strings"), 0..9)]),
        (43, vec![(String("asdf"), 0..6)]),
        (44, vec![(String(""), 0..2), (String(""), 3..5)]),

        (46, vec![(String(
            "\n\
            asdf\n\
            jkl\n\
            qwerty\
            \n"
        ), 0..19)]),


        (53, vec![(Comment(" symbols"), 0..9)]),
        (54, vec![(Symbol("asdf"), 0..4)]),
        (55, vec![(Symbol("jkl"), 0..3)]),
        (56, vec![(Symbol("qwerty"), 0..6)]),

        (58, vec![(Comment(" quoted symbols"), 0..16)]),
        (59, vec![(Symbol("asdf"), 0..6)]),
        (60, vec![(Symbol(""), 0..2), (Symbol(""), 3..5)]),

        (62, vec![(Symbol(
            "\n\
            asdf\n\
            jkl\n\
            qwerty\
            \n"
        ), 0..19)]),

        (68, vec![(Comment(" variables"), 0..11)]),
        (69, vec![(Variable("asdf"), 0..5)]),
        (70, vec![(Variable("jkl"), 0..4)]),
        (71, vec![(Variable("qwerty"), 0..7)]),

        (73, vec![(Comment(" kDataUnhandled is its own token"), 0..33)]),
        (74, vec![(Unhandled, 0..14)]),

        (77, vec![(Comment(" arrays"), 0..8)]),
        (78, vec![(ArrayOpen, 0..1), (Symbol("array"), 1..6), (Integer(1), 7..8), (Integer(2), 9..10), (ArrayClose, 10..11), (Comment(" array"), 13..20)]),
        (79, vec![(CommandOpen, 0..1), (Symbol("+"), 1..2), (Integer(1), 3..4), (Integer(2), 5..6), (CommandClose, 6..7), (Comment(" command"), 13..22)]),
        (80, vec![(PropertyOpen, 0..1), (Symbol("property"), 1..9), (PropertyClose, 9..10), (Comment(" property"), 13..23)]),

        (83, vec![(Comment(" directives"), 0..12)]),
        (84, vec![(IncludeOptional, 0..12), (Symbol("../file.dta"), 13..24)]),
        (85, vec![(Include, 0..8), (Symbol("../file.dta"), 9..20)]),
        (86, vec![(Merge, 0..6), (Symbol("../file.dta"), 7..18)]),
        (87, vec![(Ifdef, 0..6), (Symbol("kDefine"), 7..14)]),
        (88, vec![(Undefine, 0..6), (Symbol("kDefine"), 7..14)]),
        (89, vec![(Endif, 0..6)]),
        (90, vec![(Ifndef, 0..7), (Symbol("kDefine"), 8..15)]),
        (91, vec![(Define, 0..7), (Symbol("kDefine"), 8..15)]),
        (92, vec![(Else, 0..5)]),
        (93, vec![(Autorun, 0..8), (CommandOpen, 9..10), (Symbol("action"), 10..16), (CommandClose, 16..17)]),
        (94, vec![(Endif, 0..6)]),
        (95, vec![(Comment(" invalid"), 0..9)]),
        (96, vec![(BadDirective("bad"), 0..4)]),
        (97, vec![(BadDirective("#"), 0..2)]),

        (99, vec![(Comment(" *not* directives, these are lexed as symbols"), 0..46)]),
        (100, vec![(Symbol("#"), 0..1)]),
        (101, vec![(Symbol("#"), 0..1), (Symbol("#"), 2..3), (Comment(" space-separated"), 4..21)]),
        (102, vec![(Symbol("#"), 0..1), (Symbol("#"), 2..3), (Comment(" tab-separated"), 4..19)]),
        (103, vec![(Comment(" lexed as symbols and arrays"), 0..29)]),
        (104, vec![(Symbol("#"), 0..1), (ArrayOpen, 1..2), (Symbol("#"), 2..3), (ArrayClose, 3..4), (Comment(" '#', '(', '#', ')'"), 5..25)]),
        (105, vec![(Symbol("#"), 0..1), (CommandOpen, 1..2), (Symbol("#"), 2..3), (CommandClose, 3..4), (Comment(" '#', '{', '#', '}'"), 5..25)]),
        (106, vec![(Symbol("#"), 0..1), (PropertyOpen, 1..2), (Symbol("#"), 2..3), (PropertyClose, 3..4), (Comment(" '#', '[', '#', ']'"), 5..25)]),

        (109, vec![(Comment(" line comment"), 0..14)]),
        (110, vec![(Comment(";"), 0..2)]),
        (111, vec![(Comment(" ;"), 0..3)]),
        (112, vec![(Comment("	;"), 0..3)]),
        (113, vec![(Comment(";;;;;;;"), 0..8)]),
        (114, vec![(Comment("nospace"), 0..8)]),
        (115, vec![(Symbol("asdf;jkl"), 0..8), (Comment(" invalid, lexed as part of the symbol"), 9..47)]),

        (117, vec![(BlockComment(
            "/*\n\
            block comment\n\
            */"
        ), 0..19)]),

        (121, vec![(Symbol("/*asdf*/"), 0..8), (Comment(" invalid, lexed as a symbol"), 9..37)]),
        (122, vec![(BlockComment("/*jkl */"), 0..8)]),

        (124, vec![(Symbol("/**/"), 0..4), (Comment(" invalid, lexed as a symbol"), 5..33)]),
        (125, vec![(BlockComment("/* */"), 0..5)]),
        (126, vec![(BlockComment("/*\t*/"), 0..5)]),

        (128, vec![(Comment(" stray block-comment close, lexed as a symbol"), 0..46)]),
        (129, vec![(Symbol("*/"), 0..2)]),

        (131, vec![(Symbol("/*****/"), 0..7), (Comment(" invalid, lexed as a symbol"), 8..36)]),

        (133, vec![(Symbol("/*****"), 0..6), (Comment(" invalid, lexed as a symbol"), 7..35)]),
        (135, vec![(BlockComment(
            "/*\
            \n\
            \n    *****\n\
            \n\
            ***/"
        ), 4..23)]),
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
