// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::Range;

use arson_parse::{BlockCommentToken, DiagnosticKind, IntegerValue, TextToken, Token, TokenValue, Tokenizer};

#[test]
fn thorough() {
    use TokenValue::*;

    let text = include_str!("../test_files/thorough.dta").replace("\r\n", "\n");

    macro_rules! blank {
        () => {
            vec![(BlankLine, -1..1)]
        };
        ($length:literal) => {
            vec![(BlankLine, -1..$length - 1)]
        };
    }

    const fn string(text: &str) -> TokenValue<'_> {
        TokenValue::make_string(text)
    }

    const fn symbol(text: &str) -> TokenValue<'_> {
        TokenValue::make_symbol(text)
    }

    const fn variable(text: &str) -> TokenValue<'_> {
        TokenValue::make_variable(text)
    }

    const fn comment(text: &str) -> TokenValue<'_> {
        TokenValue::make_comment(text)
    }

    const fn block_comment<'src>(
        start: usize,
        open: &'src str,
        body: &'src str,
        close: &'src str,
    ) -> TokenValue<'src> {
        TokenValue::BlockComment(BlockCommentToken::new(start, open, body, close))
    }

    // Format for this behemoth:
    // - each line is its own inner vec
    // - each token is stored as the value + location
    // - token location is relative to the current line, not the whole file
    #[rustfmt::skip] // structure/formatting matches the test file
    let tokens: Vec<(usize, Vec<(TokenValue<'_>, Range<isize>)>)> = vec![
        (1, vec![(comment(" SPDX-License-Identifier: LGPL-3.0-or-later"), 0..44)]),
        (2, blank!()),
        (3, vec![(comment(" integers"), 0..10)]),
        (4, vec![(Integer(1), 0..1), (Integer(2), 2..4), (Integer(-3), 5..7)]),
        (5, vec![(Integer(1), 0..2), (Integer(2), 3..6), (Integer(-3), 7..10)]),
        (6, blank!()),
        (7, vec![(comment(" hex numbers"), 0..13)]),
        (8, vec![(Integer(0x1), 0..3), (Integer(0xA), 4..7), (Integer(0xa), 8..11)]),
        (9, vec![(Integer(0xFFFFFFFF), 0..10)]),
        (10, vec![(Integer(0xFFFFFFFFFFFFFFFFu64 as IntegerValue), 0..18)]),
        (11, vec![(comment(" invalid (too big for 64 bits)"), 0..31)]),
        (12, vec![(
            TokenValue::Error(DiagnosticKind::IntegerParseError(
                IntegerValue::from_str_radix("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).unwrap_err(),
            )),
            0..34,
        )]),
        (13, vec![(comment(" invalid (lexed as symbols)"), 0..28)]),
        (14, vec![(symbol("0x"), 0..2), (symbol("x1"), 5..7)]),
        (15, vec![(symbol("+0x2"), 0..4), (symbol("-0x3"), 5..9)]),
        (16, vec![(symbol("+0xB"), 0..4), (symbol("-0xC"), 5..9)]),
        (17, vec![(symbol("+0xb"), 0..4), (symbol("-0xc"), 5..9)]),
        (18, blank!()),
        (19, vec![(comment(" floats"), 0..8)]),
        (20, vec![(Float(1.0), 0..3), (Float(2.0), 7..11), (Float(-3.0), 15..19)]),
        (21, vec![(Float(1.0), 0..2), (Float(2.0), 7..10), (Float(-3.0), 15..18)]),
        (22, vec![(Float(0.1), 0..2), (Float(0.2), 7..10), (Float(-0.3), 15..18)]),
        (23, vec![(comment(" these are valid"), 0..17)]),
        (24, vec![(Float(0.0), 0..1), (Float(0.0), 7..9), (Float(-0.0), 15..17)]),
        (25, blank!()),
        (26, vec![(comment(" floats with exponents"), 0..23)]),
        (27, vec![(comment(" valid                 -  invalid"), 0..34)]),
        (28, vec![(Float(1.0E1), 0..5),  (Float(2.0E1), 7..13),  (Float(-3.0E1), 15..21),      (symbol("1.0-E1"), 27..33),  (symbol("+2.0-E1"), 35..42),  (symbol("-3.0-E1"), 44..51)]),
        (29, vec![(Float(1.0E+1), 0..6), (Float(2.0E+1), 7..14), (Float(-3.0E+1), 15..22),     (symbol("1.0-E+1"), 27..34), (symbol("+2.0-E+1"), 35..43), (symbol("-3.0-E+1"), 44..52)]),
        (30, vec![(Float(1.0E-1), 0..6), (Float(2.0E-1), 7..14), (Float(-3.0E-1), 15..22),     (symbol("1.0-E-1"), 27..34), (symbol("+2.0-E-1"), 35..43), (symbol("-3.0-E-1"), 44..52)]),
        (31, blank!()),
        (32, vec![(Float(1.0E1), 0..4),  (Float(2.0E1), 7..12),  (Float(-3.0E1), 15..20),      (symbol("1.-E1"), 27..32),   (symbol("+2.-E1"), 35..41),   (symbol("-3.-E1"), 44..50)]),
        (33, vec![(Float(1.0E+1), 0..5), (Float(2.0E+1), 7..13), (Float(-3.0E+1), 15..21),     (symbol("1.-E+1"), 27..33),  (symbol("+2.-E+1"), 35..42),  (symbol("-3.-E+1"), 44..51)]),
        (34, vec![(Float(1.0E-1), 0..5), (Float(2.0E-1), 7..13), (Float(-3.0E-1), 15..21),     (symbol("1.-E-1"), 27..33),  (symbol("+2.-E-1"), 35..42),  (symbol("-3.-E-1"), 44..51)]),
        (35, blank!()),
        (36, vec![(Float(0.1E1), 0..4),  (Float(0.2E1), 7..12),  (Float(-0.3E1), 15..20),      (symbol(".1-E1"), 27..32),   (symbol("+.2-E1"), 35..41),   (symbol("-.3-E1"), 44..50)]),
        (37, vec![(Float(0.1E+1), 0..5), (Float(0.2E+1), 7..13), (Float(-0.3E+1), 15..21),     (symbol(".1-E+1"), 27..33),  (symbol("+.2-E+1"), 35..42),  (symbol("-.3-E+1"), 44..51)]),
        (38, vec![(Float(0.1E-1), 0..5), (Float(0.2E-1), 7..13), (Float(-0.3E-1), 15..21),     (symbol(".1-E-1"), 27..33),  (symbol("+.2-E-1"), 35..42),  (symbol("-.3-E-1"), 44..51)]),
        (39, blank!()),
        (40, vec![(Float(0.0E1), 0..3),  (Float(0.0E1), 7..11),  (Float(-0.0E1), 15..19),      (symbol(".-E1"), 27..31),    (symbol("+.-E1"), 35..40),    (symbol("-.-E1"), 44..49)]),
        (41, vec![(Float(0.0E+1), 0..4), (Float(0.0E+1), 7..12), (Float(-0.0E+1), 15..20),     (symbol(".-E+1"), 27..32),   (symbol("+.-E+1"), 35..41),   (symbol("-.-E+1"), 44..50)]),
        (42, vec![(Float(0.0E-1), 0..4), (Float(0.0E-1), 7..12), (Float(-0.0E-1), 15..20),     (symbol(".-E-1"), 27..32),   (symbol("+.-E-1"), 35..41),   (symbol("-.-E-1"), 44..50)]),
        (43, blank!()),
        (44, vec![(comment(" strings"), 0..9)]),
        (45, vec![(string("asdf"), 0..6)]),
        (46, vec![(string(""), 0..2), (string(""), 3..5)]),
        (47, blank!()),
        (48, vec![(string(
            "\n\
            asdf\n\
            jkl\n\
            qwerty\
            \n"
        ), 0..19)]),
        (53, blank!(3)),

        (55, vec![(comment(" symbols"), 0..9)]),
        (56, vec![(symbol("asdf"), 0..4)]),
        (57, vec![(symbol("jkl"), 0..3)]),
        (58, vec![(symbol("qwerty"), 0..6)]),
        (59, blank!()),
        (60, vec![(comment(" quoted symbols"), 0..16)]),
        (61, vec![(symbol("asdf"), 0..6)]),
        (62, vec![(symbol(""), 0..2), (symbol(""), 3..5)]),
        (63, blank!()),
        (64, vec![(symbol(
            "\n\
            asdf\n\
            jkl\n\
            qwerty\
            \n"
        ), 0..19)]),
        (69, blank!()),
        (70, vec![(comment(" variables"), 0..11)]),
        (71, vec![(variable("asdf"), 0..5)]),
        (72, vec![(variable("jkl"), 0..4)]),
        (73, vec![(variable("qwerty"), 0..7)]),
        (74, blank!()),
        (75, vec![(comment(" kDataUnhandled is its own token"), 0..33)]),
        (76, vec![(Unhandled, 0..14)]),
        (77, blank!(3)),

        (79, vec![(comment(" arrays"), 0..8)]),
        (80, vec![(ArrayOpen, 0..1), (symbol("array"), 1..6), (Integer(1), 7..8), (Integer(2), 9..10), (ArrayClose, 10..11), (comment(" array"), 13..20)]),
        (81, vec![(CommandOpen, 0..1), (symbol("+"), 1..2), (Integer(1), 3..4), (Integer(2), 5..6), (CommandClose, 6..7), (comment(" command"), 13..22)]),
        (82, vec![(PropertyOpen, 0..1), (symbol("property"), 1..9), (PropertyClose, 9..10), (comment(" property"), 13..23)]),
        (83, blank!(3)),

        (85, vec![(comment(" directives"), 0..12)]),
        (86, vec![(IncludeOptional, 0..12), (symbol("../file.dta"), 13..24)]),
        (87, vec![(Include, 0..8), (symbol("../file.dta"), 9..20)]),
        (88, vec![(Merge, 0..6), (symbol("../file.dta"), 7..18)]),
        (89, vec![(Ifdef, 0..6), (symbol("kDefine"), 7..14)]),
        (90, vec![(Undefine, 0..6), (symbol("kDefine"), 7..14)]),
        (91, vec![(Endif, 0..6)]),
        (92, vec![(Ifndef, 0..7), (symbol("kDefine"), 8..15)]),
        (93, vec![(Define, 0..7), (symbol("kDefine"), 8..15), (ArrayOpen, 16..17), (Integer(1), 17..18), (ArrayClose, 18..19)]),
        (94, vec![(Else, 0..5)]),
        (95, vec![(Autorun, 0..8), (CommandOpen, 9..10), (symbol("action"), 10..16), (CommandClose, 16..17)]),
        (96, vec![(Endif, 0..6)]),
        (97, vec![(comment(" invalid"), 0..9)]),
        (98, vec![(Error(DiagnosticKind::BadDirective), 0..4)]),
        (99, vec![(Error(DiagnosticKind::BadDirective), 0..2)]),
        (100, blank!()),
        (101, vec![(comment(" *not* directives, these are lexed as symbols"), 0..46)]),
        (102, vec![(symbol("#"), 0..1)]),
        (103, vec![(symbol("#"), 0..1), (symbol("#"), 2..3), (comment(" space-separated"), 4..21)]),
        (104, vec![(symbol("#"), 0..1), (symbol("#"), 2..3), (comment(" tab-separated"), 4..19)]),
        (105, vec![(comment(" lexed as symbols and arrays"), 0..29)]),
        (106, vec![(symbol("#"), 0..1), (ArrayOpen, 1..2), (symbol("#"), 2..3), (ArrayClose, 3..4), (comment(" '#', '(', '#', ')'"), 5..25)]),
        (107, vec![(symbol("#"), 0..1), (CommandOpen, 1..2), (symbol("#"), 2..3), (CommandClose, 3..4), (comment(" '#', '{', '#', '}'"), 5..25)]),
        (108, vec![(symbol("#"), 0..1), (PropertyOpen, 1..2), (symbol("#"), 2..3), (PropertyClose, 3..4), (comment(" '#', '[', '#', ']'"), 5..25)]),
        (109, blank!(3)),

        (111, vec![(comment(" line comment"), 0..14)]),
        (112, vec![(comment(";"), 0..2)]),
        (113, vec![(comment(" ;"), 0..3)]),
        (114, vec![(comment("	;"), 0..3)]),
        (115, vec![(comment(";;;;;;;"), 0..8)]),
        (116, vec![(comment("nospace"), 0..8)]),
        (117, vec![(symbol("asdf;jkl"), 0..8), (comment(" invalid, lexed as part of the symbol"), 9..47)]),
        (118, blank!()),
        (119, vec![(block_comment(
            0,
            "/*",
            "\
           \nblock comment\
           \n",
            "*/",
        ), 0..19)]),
        (122, blank!()),
        (123, vec![(symbol("/*asdf*/"), 0..8), (comment(" invalid, lexed as a symbol"), 9..37)]),
        (124, vec![(block_comment(0, "/*", "jkl ", "*/"), 0..8)]),
        (125, blank!()),
        (126, vec![(symbol("/**/"), 0..4), (comment(" invalid, lexed as a symbol"), 5..33)]),
        (127, vec![(block_comment(0, "/*", " ", "*/"), 0..5 )]),
        (128, vec![(block_comment(0, "/*", "\t", "*/"), 0..5)]),
        (129, blank!()),
        (130, vec![(comment(" stray block-comment close, lexed as a symbol"), 0..46)]),
        (131, vec![(symbol("*/"), 0..2)]),
        (132, blank!()),
        (133, vec![(symbol("/*****/"), 0..7), (comment(" invalid, lexed as a symbol"), 8..36)]),
        (134, blank!()),
        (135, vec![(symbol("/*****"), 0..6), (comment(" invalid, lexed as a symbol"), 7..35)]),
        (136, blank!(6)),
        (137, vec![(block_comment(
            4,
            "/*",
            "\
           \n\
           \n    *****\
           \n\
           \n",
            "***/",
        ),  4..23)]),
        (142, blank!()),
        (143, vec![(comment(" comments between directives"), 0..29)]),
        (144, vec![(IncludeOptional, 0..12), (comment(" asdf"), 13..19)]),
        (145, vec![(symbol("../file.dta"), 4..15), (comment(" asdf"), 16..22)]),
        (146, vec![(Include, 0..8), (comment(" asdf"), 9..15)]),
        (147, vec![(symbol("../file.dta"), 4..15), (comment(" asdf"), 16..22)]),
        (148, vec![(Merge, 0..6), (comment(" asdf"), 7..13)]),
        (149, vec![(symbol("../file.dta"), 4..15), (comment(" asdf"), 16..22)]),
        (150, vec![(Ifdef, 0..6), (comment(" asdf"), 7..13)]),
        (151, vec![(symbol("kDefine"), 4..11), (comment(" asdf"), 12..18)]),
        (152, vec![(Undefine, 0..6), (comment(" asdf"), 7..13)]),
        (153, vec![(symbol("kDefine"), 4..11), (comment(" asdf"), 12..18)]),
        (154, vec![(Endif, 0..6), (comment(" asdf"), 7..13)]),
        (155, vec![(Ifndef, 0..7), (comment(" asdf"), 8..14)]),
        (156, vec![(symbol("kDefine"), 4..11), (comment(" asdf"), 12..18)]),
        (157, vec![(Define, 0..7), (comment(" asdf"), 8..14)]),
        (158, vec![(symbol("kDefine"), 4..11), (comment(" asdf"), 12..18)]),
        (159, vec![(ArrayOpen, 4..5), (Integer(1), 5..6), (ArrayClose, 6..7), (comment(" asdf"), 8..14)]),
        (160, vec![(Else, 0..5), (comment(" asdf"), 6..12)]),
        (161, vec![(Autorun, 0..8), (comment(" asdf"), 9..15)]),
        (162, vec![(CommandOpen, 4..5), (symbol("action"), 5..11), (CommandClose, 11..12), (comment(" asdf"), 13..19)]),
        (163, vec![(Endif, 0..6), (comment(" asdf"), 7..13)]),
        (164, blank!()),
        (165, vec![(comment(" block comments between directives"), 0..35)]),
        (166, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (IncludeOptional, 11..23), (block_comment(24, "/*", " asdf ", "*/"), 24..34), (symbol("../file.dta"), 35..46), (block_comment(47, "/*", " asdf ", "*/"), 47..57)]),
        (167, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Include, 11..19), (block_comment(20, "/*", " asdf ", "*/"), 20..30), (symbol("../file.dta"), 31..42), (block_comment(43, "/*", " asdf ", "*/"), 43..53)]),
        (168, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Merge, 11..17), (block_comment(18, "/*", " asdf ", "*/"), 18..28), (symbol("../file.dta"), 29..40), (block_comment(41, "/*", " asdf ", "*/"), 41..51)]),
        (169, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Ifdef, 11..17), (block_comment(18, "/*", " asdf ", "*/"), 18..28), (symbol("kDefine"), 29..36), (block_comment(37, "/*", " asdf ", "*/"), 37..47)]),
        (170, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Undefine, 11..17), (block_comment(18, "/*", " asdf ", "*/"), 18..28), (symbol("kDefine"), 29..36), (block_comment(37, "/*", " asdf ", "*/"), 37..47)]),
        (171, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Endif, 11..17), (block_comment(18, "/*", " asdf ", "*/"), 18..28)]),
        (172, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Ifndef, 11..18), (block_comment(19, "/*", " asdf ", "*/"), 19..29), (symbol("kDefine"), 30..37), (block_comment(38, "/*", " asdf ", "*/"), 38..48)]),
        (173, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Define, 11..18), (block_comment(19, "/*", " asdf ", "*/"), 19..29), (symbol("kDefine"), 30..37), (block_comment(38, "/*", " asdf ", "*/"), 38..48), (ArrayOpen, 49..50), (Integer(1), 50..51), (ArrayClose, 51..52), (block_comment(53, "/*", " asdf ", "*/"), 53..63)]),
        (174, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Else, 11..16), (block_comment(17, "/*", " asdf ", "*/"), 17..27)]),
        (175, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Autorun, 11..19), (block_comment(20, "/*", " asdf ", "*/"), 20..30), (CommandOpen, 31..32), (symbol("action"), 32..38), (CommandClose, 38..39), (block_comment(40, "/*", " asdf ", "*/"), 40..50)]),
        (176, vec![(block_comment(0, "/*", " asdf ", "*/"), 0..10), (Endif, 11..17), (block_comment(18, "/*", " asdf ", "*/"), 18..28)]),
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

        fn relocate_u(offset: usize, location: Range<usize>) -> Range<usize> {
            let start = offset + location.start;
            let end = offset + location.end;
            start..end
        }

        let mut adjusted_tokens = vec![];
        for (line_number, token_line) in tokens {
            let line_location = line_locations[line_number - 1];
            for token in token_line {
                let new_start = (line_location as isize + token.1.start) as usize;
                let new_end = (line_location as isize + token.1.end) as usize;

                let value = match token.0 {
                    BlockComment(BlockCommentToken { open, body, close }) => {
                        BlockComment(BlockCommentToken {
                            open: TextToken::from_cow(open.text, relocate_u(line_location, open.location)),
                            body: TextToken::from_cow(body.text, relocate_u(line_location, body.location)),
                            close: TextToken::from_cow(close.text, relocate_u(line_location, close.location)),
                        })
                    },
                    value => value,
                };

                adjusted_tokens.push((value, new_start..new_end))
            }
        }

        adjusted_tokens
    };

    let expected = Vec::from_iter(tokens.into_iter().map(|t| Token { value: t.0, location: t.1 }));
    let actual = Tokenizer::new(&text).collect::<Vec<_>>();
    assert_eq!(actual, expected);
}
