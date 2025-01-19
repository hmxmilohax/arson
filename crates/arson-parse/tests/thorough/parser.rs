// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::Range;

use arson_parse::{
    ArrayExpression,
    ArrayKind,
    Diagnostic,
    DiagnosticKind,
    DirectiveArgumentDescription,
    Expression,
    ExpressionValue,
    FloatValue,
    IntegerValue,
    StrExpression,
    TokenKind,
};

#[test]
fn thorough() {
    type ThoroughArray<'src> = Vec<(usize, Vec<(ThoroughExpression<'src>, Range<isize>)>)>;
    type ThoroughArrayExpr<'src> = (ThoroughArray<'src>, Range<isize>);
    type ThoroughStrExpr<'src> = (&'src str, Range<isize>);

    enum ThoroughExpression<'src> {
        Integer(IntegerValue),
        Float(FloatValue),
        String(&'src str),

        Symbol(&'src str),
        Variable(&'src str),
        Unhandled,

        Array(ThoroughArray<'src>),
        Command(ThoroughArray<'src>),
        Property(ThoroughArray<'src>),

        Define(ThoroughStrExpr<'src>, ThoroughArrayExpr<'src>),
        Undefine(ThoroughStrExpr<'src>),
        Include(ThoroughStrExpr<'src>),
        IncludeOptional(ThoroughStrExpr<'src>),
        Merge(ThoroughStrExpr<'src>),
        Autorun(ThoroughArrayExpr<'src>),

        Conditional {
            is_positive: bool,
            symbol: ThoroughStrExpr<'src>,
            true_branch: ThoroughArrayExpr<'src>,
            false_branch: Option<ThoroughArrayExpr<'src>>,
        },

        BlankLine,
        Comment(&'src str),
        BlockComment(&'src str),

        Error(DiagnosticKind),
    }

    use ThoroughExpression::*;

    let text = include_str!("../test_files/thorough.dta").replace("\r\n", "\n");

    macro_rules! blank {
        () => {
            vec![(BlankLine, -1..1)]
        };
        ($length:literal) => {
            vec![(BlankLine, -1..$length - 1)]
        };
    }

    // Format for this behemoth:
    // - each line is its own inner vec
    // - each expr is stored as the value + location
    // - expr location is relative to the current line, not the whole file
    #[rustfmt::skip] // structure/formatting matches the test file
    let exprs: ThoroughArray = vec![
        (1, vec![(Comment(" SPDX-License-Identifier: LGPL-3.0-or-later"), 0..44)]),
        (2, blank!()),
        (3, vec![(Comment(" integers"), 0..10)]),
        (4, vec![(Integer(1), 0..1), (Integer(2), 2..4), (Integer(-3), 5..7)]),
        (5, vec![(Integer(1), 0..2), (Integer(2), 3..6), (Integer(-3), 7..10)]),
        (6, blank!()),
        (7, vec![(Comment(" hex numbers"), 0..13)]),
        (8, vec![(Integer(0x1), 0..3), (Integer(0xA), 4..7), (Integer(0xa), 8..11)]),
        (9, vec![(Integer(0xFFFFFFFF), 0..10)]),
        (10, vec![(Integer(0xFFFFFFFFFFFFFFFFu64 as IntegerValue), 0..18)]),
        (11, vec![(Comment(" invalid (too big for 64 bits)"), 0..31)]),
        (12, vec![(
            Error(DiagnosticKind::IntegerParseError(
                IntegerValue::from_str_radix("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).unwrap_err(),
            )),
            0..34,
        )]),
        (13, vec![(Comment(" invalid (lexed as symbols)"), 0..28)]),
        (14, vec![(Symbol("0x"), 0..2), (Symbol("x1"), 5..7)]),
        (15, vec![(Symbol("+0x2"), 0..4), (Symbol("-0x3"), 5..9)]),
        (16, vec![(Symbol("+0xB"), 0..4), (Symbol("-0xC"), 5..9)]),
        (17, vec![(Symbol("+0xb"), 0..4), (Symbol("-0xc"), 5..9)]),
        (18, blank!()),
        (19, vec![(Comment(" floats"), 0..8)]),
        (20, vec![(Float(1.0), 0..3), (Float(2.0), 7..11), (Float(-3.0), 15..19)]),
        (21, vec![(Float(1.0), 0..2), (Float(2.0), 7..10), (Float(-3.0), 15..18)]),
        (22, vec![(Float(0.1), 0..2), (Float(0.2), 7..10), (Float(-0.3), 15..18)]),
        (23, vec![(Comment(" these are valid"), 0..17)]),
        (24, vec![(Float(0.0), 0..1), (Float(0.0), 7..9), (Float(-0.0), 15..17)]),
        (25, blank!()),
        (26, vec![(Comment(" floats with exponents"), 0..23)]),
        (27, vec![(Comment(" valid                 -  invalid"), 0..34)]),
        (28, vec![(Float(1.0E1), 0..5),  (Float(2.0E1), 7..13),  (Float(-3.0E1), 15..21),      (Symbol("1.0-E1"), 27..33),  (Symbol("+2.0-E1"), 35..42),  (Symbol("-3.0-E1"), 44..51)]),
        (29, vec![(Float(1.0E+1), 0..6), (Float(2.0E+1), 7..14), (Float(-3.0E+1), 15..22),     (Symbol("1.0-E+1"), 27..34), (Symbol("+2.0-E+1"), 35..43), (Symbol("-3.0-E+1"), 44..52)]),
        (30, vec![(Float(1.0E-1), 0..6), (Float(2.0E-1), 7..14), (Float(-3.0E-1), 15..22),     (Symbol("1.0-E-1"), 27..34), (Symbol("+2.0-E-1"), 35..43), (Symbol("-3.0-E-1"), 44..52)]),
        (31, blank!()),
        (32, vec![(Float(1.0E1), 0..4),  (Float(2.0E1), 7..12),  (Float(-3.0E1), 15..20),      (Symbol("1.-E1"), 27..32),   (Symbol("+2.-E1"), 35..41),   (Symbol("-3.-E1"), 44..50)]),
        (33, vec![(Float(1.0E+1), 0..5), (Float(2.0E+1), 7..13), (Float(-3.0E+1), 15..21),     (Symbol("1.-E+1"), 27..33),  (Symbol("+2.-E+1"), 35..42),  (Symbol("-3.-E+1"), 44..51)]),
        (34, vec![(Float(1.0E-1), 0..5), (Float(2.0E-1), 7..13), (Float(-3.0E-1), 15..21),     (Symbol("1.-E-1"), 27..33),  (Symbol("+2.-E-1"), 35..42),  (Symbol("-3.-E-1"), 44..51)]),
        (35, blank!()),
        (36, vec![(Float(0.1E1), 0..4),  (Float(0.2E1), 7..12),  (Float(-0.3E1), 15..20),      (Symbol(".1-E1"), 27..32),   (Symbol("+.2-E1"), 35..41),   (Symbol("-.3-E1"), 44..50)]),
        (37, vec![(Float(0.1E+1), 0..5), (Float(0.2E+1), 7..13), (Float(-0.3E+1), 15..21),     (Symbol(".1-E+1"), 27..33),  (Symbol("+.2-E+1"), 35..42),  (Symbol("-.3-E+1"), 44..51)]),
        (38, vec![(Float(0.1E-1), 0..5), (Float(0.2E-1), 7..13), (Float(-0.3E-1), 15..21),     (Symbol(".1-E-1"), 27..33),  (Symbol("+.2-E-1"), 35..42),  (Symbol("-.3-E-1"), 44..51)]),
        (39, blank!()),
        (40, vec![(Float(0.0E1), 0..3),  (Float(0.0E1), 7..11),  (Float(-0.0E1), 15..19),      (Symbol(".-E1"), 27..31),    (Symbol("+.-E1"), 35..40),    (Symbol("-.-E1"), 44..49)]),
        (41, vec![(Float(0.0E+1), 0..4), (Float(0.0E+1), 7..12), (Float(-0.0E+1), 15..20),     (Symbol(".-E+1"), 27..32),   (Symbol("+.-E+1"), 35..41),   (Symbol("-.-E+1"), 44..50)]),
        (42, vec![(Float(0.0E-1), 0..4), (Float(0.0E-1), 7..12), (Float(-0.0E-1), 15..20),     (Symbol(".-E-1"), 27..32),   (Symbol("+.-E-1"), 35..41),   (Symbol("-.-E-1"), 44..50)]),
        (43, blank!()),
        (44, vec![(Comment(" strings"), 0..9)]),
        (45, vec![(String("asdf"), 0..6)]),
        (46, vec![(String(""), 0..2), (String(""), 3..5)]),
        (47, blank!()),
        (48, vec![(String(
            "\n\
            asdf\n\
            jkl\n\
            qwerty\
            \n"
        ), 0..19)]),
        (53, blank!(3)),

        (55, vec![(Comment(" symbols"), 0..9)]),
        (56, vec![(Symbol("asdf"), 0..4)]),
        (57, vec![(Symbol("jkl"), 0..3)]),
        (58, vec![(Symbol("qwerty"), 0..6)]),
        (59, blank!()),
        (60, vec![(Comment(" quoted symbols"), 0..16)]),
        (61, vec![(Symbol("asdf"), 0..6)]),
        (62, vec![(Symbol(""), 0..2), (Symbol(""), 3..5)]),
        (63, blank!()),
        (64, vec![(Symbol(
            "\n\
            asdf\n\
            jkl\n\
            qwerty\
            \n"
        ), 0..19)]),
        (69, blank!()),
        (70, vec![(Comment(" variables"), 0..11)]),
        (71, vec![(Variable("asdf"), 0..5)]),
        (72, vec![(Variable("jkl"), 0..4)]),
        (73, vec![(Variable("qwerty"), 0..7)]),
        (74, blank!()),
        (75, vec![(Comment(" kDataUnhandled is its own token"), 0..33)]),
        (76, vec![(Unhandled, 0..14)]),
        (77, blank!(3)),

        (79, vec![(Comment(" arrays"), 0..8)]),
        (80, vec![(Array(vec![(80, vec![(Symbol("array"), 1..6), (Integer(1), 7..8), (Integer(2), 9..10)])]), 0..11), (Comment(" array"), 13..20)]),
        (81, vec![(Command(vec![(81, vec![(Symbol("+"), 1..2), (Integer(1), 3..4), (Integer(2), 5..6)])]), 0..7), (Comment(" command"), 13..22)]),
        (82, vec![(Property(vec![(82, vec![(Symbol("property"), 1..9)])]), 0..10), (Comment(" property"), 13..23)]),
        (83, blank!(3)),

        (85, vec![(Comment(" directives"), 0..12)]),
        (86, vec![(IncludeOptional(("../file.dta", 13..24)), 0..24)]),
        (87, vec![(Include(("../file.dta", 9..20)), 0..20)]),
        (88, vec![(Merge(("../file.dta", 7..18)), 0..18)]),
        (89, vec![(Conditional {
            is_positive: true,
            symbol: ("kDefine", 7..14),
            true_branch: (vec![
                (90, vec![(Undefine(("kDefine", 7..14)), 0..14)])
            ], 0..36),
            false_branch: None,
        }, 0..36)]),
        (92, vec![(Conditional {
            is_positive: false,
            symbol: ("kDefine", 8..15),
            true_branch: (vec![
                (93, vec![(Define(("kDefine", 8..15), (vec![(93, vec![(Integer(1), 17..18)])], 16..19)), 0..19)])
            ], 0..41),
            false_branch: Some((vec![
                (95, vec![(Autorun((vec![(95, vec![(Symbol("action"), 10..16)])], 9..17)), 0..17)
            ])], 36..66)),
        }, 0..66)]),
        (97, vec![(Comment(" invalid"), 0..9)]),
        (98, vec![(Error(DiagnosticKind::BadDirective), 0..4)]),
        (99, vec![(Error(DiagnosticKind::BadDirective), 0..2)]),
        (100, blank!()),
        (101, vec![(Comment(" *not* directives, these are lexed as symbols"), 0..46)]),
        (102, vec![(Symbol("#"), 0..1)]),
        (103, vec![(Symbol("#"), 0..1), (Symbol("#"), 2..3), (Comment(" space-separated"), 4..21)]),
        (104, vec![(Symbol("#"), 0..1), (Symbol("#"), 2..3), (Comment(" tab-separated"), 4..19)]),
        (105, vec![(Comment(" lexed as symbols and arrays"), 0..29)]),
        (106, vec![(Symbol("#"), 0..1), (Array(vec![(106, vec![(Symbol("#"), 2..3)])]), 1..4), (Comment(" '#', '(', '#', ')'"), 5..25)]),
        (107, vec![(Symbol("#"), 0..1), (Command(vec![(107, vec![(Symbol("#"), 2..3)])]), 1..4), (Comment(" '#', '{', '#', '}'"), 5..25)]),
        (108, vec![(Symbol("#"), 0..1), (Property(vec![(108, vec![(Symbol("#"), 2..3)])]), 1..4), (Comment(" '#', '[', '#', ']'"), 5..25)]),
        (109, blank!(3)),

        (111, vec![(Comment(" line comment"), 0..14)]),
        (112, vec![(Comment(";"), 0..2)]),
        (113, vec![(Comment(" ;"), 0..3)]),
        (114, vec![(Comment("	;"), 0..3)]),
        (115, vec![(Comment(";;;;;;;"), 0..8)]),
        (116, vec![(Comment("nospace"), 0..8)]),
        (117, vec![(Symbol("asdf;jkl"), 0..8), (Comment(" invalid, lexed as part of the symbol"), 9..47)]),
        (118, blank!()),
        (119, vec![(BlockComment(
            "/*\n\
            block comment\n\
            */"
        ), 0..19)]),
        (122, blank!()),
        (123, vec![(Symbol("/*asdf*/"), 0..8), (Comment(" invalid, lexed as a symbol"), 9..37)]),
        (124, vec![(BlockComment("/*jkl */"), 0..8)]),
        (125, blank!()),
        (126, vec![(Symbol("/**/"), 0..4), (Comment(" invalid, lexed as a symbol"), 5..33)]),
        (127, vec![(BlockComment("/* */"), 0..5)]),
        (128, vec![(BlockComment("/*\t*/"), 0..5)]),
        (129, blank!()),
        (130, vec![(Comment(" stray block-comment close, lexed as a symbol"), 0..46)]),
        (131, vec![(Symbol("*/"), 0..2)]),
        (132, blank!()),
        (133, vec![(Symbol("/*****/"), 0..7), (Comment(" invalid, lexed as a symbol"), 8..36)]),
        (134, blank!()),
        (135, vec![(Symbol("/*****"), 0..6), (Comment(" invalid, lexed as a symbol"), 7..35)]),
        (136, blank!(6)),
        (137, vec![(BlockComment(
            "/*\
            \n\
            \n    *****\n\
            \n\
            ***/"
        ), 4..23)]),
        (142, blank!()),
        (143, vec![(Comment(" comments between directives"), 0..29)]),
        (144, vec![(Comment(" asdf"), 13..19), (IncludeOptional(("../file.dta", 24..35)), 0..35), (Comment(" asdf"), 36..42)]),
        (146, vec![(Comment(" asdf"), 9..15), (Include(("../file.dta", 20..31)), 0..31), (Comment(" asdf"), 32..38)]),
        (148, vec![(Comment(" asdf"), 7..13), (Merge(("../file.dta", 18..29)), 0..29), (Comment(" asdf"), 30..36)]),
        (150, vec![
            (Comment(" asdf"), 7..13),
            (Conditional {
                is_positive: true,
                symbol: ("kDefine", 18..25),
                true_branch: (vec![
                    (151, vec![(Comment(" asdf"), 12..18)]),
                    (152, vec![(Comment(" asdf"), 7..13), (Undefine(("kDefine", 18..25)), 0..25)]),
                    (153, vec![(Comment(" asdf"), 12..18)]),
                ], 0..72),
                false_branch: None,
            }, 0..72),
        ]),
        (154, vec![(Comment(" asdf"), 7..13)]),
        (155, vec![
            (Comment(" asdf"), 8..14),
            (Conditional {
                is_positive: false,
                symbol: ("kDefine", 19..26),
                true_branch: (vec![
                    (156, vec![(Comment(" asdf"), 12..18)]),
                    (157, vec![(Comment(" asdf"), 8..14)]),
                    (158, vec![(Comment(" asdf"), 12..18)]),
                    (157, vec![(Define(("kDefine", 19..26), (vec![(159, vec![(Integer(1), 5..6)])], 38..41)), 0..41)]),
                    (159, vec![(Comment(" asdf"), 8..14)]),
                ], 0..88),
                false_branch: Some((vec![
                    (160, vec![(Comment(" asdf"), 6..12)]),
                    (161, vec![(Comment(" asdf"), 9..15), (Autorun((vec![(162, vec![(Symbol("action"), 5..11)])], 20..28)), 0..28)]),
                    (162, vec![(Comment(" asdf"), 13..19)]),
                ], 83..138)),
            }, 0..138),
        ]),
        (163, vec![(Comment(" asdf"), 7..13)]),
        (164, blank!()),
        (165, vec![(Comment(" block comments between directives"), 0..35)]),
        (166, vec![(BlockComment("/* asdf */"), 0..10), (BlockComment("/* asdf */"), 24..34), (IncludeOptional(("../file.dta", 35..46)), 11..46), (BlockComment("/* asdf */"), 47..57)]),
        (167, vec![(BlockComment("/* asdf */"), 0..10), (BlockComment("/* asdf */"), 20..30), (Include(("../file.dta", 31..42)), 11..42), (BlockComment("/* asdf */"), 43..53)]),
        (168, vec![(BlockComment("/* asdf */"), 0..10), (BlockComment("/* asdf */"), 18..28), (Merge(("../file.dta", 29..40)), 11..40), (BlockComment("/* asdf */"), 41..51)]),
        (169, vec![
            (BlockComment("/* asdf */"), 0..10),
            (BlockComment("/* asdf */"), 18..28),
            (Conditional {
                is_positive: true,
                symbol: ("kDefine", 29..36),
                true_branch: (vec![
                    (169, vec![(BlockComment("/* asdf */"), 37..47)]),
                    (170, vec![(BlockComment("/* asdf */"), 0..10), (BlockComment("/* asdf */"), 18..28), (Undefine(("kDefine", 29..36)), 11..36), (BlockComment("/* asdf */"), 37..47)]),
                    (171, vec![(BlockComment("/* asdf */"), 0..10)]),
                ], 11..113),
                false_branch: None,
            }, 11..113),
        ]),
        (171, vec![(BlockComment("/* asdf */"), 18..28)]),
        (172, vec![
            (BlockComment("/* asdf */"), 0..10),
            (BlockComment("/* asdf */"), 19..29),
            (Conditional {
                is_positive: false,
                symbol: ("kDefine", 30..37),
                true_branch: (vec![
                    (172, vec![(BlockComment("/* asdf */"), 38..48)]),
                    (173, vec![(BlockComment("/* asdf */"), 0..10), (BlockComment("/* asdf */"), 19..29), (BlockComment("/* asdf */"), 38..48), (Define(("kDefine", 30..37), (vec![(173, vec![(Integer(1), 50..51)])], 49..52)), 11..52), (BlockComment("/* asdf */"), 53..63)]),
                    (174, vec![(BlockComment("/* asdf */"), 0..10)]),
                ], 11..129),
                false_branch: Some((vec![
                    (174, vec![(BlockComment("/* asdf */"), 17..27)]),
                    (175, vec![(BlockComment("/* asdf */"), 0..10), (BlockComment("/* asdf */"), 20..30), (Autorun((vec![(175, vec![(Symbol("action"), 32..38)])], 31..39)), 11..39), (BlockComment("/* asdf */"), 40..50)]),
                    (176, vec![(BlockComment("/* asdf */"), 0..10)]),
                ], 124..209)),
            }, 11..209),
        ]),
        (176, vec![(BlockComment("/* asdf */"), 18..28)]),
    ];

    // Apply proper locations to expressions
    fn relocate_exprs<'src>(
        text: &str,
        exprs: ThoroughArray<'src>,
    ) -> (Vec<Expression<'src>>, Vec<Diagnostic>) {
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

        fn relocate(offset: usize, location: Range<isize>) -> Range<usize> {
            let start = ((offset as isize) + location.start) as usize;
            let end = ((offset as isize) + location.end) as usize;
            start..end
        }

        fn relocate_str(offset: usize, str: ThoroughStrExpr<'_>) -> StrExpression<'_> {
            let location = relocate(offset, str.1);
            StrExpression::new(str.0, location)
        }

        fn relocate_array<'src>(
            offset: usize,
            array: ThoroughArrayExpr<'src>,
            text: &str,
            diagnostics: &mut Vec<Diagnostic>,
        ) -> ArrayExpression<'src> {
            let (exprs, mut diags) = relocate_exprs(text, array.0);
            diagnostics.append(&mut diags);

            let location = relocate(offset, array.1);
            ArrayExpression::new(exprs, location)
        }

        let mut adjusted_exprs = vec![];
        let mut diagnostics = vec![];
        for (line_number, expr_line) in exprs {
            let line_location = line_locations[line_number - 1];
            for expr in expr_line {
                let new_location = relocate(line_location, expr.1);

                let value = match expr.0 {
                    Integer(value) => ExpressionValue::Integer(value),
                    Float(value) => ExpressionValue::Float(value),
                    String(value) => ExpressionValue::String(value),

                    Symbol(value) => ExpressionValue::Symbol(value),
                    Variable(value) => ExpressionValue::Variable(value),
                    Unhandled => ExpressionValue::Unhandled,

                    Array(exprs) => {
                        let (exprs, mut diags) = relocate_exprs(text, exprs);
                        diagnostics.append(&mut diags);
                        ExpressionValue::Array(exprs)
                    },
                    Command(exprs) => {
                        let (exprs, mut diags) = relocate_exprs(text, exprs);
                        diagnostics.append(&mut diags);
                        ExpressionValue::Command(exprs)
                    },
                    Property(exprs) => {
                        let (exprs, mut diags) = relocate_exprs(text, exprs);
                        diagnostics.append(&mut diags);
                        ExpressionValue::Property(exprs)
                    },

                    Define(name, exprs) => {
                        let name = relocate_str(line_location, name);
                        let exprs = relocate_array(line_location, exprs, text, &mut diagnostics);
                        ExpressionValue::Define(name, exprs)
                    },
                    Undefine(name) => ExpressionValue::Undefine(relocate_str(line_location, name)),
                    Include(name) => ExpressionValue::Include(relocate_str(line_location, name)),
                    IncludeOptional(name) => {
                        ExpressionValue::IncludeOptional(relocate_str(line_location, name))
                    },
                    Merge(name) => ExpressionValue::Merge(relocate_str(line_location, name)),
                    Autorun(exprs) => {
                        let exprs = relocate_array(line_location, exprs, text, &mut diagnostics);
                        ExpressionValue::Autorun(exprs)
                    },

                    Conditional { is_positive, symbol, true_branch, false_branch } => {
                        ExpressionValue::Conditional {
                            is_positive,
                            symbol: relocate_str(line_location, symbol),
                            true_branch: relocate_array(line_location, true_branch, text, &mut diagnostics),
                            false_branch: false_branch
                                .map(|f| relocate_array(line_location, f, text, &mut diagnostics)),
                        }
                    },

                    BlankLine => ExpressionValue::BlankLine,
                    Comment(text) => ExpressionValue::Comment(text),
                    BlockComment(text) => ExpressionValue::BlockComment(text),

                    Error(err) => {
                        diagnostics.push(Diagnostic::new(err, new_location));
                        continue;
                    },
                };
                adjusted_exprs.push(Expression::new(value, new_location))
            }
        }

        (adjusted_exprs, diagnostics)
    }

    let (exprs, expected_diagnostics) = relocate_exprs(&text, exprs);

    let expected = Vec::from_iter(exprs.into_iter().map(Expression::from));
    let (actual, actual_diagnostics) = match arson_parse::parse_text(&text) {
        Ok(ast) => (ast, Vec::new()),
        Err(err) => (err.recovered, err.diagnostics),
    };
    assert_eq!(actual, expected);
    assert_eq!(actual_diagnostics, expected_diagnostics);
}

#[test]
fn thorough_errors() {
    let text = include_str!("../test_files/thorough_errors.dta").replace("\r\n", "\n");

    #[rustfmt::skip] // spacing matches the test file
    let errors = vec![
        (3, vec![(DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: true }, 0..1)]),
        (4, vec![(DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Command, open: true }, 0..1)]),
        (5, vec![(DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Property, open: true }, 0..1)]),

        (7, vec![(
            DiagnosticKind::IntegerParseError(
                IntegerValue::from_str_radix("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16).unwrap_err(),
            ),
            9..43,
        )]),

        (9, vec![(DiagnosticKind::BadDirective, 15..19)]),

        (12, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::MacroName,
                actual: TokenKind::Integer,
                expecting_location: 4..11
            },
            12..13,
        )]),
        (13, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::MacroName,
                actual: TokenKind::Integer,
                expecting_location: 4..10
            },
            11..12,
        )]),
        (14, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::FilePath,
                actual: TokenKind::Integer,
                expecting_location: 4..12
            },
            13..14,
        )]),
        (15, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::FilePath,
                actual: TokenKind::Integer,
                expecting_location: 4..16
            },
            17..18,
        )]),
        (16, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::FilePath,
                actual: TokenKind::Integer,
                expecting_location: 4..10
            },
            11..12,
        )]),

        (18, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::ArrayOpen,
                expected_description: DirectiveArgumentDescription::MacroBody,
                actual: TokenKind::Integer,
                expecting_location: 4..19,
            },
            20..21,
        )]),
        (19, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::CommandOpen,
                expected_description: DirectiveArgumentDescription::CommandBody,
                actual: TokenKind::Symbol,
                expecting_location: 4..12,
            },
            13..19,
        )]),

        (21, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::MacroName,
                actual: TokenKind::Integer,
                expecting_location: 4..10
            },
            11..12,
        )]),
        (22, vec![(
            DiagnosticKind::IncorrectDirectiveArgument {
                expected: TokenKind::Symbol,
                expected_description: DirectiveArgumentDescription::MacroName,
                actual: TokenKind::Integer,
                expecting_location: 4..11
            },
            12..13,
        )]),

        (26, vec![(DiagnosticKind::UnbalancedConditional, 4..48)]),
        (28, vec![(DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: false }, 4..5)]),
        (29, vec![(DiagnosticKind::UnbalancedConditional, 4..40)]),
        (31, vec![(DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: false }, 4..5)]),
        (34, vec![(DiagnosticKind::UnbalancedConditional, 4..50)]),
        (36, vec![(DiagnosticKind::UnmatchedBrace { kind: ArrayKind::Array, open: false }, 4..5)]),

        (41, vec![(DiagnosticKind::UnexpectedConditional, 4..10)]),
        (42, vec![(DiagnosticKind::UnexpectedConditional, 4..9)]),

        (45, vec![(DiagnosticKind::UnmatchedConditional, 4..11)]),
        (46, vec![(DiagnosticKind::UnmatchedConditional, 19..24)]),

        (48, vec![(DiagnosticKind::UnclosedBlockComment, 0..25)]),
    ];

    // Apply proper locations
    let errors = {
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

        let mut adjusted_errors = vec![];
        for (line_number, error_line) in errors {
            let line_location = line_locations[line_number - 1];
            for (kind, location) in error_line {
                let new_start = line_location + location.start;
                let new_end = line_location + location.end;

                let kind = match kind {
                    DiagnosticKind::IncorrectDirectiveArgument {
                        expected,
                        expected_description,
                        actual,
                        expecting_location,
                    } => {
                        let new_start = line_location + expecting_location.start;
                        let new_end = line_location + expecting_location.end;
                        DiagnosticKind::IncorrectDirectiveArgument {
                            expected,
                            expected_description,
                            actual,
                            expecting_location: new_start..new_end,
                        }
                    },
                    _ => kind,
                };

                adjusted_errors.push((kind, new_start..new_end))
            }
        }

        adjusted_errors.push((DiagnosticKind::UnexpectedEof, text.len()..text.len()));
        adjusted_errors
    };

    let expected = Vec::from_iter(errors.into_iter().map(|(k, l)| Diagnostic::new(k, l)));
    let error = match arson_parse::parse_text(&text) {
        Ok(ast) => {
            panic!("Expected parsing errors, got success instead: {ast:?}")
        },
        Err(error) => error,
    };
    assert_eq!(error.diagnostics, expected, "Unexpected result for '{text}'");
}
