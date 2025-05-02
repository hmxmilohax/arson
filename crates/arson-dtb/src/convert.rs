// SPDX-License-Identifier: LGPL-3.0-or-later

use std::borrow::Cow;
use std::num::TryFromIntError;

use arson_parse::{
    ArrayKind,
    BlockCommentToken,
    Expression,
    ExpressionKind,
    ExpressionValue,
    ParseError,
    ParseOptions,
    ParseRecoveryError,
    TextToken,
    TokenValue,
};

use crate::{DataArray, DataNode};

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum DataParseError {
    #[error("failed to parse the given text")]
    Parse(#[from] ParseError),

    #[error("integer value too large")]
    IntegerOutOfRange(#[from] TryFromIntError),

    #[error("expression kind {0:?} cannot be parsed into a DataArray")]
    UnsupportedExpression(ExpressionKind),
}

impl<'src> From<ParseRecoveryError<'src>> for DataParseError {
    fn from(value: ParseRecoveryError<'src>) -> Self {
        Self::Parse(value.into())
    }
}

#[derive(Default, Clone)]
pub struct TokenizeOptions {
    pub line_numbers: bool,
    pub array_ids: bool,
}

impl DataArray {
    pub fn parse(text: &str) -> Result<Self, DataParseError> {
        let parse_options = ParseOptions { include_comments: true };
        let ast = arson_parse::parse_text(text, parse_options)?;
        convert_to_array(text, &ast, &(0..text.len()))
    }

    pub fn parse_with_recovery(text: &str) -> Result<Self, DataParseError> {
        let parse_options = ParseOptions { include_comments: true };
        let ast = match arson_parse::parse_text(text, parse_options) {
            Ok(ast) => ast,
            Err(err) => err.recovered,
        };

        convert_to_array(text, &ast, &(0..text.len()))
    }

    pub fn to_tokens(&self, options: TokenizeOptions) -> Vec<TokenValue<'_>> {
        convert_to_tokens(self, &options)
    }
}

fn convert_to_array(
    source_text: &str,
    ast: &Vec<Expression<'_>>,
    location: &std::ops::Range<usize>,
) -> Result<DataArray, DataParseError> {
    struct ConvertState {
        array_id: usize,
    }

    fn convert_array_inner(
        state: &mut ConvertState,
        ast: &Vec<Expression<'_>>,
        location: &std::ops::Range<usize>,
        lines: &Vec<usize>,
    ) -> Result<DataArray, DataParseError> {
        let line = match lines.binary_search(&location.start) {
            Ok(i) => i + 1,
            Err(i) => i,
        };
        let mut array = DataArray::new(line, state.array_id);
        state.array_id += 1;

        convert_exprs_inner(state, &mut array, ast, lines)?;
        Ok(array)
    }

    fn convert_exprs_inner(
        state: &mut ConvertState,
        array: &mut DataArray,
        ast: &Vec<Expression<'_>>,
        lines: &Vec<usize>,
    ) -> Result<(), DataParseError> {
        for expr in ast {
            let node = match &expr.value {
                ExpressionValue::Integer(value) => DataNode::Integer(i32::try_from(*value)?),
                ExpressionValue::Float(value) => DataNode::Float(*value as f32),
                ExpressionValue::String(value) => DataNode::String(value.as_ref().to_owned()),
                ExpressionValue::Symbol(value) => DataNode::Symbol(value.as_ref().to_owned()),
                ExpressionValue::Variable(value) => DataNode::Variable(value.as_ref().to_owned()),
                ExpressionValue::Unhandled => DataNode::Unhandled,

                ExpressionValue::Array(ast) => {
                    DataNode::Array(convert_array_inner(state, ast, &expr.location, lines)?)
                },
                ExpressionValue::Command(ast) => {
                    DataNode::Command(convert_array_inner(state, ast, &expr.location, lines)?)
                },
                ExpressionValue::Property(ast) => {
                    DataNode::Property(convert_array_inner(state, ast, &expr.location, lines)?)
                },

                ExpressionValue::Define(name, ast) => DataNode::Define(
                    name.text.as_ref().to_owned(),
                    convert_array_inner(state, &ast.exprs, &ast.location, lines)?,
                ),
                ExpressionValue::Undefine(name) => DataNode::Undefine(name.text.as_ref().to_owned()),
                ExpressionValue::Include(path) => DataNode::Include(path.text.as_ref().to_owned()),
                ExpressionValue::IncludeOptional(_) => {
                    return Err(DataParseError::UnsupportedExpression(ExpressionKind::IncludeOptional));
                },
                ExpressionValue::Merge(path) => DataNode::Merge(path.text.as_ref().to_owned()),
                ExpressionValue::Autorun(ast) => {
                    DataNode::Autorun(convert_array_inner(state, &ast.exprs, &ast.location, lines)?)
                },

                ExpressionValue::Conditional { is_positive, symbol, true_branch, false_branch } => {
                    match is_positive {
                        true => array.push(DataNode::Ifdef(symbol.text.as_ref().to_owned())),
                        false => array.push(DataNode::Ifndef(symbol.text.as_ref().to_owned())),
                    };
                    convert_exprs_inner(state, array, &true_branch.exprs, lines)?;

                    if let Some(false_branch) = false_branch {
                        array.push(DataNode::Else);
                        convert_exprs_inner(state, array, &false_branch.exprs, lines)?;
                    }

                    array.push(DataNode::Endif);
                    continue;
                },

                ExpressionValue::BlankLine => continue,
                ExpressionValue::Comment(_) => continue,
                ExpressionValue::BlockComment(_) => continue,
            };
            array.push(node);
        }

        Ok(())
    }

    let mut lines = vec![0];
    for (index, _) in source_text.match_indices('\n') {
        lines.push(index + 1);
    }

    let mut state = ConvertState { array_id: 0 };
    convert_array_inner(&mut state, ast, location, &lines)
}

fn convert_to_tokens<'src>(array: &'src DataArray, options: &TokenizeOptions) -> Vec<TokenValue<'src>> {
    fn symbol_pair<'src>(
        tokens: &mut Vec<TokenValue<'src>>,
        token: TokenValue<'src>,
        name: &'src str,
    ) -> TokenValue<'src> {
        tokens.push(token);
        TokenValue::make_symbol(name)
    }

    fn convert_array<'src>(
        tokens: &mut Vec<TokenValue<'src>>,
        options: &TokenizeOptions,
        kind: ArrayKind,
        array: &'src DataArray,
    ) -> TokenValue<'src> {
        let (open, close) = kind.delimiter_tokens();
        tokens.push(open);

        let mut array_tokens = convert_to_tokens(array, options);

        // Display line/ID info if enabled
        'info_comment: {
            let comment = match (options.line_numbers, options.array_ids) {
                (true, true) => format!("Line: {}, ID: {}", array.line(), array.id()),
                (true, false) => format!("Line: {}", array.line()),
                (false, true) => format!("ID: {}", array.id()),
                (false, false) => break 'info_comment,
            };

            let token = TokenValue::BlockComment(BlockCommentToken {
                open: TextToken::new("/* ", 0..0),
                body: TextToken::from_cow(Cow::Owned(comment), 0..0),
                close: TextToken::new(" */", 0..0),
            });

            array_tokens.insert(0, token);
        }

        tokens.append(&mut array_tokens);
        close
    }

    let mut tokens = Vec::new();

    for node in array {
        let expr = match node {
            DataNode::Integer(value) => TokenValue::Integer(*value as i64),
            DataNode::Float(value) => TokenValue::Float(*value as f64),
            DataNode::Variable(name) => TokenValue::make_variable(name),
            DataNode::Function(_name) => todo!("DataNode::Function"),
            DataNode::Object(_name) => todo!("DataNode::Object"),
            DataNode::Symbol(name) => TokenValue::make_symbol(name),
            DataNode::Unhandled => TokenValue::Unhandled,

            DataNode::Ifdef(name) => symbol_pair(&mut tokens, TokenValue::Ifdef, name),
            DataNode::Else => TokenValue::Else,
            DataNode::Endif => TokenValue::Endif,

            DataNode::Array(body) => convert_array(&mut tokens, options, ArrayKind::Array, body),
            DataNode::Command(body) => convert_array(&mut tokens, options, ArrayKind::Command, body),
            DataNode::String(value) => TokenValue::make_string(value),
            DataNode::Property(body) => convert_array(&mut tokens, options, ArrayKind::Property, body),
            DataNode::Glob(_value) => todo!("DataNode::Glob"),

            DataNode::Define(name, body) => {
                let token = symbol_pair(&mut tokens, TokenValue::Define, name);
                tokens.push(token);
                convert_array(&mut tokens, options, ArrayKind::Array, body)
            },
            DataNode::Include(path) => symbol_pair(&mut tokens, TokenValue::Include, path),
            DataNode::Merge(path) => symbol_pair(&mut tokens, TokenValue::Merge, path),
            DataNode::Ifndef(name) => symbol_pair(&mut tokens, TokenValue::Ifndef, name),
            DataNode::Autorun(body) => {
                tokens.push(TokenValue::Autorun);
                convert_array(&mut tokens, options, ArrayKind::Command, body)
            },
            DataNode::Undefine(name) => symbol_pair(&mut tokens, TokenValue::Undefine, name),
        };
        tokens.push(expr);
    }

    tokens
}

#[cfg(test)]
#[rustfmt::skip] // Lots of deliberate formatting for ease of readability
mod tests {
    use super::*;

    mod parse {
        use super::*;

        fn assert_parsed(text: &str, nodes: Vec<DataNode>) {
            assert_eq!(
                DataArray::parse(text),
                Ok(DataArray::from_nodes(1, 0, nodes)),
                "Unexpected result for '{text}'"
            );
        }

        #[test]
        fn values() {
            assert_parsed("1 2 3", vec![
                DataNode::Integer(1),
                DataNode::Integer(2),
                DataNode::Integer(3),
            ]);
            assert_parsed("1.0 2.0 3.0", vec![
                DataNode::Float(1.0),
                DataNode::Float(2.0),
                DataNode::Float(3.0),
            ]);
            assert_parsed("\"a\" \"b\" \"c\"", vec![
                DataNode::String("a".to_owned()),
                DataNode::String("b".to_owned()),
                DataNode::String("c".to_owned()),
            ]);
            assert_parsed("asdf + '10'", vec![
                DataNode::Symbol("asdf".to_owned()),
                DataNode::Symbol("+".to_owned()),
                DataNode::Symbol("10".to_owned()),
            ]);
            assert_parsed("$asdf $this", vec![
                DataNode::Variable("asdf".to_owned()),
                DataNode::Variable("this".to_owned()),
            ]);
            assert_parsed("kDataUnhandled", vec![DataNode::Unhandled]);
        }

        #[test]
        fn arrays() {
            assert_parsed("(asdf \"text\" 1)", vec![
                DataNode::Array(DataArray::from_nodes(1, 1, vec![
                    DataNode::Symbol("asdf".to_owned()),
                    DataNode::String("text".to_owned()),
                    DataNode::Integer(1),
                ])),
            ]);
            assert_parsed("{set $var \"asdf\"}", vec![
                DataNode::Command(DataArray::from_nodes(1, 1, vec![
                    DataNode::Symbol("set".to_owned()),
                    DataNode::Variable("var".to_owned()),
                    DataNode::String("asdf".to_owned()),
                ])),
            ]);
            assert_parsed("[asdf]", vec![
                DataNode::Property(DataArray::from_nodes(1, 1, vec![
                    DataNode::Symbol("asdf".to_owned()),
                ])),
            ]);

            assert_parsed("(handle {set [var] \"asdf\"})", vec![
                DataNode::Array(DataArray::from_nodes(1, 1, vec![
                    DataNode::Symbol("handle".to_owned()),
                    DataNode::Command(DataArray::from_nodes(1, 2, vec![
                        DataNode::Symbol("set".to_owned()),
                        DataNode::Property(DataArray::from_nodes(1, 3, vec![
                            DataNode::Symbol("var".to_owned()),
                        ])),
                        DataNode::String("asdf".to_owned()),
                    ])),
                ])),
            ]);

            assert_parsed(
                "(sym1 5)\
               \n(sym2\
               \n    (asdf 100)\
               \n    (jkl 250)\
               \n    (1\
               \n        (5 \"foo\")\
               \n        (10 \"baz\")\
               \n    )\
               \n)\
               \n(4 4)",
                vec![
                    DataNode::Array(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("sym1".to_owned()),
                        DataNode::Integer(5),
                    ])),
                    DataNode::Array(DataArray::from_nodes(2, 2, vec![
                        DataNode::Symbol("sym2".to_owned()),
                        DataNode::Array(DataArray::from_nodes(3, 3, vec![
                            DataNode::Symbol("asdf".to_owned()),
                            DataNode::Integer(100),
                        ])),
                        DataNode::Array(DataArray::from_nodes(4, 4, vec![
                            DataNode::Symbol("jkl".to_owned()),
                            DataNode::Integer(250),
                        ])),
                        DataNode::Array(DataArray::from_nodes(5, 5, vec![
                            DataNode::Integer(1),
                            DataNode::Array(DataArray::from_nodes(6, 6, vec![
                                DataNode::Integer(5),
                                DataNode::String("foo".to_owned()),
                            ])),
                            DataNode::Array(DataArray::from_nodes(7, 7, vec![
                                DataNode::Integer(10),
                                DataNode::String("baz".to_owned()),
                            ])),
                        ])),
                    ])),
                    DataNode::Array(DataArray::from_nodes(10, 8, vec![
                        DataNode::Integer(4),
                        DataNode::Integer(4),
                    ])),
                ],
            );
        }

        #[test]
        fn directives() {
            assert_parsed("#ifdef kDefine (array1 10) #else (array2 5) #endif", vec![
                DataNode::Ifdef("kDefine".to_owned()),
                DataNode::Array(DataArray::from_nodes(1, 1, vec![
                    DataNode::Symbol("array1".to_owned()),
                    DataNode::Integer(10),
                ])),
                DataNode::Else,
                DataNode::Array(DataArray::from_nodes(1, 2, vec![
                    DataNode::Symbol("array2".to_owned()),
                    DataNode::Integer(5),
                ])),
                DataNode::Endif,
            ]);
            assert_parsed("#ifndef kDefine (array 10) #endif", vec![
                DataNode::Ifndef("kDefine".to_owned()),
                DataNode::Array(DataArray::from_nodes(1, 1, vec![
                    DataNode::Symbol("array".to_owned()),
                    DataNode::Integer(10),
                ])),
                DataNode::Endif,
            ]);

            assert_parsed("#define kDefine (1)", vec![
                DataNode::Define("kDefine".to_owned(), DataArray::from_nodes(1, 1, vec![
                    DataNode::Integer(1),
                ])),
            ]);
            assert_parsed("#undef kDefine", vec![
                DataNode::Undefine("kDefine".to_owned()),
            ]);
            assert_parsed("#include file.dta", vec![
                DataNode::Include("file.dta".to_owned()),
            ]);
            assert_parsed("#merge file.dta", vec![
                DataNode::Merge("file.dta".to_owned()),
            ]);
            assert_parsed("#autorun {print \"text\"}", vec![
                DataNode::Autorun(DataArray::from_nodes(1, 1, vec![
                    DataNode::Symbol("print".to_owned()),
                    DataNode::String("text".to_owned()),
                ])),
            ]);
        }
    }

    mod tokenize {
        use super::*;

        fn assert_tokenized(nodes: Vec<DataNode>, tokens: Vec<TokenValue<'_>>) {
            assert_tokenized_options(TokenizeOptions::default(), nodes, tokens)
        }

        fn assert_tokenized_options(
            options: TokenizeOptions,
            nodes: Vec<DataNode>,
            tokens: Vec<TokenValue<'_>>,
        ) {
            assert_eq!(DataArray::from_nodes(1, 0, nodes).to_tokens(options), tokens,);
        }

        #[test]
        fn values() {
            // 1 2 3
            assert_tokenized(
                vec![DataNode::Integer(1), DataNode::Integer(2), DataNode::Integer(3)],
                vec![TokenValue::Integer(1), TokenValue::Integer(2), TokenValue::Integer(3)],
            );
            // 1.0 2.0 3.0
            assert_tokenized(
                vec![DataNode::Float(1.0), DataNode::Float(2.0), DataNode::Float(3.0)],
                vec![TokenValue::Float(1.0), TokenValue::Float(2.0), TokenValue::Float(3.0)],
            );
            // "a" "b" "c"
            assert_tokenized(
                vec![
                    DataNode::String("a".to_owned()),
                    DataNode::String("b".to_owned()),
                    DataNode::String("c".to_owned()),
                ],
                vec![
                    TokenValue::make_string("a"),
                    TokenValue::make_string("b"),
                    TokenValue::make_string("c"),
                ],
            );
            // asdf + '10'
            assert_tokenized(
                vec![
                    DataNode::Symbol("asdf".to_owned()),
                    DataNode::Symbol("+".to_owned()),
                    DataNode::Symbol("10".to_owned()),
                ],
                vec![
                    TokenValue::make_symbol("asdf"),
                    TokenValue::make_symbol("+"),
                    TokenValue::make_symbol("10"),
                ],
            );
            // $asdf $this
            assert_tokenized(
                vec![
                    DataNode::Variable("asdf".to_owned()),
                    DataNode::Variable("this".to_owned()),
                ],
                vec![
                    TokenValue::make_variable("asdf"),
                    TokenValue::make_variable("this"),
                ],
            );
            // kDataUnhandled
            assert_tokenized(
                vec![DataNode::Unhandled],
                vec![TokenValue::Unhandled],
            );
        }

        #[test]
        fn arrays() {
            // (asdf "text" 1)
            assert_tokenized(
                vec![
                    DataNode::Array(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("asdf".to_owned()),
                        DataNode::String("text".to_owned()),
                        DataNode::Integer(1),
                    ])),
                ],
                vec![
                    TokenValue::ArrayOpen,
                        TokenValue::make_symbol("asdf"),
                        TokenValue::make_string("text"),
                        TokenValue::Integer(1),
                    TokenValue::ArrayClose,
                ],
            );
            // {set $var "asdf"}
            assert_tokenized(
                vec![
                    DataNode::Command(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("set".to_owned()),
                        DataNode::Variable("var".to_owned()),
                        DataNode::String("asdf".to_owned()),
                    ])),
                ],
                vec![
                    TokenValue::CommandOpen,
                        TokenValue::make_symbol("set"),
                        TokenValue::make_variable("var"),
                        TokenValue::make_string("asdf"),
                    TokenValue::CommandClose,
                ],
            );
            // [asdf]
            assert_tokenized(
                vec![
                    DataNode::Property(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("asdf".to_owned()),
                    ])),
                ],
                vec![
                    TokenValue::PropertyOpen,
                        TokenValue::make_symbol("asdf"),
                    TokenValue::PropertyClose,
                ],
            );

            // (handle {set [var] "asdf"})
            assert_tokenized(
                vec![
                    DataNode::Array(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("handle".to_owned()),
                        DataNode::Command(DataArray::from_nodes(1, 2, vec![
                            DataNode::Symbol("set".to_owned()),
                            DataNode::Property(DataArray::from_nodes(1, 3, vec![
                                DataNode::Symbol("var".to_owned()),
                            ])),
                            DataNode::String("asdf".to_owned()),
                        ])),
                    ])),
                ],
                vec![
                    TokenValue::ArrayOpen,
                        TokenValue::make_symbol("handle"),
                        TokenValue::CommandOpen,
                            TokenValue::make_symbol("set"),
                            TokenValue::PropertyOpen,
                                TokenValue::make_symbol("var"),
                            TokenValue::PropertyClose,
                            TokenValue::make_string("asdf"),
                        TokenValue::CommandClose,
                    TokenValue::ArrayClose,
                ],
            );

            /*
            (sym1 5)
            (sym2
                (asdf 100)
                (jkl 250)
                (1
                    (5 "foo")
                    (10 "baz")
                )
            )
            (4 4)
            */
            assert_tokenized(
                vec![
                    DataNode::Array(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("sym1".to_owned()),
                        DataNode::Integer(5),
                    ])),
                    DataNode::Array(DataArray::from_nodes(2, 2, vec![
                        DataNode::Symbol("sym2".to_owned()),
                        DataNode::Array(DataArray::from_nodes(3, 3, vec![
                            DataNode::Symbol("asdf".to_owned()),
                            DataNode::Integer(100),
                        ])),
                        DataNode::Array(DataArray::from_nodes(4, 4, vec![
                            DataNode::Symbol("jkl".to_owned()),
                            DataNode::Integer(250),
                        ])),
                        DataNode::Array(DataArray::from_nodes(5, 5, vec![
                            DataNode::Integer(1),
                            DataNode::Array(DataArray::from_nodes(6, 6, vec![
                                DataNode::Integer(5),
                                DataNode::String("foo".to_owned()),
                            ])),
                            DataNode::Array(DataArray::from_nodes(7, 7, vec![
                                DataNode::Integer(10),
                                DataNode::String("baz".to_owned()),
                            ])),
                        ])),
                    ])),
                    DataNode::Array(DataArray::from_nodes(10, 8, vec![
                        DataNode::Integer(4),
                        DataNode::Integer(4),
                    ])),
                ],
                vec![
                    TokenValue::ArrayOpen,
                        TokenValue::make_symbol("sym1"),
                        TokenValue::Integer(5),
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::make_symbol("sym2"),
                        TokenValue::ArrayOpen,
                            TokenValue::make_symbol("asdf"),
                            TokenValue::Integer(100),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::make_symbol("jkl"),
                            TokenValue::Integer(250),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::Integer(1),
                            TokenValue::ArrayOpen,
                                TokenValue::Integer(5),
                                TokenValue::make_string("foo"),
                            TokenValue::ArrayClose,
                            TokenValue::ArrayOpen,
                                TokenValue::Integer(10),
                                TokenValue::make_string("baz"),
                            TokenValue::ArrayClose,
                        TokenValue::ArrayClose,
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::Integer(4),
                        TokenValue::Integer(4),
                    TokenValue::ArrayClose,
                ],
            );
        }

        #[test]
        fn array_info() {
            /*
            (sym1 5)
            (sym2
                (asdf 100)
                (jkl 250)
                (1
                    (5 "foo")
                    (10 "baz")
                )
            )
            (4 4)
            */
            let array = vec![
                DataNode::Array(DataArray::from_nodes(1, 1, vec![
                    DataNode::Symbol("sym1".to_owned()),
                    DataNode::Integer(5),
                ])),
                DataNode::Array(DataArray::from_nodes(2, 2, vec![
                    DataNode::Symbol("sym2".to_owned()),
                    DataNode::Array(DataArray::from_nodes(3, 3, vec![
                        DataNode::Symbol("asdf".to_owned()),
                        DataNode::Integer(100),
                    ])),
                    DataNode::Array(DataArray::from_nodes(4, 4, vec![
                        DataNode::Symbol("jkl".to_owned()),
                        DataNode::Integer(250),
                    ])),
                    DataNode::Array(DataArray::from_nodes(5, 5, vec![
                        DataNode::Integer(1),
                        DataNode::Array(DataArray::from_nodes(6, 6, vec![
                            DataNode::Integer(5),
                            DataNode::String("foo".to_owned()),
                        ])),
                        DataNode::Array(DataArray::from_nodes(7, 7, vec![
                            DataNode::Integer(10),
                            DataNode::String("baz".to_owned()),
                        ])),
                    ])),
                ])),
                DataNode::Array(DataArray::from_nodes(10, 8, vec![
                    DataNode::Integer(4),
                    DataNode::Integer(4),
                ])),
            ];

            assert_tokenized_options(
                TokenizeOptions { line_numbers: false, array_ids: false },
                array.clone(),
                vec![
                    TokenValue::ArrayOpen,
                        TokenValue::make_symbol("sym1"),
                        TokenValue::Integer(5),
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::make_symbol("sym2"),
                        TokenValue::ArrayOpen,
                            TokenValue::make_symbol("asdf"),
                            TokenValue::Integer(100),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::make_symbol("jkl"),
                            TokenValue::Integer(250),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::Integer(1),
                            TokenValue::ArrayOpen,
                                TokenValue::Integer(5),
                                TokenValue::make_string("foo"),
                            TokenValue::ArrayClose,
                            TokenValue::ArrayOpen,
                                TokenValue::Integer(10),
                                TokenValue::make_string("baz"),
                            TokenValue::ArrayClose,
                        TokenValue::ArrayClose,
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::Integer(4),
                        TokenValue::Integer(4),
                    TokenValue::ArrayClose,
                ],
            );
            assert_tokenized_options(
                TokenizeOptions { line_numbers: true, array_ids: false },
                array.clone(),
                vec![
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("Line: 1", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::make_symbol("sym1"),
                        TokenValue::Integer(5),
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("Line: 2", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::make_symbol("sym2"),
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("Line: 3", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::make_symbol("asdf"),
                            TokenValue::Integer(100),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("Line: 4", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::make_symbol("jkl"),
                            TokenValue::Integer(250),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("Line: 5", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::Integer(1),
                            TokenValue::ArrayOpen,
                                TokenValue::BlockComment(BlockCommentToken {
                                    open: TextToken::new("/* ", 0..0),
                                    body: TextToken::new("Line: 6", 0..0),
                                    close: TextToken::new(" */", 0..0),
                                }),
                                TokenValue::Integer(5),
                                TokenValue::make_string("foo"),
                            TokenValue::ArrayClose,
                            TokenValue::ArrayOpen,
                                TokenValue::BlockComment(BlockCommentToken {
                                    open: TextToken::new("/* ", 0..0),
                                    body: TextToken::new("Line: 7", 0..0),
                                    close: TextToken::new(" */", 0..0),
                                }),
                                TokenValue::Integer(10),
                                TokenValue::make_string("baz"),
                            TokenValue::ArrayClose,
                        TokenValue::ArrayClose,
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("Line: 10", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::Integer(4),
                        TokenValue::Integer(4),
                    TokenValue::ArrayClose,
                ],
            );
            assert_tokenized_options(
                TokenizeOptions { line_numbers: false, array_ids: true },
                array.clone(),
                vec![
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("ID: 1", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::make_symbol("sym1"),
                        TokenValue::Integer(5),
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("ID: 2", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::make_symbol("sym2"),
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("ID: 3", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::make_symbol("asdf"),
                            TokenValue::Integer(100),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("ID: 4", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::make_symbol("jkl"),
                            TokenValue::Integer(250),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("ID: 5", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::Integer(1),
                            TokenValue::ArrayOpen,
                                TokenValue::BlockComment(BlockCommentToken {
                                    open: TextToken::new("/* ", 0..0),
                                    body: TextToken::new("ID: 6", 0..0),
                                    close: TextToken::new(" */", 0..0),
                                }),
                                TokenValue::Integer(5),
                                TokenValue::make_string("foo"),
                            TokenValue::ArrayClose,
                            TokenValue::ArrayOpen,
                                TokenValue::BlockComment(BlockCommentToken {
                                    open: TextToken::new("/* ", 0..0),
                                    body: TextToken::new("ID: 7", 0..0),
                                    close: TextToken::new(" */", 0..0),
                                }),
                                TokenValue::Integer(10),
                                TokenValue::make_string("baz"),
                            TokenValue::ArrayClose,
                        TokenValue::ArrayClose,
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("ID: 8", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::Integer(4),
                        TokenValue::Integer(4),
                    TokenValue::ArrayClose,
                ],
            );
            assert_tokenized_options(
                TokenizeOptions { line_numbers: true, array_ids: true },
                array.clone(),
                vec![
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("Line: 1, ID: 1", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::make_symbol("sym1"),
                        TokenValue::Integer(5),
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("Line: 2, ID: 2", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::make_symbol("sym2"),
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("Line: 3, ID: 3", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::make_symbol("asdf"),
                            TokenValue::Integer(100),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("Line: 4, ID: 4", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::make_symbol("jkl"),
                            TokenValue::Integer(250),
                        TokenValue::ArrayClose,
                        TokenValue::ArrayOpen,
                            TokenValue::BlockComment(BlockCommentToken {
                                open: TextToken::new("/* ", 0..0),
                                body: TextToken::new("Line: 5, ID: 5", 0..0),
                                close: TextToken::new(" */", 0..0),
                            }),
                            TokenValue::Integer(1),
                            TokenValue::ArrayOpen,
                                TokenValue::BlockComment(BlockCommentToken {
                                    open: TextToken::new("/* ", 0..0),
                                    body: TextToken::new("Line: 6, ID: 6", 0..0),
                                    close: TextToken::new(" */", 0..0),
                                }),
                                TokenValue::Integer(5),
                                TokenValue::make_string("foo"),
                            TokenValue::ArrayClose,
                            TokenValue::ArrayOpen,
                                TokenValue::BlockComment(BlockCommentToken {
                                    open: TextToken::new("/* ", 0..0),
                                    body: TextToken::new("Line: 7, ID: 7", 0..0),
                                    close: TextToken::new(" */", 0..0),
                                }),
                                TokenValue::Integer(10),
                                TokenValue::make_string("baz"),
                            TokenValue::ArrayClose,
                        TokenValue::ArrayClose,
                    TokenValue::ArrayClose,
                    TokenValue::ArrayOpen,
                        TokenValue::BlockComment(BlockCommentToken {
                            open: TextToken::new("/* ", 0..0),
                            body: TextToken::new("Line: 10, ID: 8", 0..0),
                            close: TextToken::new(" */", 0..0),
                        }),
                        TokenValue::Integer(4),
                        TokenValue::Integer(4),
                    TokenValue::ArrayClose,
                ],
            );
        }

        #[test]
        fn directives() {
            // #ifdef kDefine (array1 10) #else (array2 5) #endif
            assert_tokenized(
                vec![
                    DataNode::Ifdef("kDefine".to_owned()),
                    DataNode::Array(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("array1".to_owned()),
                        DataNode::Integer(10),
                    ])),
                    DataNode::Else,
                    DataNode::Array(DataArray::from_nodes(1, 2, vec![
                        DataNode::Symbol("array2".to_owned()),
                        DataNode::Integer(5),
                    ])),
                    DataNode::Endif,
                ],
                vec![
                    TokenValue::Ifdef,
                    TokenValue::make_symbol("kDefine"),
                        TokenValue::ArrayOpen,
                            TokenValue::make_symbol("array1"),
                            TokenValue::Integer(10),
                        TokenValue::ArrayClose,
                    TokenValue::Else,
                        TokenValue::ArrayOpen,
                            TokenValue::make_symbol("array2"),
                            TokenValue::Integer(5),
                        TokenValue::ArrayClose,
                    TokenValue::Endif,
                ],
            );
            // #ifndef kDefine (array 10) #endif
            assert_tokenized(
                vec![
                    DataNode::Ifndef("kDefine".to_owned()),
                    DataNode::Array(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("array".to_owned()),
                        DataNode::Integer(10),
                    ])),
                    DataNode::Endif,
                ],
                vec![
                    TokenValue::Ifndef,
                    TokenValue::make_symbol("kDefine"),
                        TokenValue::ArrayOpen,
                            TokenValue::make_symbol("array"),
                            TokenValue::Integer(10),
                        TokenValue::ArrayClose,
                    TokenValue::Endif,
                ],
            );

            // #define kDefine (1)
            assert_tokenized(
                vec![
                    DataNode::Define("kDefine".to_owned(), DataArray::from_nodes(1, 1, vec![
                        DataNode::Integer(1),
                    ])),
                ],
                vec![
                    TokenValue::Define,
                    TokenValue::make_symbol("kDefine"),
                    TokenValue::ArrayOpen,
                        TokenValue::Integer(1),
                    TokenValue::ArrayClose,
                ],
            );
            // #undef kDefine
            assert_tokenized(
                vec![
                    DataNode::Undefine("kDefine".to_owned()),
                ],
                vec![
                    TokenValue::Undefine,
                    TokenValue::make_symbol("kDefine"),
                ],
            );
            // #include file.dta
            assert_tokenized(
                vec![
                    DataNode::Include("file.dta".to_owned()),
                ],
                vec![
                    TokenValue::Include,
                    TokenValue::make_symbol("file.dta"),
                ],
            );
            // #merge file.dta
            assert_tokenized(
                vec![
                    DataNode::Merge("file.dta".to_owned()),
                ],
                vec![
                    TokenValue::Merge,
                    TokenValue::make_symbol("file.dta"),
                ],
            );
            // #autorun {print "text"}
            assert_tokenized(
                vec![
                    DataNode::Autorun(DataArray::from_nodes(1, 1, vec![
                        DataNode::Symbol("print".to_owned()),
                        DataNode::String("text".to_owned()),
                    ])),
                ],
                vec![
                    TokenValue::Autorun,
                    TokenValue::CommandOpen,
                        TokenValue::make_symbol("print"),
                        TokenValue::make_string("text"),
                    TokenValue::CommandClose,
                ],
            );
        }
    }
}
