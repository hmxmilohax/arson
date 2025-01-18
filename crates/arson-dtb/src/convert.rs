// SPDX-License-Identifier: LGPL-3.0-or-later

use std::num::TryFromIntError;

use arson_parse::{
    ArrayKind,
    Expression,
    ExpressionKind,
    ExpressionValue,
    ParseError,
    ParseRecoveryError,
    TokenValue,
};

use crate::{DataArray, DataNode};

#[derive(thiserror::Error, Debug)]
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

impl DataArray {
    pub fn parse(text: &str) -> Result<Self, DataParseError> {
        let ast = arson_parse::parse_text(text)?;

        let lines = arson_parse::reporting::files::line_starts(text).collect();
        convert_to_array(&ast, &(0..text.len()), &lines)
    }

    pub fn parse_with_recovery(text: &str) -> Result<Self, DataParseError> {
        let ast = match arson_parse::parse_text(text) {
            Ok(ast) => ast,
            Err(err) => err.recovered,
        };

        let lines = arson_parse::reporting::files::line_starts(text).collect();
        convert_to_array(&ast, &(0..text.len()), &lines)
    }

    pub fn to_tokens(&self) -> Vec<TokenValue<'_>> {
        convert_to_tokens(self)
    }
}

fn convert_to_array(
    ast: &Vec<Expression<'_>>,
    location: &std::ops::Range<usize>,
    lines: &Vec<usize>,
) -> Result<DataArray, DataParseError> {
    let line = lines.binary_search(&location.start).unwrap_or_else(|i| i);
    let mut array = DataArray::new(line);

    for expr in ast {
        let node = match &expr.value {
            ExpressionValue::Integer(value) => DataNode::Integer(i32::try_from(*value)?),
            ExpressionValue::Float(value) => DataNode::Float(*value as f32),
            ExpressionValue::String(value) => DataNode::String((*value).to_owned()),
            ExpressionValue::Symbol(value) => DataNode::Symbol((*value).to_owned()),
            ExpressionValue::Variable(value) => DataNode::Variable((*value).to_owned()),
            ExpressionValue::Unhandled => DataNode::Unhandled,

            ExpressionValue::Array(ast) => DataNode::Array(convert_to_array(ast, &expr.location, lines)?),
            ExpressionValue::Command(ast) => DataNode::Command(convert_to_array(ast, &expr.location, lines)?),
            ExpressionValue::Property(ast) => {
                DataNode::Property(convert_to_array(ast, &expr.location, lines)?)
            },

            ExpressionValue::Define(name, ast) => {
                DataNode::Define(name.text.to_owned(), convert_to_array(&ast.exprs, &ast.location, lines)?)
            },
            ExpressionValue::Undefine(name) => DataNode::Variable(name.text.to_owned()),
            ExpressionValue::Include(path) => DataNode::Variable(path.text.to_owned()),
            ExpressionValue::IncludeOptional(_) => {
                return Err(DataParseError::UnsupportedExpression(ExpressionKind::IncludeOptional));
            },
            ExpressionValue::Merge(path) => DataNode::Variable(path.text.to_owned()),
            ExpressionValue::Autorun(ast) => {
                DataNode::Autorun(convert_to_array(&ast.exprs, &ast.location, lines)?)
            },

            ExpressionValue::Conditional { is_positive, symbol, true_branch, false_branch } => {
                match is_positive {
                    true => array.push(DataNode::Ifdef(symbol.text.to_owned())),
                    false => array.push(DataNode::Ifndef(symbol.text.to_owned())),
                };

                let mut true_branch = convert_to_array(&true_branch.exprs, &true_branch.location, lines)?;
                array.append(&mut true_branch);

                if let Some(false_branch) = false_branch {
                    array.push(DataNode::Else);
                    let mut false_branch =
                        convert_to_array(&false_branch.exprs, &false_branch.location, lines)?;
                    array.append(&mut false_branch);
                }

                array.push(DataNode::Endif);
                continue;
            },

            ExpressionValue::Comment(_) => continue,
            ExpressionValue::BlockComment(_) => continue,
        };
        array.push(node);
    }

    Ok(array)
}

fn convert_to_tokens(array: &DataArray) -> Vec<TokenValue<'_>> {
    let mut tokens = Vec::new();

    fn symbol_pair<'src>(
        tokens: &mut Vec<TokenValue<'src>>,
        token: TokenValue<'src>,
        name: &'src str,
    ) -> TokenValue<'src> {
        tokens.push(token);
        TokenValue::Symbol(name)
    }

    fn convert_array<'src>(
        tokens: &mut Vec<TokenValue<'src>>,
        kind: ArrayKind,
        array: &'src DataArray,
    ) -> TokenValue<'src> {
        let (open, close) = kind.delimiter_tokens();
        tokens.push(open);
        tokens.append(&mut convert_to_tokens(array));
        close
    }

    for node in array {
        let expr = match node {
            DataNode::Integer(value) => TokenValue::Integer(*value as i64),
            DataNode::Float(value) => TokenValue::Float(*value as f64),
            DataNode::Variable(name) => TokenValue::Variable(name),
            DataNode::Function(_name) => todo!("DataNode::Function"),
            DataNode::Object(_name) => todo!("DataNode::Object"),
            DataNode::Symbol(name) => TokenValue::Symbol(name),
            DataNode::Unhandled => TokenValue::Unhandled,

            DataNode::Ifdef(name) => symbol_pair(&mut tokens, TokenValue::Ifdef, name),
            DataNode::Else => TokenValue::Else,
            DataNode::Endif => TokenValue::Endif,

            DataNode::Array(body) => convert_array(&mut tokens, ArrayKind::Array, body),
            DataNode::Command(body) => convert_array(&mut tokens, ArrayKind::Command, body),
            DataNode::String(value) => TokenValue::String(value),
            DataNode::Property(body) => convert_array(&mut tokens, ArrayKind::Property, body),
            DataNode::Glob(_value) => todo!("DataNode::Glob"),

            DataNode::Define(name, body) => {
                let token = symbol_pair(&mut tokens, TokenValue::Include, name);
                tokens.push(token);
                convert_array(&mut tokens, ArrayKind::Array, body)
            },
            DataNode::Include(path) => symbol_pair(&mut tokens, TokenValue::Include, path),
            DataNode::Merge(path) => symbol_pair(&mut tokens, TokenValue::Merge, path),
            DataNode::Ifndef(name) => symbol_pair(&mut tokens, TokenValue::Ifndef, name),
            DataNode::Autorun(body) => {
                tokens.push(TokenValue::Autorun);
                convert_array(&mut tokens, ArrayKind::Command, body)
            },
            DataNode::Undefine(name) => symbol_pair(&mut tokens, TokenValue::Undefine, name),
        };
        tokens.push(expr);
    }

    tokens
}
