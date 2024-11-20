// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{io, marker::PhantomData, rc::Rc};

use crate::fs::VirtualPath;
use crate::{Context, Node, NodeArray, NodeCommand, NodeProperty, NodeValue, RawNodeValue, Variable};

use super::parser::{self, Expression, ExpressionKind, ParseError};

#[derive(Clone)]
pub struct LoadOptions {
    pub allow_include: bool,
}

#[derive(thiserror::Error, Debug)]
pub enum LoadError {
    #[error("IO error: {0}")]
    IO(#[from] io::Error),

    #[error("Failed to parse the given file")]
    Parse(Vec<ParseError>),

    #[error("A required macro definition was not found")]
    MacroNotFound,

    #[error("Inclusion is disallowed by the given load options")]
    IncludeNotAllowed,

    #[error("Encountered errors while including other files")]
    Inner {
        recovered: NodeArray,
        errors: Vec<LoadError>,
    },
}

// manual impl because io::Error has no eq impl, but io::Error.kind() does
impl PartialEq for LoadError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::IO(left), Self::IO(right)) => left.kind() == right.kind(),
            (Self::Parse(left), Self::Parse(right)) => left == right,
            (
                Self::Inner { recovered: l_recovered, errors: l_errors },
                Self::Inner { recovered: r_recovered, errors: r_errors },
            ) => l_recovered == r_recovered && l_errors == r_errors,
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}

struct Loader<'ctx, 'src> {
    context: &'ctx mut Context,
    options: LoadOptions,
    phantom: PhantomData<&'src ()>,
}

enum NodeResult<'define> {
    Value(Node),
    Include(NodeArray),
    MergeFile(NodeArray),
    MergeMacro(&'define NodeArray),
    Skip,
}

impl<'ctx, 'src> Loader<'ctx, 'src> {
    fn new(context: &'ctx mut Context, options: LoadOptions) -> Self {
        Self { context, options, phantom: PhantomData }
    }

    fn load_array(&mut self, ast: impl Iterator<Item = Expression<'src>>) -> Result<NodeArray, LoadError> {
        let mut array = NodeArray::with_capacity(4);
        let mut errors = vec![];

        for expr in ast {
            match self.load_node(expr) {
                Ok(result) => match result {
                    NodeResult::Value(node) => array.push(node),
                    NodeResult::Include(mut file) => array.append(&mut file),
                    NodeResult::MergeFile(file) => Self::merge_array(&mut array, &file),
                    NodeResult::MergeMacro(define) => Self::merge_array(&mut array, define),
                    NodeResult::Skip => continue,
                },
                Err(error) => {
                    errors.push(error);
                    continue;
                },
            }
        }

        array.shrink_to_fit();

        if !errors.is_empty() {
            Err(LoadError::Inner { recovered: array, errors })
        } else {
            Ok(array)
        }
    }

    // This is implemented here instead of on NodeArray itself,
    // since [`Rc::get_mut`] requires that no other references to the stored array exist,
    // and the only place that this can be ensured is within the loader.
    fn merge_array(target: &mut NodeArray, source: &NodeArray) {
        for node in source.iter() {
            let RawNodeValue::Array(array) = node.unevaluated() else {
                continue;
            };
            let Ok(tag) = array.get(0) else {
                continue;
            };

            match target.find_array_opt(tag.unevaluated()) {
                // Some(mut found) => match Rc::get_mut(&mut found) {
                //     Some(found) => Self::merge_array(found, array),
                //     None => continue,
                // },
                Some(mut found) => {
                    let found =
                        Rc::get_mut(&mut found).expect("no external array references should exist during loading");
                    Self::merge_array(found, array);
                },
                None => target.push(NodeValue::Array(array.clone())),
            }
        }
    }

    fn load_node(&mut self, expr: Expression<'src>) -> Result<NodeResult<'_>, LoadError> {
        let node = match expr.kind {
            ExpressionKind::Integer(value) => value.into(),
            ExpressionKind::Float(value) => value.into(),
            ExpressionKind::String(value) => value.into(),
            ExpressionKind::Symbol(value) => self.context.add_symbol(value).into(),
            ExpressionKind::Variable(value) => Variable::new(value, self.context).into(),
            ExpressionKind::Unhandled => Node::UNHANDLED,

            ExpressionKind::Array(exprs) => self.load_array(exprs.into_iter())?.into(),
            ExpressionKind::Command(exprs) => NodeCommand::from(self.load_array(exprs.into_iter())?).into(),
            ExpressionKind::Property(exprs) => NodeProperty::from(self.load_array(exprs.into_iter())?).into(),

            ExpressionKind::Define(name, exprs) => {
                let name = self.context.add_symbol(name.text);
                let define = self.load_array(exprs.exprs.into_iter())?;
                self.context.add_macro(&name, define);
                return Ok(NodeResult::Skip);
            },
            ExpressionKind::Undefine(name) => {
                if let Some(name) = self.context.get_symbol(name.text) {
                    self.context.remove_macro(&name);
                }
                return Ok(NodeResult::Skip);
            },
            ExpressionKind::Include(path) => {
                if !self.options.allow_include {
                    return Err(LoadError::IncludeNotAllowed);
                }

                let file = self.load_path(path.text)?;
                return Ok(NodeResult::Include(file));
            },
            ExpressionKind::IncludeOptional(path) => {
                if !self.options.allow_include {
                    return Ok(NodeResult::Skip);
                }

                match self.load_path_opt(path.text)? {
                    Some(file) => return Ok(NodeResult::Include(file)),
                    None => return Ok(NodeResult::Skip),
                }
            },
            ExpressionKind::Merge(name) => {
                if let Some(symbol) = self.context.get_symbol(name.text) {
                    match self.context.get_macro(&symbol) {
                        Some(define) => return Ok(NodeResult::MergeMacro(define)),
                        None => return Err(LoadError::MacroNotFound),
                    }
                } else {
                    if !self.options.allow_include {
                        return Err(LoadError::IncludeNotAllowed);
                    }

                    let file = self.load_path(name.text)?;
                    return Ok(NodeResult::MergeFile(file));
                }
            },
            ExpressionKind::Autorun(_exprs) => {
                todo!("#autorun loading")
            },

            ExpressionKind::Conditional { is_positive, symbol, true_branch, false_branch } => {
                let defined = match self.context.get_symbol(symbol.text) {
                    Some(name) => self.context.get_macro(&name).is_some(),
                    None => false,
                };

                let array = match defined == is_positive {
                    true => self.load_array(true_branch.exprs.into_iter())?,
                    false => match false_branch {
                        Some(false_branch) => self.load_array(false_branch.exprs.into_iter())?,
                        None => return Ok(NodeResult::Skip),
                    },
                };

                return Ok(NodeResult::Include(array));
            },
        };

        Ok(NodeResult::Value(node))
    }

    fn load_path_opt(&mut self, path: &str) -> Result<Option<NodeArray>, LoadError> {
        let Some(file_system) = self.context.file_system_opt() else {
            return Ok(None);
        };

        match file_system.exists(path) {
            true => self.load_path(path).map(Some),
            false => Ok(None),
        }
    }

    fn load_path(&mut self, path: &str) -> Result<NodeArray, LoadError> {
        self.context.load_path(self.options.clone(), path)
    }
}

pub fn load_path<P: AsRef<VirtualPath>>(
    context: &mut Context,
    options: LoadOptions,
    path: P,
) -> Result<NodeArray, LoadError> {
    let file_system = context.file_system()?;

    let file = file_system.open_execute(&path)?;
    let text = io::read_to_string(file)?;

    let canon = file_system.canonicalize(&path);
    let Some(dir) = canon.parent() else {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "file has no containing directory (how???)").into());
    };

    let old_cwd = context
        .set_cwd(dir)
        .expect("file system is known to be registered");
    let array = load_text(context, options, &text)?;
    context.set_cwd(&old_cwd);

    Ok(array)
}

pub fn load_text(context: &mut Context, options: LoadOptions, text: &str) -> Result<NodeArray, LoadError> {
    let ast = match parser::parse_text(text) {
        Ok(ast) => ast,
        Err(errors) => return Err(LoadError::Parse(errors)),
    };
    load_ast(context, options, ast.into_iter())
}

pub fn load_ast<'src>(
    context: &mut Context,
    options: LoadOptions,
    ast: impl Iterator<Item = Expression<'src>>,
) -> Result<NodeArray, LoadError> {
    let mut loader = Loader::new(context, options);
    loader.load_array(ast)
}

#[cfg(test)]
mod tests {
    use logos::Span;
    use parser::ArrayKind;

    use crate::arson_array;
    use crate::fs::drivers::MockFileSystemDriver;
    use crate::fs::AbsolutePath;
    use crate::parse::lexer::{OwnedToken, OwnedTokenValue, TokenKind};

    use super::*;

    fn assert_loaded(text: &str, expected: NodeArray) {
        let mut context = Context::new();
        assert_loaded_with_context(&mut context, text, expected)
    }

    fn assert_loaded_with_context(context: &mut Context, text: &str, expected: NodeArray) {
        let options = LoadOptions { allow_include: true };
        let array = match load_text(context, options, text) {
            Ok(array) => array,
            Err(errs) => panic!("Errors encountered while parsing: {errs:?}"),
        };
        assert_eq!(array, expected, "Unexpected result for '{text}'");
    }

    fn assert_error(text: &str, expected: LoadError) {
        let mut context = Context::new();
        let options = LoadOptions { allow_include: true };
        let errors = match load_text(&mut context, options, text) {
            Ok(ast) => panic!("Expected parsing errors, got AST instead: {ast:?}"),
            Err(errors) => errors,
        };
        assert_eq!(errors, expected, "Unexpected result for '{text}'");
    }

    #[test]
    fn integer() {
        assert_loaded("1 2 3", arson_array![1, 2, 3]);
    }

    #[test]
    fn float() {
        assert_loaded("1.0 2.0 3.0", arson_array![1.0, 2.0, 3.0]);
    }

    #[test]
    fn string() {
        assert_loaded("\"a\" \"b\" \"c\"", arson_array!["a", "b", "c"]);
    }

    #[test]
    fn symbol() {
        let mut context = Context::new();

        let sym_asdf = context.add_symbol("asdf");
        let sym_plus = context.add_symbol("+");
        let sym_10 = context.add_symbol("10");

        assert_loaded_with_context(&mut context, "asdf + '10'", arson_array![sym_asdf, sym_plus, sym_10]);
    }

    #[test]
    fn variable() {
        let mut context = Context::new();

        let var_asdf = Variable::new("asdf", &mut context);
        let var_this = Variable::new("this", &mut context);

        assert_loaded_with_context(&mut context, "$asdf $this", arson_array![var_asdf, var_this]);
    }

    #[test]
    fn unhandled() {
        assert_loaded("kDataUnhandled", arson_array![Node::UNHANDLED])
    }

    #[test]
    fn arrays() {
        let mut context = Context::new();

        {
            let sym_asdf = context.add_symbol("asdf");
            let array = arson_array![sym_asdf, "text", 1];
            assert_loaded_with_context(&mut context, "(asdf \"text\" 1)", arson_array![array]);
        }

        {
            let sym_set = context.add_symbol("set");
            let var_var = Variable::new("var", &mut context);
            let command = NodeCommand::from(arson_array![sym_set, var_var, "asdf"]);
            assert_loaded_with_context(&mut context, "{set $var \"asdf\"}", arson_array![command]);
        }

        {
            let sym_asdf = context.add_symbol("asdf");
            let property = NodeProperty::from(arson_array![sym_asdf]);
            assert_loaded_with_context(&mut context, "[asdf]", arson_array![property]);
        }

        {
            let sym_handle = context.add_symbol("handle");
            let sym_set = context.add_symbol("set");
            let sym_var = context.add_symbol("var");

            let property = NodeProperty::from(arson_array![sym_var]);
            let command = NodeCommand::from(arson_array![sym_set, property, "asdf"]);
            let array = arson_array![sym_handle, command];

            assert_loaded_with_context(&mut context, "(handle {set [var] \"asdf\"})", arson_array![array]);
        }
    }

    #[test]
    #[ignore = "#merge does not function correctly currently"]
    fn directives() {
        assert_loaded("#define kDefine (1)", arson_array![]);
        assert_loaded("#undef kDefine", arson_array![]);

        let mut driver = MockFileSystemDriver::new();
        driver.add_text_file(AbsolutePath::new_rooted("empty.dta"), "");
        driver.add_text_file(AbsolutePath::new_rooted("numbers.dta"), "1 2 3 4 5");
        driver.add_text_file(
            AbsolutePath::new_rooted("merge.dta"),
            "
            (number 1)
            (string \"merge.dta\")
            (list 1 2 3)
            (number2 2)
            (string2 \"foo\")
            (list2 4 5 6)
            ",
        );

        let mut context = Context::with_file_driver(driver);

        // Load an empty file
        assert_loaded_with_context(&mut context, "#include empty.dta", arson_array![]);
        assert_loaded_with_context(&mut context, "#include_opt empty.dta", arson_array![]);
        assert_loaded_with_context(&mut context, "#merge empty.dta", arson_array![]);

        // Load a file with numbers
        assert_loaded_with_context(&mut context, "#include numbers.dta", arson_array![1, 2, 3, 4, 5]);
        assert_loaded_with_context(&mut context, "#include_opt numbers.dta", arson_array![1, 2, 3, 4, 5]);
        assert_loaded_with_context(&mut context, "#merge numbers.dta", arson_array![1, 2, 3, 4, 5]);

        // Ensure #include_opt is truly optional
        assert!(!context.file_system().unwrap().exists("nonexistent.dta"));
        assert_loaded_with_context(&mut context, "#include_opt nonexistent.dta", arson_array![]);

        // Ensure #merge overrides included keys with already-present ones
        let sym_number = context.add_symbol("number");
        let sym_string = context.add_symbol("string");
        let sym_list = context.add_symbol("list");
        let sym_number2 = context.add_symbol("number2");
        let sym_string2 = context.add_symbol("string2");
        let sym_list2 = context.add_symbol("list2");
        assert_loaded_with_context(
            &mut context,
            "
            (number 10)
            (string \"test.dta\")
            (list 10 20 30)
            #merge merge.dta
            ",
            arson_array![
                arson_array![sym_number, 10],
                arson_array![sym_string, "test.dta"],
                arson_array![sym_list, 10, 20, 30],
                arson_array![sym_number2, 2],
                arson_array![sym_string2, "foo"],
                arson_array![sym_list2, 4, 5, 6],
            ],
        );

        // TODO: #autorun test
        // assert_loaded_with_context(&mut context, "#autorun {print \"Auto-run action\"}", arson_array![]);
    }

    #[test]
    fn conditionals() {
        let mut context = Context::new();

        let sym_define = context.add_symbol("kDefine");

        let sym_array = context.add_symbol("array");
        let sym_array1 = context.add_symbol("array1");
        let sym_array2 = context.add_symbol("array2");

        context.add_macro_define(&sym_define);
        assert_loaded_with_context(
            &mut context,
            "#ifdef kDefine (array1 10) #else (array2 5) #endif",
            arson_array![arson_array![sym_array1, 10]],
        );

        context.remove_macro(&sym_define);
        assert_loaded_with_context(
            &mut context,
            "#ifdef kDefine (array1 10) #else (array2 5) #endif",
            arson_array![arson_array![sym_array2, 5]],
        );

        context.add_macro_define(&sym_define);
        assert_loaded_with_context(&mut context, "#ifndef kDefine (array 10) #endif", arson_array![]);

        context.remove_macro(&sym_define);
        assert_loaded_with_context(
            &mut context,
            "#ifndef kDefine (array 10) #endif",
            arson_array![arson_array![sym_array, 10]],
        );
    }

    #[test]
    fn parse_errors() {
        fn assert_parse_errors(text: &str, expected: Vec<ParseError>) {
            assert_error(text, LoadError::Parse(expected))
        }

        // #region Arrays

        fn assert_array_mismatch(text: &str, kind: ArrayKind, location: Span, eof_expected: bool) {
            let mut expected = vec![ParseError::UnmatchedBrace(location, kind)];
            if !eof_expected {
                expected.push(ParseError::UnexpectedEof);
            }

            assert_parse_errors(text, expected);
        }

        fn assert_array_mismatches(kind: ArrayKind) {
            let (l, r) = kind.delimiters();
            assert_array_mismatch(&format!("{l} {l} {r}"), kind, 0..1, false);
            assert_array_mismatch(&format!("{r} {l} {r}"), kind, 0..1, true);
            assert_array_mismatch(&format!("{l} {r} {l}"), kind, 4..5, false);
            assert_array_mismatch(&format!("{l} {r} {r}"), kind, 4..5, true);
        }

        fn assert_multi_array_mismatches(matched_kind: ArrayKind, unmatched_kind: ArrayKind) {
            let (ml, mr) = matched_kind.delimiters();
            let (ul, ur) = unmatched_kind.delimiters();

            assert_array_mismatch(&format!("{ul} {ml} {mr}"), unmatched_kind, 0..1, false);
            assert_array_mismatch(&format!("{ur} {ml} {mr}"), unmatched_kind, 0..1, true);
            assert_array_mismatch(&format!("{ml} {ul} {mr}"), unmatched_kind, 2..3, true);
            assert_array_mismatch(&format!("{ml} {ur} {mr}"), unmatched_kind, 2..3, true);
            assert_array_mismatch(&format!("{ml} {mr} {ul}"), unmatched_kind, 4..5, false);
            assert_array_mismatch(&format!("{ml} {mr} {ur}"), unmatched_kind, 4..5, true);
        }

        assert_array_mismatches(ArrayKind::Array);
        assert_array_mismatches(ArrayKind::Command);
        assert_array_mismatches(ArrayKind::Property);

        assert_multi_array_mismatches(ArrayKind::Array, ArrayKind::Command);
        assert_multi_array_mismatches(ArrayKind::Array, ArrayKind::Property);
        assert_multi_array_mismatches(ArrayKind::Command, ArrayKind::Array);
        assert_multi_array_mismatches(ArrayKind::Command, ArrayKind::Property);
        assert_multi_array_mismatches(ArrayKind::Property, ArrayKind::Array);
        assert_multi_array_mismatches(ArrayKind::Property, ArrayKind::Command);

        // #endregion Arrays

        // #region Directives

        assert_parse_errors(
            "#define kDefine 1",
            vec![ParseError::IncorrectToken {
                expected: TokenKind::ArrayOpen,
                actual: OwnedToken::new(OwnedTokenValue::Integer(1), 16..17),
            }],
        );
        assert_parse_errors(
            "#autorun kDefine",
            vec![ParseError::IncorrectToken {
                expected: TokenKind::CommandOpen,
                actual: OwnedToken::new(OwnedTokenValue::Symbol("kDefine".to_owned()), 9..16),
            }],
        );

        assert_parse_errors("#bad", vec![ParseError::BadDirective(0..4)]);

        fn assert_directive_symbol_error(name: &str) {
            let text = name.to_owned() + " 1";
            assert_parse_errors(
                &text,
                vec![ParseError::IncorrectToken {
                    expected: TokenKind::Symbol,
                    actual: OwnedToken::new(OwnedTokenValue::Integer(1), name.len() + 1..name.len() + 2),
                }],
            );
        }

        fn assert_directive_eof_error(name: &str) {
            assert_parse_errors(name, vec![ParseError::UnexpectedEof]);
        }

        assert_directive_symbol_error("#ifdef");
        assert_directive_symbol_error("#ifndef");
        assert_directive_symbol_error("#define");
        assert_directive_symbol_error("#undef");
        assert_directive_symbol_error("#include");
        assert_directive_symbol_error("#include_opt");
        assert_directive_symbol_error("#merge");

        assert_directive_eof_error("#ifdef");
        assert_directive_eof_error("#ifndef");
        assert_directive_eof_error("#define");
        assert_directive_eof_error("#undef");
        assert_directive_eof_error("#include");
        assert_directive_eof_error("#include_opt");
        assert_directive_eof_error("#merge");
        assert_directive_eof_error("#autorun");

        // #endregion Directives

        // #region Conditionals

        assert_parse_errors(
            "#ifndef kDefine (array 10)",
            vec![ParseError::UnmatchedConditional(0..7), ParseError::UnexpectedEof],
        );
        assert_parse_errors(
            "#ifdef kDefine (array1 10) #else",
            vec![ParseError::UnmatchedConditional(27..32), ParseError::UnexpectedEof],
        );
        assert_parse_errors("#else (array2 5) #endif", vec![ParseError::UnexpectedConditional(0..5)]);
        assert_parse_errors("(array 10) #endif", vec![ParseError::UnexpectedConditional(11..17)]);

        assert_parse_errors(
            "(#ifdef kDefine array1 10) #else array2 5) #endif)",
            vec![
                ParseError::UnbalancedConditional(1..32),
                ParseError::UnmatchedBrace(25..26, ArrayKind::Array),
                ParseError::UnbalancedConditional(27..49),
                ParseError::UnmatchedBrace(41..42, ArrayKind::Array),
            ],
        );

        assert_parse_errors(
            "\
            #ifdef kDefine\n\
            {do\n\
            #endif\
            \n    {+ 1 2}\n\
            #ifdef kDefine\n\
            }\n\
            #endif\
            ",
            vec![
                ParseError::UnbalancedConditional(0..25),
                ParseError::UnmatchedBrace(15..16, ArrayKind::Command),
                ParseError::UnbalancedConditional(38..61),
                ParseError::UnmatchedBrace(53..54, ArrayKind::Command),
            ],
        );

        // #endregion Conditionals
    }
}
