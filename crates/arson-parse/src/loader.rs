// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io;

use arson_core::{Context, Node, NodeArray, NodeCommand, NodeProperty, Variable};
use arson_fs::{AbsolutePath, FileSystemState, VirtualPath};

use crate::{Diagnostic, Expression, ExpressionValue};

#[derive(Clone)]
pub struct LoadOptions {
    pub allow_include: bool,
    pub allow_autorun: bool,
}

#[derive(thiserror::Error, Debug)]
pub enum LoadError {
    #[error("IO error: {0}")]
    IO(#[from] io::Error),

    #[error("Failed to parse the given file")]
    Parse(Vec<Diagnostic>),

    #[error("A required macro definition was not found")]
    MacroNotFound,

    #[error("Inclusion is disallowed by the given load options")]
    IncludeNotAllowed,

    #[error("File {0} is included recursively")]
    RecursiveInclude(AbsolutePath),

    #[error("Auto-run is disallowed by the given load options")]
    AutorunNotAllowed,

    #[error("Error occurred in #autorun block: {0}")]
    AutorunError(#[from] arson_core::Error),

    #[error("Encountered errors while including other files")]
    Inner { recovered: NodeArray, errors: Vec<LoadError> },
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

impl From<LoadError> for io::Error {
    fn from(value: LoadError) -> Self {
        let io_kind = match value {
            LoadError::IO(error) => return error,
            LoadError::Parse(_) => io::ErrorKind::InvalidData,
            LoadError::MacroNotFound => io::ErrorKind::Other,
            LoadError::IncludeNotAllowed => io::ErrorKind::PermissionDenied,
            LoadError::RecursiveInclude(_) => io::ErrorKind::Other,
            LoadError::AutorunNotAllowed => io::ErrorKind::PermissionDenied,
            LoadError::AutorunError(_) => io::ErrorKind::Other,
            LoadError::Inner { .. } => io::ErrorKind::Other,
        };
        io::Error::new(io_kind, value.to_string())
    }
}

impl From<LoadError> for arson_core::Error {
    fn from(value: LoadError) -> Self {
        io::Error::from(value).into()
    }
}

struct Loader<'ctx, S: FileSystemState> {
    context: &'ctx mut Context<S>,
    options: LoadOptions,
    include_stack: Vec<AbsolutePath>,
}

enum NodeResult<'define> {
    Value(Node),
    IncludeFile(NodeArray),
    IncludeMacro(&'define NodeArray),
    MergeFile(NodeArray),
    MergeMacro(&'define NodeArray),
    Skip,
}

impl<'ctx, S: FileSystemState> Loader<'ctx, S> {
    fn new(context: &'ctx mut Context<S>, options: LoadOptions) -> Self {
        Self { context, options, include_stack: Vec::new() }
    }

    fn load_array<'a>(
        &mut self,
        ast: impl IntoIterator<Item = Expression<'a>>,
    ) -> Result<NodeArray, LoadError> {
        let mut array = NodeArray::with_capacity(4);
        let mut errors = vec![];

        for expr in ast {
            match self.load_node(expr) {
                Ok(result) => match result {
                    NodeResult::Value(node) => array.push(node),
                    NodeResult::IncludeFile(mut file) => array.append(&mut file),
                    NodeResult::IncludeMacro(define) => array.extend_from_slice(define),
                    NodeResult::MergeFile(file) => array.merge_tags(&file),
                    NodeResult::MergeMacro(define) => array.merge_tags(define),
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

    fn load_node<'a>(&mut self, expr: Expression<'a>) -> Result<NodeResult, LoadError> {
        let node = match expr.value {
            ExpressionValue::Integer(value) => value.into(),
            ExpressionValue::Float(value) => value.into(),
            ExpressionValue::String(value) => value.into(),

            ExpressionValue::Symbol(value) => {
                let symbol = self.context.add_symbol(value);
                match self.context.get_macro(&symbol) {
                    Some(replacement) => return Ok(NodeResult::IncludeMacro(replacement)),
                    None => symbol.into(),
                }
            },
            ExpressionValue::Variable(value) => Variable::new(value, self.context).into(),
            ExpressionValue::Unhandled => Node::UNHANDLED,

            ExpressionValue::Array(exprs) => self.load_array(exprs)?.into(),
            ExpressionValue::Command(exprs) => NodeCommand::from(self.load_array(exprs)?).into(),
            ExpressionValue::Property(exprs) => NodeProperty::from(self.load_array(exprs)?).into(),

            ExpressionValue::Define(name, exprs) => {
                let name = self.context.add_symbol(name.text);
                let define = self.load_array(exprs.exprs)?;
                self.context.add_macro(&name, define);
                return Ok(NodeResult::Skip);
            },
            ExpressionValue::Undefine(name) => {
                if let Some(name) = self.context.get_symbol(name.text) {
                    self.context.remove_macro(&name);
                }
                return Ok(NodeResult::Skip);
            },
            ExpressionValue::Include(path) => {
                if !self.options.allow_include {
                    return Err(LoadError::IncludeNotAllowed);
                }

                let file = self.load_path(path.text)?;
                return Ok(NodeResult::IncludeFile(file));
            },
            ExpressionValue::IncludeOptional(path) => {
                if !self.options.allow_include {
                    return Ok(NodeResult::Skip);
                }

                match self.load_path_opt(path.text)? {
                    Some(file) => return Ok(NodeResult::IncludeFile(file)),
                    None => return Ok(NodeResult::Skip),
                }
            },
            ExpressionValue::Merge(name) => {
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
            ExpressionValue::Autorun(exprs) => {
                if !self.options.allow_autorun {
                    return Err(LoadError::AutorunNotAllowed);
                }

                let command = self.load_array(exprs.exprs)?;
                match self.context.execute(&NodeCommand::from(command)) {
                    Ok(_) => return Ok(NodeResult::Skip),
                    Err(err) => return Err(LoadError::AutorunError(err)),
                }
            },

            ExpressionValue::Conditional { is_positive, symbol, true_branch, false_branch } => {
                let defined = match self.context.get_symbol(symbol.text) {
                    Some(name) => self.context.get_macro(&name).is_some(),
                    None => false,
                };

                let array = match defined == is_positive {
                    true => self.load_array(true_branch.exprs)?,
                    false => match false_branch {
                        Some(false_branch) => self.load_array(false_branch.exprs)?,
                        None => return Ok(NodeResult::Skip),
                    },
                };

                return Ok(NodeResult::IncludeFile(array));
            },
        };

        Ok(NodeResult::Value(node))
    }

    fn load_path_opt(&mut self, path: &str) -> Result<Option<NodeArray>, LoadError> {
        match self.context.file_system().exists(path) {
            true => self.load_path(path).map(Some),
            false => Ok(None),
        }
    }

    fn load_path<P: AsRef<VirtualPath>>(&mut self, path: P) -> Result<NodeArray, LoadError> {
        let file = self.context.file_system().open_execute(&path)?;
        let text = io::read_to_string(file)?;

        let canon = self.context.file_system().canonicalize(&path);
        let Some(dir) = canon.parent() else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "file has no containing directory (how???)",
            )
            .into());
        };

        if self.include_stack.contains(&canon) {
            return Err(LoadError::RecursiveInclude(canon));
        }

        let old_cwd = self.context.file_system_mut().set_cwd(dir);
        self.include_stack.push(canon);

        let array = self.load_text(&text)?;

        self.context.file_system_mut().set_cwd(&old_cwd);
        self.include_stack.pop().expect("path was added above");

        Ok(array)
    }

    fn load_text(&mut self, text: &str) -> Result<NodeArray, LoadError> {
        let ast = match super::parse_text(text) {
            Ok(ast) => ast,
            Err(errors) => return Err(LoadError::Parse(errors)),
        };
        self.load_array(ast)
    }
}

pub fn load_path<S: FileSystemState, P: AsRef<VirtualPath>>(
    context: &mut Context<S>,
    options: LoadOptions,
    path: P,
) -> Result<NodeArray, LoadError> {
    Loader::new(context, options).load_path(path)
}

pub fn load_text<S: FileSystemState>(
    context: &mut Context<S>,
    options: LoadOptions,
    text: &str,
) -> Result<NodeArray, LoadError> {
    Loader::new(context, options).load_text(text)
}

pub fn load_ast<'src, S: FileSystemState>(
    context: &mut Context<S>,
    options: LoadOptions,
    ast: impl IntoIterator<Item = Expression<'src>>,
) -> Result<NodeArray, LoadError> {
    Loader::new(context, options).load_array(ast)
}

#[cfg(test)]
mod tests {
    use arson_core::prelude::*;
    use arson_fs::drivers::MockFileSystemDriver;
    use arson_fs::prelude::*;

    use super::*;

    fn default_context() -> Context<FileSystem> {
        Context::new(FileSystem::new(MockFileSystemDriver::new()))
    }

    fn assert_loaded<S: FileSystemState>(context: &mut Context<S>, text: &str, expected: NodeArray) {
        let options = LoadOptions { allow_include: true, allow_autorun: true };
        let array = match load_text(context, options, text) {
            Ok(array) => array,
            Err(errs) => panic!("Errors encountered while parsing.\nText: {text}\nResult: {errs:?}"),
        };
        assert_eq!(array, expected, "Unexpected result for '{text}'");
    }

    #[expect(
        dead_code,
        reason = "Currently unused, left for future use (remove this message when used)"
    )]
    fn assert_error(text: &str, expected: LoadError) {
        let mut context = default_context();
        let options = LoadOptions { allow_include: true, allow_autorun: true };
        let errors = match load_text(&mut context, options, text) {
            Ok(ast) => panic!("Expected parsing errors, got success instead.\nText: {text}\nResult: {ast:?}"),
            Err(errors) => errors,
        };
        assert_eq!(errors, expected, "Unexpected result for '{text}'");
    }

    #[test]
    fn integer() {
        let mut context = default_context();
        assert_loaded(&mut context, "1 2 3", arson_array![1, 2, 3]);
    }

    #[test]
    fn float() {
        let mut context = default_context();
        assert_loaded(&mut context, "1.0 2.0 3.0", arson_array![1.0, 2.0, 3.0]);
    }

    #[test]
    fn string() {
        let mut context = default_context();
        assert_loaded(&mut context, "\"a\" \"b\" \"c\"", arson_array!["a", "b", "c"]);
    }

    #[test]
    fn symbol() {
        let mut context = default_context();

        let sym_asdf = context.add_symbol("asdf");
        let sym_plus = context.add_symbol("+");
        let sym_10 = context.add_symbol("10");

        assert_loaded(&mut context, "asdf + '10'", arson_array![sym_asdf, sym_plus, sym_10]);
    }

    #[test]
    fn variable() {
        let mut context = default_context();

        let var_asdf = Variable::new("asdf", &mut context);
        let var_this = Variable::new("this", &mut context);

        assert_loaded(&mut context, "$asdf $this", arson_array![var_asdf, var_this]);
    }

    #[test]
    fn unhandled() {
        let mut context = default_context();
        assert_loaded(&mut context, "kDataUnhandled", arson_array![Node::UNHANDLED])
    }

    #[test]
    fn arrays() {
        let mut context = default_context();

        {
            let sym_asdf = context.add_symbol("asdf");
            let array = arson_array![sym_asdf, "text", 1];
            assert_loaded(&mut context, "(asdf \"text\" 1)", arson_array![array]);
        }

        {
            let sym_set = context.add_symbol("set");
            let var_var = Variable::new("var", &mut context);
            let command = NodeCommand::from(arson_array![sym_set, var_var, "asdf"]);
            assert_loaded(&mut context, "{set $var \"asdf\"}", arson_array![command]);
        }

        {
            let sym_asdf = context.add_symbol("asdf");
            let property = NodeProperty::from(arson_array![sym_asdf]);
            assert_loaded(&mut context, "[asdf]", arson_array![property]);
        }

        {
            let sym_handle = context.add_symbol("handle");
            let sym_set = context.add_symbol("set");
            let sym_var = context.add_symbol("var");

            let property = NodeProperty::from(arson_array![sym_var]);
            let command = NodeCommand::from(arson_array![sym_set, property, "asdf"]);
            let array = arson_array![sym_handle, command];

            assert_loaded(&mut context, "(handle {set [var] \"asdf\"})", arson_array![array]);
        }
    }

    #[test]
    fn directives() {
        let mut driver = MockFileSystemDriver::new();
        driver.add_text_file(AbsolutePath::new_rooted("empty.dta"), "");
        driver.add_text_file(AbsolutePath::new_rooted("numbers.dta"), "1 2 3 4 5");
        driver.add_text_file(AbsolutePath::new_rooted("config/config.dta"), "#include ../numbers.dta");
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

        struct TestState {
            autorun_str: String,
            file_system: FileSystem,
        }

        impl FileSystemState for TestState {
            fn file_system(&self) -> &FileSystem {
                &self.file_system
            }

            fn file_system_mut(&mut self) -> &mut FileSystem {
                &mut self.file_system
            }
        }

        let mut context = Context::new(TestState {
            autorun_str: String::new(),
            file_system: FileSystem::new(driver),
        });

        // Defines
        #[allow(non_snake_case)]
        let sym_kDefine = context.add_symbol("kDefine");
        assert_eq!(context.get_macro(&sym_kDefine), None);

        assert_loaded(&mut context, "#define kDefine (1)", arson_array![]);
        assert_eq!(context.get_macro(&sym_kDefine), Some(&arson_array![1]));

        assert_loaded(&mut context, "#undef kDefine", arson_array![]);
        assert_eq!(context.get_macro(&sym_kDefine), None);

        // Includes
        assert_loaded(&mut context, "#include empty.dta", arson_array![]);
        assert_loaded(&mut context, "#include_opt empty.dta", arson_array![]);

        assert_loaded(&mut context, "#include numbers.dta", arson_array![1, 2, 3, 4, 5]);
        assert_loaded(&mut context, "#include_opt numbers.dta", arson_array![1, 2, 3, 4, 5]);

        // Merges
        assert_loaded(&mut context, "#merge empty.dta", arson_array![]);
        // Note: #merge does *not* copy over all top-level elements!
        // It only copies non-empty arrays whose tags don't match an existing array
        assert_loaded(&mut context, "#merge numbers.dta", arson_array![]);

        // Ensure working directory behaves properly during includes
        let cwd = context.file_system().cwd().clone();
        let sym_included = context.add_symbol("included");
        assert_loaded(&mut context, "(included #include ./config/config.dta)", arson_array![
            arson_array![sym_included, 1, 2, 3, 4, 5]
        ]);
        assert_eq!(*context.file_system().cwd(), cwd);

        // Ensure included paths are not added as symbols
        // (despite being lexed as them)
        assert_eq!(context.get_symbol("empty.dta"), None);
        assert_eq!(context.get_symbol("numbers.dta"), None);
        assert_eq!(context.get_symbol("nonexistent.dta"), None);
        assert_eq!(context.get_symbol("./config/config.dta"), None);
        assert_eq!(context.get_symbol("../numbers.dta"), None);

        // Ensure #include_opt is truly optional
        assert!(!context.file_system().exists("nonexistent.dta"));
        assert_loaded(&mut context, "#include_opt nonexistent.dta", arson_array![]);

        // Ensure #merge overrides included keys with already-present ones
        let sym_number = context.add_symbol("number");
        let sym_string = context.add_symbol("string");
        let sym_list = context.add_symbol("list");
        let sym_number2 = context.add_symbol("number2");
        let sym_string2 = context.add_symbol("string2");
        let sym_list2 = context.add_symbol("list2");
        assert_loaded(
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

        // Autorun
        context.register_func("autorun_func", |context, args| {
            context.state.autorun_str = args.string(context, 0)?.as_ref().clone();
            Ok(Node::HANDLED)
        });

        assert_loaded(
            &mut context,
            "#autorun {autorun_func \"Auto-run was run\"}",
            arson_array![],
        );
        assert_eq!(context.state.autorun_str, "Auto-run was run");
    }

    #[test]
    fn conditionals() {
        let mut context = default_context();

        let sym_define = context.add_symbol("kDefine");

        let sym_array = context.add_symbol("array");
        let sym_array1 = context.add_symbol("array1");
        let sym_array2 = context.add_symbol("array2");

        context.add_macro_define(&sym_define);
        assert_loaded(
            &mut context,
            "#ifdef kDefine (array1 10) #else (array2 5) #endif",
            arson_array![arson_array![sym_array1, 10]],
        );

        context.remove_macro(&sym_define);
        assert_loaded(
            &mut context,
            "#ifdef kDefine (array1 10) #else (array2 5) #endif",
            arson_array![arson_array![sym_array2, 5]],
        );

        context.add_macro_define(&sym_define);
        assert_loaded(&mut context, "#ifndef kDefine (array 10) #endif", arson_array![]);

        context.remove_macro(&sym_define);
        assert_loaded(&mut context, "#ifndef kDefine (array 10) #endif", arson_array![
            arson_array![sym_array, 10]
        ]);
    }

    #[test]
    fn recursive_include() {
        let mut driver = MockFileSystemDriver::new();

        driver.add_text_file(AbsolutePath::new_rooted("self_include.dta"), "#include self_include.dta");
        driver.add_text_file(AbsolutePath::new_rooted("self_merge.dta"), "#merge self_merge.dta");

        driver.add_text_file(
            AbsolutePath::new_rooted("include_loop_short_1.dta"),
            "#include include_loop_short_2.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("include_loop_short_2.dta"),
            "#include include_loop_short_1.dta",
        );

        driver.add_text_file(
            AbsolutePath::new_rooted("merge_loop_short_1.dta"),
            "#merge merge_loop_short_2.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("merge_loop_short_2.dta"),
            "#merge merge_loop_short_1.dta",
        );

        driver.add_text_file(
            AbsolutePath::new_rooted("include_loop_long_1.dta"),
            "#include include_loop_long_2.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("include_loop_long_2.dta"),
            "#include include_loop_long_3.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("include_loop_long_3.dta"),
            "#include include_loop_long_4.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("include_loop_long_4.dta"),
            "#include include_loop_long_5.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("include_loop_long_5.dta"),
            "#include include_loop_long_1.dta",
        );

        driver.add_text_file(
            AbsolutePath::new_rooted("merge_loop_long_1.dta"),
            "#merge merge_loop_long_2.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("merge_loop_long_2.dta"),
            "#merge merge_loop_long_3.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("merge_loop_long_3.dta"),
            "#merge merge_loop_long_4.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("merge_loop_long_4.dta"),
            "#merge merge_loop_long_5.dta",
        );
        driver.add_text_file(
            AbsolutePath::new_rooted("merge_loop_long_5.dta"),
            "#merge merge_loop_long_1.dta",
        );

        let mut context = Context::new(FileSystem::new(driver));

        let options = LoadOptions { allow_include: true, allow_autorun: true };

        fn recursion_error(path: &str, depth: usize) -> LoadError {
            let mut error = LoadError::Inner {
                recovered: arson_array![],
                errors: vec![LoadError::RecursiveInclude(AbsolutePath::new_rooted(path))],
            };

            for _i in 0..depth {
                error = LoadError::Inner { recovered: arson_array![], errors: vec![error] };
            }

            error
        }

        let result = super::load_path(&mut context, options.clone(), "self_include.dta");
        assert_eq!(result, Err(recursion_error("self_include.dta", 0)));
        let result = super::load_path(&mut context, options.clone(), "self_merge.dta");
        assert_eq!(result, Err(recursion_error("self_merge.dta", 0)));

        let result = super::load_path(&mut context, options.clone(), "include_loop_short_1.dta");
        assert_eq!(result, Err(recursion_error("include_loop_short_1.dta", 1)));
        let result = super::load_path(&mut context, options.clone(), "include_loop_short_2.dta");
        assert_eq!(result, Err(recursion_error("include_loop_short_2.dta", 1)));

        let result = super::load_path(&mut context, options.clone(), "merge_loop_short_1.dta");
        assert_eq!(result, Err(recursion_error("merge_loop_short_1.dta", 1)));
        let result = super::load_path(&mut context, options.clone(), "merge_loop_short_2.dta");
        assert_eq!(result, Err(recursion_error("merge_loop_short_2.dta", 1)));

        let result = super::load_path(&mut context, options.clone(), "include_loop_long_1.dta");
        assert_eq!(result, Err(recursion_error("include_loop_long_1.dta", 4)));
        let result = super::load_path(&mut context, options.clone(), "include_loop_long_2.dta");
        assert_eq!(result, Err(recursion_error("include_loop_long_2.dta", 4)));
        let result = super::load_path(&mut context, options.clone(), "include_loop_long_3.dta");
        assert_eq!(result, Err(recursion_error("include_loop_long_3.dta", 4)));
        let result = super::load_path(&mut context, options.clone(), "include_loop_long_4.dta");
        assert_eq!(result, Err(recursion_error("include_loop_long_4.dta", 4)));
        let result = super::load_path(&mut context, options.clone(), "include_loop_long_5.dta");
        assert_eq!(result, Err(recursion_error("include_loop_long_5.dta", 4)));

        let result = super::load_path(&mut context, options.clone(), "merge_loop_long_1.dta");
        assert_eq!(result, Err(recursion_error("merge_loop_long_1.dta", 4)));
        let result = super::load_path(&mut context, options.clone(), "merge_loop_long_2.dta");
        assert_eq!(result, Err(recursion_error("merge_loop_long_2.dta", 4)));
        let result = super::load_path(&mut context, options.clone(), "merge_loop_long_3.dta");
        assert_eq!(result, Err(recursion_error("merge_loop_long_3.dta", 4)));
        let result = super::load_path(&mut context, options.clone(), "merge_loop_long_4.dta");
        assert_eq!(result, Err(recursion_error("merge_loop_long_4.dta", 4)));
        let result = super::load_path(&mut context, options.clone(), "merge_loop_long_5.dta");
        assert_eq!(result, Err(recursion_error("merge_loop_long_5.dta", 4)));
    }
}
