// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_fmtlib::{Indentation, Options};
use arson_parse::{Expression, ExpressionValue};

fn assert_format(input: &str, expected: &str) {
    let options = Options::default();
    let actual = arson_fmtlib::expr::format_to_string(input, options).unwrap();
    assert_eq!(actual, expected);

    fn assert_ast_eq(left: &[Expression<'_>], right: &[Expression<'_>], msg: &str) {
        fn check_ast_eq(left: &[Expression<'_>], right: &[Expression<'_>]) -> Result<(), ()> {
            let mut left_i = left.iter().filter(|e| !matches!(e.value, ExpressionValue::BlankLine));
            let mut right_i = right.iter().filter(|e| !matches!(e.value, ExpressionValue::BlankLine));

            while let (Some(left), Some(right)) = (left_i.next(), right_i.next()) {
                match (&left.value, &right.value) {
                    (ExpressionValue::Array(left), ExpressionValue::Array(right))
                    | (ExpressionValue::Command(left), ExpressionValue::Command(right))
                    | (ExpressionValue::Property(left), ExpressionValue::Property(right)) => {
                        check_ast_eq(left, right)?;
                    },

                    (ExpressionValue::Undefine(left), ExpressionValue::Undefine(right))
                    | (ExpressionValue::Include(left), ExpressionValue::Include(right))
                    | (ExpressionValue::IncludeOptional(left), ExpressionValue::IncludeOptional(right))
                    | (ExpressionValue::Merge(left), ExpressionValue::Merge(right)) => {
                        if left.text != right.text {
                            return Err(());
                        }
                    },

                    (
                        ExpressionValue::Define(left_name, left),
                        ExpressionValue::Define(right_name, right),
                    ) => {
                        if left_name.text != right_name.text {
                            return Err(());
                        }

                        check_ast_eq(&left.exprs, &right.exprs)?;
                    },
                    (ExpressionValue::Autorun(left), ExpressionValue::Autorun(right)) => {
                        check_ast_eq(&left.exprs, &right.exprs)?;
                    },

                    (
                        ExpressionValue::Conditional {
                            is_positive: left_positive,
                            symbol: left_name,
                            true_branch: left_true,
                            false_branch: left_false,
                        },
                        ExpressionValue::Conditional {
                            is_positive: right_positive,
                            symbol: right_name,
                            true_branch: right_true,
                            false_branch: right_false,
                        },
                    ) => {
                        if left_positive != right_positive || left_name.text != right_name.text {
                            return Err(());
                        }

                        check_ast_eq(&left_true.exprs, &right_true.exprs)?;

                        match (left_false, right_false) {
                            (Some(left_false), Some(right_false)) => {
                                check_ast_eq(&left_false.exprs, &right_false.exprs)?
                            },
                            (None, None) => (),
                            _ => return Err(()),
                        }
                    },

                    (left, right) => {
                        if left != right {
                            return Err(());
                        }
                    },
                }
            }

            match (left_i.next(), right_i.next()) {
                (None, None) => Ok(()),
                _ => Err(()),
            }
        }

        if let Err(_) = check_ast_eq(left, right) {
            panic!(
                "{msg}\
               \n left: {left:?}\
               \nright: {right:?}"
            );
        }
    }

    let input = arson_parse::parse_text(input).unwrap();
    let expected = arson_parse::parse_text(expected).unwrap();
    let actual = arson_parse::parse_text(&actual).unwrap();
    assert_ast_eq(&input, &expected, "input ast differs from expected ast");
    assert_ast_eq(&actual, &expected, "actual ast differs from expected ast");
    // sanity check
    assert_ast_eq(&input, &actual, "input ast differs from actual ast");
}

fn assert_preserved(input: &str) {
    assert_format(input, input)
}

#[test]
fn general() {
    assert_preserved("5");
    assert_preserved("10.0");
    assert_preserved("10.12345678901234567890123456789012345678901234567890");
    assert_preserved("\"asdf\"");
    assert_preserved("sym");
    assert_preserved("$var");
    assert_preserved("kDataUnhandled");

    assert_preserved("(5)");
    assert_preserved("(10.0)");
    assert_preserved("(10.12345678901234567890123456789012345678901234567890)");
    assert_preserved("(\"asdf\")");
    assert_preserved("(sym)");
    assert_preserved("($var)");
    assert_preserved("(kDataUnhandled)");
}

#[test]
fn arrays() {
    assert_preserved("(1 2 3)");
    assert_preserved("{1 2 3}");
    assert_preserved("[1 2 3]");
}

#[test]
fn inner_arrays() {
    assert_preserved("(sym1 (sym2 100))");
    assert_preserved("(sym1 {sym2 100})");
    assert_preserved("(sym1 [sym2 100])");

    assert_format(
        "(sym1 (sym2 (foo 5) (bar 10)))",
        "(sym1\
       \n   (sym2\
       \n      (foo 5)\
       \n      (bar 10)\
       \n   )\
       \n)",
    );
    assert_preserved("(sym1 {sym2 (foo 5) (bar 10)})");
    assert_preserved("(sym1 [sym2 (foo 5) (bar 10)])");
}

#[test]
fn multiple_arrays() {
    assert_format(
        "(foo (10 20 30) {bar 100} [bar])",
        "(foo\
       \n   (10 20 30)\
       \n   {bar 100}\
       \n   [bar]\
       \n)",
    );
    assert_format(
        "(400 (10 20 30) {bar 100} [bar])",
        "(400\
       \n   (10 20 30)\
       \n   {bar 100}\
       \n   [bar]\
       \n)",
    );
    assert_format(
        "(\"foo\" (10 20 30) {bar 100} [bar])",
        "(\"foo\"\
       \n   (10 20 30)\
       \n   {bar 100}\
       \n   [bar]\
       \n)",
    );
}

#[test]
fn directives() {
    assert_preserved("#define kDefine (1)");
    assert_preserved("#undef kDefine");
    assert_preserved("#include items.dta");
    assert_preserved("#include_opt items.dta");
    assert_preserved("#merge items.dta");
    assert_preserved("#autorun {print \"bar\"}");

    assert_preserved("(#define kDefine (1))");
    assert_preserved("(#undef kDefine)");
    assert_preserved("(#include items.dta)");
    assert_preserved("(#include_opt items.dta)");
    assert_preserved("(#merge items.dta)");
    assert_preserved("(#autorun {print \"bar\"})");

    assert_format(
        "#define kDefine\
       \n(1)",
        "#define kDefine (1)",
    );
    assert_format(
        "#define\
       \nkDefine\
       \n(1)",
        "#define kDefine (1)",
    );
    assert_format(
        "#undef\
       \nkDefine",
        "#undef kDefine",
    );
    assert_format(
        "#include\
       \nitems.dta",
        "#include items.dta",
    );
    assert_format(
        "#include_opt\
       \nitems.dta",
        "#include_opt items.dta",
    );
    assert_format(
        "#merge\
       \nitems.dta",
        "#merge items.dta",
    );
    assert_format(
        "#autorun\
       \n{print \"bar\"}",
        "#autorun {print \"bar\"}",
    );

    assert_format(
        "(foo #define kDefine (1))",
        "(foo\
       \n   #define kDefine (1)\
       \n)",
    );
    assert_format(
        "(foo #undef kDefine)",
        "(foo\
       \n   #undef kDefine\
       \n)",
    );
    assert_format(
        "(items #include items.dta)",
        "(items\
       \n   #include items.dta\
       \n)",
    );
    assert_format(
        "(items #include_opt items.dta)",
        "(items\
       \n   #include_opt items.dta\
       \n)",
    );
    assert_format(
        "(items #merge items.dta)",
        "(items\
       \n   #merge items.dta\
       \n)",
    );
    assert_format(
        "(foo #autorun {print \"bar\"})",
        "(foo\
       \n   #autorun {print \"bar\"}\
       \n)",
    );

    assert_format(
        "#define kDefine (kValue1 kValue2 kValue3 kValue4 kValue5 kValue6 kValue7 kValue8 kValue9 kValue10)",
        "#define kDefine (\
       \n   kValue1\
       \n   kValue2\
       \n   kValue3\
       \n   kValue4\
       \n   kValue5\
       \n   kValue6\
       \n   kValue7\
       \n   kValue8\
       \n   kValue9\
       \n   kValue10\
       \n)",
    );
    assert_format(
        "(#define kDefine (kValue1 kValue2 kValue3 kValue4 kValue5 kValue6 kValue7 kValue8 kValue9 kValue10))",
        "(\
       \n   #define kDefine (\
       \n      kValue1\
       \n      kValue2\
       \n      kValue3\
       \n      kValue4\
       \n      kValue5\
       \n      kValue6\
       \n      kValue7\
       \n      kValue8\
       \n      kValue9\
       \n      kValue10\
       \n   )\
       \n)",
    );
}

#[test]
fn conditionals() {
    // Top-scope
    assert_format(
        "#ifdef kDefine (array1 50) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );

    // Nested within another conditional
    assert_format(
        "#ifdef kDefine   #ifdef kDefine2 (array1 50) #endif   #endif",
        "#ifdef kDefine\
       \n   #ifdef kDefine2\
       \n   (array1 50)\
       \n   #endif\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine   #ifdef kDefine2 (array1 50) #else (array2 100) #endif   #else (array2 100) #endif",
        "#ifdef kDefine\
       \n   #ifdef kDefine2\
       \n   (array1 50)\
       \n   #else\
       \n   (array2 100)\
       \n   #endif\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );

    // Nested within an array
    assert_format(
        "(#ifdef kDefine (array1 50) #endif)",
        "(\
       \n   #ifdef kDefine\
       \n   (array1 50)\
       \n   #endif\
       \n)",
    );
    assert_format(
        "(#ifdef kDefine (array1 50) #else (array2 100) #endif)",
        "(\
       \n   #ifdef kDefine\
       \n   (array1 50)\
       \n   #else\
       \n   (array2 100)\
       \n   #endif\
       \n)",
    );

    // Nested within two arrays
    assert_format(
        "(foo (bar #ifdef kDefine (array1 50) #endif))",
        "(foo\
       \n   (bar\
       \n      #ifdef kDefine\
       \n      (array1 50)\
       \n      #endif\
       \n   )\
       \n)",
    );
    assert_format(
        "(foo (bar #ifdef kDefine (array1 50) #else (array2 100) #endif))",
        "(foo\
       \n   (bar\
       \n      #ifdef kDefine\
       \n      (array1 50)\
       \n      #else\
       \n      (array2 100)\
       \n      #endif\
       \n   )\
       \n)",
    );
}

#[test]
fn command_args() {
    assert_format(
        "{foreach $elem $array {print $elem} {...}}",
        "{foreach $elem $array\
       \n   {print $elem}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{foreach_int $i 0 5 {print $i} {...}}",
        "{foreach_int $i 0 5\
       \n   {print $i}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{func print_thing ($the_thing) {print $the_thing} {...}}",
        "{func print_thing\
       \n   ($the_thing)\
       \n   {print $the_thing}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{if {== $i 5} {print $i} {...}}",
        "{if {== $i 5}\
       \n   {print $i}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{if_else {== $i 5} {print $i} {print \"Bad number\"}}",
        "{if_else {== $i 5}\
       \n   {print $i}\
       \n   {print \"Bad number\"}\
       \n}",
    );
    assert_format(
        "{set $var \"Some really quite long text which will get wrapped by the formatter\"}",
        "{set $var\
       \n   \"Some really quite long text which will get wrapped by the formatter\"\
       \n}",
    );
    assert_format(
        "{switch $var (case_1 ...) (case_2 ...) (...)}",
        "{switch $var\
       \n   (case_1 ...)\
       \n   (case_2 ...)\
       \n   (...)\
       \n}",
    );
    assert_format(
        "{unless {== $i 5} {print $i} {...}}",
        "{unless {== $i 5}\
       \n   {print $i}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{with $object {print $i} {...}}",
        "{with $object\
       \n   {print $i}\
       \n   {...}\
       \n}",
    );

    assert_format(
        "{$object foreach_thing $thing {print $thing} {...}}",
        "{$object foreach_thing $thing\
       \n   {print $thing}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{$object with_thing $the_thing {print $the_thing} {...}}",
        "{$object with_thing $the_thing\
       \n   {print $the_thing}\
       \n   {...}\
       \n}",
    );

    assert_format(
        "{\"object\" foreach_thing $thing {print $thing} {...}}",
        "{\"object\" foreach_thing $thing\
       \n   {print $thing}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{\"object\" with_thing $the_thing {print $the_thing} {...}}",
        "{\"object\" with_thing $the_thing\
       \n   {print $the_thing}\
       \n   {...}\
       \n}",
    );

    assert_format(
        "{{get_thing} foreach_thing $thing {print $thing} {...}}",
        "{{get_thing} foreach_thing $thing\
       \n   {print $thing}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{{get_thing} with_thing $the_thing {print $the_thing} {...}}",
        "{{get_thing} with_thing $the_thing\
       \n   {print $the_thing}\
       \n   {...}\
       \n}",
    );

    assert_format(
        "{[thing] foreach_thing $thing {print $thing} {...}}",
        "{[thing] foreach_thing $thing\
       \n   {print $thing}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{[thing] with_thing $the_thing {print $the_thing} {...}}",
        "{[thing] with_thing $the_thing\
       \n   {print $the_thing}\
       \n   {...}\
       \n}",
    );
}

#[test]
fn comments() {
    assert_preserved(
        "(array1 10) ; comment\
       \n(array2 50) ; comment\
       \n(array3 250) ; comment",
    );
    assert_preserved(
        "; comment\
       \n(array1 10)\
       \n; comment\
       \n(array2 50)\
       \n; comment\
       \n(array3 250)",
    );

    assert_preserved(
        "(array1 10) /* comment */\
       \n(array2 50) /* comment */\
       \n(array3 250) /* comment */",
    );
    assert_preserved(
        "/* comment */\
       \n(array1 10)\
       \n/* comment */\
       \n(array2 50)\
       \n/* comment */\
       \n(array3 250)",
    );

    assert_preserved(
        "(foo ; comment\
       \n   (bar 50)\
       \n)",
    );
    assert_preserved(
        "(foo\
       \n   ; comment\
       \n   (bar 50)\
       \n)",
    );
    assert_preserved(
        "(foo\
       \n   (bar 50) ; comment\
       \n)",
    );
    assert_preserved(
        "(foo\
       \n   (bar 50)\
       \n   ; comment\
       \n)",
    );

    assert_format(
        "( ; comment\
       \n   (bar 50)\
       \n)",
        "(\
       \n   ; comment\
       \n   (bar 50)\
       \n)",
    );
    assert_preserved(
        "(\
       \n   ; comment\
       \n   (bar 50)\
       \n)",
    );
    assert_preserved(
        "(\
       \n   (bar 50) ; comment\
       \n)",
    );
    assert_preserved(
        "(\
       \n   (bar 50)\
       \n   ; comment\
       \n)",
    );
}

#[test]
fn directive_comments() {
    assert_format(
        "/* comment */ #define kDefine (1)",
        "/* comment */\
       \n#define kDefine (1)",
    );
    assert_format(
        "#define /* comment */ kDefine (1)",
        "/* comment */\
       \n#define kDefine (1)",
    );
    assert_format(
        "#define kDefine /* comment */ (1)",
        "/* comment */\
       \n#define kDefine (1)",
    );
    assert_preserved("#define kDefine (/* comment */ 1)");
    assert_preserved("#define kDefine (1) /* comment */");

    assert_format(
        "/* comment */ #undef kDefine",
        "/* comment */\
       \n#undef kDefine",
    );
    assert_format(
        "#undef /* comment */ kDefine",
        "/* comment */\
       \n#undef kDefine",
    );
    assert_preserved("#undef kDefine /* comment */");

    assert_format(
        "/* comment */ #include items.dta",
        "/* comment */\
       \n#include items.dta",
    );
    assert_format(
        "#include /* comment */ items.dta",
        "/* comment */\
       \n#include items.dta",
    );
    assert_preserved("#include items.dta /* comment */");

    assert_format(
        "/* comment */ #include_opt items.dta",
        "/* comment */\
       \n#include_opt items.dta",
    );
    assert_format(
        "#include_opt /* comment */ items.dta",
        "/* comment */\
       \n#include_opt items.dta",
    );
    assert_preserved("#include_opt items.dta /* comment */");

    assert_format(
        "/* comment */ #merge items.dta",
        "/* comment */\
       \n#merge items.dta",
    );
    assert_format(
        "#merge /* comment */ items.dta",
        "/* comment */\
       \n#merge items.dta",
    );
    assert_preserved("#merge items.dta /* comment */");

    assert_format(
        "/* comment */ #autorun {print \"bar\"}",
        "/* comment */\
       \n#autorun {print \"bar\"}",
    );
    assert_format(
        "#autorun /* comment */ {print \"bar\"}",
        "/* comment */\
       \n#autorun {print \"bar\"}",
    );
    assert_preserved("#autorun {/* comment */ print \"bar\"}");
    assert_preserved("#autorun {print /* comment */ \"bar\"}");
    assert_preserved("#autorun {print \"bar\" /* comment */}");
    assert_preserved("#autorun {print \"bar\"} /* comment */");
}

#[test]
fn conditional_comments() {
    // insanity

    assert_format(
        "/* comment */ #ifdef kDefine (array1 50) #endif",
        "/* comment */\
       \n#ifdef kDefine\
       \n(array1 50)\
       \n#endif",
    );
    assert_format(
        "#ifdef /* comment */ kDefine (array1 50) #endif",
        "/* comment */\
       \n#ifdef kDefine\
       \n(array1 50)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine /* comment */ (array1 50) #endif",
        "#ifdef kDefine /* comment */\
       \n(array1 50)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine\n/* comment */ (array1 50) #endif",
        "#ifdef kDefine\
       \n/* comment */\
       \n(array1 50)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (/* comment */ array1 50) #endif",
        "#ifdef kDefine\
       \n(/* comment */ array1 50)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 /* comment */ 50) #endif",
        "#ifdef kDefine\
       \n(array1 /* comment */ 50)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50 /* comment */) #endif",
        "#ifdef kDefine\
       \n(array1 50 /* comment */)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) /* comment */ #endif",
        "#ifdef kDefine\
       \n(array1 50) /* comment */\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50)\n/* comment */ #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n/* comment */\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #endif /* comment */",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#endif /* comment */",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #endif\n/* comment */",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#endif\
       \n/* comment */",
    );

    assert_format(
        "/* comment */ #ifdef kDefine (array1 50) #else (array2 100) #endif",
        "/* comment */\
       \n#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef /* comment */ kDefine (array1 50) #else (array2 100) #endif",
        "/* comment */\
       \n#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine /* comment */ (array1 50) #else (array2 100) #endif",
        "#ifdef kDefine /* comment */\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine\n/* comment */ (array1 50) #else (array2 100) #endif",
        "#ifdef kDefine\
       \n/* comment */\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (/* comment */ array1 50) #else (array2 100) #endif",
        "#ifdef kDefine\
       \n(/* comment */ array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 /* comment */ 50) #else (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 /* comment */ 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50 /* comment */) #else (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50 /* comment */)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) /* comment */ #else (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50) /* comment */\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50)\n/* comment */ #else (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n/* comment */\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else /* comment */ (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else /* comment */\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else\n/* comment */ (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n/* comment */\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else (/* comment */ array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(/* comment */ array2 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 /* comment */ 100) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 /* comment */ 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 100 /* comment */) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100 /* comment */)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 100) /* comment */ #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100) /* comment */\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 100)\n/* comment */ #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n/* comment */\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 100) #endif /* comment */",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif /* comment */",
    );
    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 100) #endif\n/* comment */",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif\
       \n/* comment */",
    );
}

#[test]
fn blank_lines() {
    assert_format(
        "(array1 10)\
       \n\
       \n\
       \n(array2 50)\
       \n(array3 250)",
        "(array1 10)\
       \n\
       \n(array2 50)\
       \n(array3 250)",
    );
    assert_preserved(
        "(array1 10)\
       \n\
       \n(array2 50)\
       \n\
       \n(array3 250)",
    );
    assert_format(
        "(array1 10)\
       \n(array2 50)\
       \n\
       \n\
       \n(array3 250)",
        "(array1 10)\
       \n(array2 50)\
       \n\
       \n(array3 250)",
    );

    assert_format(
        "(foo\
       \n\
       \n   (bar 50)\
       \n   (quz 100)\
       \n)",
        "(foo\
       \n   (bar 50)\
       \n   (quz 100)\
       \n)",
    );
    assert_preserved(
        "(foo\
       \n   (bar 50)\
       \n\
       \n   (quz 100)\
       \n)",
    );
    assert_format(
        "(foo\
       \n   (bar 50)\
       \n   (quz 100)\
       \n\
       \n)",
        "(foo\
       \n   (bar 50)\
       \n   (quz 100)\
       \n)",
    );

    assert_format(
        "#define kDefine\
       \n\
       \n(1)",
        "#define kDefine (1)",
    );
    assert_format(
        "#define kDefine (\
       \n\
       \n   1\
       \n)",
        "#define kDefine (1)",
    );

    assert_format(
        "#ifdef kDefine\
       \n\
       \n(bar 50)\
       \n(quz 100)\
       \n#endif",
        "#ifdef kDefine\
       \n(bar 50)\
       \n(quz 100)\
       \n#endif",
    );
    assert_preserved(
        "#ifdef kDefine\
       \n(bar 50)\
       \n\
       \n(quz 100)\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine\
       \n(bar 50)\
       \n(quz 100)\
       \n\
       \n#endif",
        "#ifdef kDefine\
       \n(bar 50)\
       \n(quz 100)\
       \n#endif",
    );

    assert_preserved(
        "(array1 10)\
       \n(array2 50)\
       \n\
       \n; comment\
       \n(array3 250)",
    );
    assert_preserved(
        "(array1 10)\
       \n\
       \n; comment\
       \n(array2 50)\
       \n(array3 250)",
    );
    assert_preserved(
        "(array1 10)\
       \n(array2 50)\
       \n; comment\
       \n\
       \n(array3 250)",
    );
    assert_preserved(
        "(array1 10)\
       \n; comment\
       \n\
       \n(array2 50)\
       \n(array3 250)",
    );

    assert_format(
        "(foo\
       \n\
       \n   ; comment\
       \n   (bar 50)\
       \n   (quz 100)\
       \n)",
        "(foo\
       \n   ; comment\
       \n   (bar 50)\
       \n   (quz 100)\
       \n)",
    );
    assert_preserved(
        "(foo\
       \n   (bar 50)\
       \n\
       \n   ; comment\
       \n   (quz 100)\
       \n)",
    );
    assert_preserved(
        "(foo\
       \n   (bar 50)\
       \n   (quz 100)\
       \n\
       \n   ; comment\
       \n)",
    );

    assert_preserved(
        "(foo\
       \n   ; comment\
       \n\
       \n   (bar 50)\
       \n   (quz 100)\
       \n)",
    );
    assert_preserved(
        "(foo\
       \n   (bar 50)\
       \n   ; comment\
       \n\
       \n   (quz 100)\
       \n)",
    );
    assert_format(
        "(foo\
       \n   (bar 50)\
       \n   (quz 100)\
       \n   ; comment\
       \n\
       \n)",
        "(foo\
       \n   (bar 50)\
       \n   (quz 100)\
       \n   ; comment\
       \n)",
    );

    assert_format(
        "(array1 10) \
       \n\
       \n(array2 50) \
       \n\
       \n(array3 250)",
        "(array1 10)\
       \n\
       \n(array2 50)\
       \n\
       \n(array3 250)",
    );
    assert_format(
        "(foo \
       \n\
       \n   (bar 50) \
       \n\
       \n   (quz 100) \
       \n\
       \n)",
        "(foo\
       \n   (bar 50)\
       \n\
       \n   (quz 100)\
       \n)",
    );
}

#[test]
fn big_example() {
    assert_format(
        "((sym1 5) (sym2 (asdf 100) (jkl 250) (1 (5 \"foo\") (10 \"bar\"))) (3 3) (4 4))",
        "(\
       \n   (sym1 5)\
       \n   (sym2\
       \n      (asdf 100)\
       \n      (jkl 250)\
       \n      (1\
       \n         (5 \"foo\")\
       \n         (10 \"bar\")\
       \n      )\
       \n   )\
       \n   (3 3)\
       \n   (4 4)\
       \n)",
    );
}

#[test]
fn indent_style() {
    fn assert_indent(indentation: Indentation, input: &str, expected: &str) {
        let options = Options { indentation, ..Default::default() };
        let actual = arson_fmtlib::expr::format_to_string(input, options).unwrap();
        assert_eq!(actual, expected);
    }

    assert_indent(
        Indentation::Spaces(3),
        "((1 2 3)(4 5 6))",
        "(\
       \n   (1 2 3)\
       \n   (4 5 6)\
       \n)",
    );

    assert_indent(
        Indentation::Spaces(8),
        "((1 2 3)(4 5 6))",
        "(\
       \n        (1 2 3)\
       \n        (4 5 6)\
       \n)",
    );

    assert_indent(
        Indentation::Tabs(1),
        "((1 2 3)(4 5 6))",
        "(\
       \n\t(1 2 3)\
       \n\t(4 5 6)\
       \n)",
    );
}

#[test]
#[ignore = "not implemented"]
fn max_line_width() {
    todo!()
}

#[test]
fn max_array_width() {
    fn assert_width(width: usize, input: &str, expected: &str) {
        let options = Options {
            max_array_width: width,
            max_line_width: 999,
            ..Default::default()
        };
        let actual = arson_fmtlib::expr::format_to_string(input, options).unwrap();
        assert_eq!(actual, expected);
    }

    assert_width(
        60,
        r#"("1234567890" "1234567890" "1234567890" "1234567890" "1234567890")"#,
        "(\
       \n   \"1234567890\"\
       \n   \"1234567890\"\
       \n   \"1234567890\"\
       \n   \"1234567890\"\
       \n   \"1234567890\"\
       \n)",
    );
    assert_width(
        80,
        r#"("1234567890" "1234567890" "1234567890" "1234567890" "1234567890")"#,
        "(\"1234567890\" \"1234567890\" \"1234567890\" \"1234567890\" \"1234567890\")",
    );
    assert_width(
        25,
        r#"("1234567890" "1234567890")"#,
        "(\
       \n   \"1234567890\"\
       \n   \"1234567890\"\
       \n)",
    );
    assert_width(30, r#"("1234567890" "1234567890")"#, "(\"1234567890\" \"1234567890\")");
}
