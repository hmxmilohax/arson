// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_fmtlib::{Indentation, Options};

fn assert_format(input: &str, expected: &str) {
    let options = Options::default();

    let actual = arson_fmtlib::token::format_to_string(input, options);
    assert_eq!(actual, expected);
}

#[test]
fn general() {
    assert_format("(5)", "(5)");
    assert_format("(10.0)", "(10.0)");
    assert_format("(\"asdf\")", "(\"asdf\")");
    assert_format("(sym)", "(sym)");
    assert_format("($var)", "($var)");
    assert_format("(kDataUnhandled)", "(kDataUnhandled)");
}

#[test]
fn arrays() {
    assert_format("(1 2 3)", "(1 2 3)");
    assert_format("{1 2 3}", "{1 2 3}");
    assert_format("[1 2 3]", "[1 2 3]");
}

#[test]
fn inner_arrays() {
    assert_format(
        "(sym1 (sym2 100))",
        "(sym1\
       \n   (sym2 100)\
       \n)",
    );
    assert_format(
        "(sym1 {sym2 100})",
        "(sym1\
       \n   {sym2 100}\
       \n)",
    );
    assert_format(
        "(sym1 [sym2 100])",
        "(sym1\
       \n   [sym2 100]\
       \n)",
    );

    assert_format(
        "(sym1 (sym2 (foo 5) (bar 10)))",
        "(sym1\
       \n   (sym2\
       \n      (foo 5)\
       \n      (bar 10)\
       \n   )\
       \n)",
    );
    assert_format(
        "(sym1 {sym2 (foo 5) (bar 10)})",
        "(sym1\
       \n   {sym2\
       \n      (foo 5)\
       \n      (bar 10)\
       \n   }\
       \n)",
    );
    assert_format(
        "(sym1 [sym2 (foo 5) (bar 10)])",
        "(sym1\
       \n   [sym2\
       \n      (foo 5)\
       \n      (bar 10)\
       \n   ]\
       \n)",
    );
}

#[test]
fn multiple_arrays() {
    assert_format(
        "(sym1 (10 20 30) {sym2 100} [sym2])",
        "(sym1\
       \n   (10 20 30)\
       \n   {sym2 100}\
       \n   [sym2]\
       \n)",
    );
}

#[test]
fn directives() {
    assert_format(
        "(#define kDefine (1))",
        "(\
       \n   #define kDefine (1)\
       \n)",
    );
    assert_format(
        "(#undef kDefine)",
        "(\
       \n   #undef kDefine\
       \n)",
    );
    assert_format(
        "(#include items.dta)",
        "(\
       \n   #include items.dta\
       \n)",
    );
    assert_format(
        "(#include_opt items.dta)",
        "(\
       \n   #include_opt items.dta\
       \n)",
    );
    assert_format(
        "(#merge items.dta)",
        "(\
       \n   #merge items.dta\
       \n)",
    );
    assert_format(
        "(#autorun {print \"bar\"})",
        "(\
       \n   #autorun {print \"bar\"}\
       \n)",
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
        "#define kDefine 1",
        "#define kDefine\
       \n1",
    );
    assert_format(
        "#define 1",
        "#define\
       \n1",
    );
    assert_format(
        "#undef 1",
        "#undef\
       \n1",
    );
    assert_format(
        "#include 1",
        "#include\
       \n1",
    );
    assert_format(
        "#include_opt 1",
        "#include_opt\
       \n1",
    );
    assert_format(
        "#merge 1",
        "#merge\
       \n1",
    );
    assert_format(
        "#autorun 1",
        "#autorun\
       \n1",
    );

    assert_format(
        "#define kDefine (kValue1 kValue2 kValue3 kValue4 kValue5 kValue6 kValue7 kValue8)",
        "#define kDefine (\
       \n   kValue1\
       \n   kValue2\
       \n   kValue3\
       \n   kValue4\
       \n   kValue5\
       \n   kValue6\
       \n   kValue7\
       \n   kValue8\
       \n)",
    );
    assert_format(
        "(#define kDefine (kValue1 kValue2 kValue3 kValue4 kValue5 kValue6 kValue7 kValue8))",
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
       \n#ifdef kDefine2\
       \n(array1 50)\
       \n#endif\
       \n#endif",
    );
    assert_format(
        "#ifdef kDefine   #ifdef kDefine2 (array1 50) #else (array2 100) #endif   #else (array2 100) #endif",
        "#ifdef kDefine\
       \n#ifdef kDefine2\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif\
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
        "{if\
       \n   {== $i 5}\
       \n   {print $i}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{if_else {== $i 5} {print $i} {print \"Bad number\"}}",
        "{if_else\
       \n   {== $i 5}\
       \n   {print $i}\
       \n   {print \"Bad number\"}\
       \n}",
    );
    assert_format(
        "{set $var \"Some really long text which will get wrapped by the formatter\"}",
        "{set $var\
       \n   \"Some really long text which will get wrapped by the formatter\"\
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
        "{unless\
       \n   {== $i 5}\
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
        "{$object with_thing $the_thing {print $the_thing} {...}}",
        "{$object with_thing\
       \n   $the_thing\
       \n   {print $the_thing}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{{get_thing} with_thing $the_thing {print $the_thing} {...}}",
        "{\
       \n   {get_thing}\
       \n   with_thing\
       \n   $the_thing\
       \n   {print $the_thing}\
       \n   {...}\
       \n}",
    );
    assert_format(
        "{[thing] with_thing $the_thing {print $the_thing} {...}}",
        "{\
       \n   [thing]\
       \n   with_thing\
       \n   $the_thing\
       \n   {print $the_thing}\
       \n   {...}\
       \n}",
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

        let actual = arson_fmtlib::token::format_to_string(input, options);
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

        let actual = arson_fmtlib::token::format_to_string(input, options);
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
