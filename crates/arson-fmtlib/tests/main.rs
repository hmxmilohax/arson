// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_fmtlib::{Indentation, Options};

fn assert_format(input: &str, expected: &str) {
    let options = Options::default();
    let actual = arson_fmtlib::format_to_string(input, options).unwrap();
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
    assert_format("(sym1 (sym2 100))", "(sym1 (sym2 100))");
    assert_format("(sym1 {sym2 100})", "(sym1 {sym2 100})");
    assert_format("(sym1 [sym2 100])", "(sym1 [sym2 100])");
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
    // TODO
    // assert_format(
    //     "(#ifdef kDefine (array1 50) #endif)",
    //     "(\
    //    \n   #ifdef kDefine\
    //    \n   (array1 50)\
    //    \n   #endif\
    //    \n)",
    // );
    // assert_format(
    //     "(#ifdef kDefine (array1 50) #else (array2 100) #endif)",
    //     "(\
    //    \n   #ifdef kDefine\
    //    \n   (array1 50)\
    //    \n   #else\
    //    \n   (array2 100)\
    //    \n   #endif\
    //    \n)",
    // );
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
        "{func print_thing ($the_thing) {print $the_thing} {...}}",
        "{func print_thing\
       \n   ($the_thing)\
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
       \n      (\
       \n         1\
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
        let formatted = arson_fmtlib::format_to_string(input, options).unwrap();
        assert_eq!(&formatted, expected);
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
        let formatted = arson_fmtlib::format_to_string(input, options).unwrap();
        assert_eq!(&formatted, expected);
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
