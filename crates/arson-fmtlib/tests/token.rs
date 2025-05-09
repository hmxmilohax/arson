// SPDX-License-Identifier: LGPL-3.0-or-later

use arson_fmtlib::{Indentation, Options};
use arson_parse::{Token, TokenValue, Tokenizer};

fn assert_format(input: &str, expected: &str) {
    let options = Options::default();

    let actual = arson_fmtlib::token::format_to_string(input, options);
    assert_eq!(actual, expected);

    fn assert_token_eq(left: &[Token<'_>], right: &[Token<'_>], msg: &str) {
        fn check_token_eq(left: &[Token<'_>], right: &[Token<'_>]) -> Result<(), ()> {
            let mut left_i = left.iter().filter(|e| !matches!(e.value, TokenValue::BlankLine));
            let mut right_i = right.iter().filter(|e| !matches!(e.value, TokenValue::BlankLine));

            while let (Some(left), Some(right)) = (left_i.next(), right_i.next()) {
                let eq = match (&left.value, &right.value) {
                    (TokenValue::BlockComment(left), TokenValue::BlockComment(right)) => {
                        left.open.text == right.open.text
                            && left.body.text == right.body.text
                            && left.close.text == right.close.text
                    },
                    (left, right) => left == right,
                };

                if !eq {
                    return Err(());
                }
            }

            match (left_i.next(), right_i.next()) {
                (None, None) => Ok(()),
                _ => Err(()),
            }
        }

        if let Err(_) = check_token_eq(left, right) {
            panic!(
                "{msg}\
               \n left: {left:?}\
               \nright: {right:?}"
            );
        }
    }

    let input: Vec<_> = Tokenizer::new(input).collect();
    let expected: Vec<_> = Tokenizer::new(expected).collect();
    let actual: Vec<_> = Tokenizer::new(&actual).collect();
    assert_token_eq(&input, &expected, "input tokens differs from expected tokens");
    assert_token_eq(&actual, &expected, "actual tokens differs from expected tokens");
    // sanity check
    assert_token_eq(&input, &actual, "input tokens differs from actual tokens");
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

    assert_preserved("{some_object do_thing $var}");
    assert_format(
        "{some_object do_big_thing {other_object get var1} {other_object get var2} {other_object get var3}}",
        "{some_object do_big_thing\
       \n   {other_object get var1}\
       \n   {other_object get var2}\
       \n   {other_object get var3}\
       \n}",
    );
    assert_format(
        "{some_object foreach_thing $thing {print $thing}}",
        "{some_object foreach_thing $thing\
       \n   {print $thing}\
       \n}",
    );
    assert_format(
        "{some_object with_thing $the_thing {print $the_thing} {print [thing]}}",
        "{some_object with_thing $the_thing\
       \n   {print $the_thing}\
       \n   {print\
       \n      [thing]\
       \n   }\
       \n}",
    );

    assert_preserved("{$object do_thing $var}");
    assert_format(
        "{$object do_big_thing {$other_object get var1} {$other_object get var2} {$other_object get var3}}",
        "{$object do_big_thing\
       \n   {$other_object get var1}\
       \n   {$other_object get var2}\
       \n   {$other_object get var3}\
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

    assert_preserved("{\"object\" do_thing $var}");
    assert_format(
        "{\"object\" do_big_thing {\"other_object\" get var1} {\"other_object\" get var2} {\"other_object\" get var3}}",
        "{\"object\" do_big_thing\
       \n   {\"other_object\" get var1}\
       \n   {\"other_object\" get var2}\
       \n   {\"other_object\" get var3}\
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
        "{{get_thing} do_thing $var}",
        "{\
       \n   {get_thing}\
       \n   do_thing\
       \n   $var\
       \n}",
    );
    assert_format(
        "{{get_thing} do_big_thing {{get_other_thing} get var1} {{get_other_thing} get var2} {{get_other_thing} get var3}}",
        "{\
       \n   {get_thing}\
       \n   do_big_thing\
       \n   {\
       \n      {get_other_thing}\
       \n      get\
       \n      var1\
       \n   }\
       \n   {\
       \n      {get_other_thing}\
       \n      get\
       \n      var2\
       \n   }\
       \n   {\
       \n      {get_other_thing}\
       \n      get\
       \n      var3\
       \n   }\
       \n}",
    );
    assert_format(
        "{{get_thing} foreach_thing $thing {print $thing} {...}}",
        "{\
       \n   {get_thing}\
       \n   foreach_thing\
       \n   $thing\
       \n   {print $thing}\
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
        "{[thing] do_thing $var}",
        "{\
       \n   [thing]\
       \n   do_thing\
       \n   $var\
       \n}",
    );
    assert_format(
        "{[thing] do_big_thing {[other_thing] get var1} {[other_thing] get var2} {[other_thing] get var3}}",
        "{\
       \n   [thing]\
       \n   do_big_thing\
       \n   {\
       \n      [other_thing]\
       \n      get\
       \n      var1\
       \n   }\
       \n   {\
       \n      [other_thing]\
       \n      get\
       \n      var2\
       \n   }\
       \n   {\
       \n      [other_thing]\
       \n      get\
       \n      var3\
       \n   }\
       \n}",
    );
    assert_format(
        "{[thing] foreach_thing $thing {print $thing} {...}}",
        "{\
       \n   [thing]\
       \n   foreach_thing\
       \n   $thing\
       \n   {print $thing}\
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
    assert_preserved("#define /* comment */ kDefine (1)");
    assert_preserved("#define kDefine /* comment */ (1)");
    assert_preserved("#define kDefine (/* comment */ 1)");
    assert_preserved("#define kDefine (1) /* comment */");

    assert_format(
        "/* comment */ #undef kDefine",
        "/* comment */\
       \n#undef kDefine",
    );
    assert_preserved("#undef /* comment */ kDefine");
    assert_preserved("#undef kDefine /* comment */");

    assert_format(
        "/* comment */ #include items.dta",
        "/* comment */\
       \n#include items.dta",
    );
    assert_preserved("#include /* comment */ items.dta");
    assert_preserved("#include items.dta /* comment */");

    assert_format(
        "/* comment */ #include_opt items.dta",
        "/* comment */\
       \n#include_opt items.dta",
    );
    assert_preserved("#include_opt /* comment */ items.dta");
    assert_preserved("#include_opt items.dta /* comment */");

    assert_format(
        "/* comment */ #merge items.dta",
        "/* comment */\
       \n#merge items.dta",
    );
    assert_preserved("#merge /* comment */ items.dta");
    assert_preserved("#merge items.dta /* comment */");

    assert_format(
        "/* comment */ #autorun {print \"bar\"}",
        "/* comment */\
       \n#autorun {print \"bar\"}",
    );
    assert_preserved("#autorun /* comment */ {print \"bar\"}");
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
        "#ifdef /* comment */ kDefine\
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
        "#ifdef /* comment */ kDefine\
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
fn formatter_directives() {
    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 100) #endif\
       \n; arson-fmt off\
       \n#ifdef kDefine (array1 50) #else (array2 100) #endif\
       \n; arson-fmt on\
       \n#ifdef kDefine (array1 50) #else (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif\
       \n; arson-fmt off\
       \n#ifdef kDefine (array1 50) #else (array2 100) #endif\
       \n; arson-fmt on\
       \n#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "(\
       \n\
       \n   foo\
       \n; arson-fmt off: test\
       \n (bar 100)     \
       \n      (baz\
       \n                 \"asdf\")\
       \n\
       \n; arson-fmt on: test\
       \n (bar 100)     \
       \n      (baz\
       \n                 \"asdf\")\
       \n\
       \n)",
        "(foo\
       \n   ; arson-fmt off: test\
       \n (bar 100)     \
       \n      (baz\
       \n                 \"asdf\")\
       \n\
       \n   ; arson-fmt on: test\
       \n   (bar 100)\
       \n   (baz \"asdf\")\
       \n)",
    );
    assert_format(
        "   ; arson-fmt off: test \
       \nasdf\
       \n   ; arson-fmt on: test ",
        "; arson-fmt off: test \
       \nasdf\
       \n; arson-fmt on: test ",
    );
    assert_preserved(
        "; arson-fmt off\
       \n; arson-fmt on",
    );

    assert_format(
        "#ifdef kDefine (array1 50) #else (array2 100) #endif\
       \n/* arson-fmt off */\
       \n#ifdef kDefine (array1 50) #else (array2 100) #endif\
       \n/* arson-fmt on */\
       \n#ifdef kDefine (array1 50) #else (array2 100) #endif",
        "#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif\
       \n/* arson-fmt off */\
       \n#ifdef kDefine (array1 50) #else (array2 100) #endif\
       \n/* arson-fmt on */\
       \n#ifdef kDefine\
       \n(array1 50)\
       \n#else\
       \n(array2 100)\
       \n#endif",
    );
    assert_format(
        "(\
       \n\
       \n   foo\
       \n/* arson-fmt off: test */\
       \n (bar 100)     \
       \n      (baz\
       \n                 \"asdf\")\
       \n\
       \n/* arson-fmt on: test */\
       \n (bar 100)     \
       \n      (baz\
       \n                 \"asdf\")\
       \n\
       \n)",
        "(foo\
       \n   /* arson-fmt off: test */\
       \n (bar 100)     \
       \n      (baz\
       \n                 \"asdf\")\
       \n\
       \n   /* arson-fmt on: test */\
       \n   (bar 100)\
       \n   (baz \"asdf\")\
       \n)",
    );
    assert_format(
        "/* arson-fmt off */ asdf /* arson-fmt on */",
        "/* arson-fmt off */\
       \n asdf\
       \n/* arson-fmt on */",
    );
    assert_format(
        "   /* arson-fmt off: test */ \
       \nasdf\
       \n   /* arson-fmt on: test */ ",
        "/* arson-fmt off: test */ \
       \nasdf\
       \n/* arson-fmt on: test */",
    );
    assert_preserved(
        "/* arson-fmt off */\
       \n/* arson-fmt on */",
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
        "(\
       \n\
       \n   foo\
       \n   (bar 50)\
       \n   (quz 100)\
       \n)",
        "(foo\
       \n   (bar 50)\
       \n   (quz 100)\
       \n)",
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
