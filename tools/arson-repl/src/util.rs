// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io;

#[macro_export]
macro_rules! print_flush {
    ($($arg:tt)*) => {{
        print!($($arg)*);
        std::io::Write::flush(&mut std::io::stdout()).expect("failed to flush stdout");
    }}
}

pub fn readln() -> io::Result<String> {
    let mut line = String::new();
    io::stdin().read_line(&mut line).map(|_| line.trim_end().to_owned())
}

pub fn prompt_question(question: &str) -> bool {
    print_flush!("{question} (y/n) ");

    loop {
        let answer = readln().expect("failed to read response");

        if answer.eq_ignore_ascii_case("y") || answer.eq_ignore_ascii_case("yes") {
            return true;
        } else if answer.eq_ignore_ascii_case("n") || answer.eq_ignore_ascii_case("no") {
            return false;
        }

        print_flush!("Unrecognized answer, please try again: ");
    }
}

pub fn prompt_str(message: &str) -> String {
    print_flush!("{message}: ");
    readln().expect("failed to read response")
}
