// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io;

#[macro_export]
macro_rules! print_flush {
    ($($arg:tt)*) => {{
        use ::std::io::Write;
        let mut stdout = ::std::io::stdout().lock();
        write!(stdout, $($arg)*).expect("failed to write to stdout");
        stdout.flush().expect("failed to flush stdout");
    }}
}

pub fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).expect("failed to read line");
    line.trim().to_owned()
}

pub fn prompt_question(question: &str) -> bool {
    print_flush!("{question} (y/n) ");

    loop {
        let answer = read_line();

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
    read_line()
}
