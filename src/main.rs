mod bind;
mod lisp;
mod parse;

use std::io::{self, BufRead};

use bind::*;
use lisp::*;

fn main() {
    let mut lisp_interpreter = LispInterpreter::new();

    for line_err in io::stdin().lock().lines() {
        let result_str = trap_error(
            read_expr(&line_err.unwrap())
                .bind_mut(|lisp_expr| lisp_interpreter.eval(&lisp_expr))
                .map(|x| show_val(&x)),
        )
        .unwrap();

        println!("{}", result_str);
    }
}
