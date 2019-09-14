mod bind;
mod lisp;
mod lisp_parse;
mod parse;

use crate::bind::BindMut;
use crate::lisp::{read_expr, show_val, trap_error, LispContext};
use std::io::{self, BufRead};

fn main() {
    let mut lisp_interpreter = LispContext::new();

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
