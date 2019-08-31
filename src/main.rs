mod bind;
mod lisp;
mod parse;

use std::io::{self, BufRead};

use bind::*;
use lisp::*;
use parse::*;

use LispError::ParseError;

fn main() {
    let mut lisp_interpreter = LispInterpreter::new();

    for line_err in io::stdin().lock().lines() {
        let result_str = trap_error(
            expr()
                .parse(&line_err.unwrap())
                .map_err(|rest| ParseError(rest.to_owned()))
                .bind(|(lisp_expr, rest)| {
                    if rest.is_empty() {
                        Ok(lisp_expr)
                    } else {
                        Err(ParseError(rest.to_owned()))
                    }
                })
                .bind_mut(|lisp_expr| lisp_interpreter.eval(&lisp_expr))
                .map(|x| show_val(&x)),
        )
        .unwrap();

        println!("{}", result_str);
    }
}
