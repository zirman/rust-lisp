mod bind;
mod lcr;
mod lisp;
mod lisp_parse;
mod parse;

extern crate time;
use crate::bind::BindMut;
use crate::lcr::foo;
use crate::lisp::{read_expr, show_val, trap_error, LispContext};
use std::io::{self, BufRead};
use time::PreciseTime;

fn main() {
    let start = PreciseTime::now();
    foo();
    let end = PreciseTime::now();
    println!("{:?}", start.to(end));

    //    let mut lisp_interpreter = LispContext::new();
    //
    //    for line_err in io::stdin().lock().lines() {
    //        let result_str = trap_error(
    //            read_expr(&line_err.unwrap())
    //                .bind_mut(|lisp_expr| lisp_interpreter.eval(&lisp_expr))
    //                .map(|x| show_val(&x)),
    //        )
    //        .unwrap();
    //
    //        println!("{}", result_str);
    //    }
}
