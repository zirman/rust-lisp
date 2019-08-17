//use crate::lisp::parse_atom;

mod lisp;
mod parse;

use lisp::*;
use parse::*;

fn unwrap(x: Res<LispVal>) -> LispVal {
    x.unwrap_or_else(|_| unimplemented!()).0
}

fn main() {
    println!("{:?}", expr().parse("(a test)"));
    println!("{:?}", expr().parse("(a (nested) test)"));
    println!("{:?}", expr().parse("(a (dotted . list) test)"));
    println!("{:?}", expr().parse("(a '(quoted (dotted . list)) test)"));

    println!("{:?}", show_val(&unwrap(expr().parse("(a test)"))));
    println!("{:?}", show_val(&unwrap(expr().parse("(a (nested) test)"))));
    println!("{:?}", show_val(&unwrap(expr().parse("(a (dotted . list) test)"))));
    println!("{:?}", show_val(&unwrap(expr().parse("(a '(quoted (dotted . list)) test)"))));

    println!("{:?}", eval(unwrap(expr().parse("\"rob\""))));
    println!("{:?}", eval(unwrap(expr().parse("123"))));
    println!("{:?}", eval(unwrap(expr().parse("#t"))));
    println!("{:?}", eval(unwrap(expr().parse("'(a b c)"))));
}
