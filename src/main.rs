//use crate::lisp::parse_atom;

mod lisp;
mod parse;

use lisp::*;
use parse::*;

fn main() {
    println!("{:?}", expr().parse("(a test)"));
    println!("{:?}", expr().parse("(a (nested) test)"));
    println!("{:?}", expr().parse("(a (dotted . list) test)"));
    println!("{:?}", expr().parse("(a '(quoted (dotted . list)) test)"));

    println!("{:?}", show_val(&expr().parse("(a test)").unwrap_or_else(|_| unimplemented!()).0));
    println!("{:?}", show_val(&expr().parse("(a (nested) test)").unwrap_or_else(|_| unimplemented!()).0));
    println!("{:?}", show_val(&expr().parse("(a (dotted . list) test)").unwrap_or_else(|_| unimplemented!()).0));
    println!("{:?}", show_val(&expr().parse("(a '(quoted (dotted . list)) test)").unwrap_or_else(|_| unimplemented!()).0));
}
