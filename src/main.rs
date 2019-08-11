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
}
