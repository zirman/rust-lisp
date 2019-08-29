#[macro_use]
extern crate lazy_static;

mod lisp;
mod parse;

use lisp::*;
use parse::*;

use std::io::{self, BufRead, Error};

//fn unwrap(x: Res<LispVal>) -> LispVal {
//    x.unwrap_or_else(|_| unimplemented!()).0
//}

pub trait Bind<R, F, A, B>
where
    F: Fn(A) -> R,
{
    fn bind(self, f: F) -> R;
}

impl<F, A, B, E> Bind<Result<B, E>, F, A, B> for Result<A, E>
where
    F: Fn(A) -> Result<B, E>,
{
    fn bind(self, f: F) -> Result<B, E> {
        match self {
            Ok(x) => f(x),
            Err(e) => Err(e),
        }
    }
}

impl<F, A, B> Bind<Option<B>, F, A, B> for Option<A>
where
    F: Fn(A) -> Option<B>,
{
    fn bind(self, f: F) -> Option<B> {
        match self {
            Some(x) => f(x),
            None => None,
        }
    }
}

#[derive(Debug)]
enum Errors {
    IO(Error),
    Parse(String),
}

fn main() {
    //    println!("{:?}", expr().parse("(a test)"));
    //    println!("{:?}", expr().parse("(a (nested) test)"));
    //    println!("{:?}", expr().parse("(a (dotted . list) test)"));
    //    println!("{:?}", expr().parse("(a '(quoted (dotted . list)) test)"));
    //
    //    println!("{:?}", show_val(&unwrap(expr().parse("(a test)"))));
    //    println!("{:?}", show_val(&unwrap(expr().parse("(a (nested) test)"))));
    //    println!(
    //        "{:?}",
    //        show_val(&unwrap(expr().parse("(a (dotted . list) test)")))
    //    );
    //    println!(
    //        "{:?}",
    //        show_val(&unwrap(expr().parse("(a '(quoted (dotted . list)) test)")))
    //    );
    //
    //    println!("{:?}", eval(unwrap(expr().parse("\"rob\""))));
    //    println!("{:?}", eval(unwrap(expr().parse("123"))));
    //    println!("{:?}", eval(unwrap(expr().parse("#t"))));
    //    println!("{:?}", eval(unwrap(expr().parse("'(a b c)"))));

    for line in io::stdin().lock().lines() {
        match line.map_err(Errors::IO).bind(|x| {
            expr()
                .parse(&x)
                .map_err(|y| Errors::Parse(y.to_owned()))
                .bind(|(x, rest)| {
                    if rest.is_empty() {
                        Ok(x)
                    } else {
                        Err(Errors::Parse(rest.to_owned()))
                    }
                })
                .map(|x| eval(&x))
        }) {
            Ok(x) => println!("{}", &extract_value(trap_error(x.map(|z| show_val(&z))))),
            Err(e) => println!("Error: {:?}", e),
        }
    }
}
