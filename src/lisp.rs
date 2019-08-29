use crate::parse::*;

type OwnedString = std::string::String;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LispVal {
    Atom(OwnedString),
    List(Vec<LispVal>),
    DottedList(Vec<LispVal>, Box<LispVal>),
    Number(i64),
    String(OwnedString),
    Bool(bool),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LispError {
    NumArgs(u8, Vec<LispVal>),
    TypeMismatch(&'static str, LispVal),
    ParseError(OwnedString),
    BadSpecialForm(OwnedString, LispVal),
    NotFunction(&'static str, OwnedString),
    UnboundVar(OwnedString, OwnedString),
    DivByZero,
    Default(OwnedString),
}

use LispError::*;

type ThrowsError<A> = Result<A, LispError>;

pub fn extract_value<A>(throws_error: ThrowsError<A>) -> A {
    throws_error.expect("")
}

fn read_expr(source: &str) -> ThrowsError<LispVal> {
    match expr().parse(source) {
        Ok((x, y)) => Ok(x),
        Err(y) => Err(ParseError(y.to_owned())),
    }
}

fn show_error(error: LispError) -> OwnedString {
    match error {
        NumArgs(expected, found) => "Expected "
            .chars()
            .chain(format!("{}", expected).chars())
            .chain(" args; found values ".chars())
            .chain(lispvals_to_string(&found).chars())
            .collect::<OwnedString>(),
        TypeMismatch(expected, found) => "Invalid type: expected "
            .chars()
            .chain(expected.chars())
            .chain(", found ".chars())
            .chain(show_val(&found).chars())
            .collect::<OwnedString>(),
        ParseError(parse_err) => "Parse error at "
            .chars()
            .chain(parse_err.chars())
            .collect::<OwnedString>(),
        BadSpecialForm(message, form) => message
            .chars()
            .chain(": ".chars())
            .chain(show_val(&form).chars())
            .collect::<OwnedString>(),
        NotFunction(message, func) => message
            .chars()
            .chain(": ".chars())
            .chain(func.chars())
            .collect::<OwnedString>(),
        UnboundVar(message, var_name) => message
            .chars()
            .chain(": ".chars())
            .chain(var_name.chars())
            .collect::<OwnedString>(),
        DivByZero => "Divided by zero".to_owned(),
        Default(_) => "".to_owned(),
    }
}

use crate::Bind;
use std::collections::HashMap;
use LispVal::*;
use std::marker::PhantomData;

pub fn letter() -> impl Parser<char> {
    predicate(|c| c.is_alphabetic())
}

pub fn symbol() -> impl Parser<char> {
    one_of("!#$%&|*+-/:<=>?@^_~")
}

pub fn digit() -> impl Parser<char> {
    predicate(|c| c.is_ascii_digit())
}

pub fn spaces() -> impl Parser<()> {
    skip_many1(predicate(|c| c.is_whitespace()))
}

pub fn string() -> impl Parser<LispVal> {
    pchar('"')
        .next(many(none_of("\"")))
        .fmap(|cs| cs.into_iter().collect::<OwnedString>())
        .prev(pchar('"'))
        .fmap(String)
}

pub fn atom() -> impl Parser<LispVal> {
    letter()
        .or(symbol())
        .bind(|first| {
            many(letter().or(digit()).or(symbol()))
                .fmap(move |rest| vec![first].into_iter().chain(rest))
        })
        .fmap(|x| x.collect::<OwnedString>())
        .fmap(|s| match s.as_ref() {
            "#t" => Bool(true),
            "#f" => Bool(false),
            _ => Atom(s),
        })
}

pub fn number() -> impl Parser<LispVal> {
    many1(predicate(|c| c.is_numeric())).fmap(|x| {
        Number(
            x.into_iter()
                .collect::<OwnedString>()
                .parse::<i64>()
                .unwrap(),
        )
    })
}

pub fn list() -> impl Parser<LispVal> {
    sep_by(expr(), spaces()).fmap(List)
}

pub fn dotted_list() -> impl Parser<LispVal> {
    end_by(expr(), spaces()).bind(|head| {
        pchar('.')
            .next(spaces())
            .next(expr())
            .fmap(Box::new)
            .fmap(move |tail| DottedList(head.clone(), tail))
    })
}

pub fn quoted() -> impl Parser<LispVal> {
    pchar('\'')
        .next(expr())
        .fmap(|x| List(vec![Atom("quote".to_owned()), x]))
}

pub fn expr() -> LispExprParser {
    LispExprParser {}
}

#[derive(Clone)]
pub struct LispExprParser {}

impl Parser<LispVal> for LispExprParser {
    fn parse<'a, 'b>(&'a self, source: &'b str) -> Res<'b, LispVal> {
        atom()
            .or(string())
            .or(number())
            .or(quoted())
            .or(pchar('(').next(dotted_list().or(list())).prev(pchar(')')))
            .parse(source)
    }
}

fn lispvals_to_string(items: &Vec<LispVal>) -> OwnedString {
    items
        .into_iter()
        .map(show_val)
        .collect::<Vec<OwnedString>>()
        .join(" ")
}

pub fn show_val(val: &LispVal) -> OwnedString {
    match val {
        String(contents) => "\""
            .chars()
            .chain(contents.chars())
            .chain("\"".chars())
            .collect(),
        Atom(name) => (*name).clone(),
        Number(contents) => format!("{}", contents).to_owned(),
        Bool(true) => "#t".to_owned(),
        Bool(false) => "#f".to_owned(),
        List(items) => "("
            .chars()
            .chain(lispvals_to_string(items).chars())
            .chain(")".chars())
            .collect(),
        DottedList(head, tail) => "("
            .chars()
            .chain(lispvals_to_string(head).chars())
            .chain(" . ".chars())
            .chain(show_val(tail).chars())
            .chain(")".chars())
            .collect(),
    }
}

pub fn eval(val: &LispVal) -> ThrowsError<LispVal> {
    match val {
        String(_) => Ok(val.clone()),
        Number(_) => Ok(val.clone()),
        Bool(_) => Ok(val.clone()),
        List(items) => match items.split_first() {
            Some((func, rest)) => match func {
                Atom(x) => match x.as_ref() {
                    "quote" => match rest.split_first() {
                        Some((x, [])) => Ok(x.clone()),
                        _ => unimplemented!(),
                    },
                    func => apply(
                        func,
                        &rest
                            .iter()
                            .map(eval)
                            .map(extract_value)
                            .collect::<Vec<LispVal>>(),
                    ),
                },
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        },
        Atom(name) => unimplemented!(),
        DottedList(_head, _tail) => unimplemented!(),
    }
}

struct LispFn(Box<dyn Fn(&Vec<LispVal>) -> ThrowsError<LispVal> + Sync>);
//struct NumBinOp(Box<dyn Fn(i64, i64) -> LispVal + Sync>);
//struct BoolBinOp(Box<dyn Fn(bool, bool) -> LispVal + Sync>);
//struct StrBinOp(Box<dyn Fn(&str, &str) -> bool + Sync>);

fn num_bin_op<F>(f: F) -> LispFn where F: Fn(i64, i64) -> ThrowsError<LispVal> + Sync + 'static {
    LispFn(Box::new(move |ls| {
        if ls.len() == 2 {
            unpack_num(ls.get(0).unwrap()).bind(|x| {
                unpack_num(ls.get(1).unwrap()).bind(|y| {
                    f(x, y)
                })
            })
        } else {
            Err(NumArgs(2, ls.clone()))
        }
    }))
}

fn bool_bin_op<F>(f: F) -> LispFn where F: Fn(bool, bool) -> ThrowsError<LispVal> + Sync + 'static {
    LispFn(Box::new(move |ls| {
        if ls.len() == 2 {
            unpack_bool(ls.get(0).unwrap()).bind(|x| {
                unpack_bool(ls.get(1).unwrap()).bind(|y| {
                    f(x, y)
                })
            })
        } else {
            Err(NumArgs(2, ls.clone()))
        }
    }))
}

fn str_bin_op<F>(f: F) -> LispFn where F: Fn(&str, &str) -> ThrowsError<LispVal> + Sync + 'static {
    LispFn(Box::new(move |ls| {
        if ls.len() == 2 {
            unpack_str(ls.get(0).unwrap()).bind(|x| {
                unpack_str(ls.get(1).unwrap()).bind(|y| {
                    f(x, y)
                })
            })
        } else {
            Err(NumArgs(2, ls.clone()))
        }
    }))
}

lazy_static! {
    static ref PRIMITIVE_FN: HashMap<&'static str, LispFn> = {
        let mut m = HashMap::new();
        m.insert("+", num_bin_op(|x, y| Ok(Number(x + y))));
        m.insert("-", num_bin_op(|x, y| Ok(Number(x - y))));
        m.insert("*", num_bin_op(|x, y| Ok(Number(x * y))));
        m.insert("/", num_bin_op(|x, y| if y != 0 { Ok(Number(x / y)) } else { Err(DivByZero) }));
        m.insert("mod", num_bin_op(|x, y| if y != 0 { Ok(Number(x % y)) } else { Err(DivByZero) }));
        m.insert("quotient", num_bin_op(|x, y| if y != 0 { Ok(Number(x / y)) } else { Err(DivByZero) }));
        m.insert("remainder", num_bin_op(|x, y| if y != 0 { Ok(Number(x % y)) } else { Err(DivByZero) }));

        m.insert("=", num_bin_op(|x, y| Ok(Bool(x == y))));
        m.insert("<", num_bin_op(|x, y| Ok(Bool(x < y))));
        m.insert(">", num_bin_op(|x, y| Ok(Bool(x > y))));
        m.insert("/=", num_bin_op(|x, y| Ok(Bool(x != y))));
        m.insert(">=", num_bin_op(|x, y| Ok(Bool(x >= y))));
        m.insert("<=", num_bin_op(|x, y| Ok(Bool(x <= y))));

        m.insert("&&", bool_bin_op(|x, y| Ok(Bool(x && y))));
        m.insert("||", bool_bin_op(|x, y| Ok(Bool(x || y))));

        m.insert("string=?", str_bin_op(|x, y| Ok(Bool(x == y))));
        m.insert("string<?", str_bin_op(|x, y| Ok(Bool(x < y))));
        m.insert("string>?", str_bin_op(|x, y| Ok(Bool(x > y))));
        m.insert("string<=?", str_bin_op(|x, y| Ok(Bool(x <= y))));
        m.insert("string>=?", str_bin_op(|x, y| Ok(Bool(x >= y))));
        m
    };
}

pub fn apply(s: &str, ls: &Vec<LispVal>) -> ThrowsError<LispVal> {
    (PRIMITIVE_FN.get(s) as Option<&LispFn>)
        .ok_or(LispError::NotFunction(
            "Unrecognized primitive function args",
            s.to_owned(),
        ))
        .bind(|f| f.0(ls))
}

fn unpack_num(lispval: &LispVal) -> ThrowsError<i64> {
    match lispval {
        Number(x) => Ok(x.clone()),
        not_num => Err(TypeMismatch("number", not_num.clone())),
    }
}

fn unpack_bool(lispval: &LispVal) -> ThrowsError<bool> {
    match lispval {
        Bool(x) => Ok(x.clone()),
        not_bool => Err(TypeMismatch("bool", not_bool.clone())),
    }
}

fn unpack_str(lispval: &LispVal) -> ThrowsError<&str> {
    match lispval {
        String(x) => Ok(x),
        not_bool => Err(TypeMismatch("bool", not_bool.clone())),
    }
}

pub fn trap_error(action: ThrowsError<OwnedString>) -> ThrowsError<OwnedString> {
    match action {
        Ok(x) => Ok(x),
        Err(y) => Ok(show_error(y)),
    }
}

// (-> 1 + 2 + 3 + 4)
// (+ 4 (+ 3 (+ 2 1)))
// (<- 1 + 2 + 3 + 4)
// (+ 1 (+ 2 (+ 3 4)))
// ((+ ((+ 1) 2)) 3)
