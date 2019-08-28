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
        Default(_) => "".to_owned(),
    }
}

use crate::Bind;
use std::collections::HashMap;
use LispVal::*;

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

struct BinOp(Box<dyn Fn(i64, i64) -> i64 + Sync>);

lazy_static! {
    static ref PRIMITIVES: HashMap<&'static str, BinOp> = {
        let mut m = HashMap::new();
        m.insert("+", BinOp(Box::new(|x, y| x + y)));
        m.insert("-", BinOp(Box::new(|x, y| x - y)));
        m.insert("*", BinOp(Box::new(|x, y| x * y)));
        m.insert("/", BinOp(Box::new(|x, y| x / y)));
        m.insert("mod", BinOp(Box::new(|x, y| x % y)));
        m.insert("quotient", BinOp(Box::new(|x, y| x / y)));
        m.insert("remainder", BinOp(Box::new(|x, y| x % y)));
        m
    };
}

pub fn apply(s: &str, ls: &Vec<LispVal>) -> ThrowsError<LispVal> {
    (PRIMITIVES.get(s) as Option<&BinOp>)
        .ok_or(LispError::NotFunction("Unrecognized primitive function args", s.to_owned()))
        .bind(|f| numeric_bin_op(f, ls))
}

fn numeric_bin_op(bin_op: &BinOp, ls: &Vec<LispVal>) -> ThrowsError<LispVal> {
    if ls.len() == 2 {
        unpack_num(ls.get(0).unwrap())
            .bind(|x|
                unpack_num(ls.get(1).unwrap())
                    .bind(|y| Ok(Number(bin_op.0(x, y))))
            )
    } else {
        Err(NumArgs(2 as u8, ls.clone()))
    }
}

fn unpack_num(lispval: &LispVal) -> ThrowsError<i64> {
    match lispval {
        Number(x) => Ok(x.clone()),
        not_num => Err(TypeMismatch("number", not_num.clone())),
    }
}
