use crate::parse::*;

type OwnedString = std::string::String;

#[derive(Clone, Debug, PartialEq)]
pub enum LispVal {
    Atom(OwnedString),
    List(Vec<LispVal>),
    DottedList(Vec<LispVal>, Box<LispVal>),
    Number(i64),
    String(OwnedString),
    Bool(bool),
}

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
        String(contents)=>
            "\"".chars().chain(contents.chars()).chain("\"".chars()).collect(),
        Atom(name) =>
            (*name).clone(),
        Number(contents) =>
            format!("{}", contents).to_owned(),
        Bool(true) =>
            "#t".to_owned(),
        Bool(false) =>
            "#f".to_owned(),
        List(items) =>
            "(".chars()
                .chain(
                    lispvals_to_string(items)
                    .chars()
                )
                .chain(")".chars())
                .collect(),
        DottedList(head, tail) =>
            "(".chars()
                .chain(
                    lispvals_to_string(head)
                        .chars()
                )
                .chain(" . ".chars())
                .chain(show_val(tail).chars())
                .chain(")".chars())
                .collect(),
    }
}

pub fn eval(val: LispVal) -> LispVal {
    match val {
        String(_) =>
            val,
        Number(_) =>
            val,
        Bool(_) =>
            val,
        List(items) =>
            match items.split_first() {
                Some((func, rest)) =>
                    if *func == Atom("quote".to_owned()) {
                        match rest.split_first() {
                            Some((x, [])) => x.clone(),
                            _ => unimplemented!()
                        }
                    } else {
                        unimplemented!()
                    },
                _ =>
                    unimplemented!(),
            },
        Atom(_name) =>
            unimplemented!(),
        DottedList(_head, _tail) =>
            unimplemented!(),
    }
}
