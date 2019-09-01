use std::collections::HashMap;

use crate::parse::*;
use crate::{Bind, BindMut};

type OwnedString = std::string::String;

pub struct LispInterpreter {}

type LispFn = Box<dyn Fn(&[LispVal]) -> ThrowsError<LispVal> + Sync>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LispVal {
    Atom(OwnedString),
    List(Vec<LispVal>),
    DottedList(Box<LispVal>, Box<LispVal>),
    Number(i64),
    String(OwnedString),
    Bool(bool),
    PrimitiveFn(PrimFn),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PrimFn {
    NumAdd,
    NumSub,
    NumMul,
    NumDiv,
    NumMod,
    NumQuot,
    NumRem,
    NumEq,
    NumLt,
    NumGt,
    NumNe,
    NumGte,
    NumLte,
    BoolAnd,
    BoolOr,
    StringEq,
    StringLt,
    StringGt,
    StringLte,
    StringGte,
    Cons,
    Car,
    Cdr,
}

use PrimFn::*;

use LispVal::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LispError {
    NumArgs(u8, Vec<LispVal>),
    TypeMismatch(&'static str, LispVal),
    ParseError(OwnedString),
    BadSpecialForm(&'static str, LispVal),
    NotFunction(&'static str, OwnedString),
    UnboundVar(OwnedString, OwnedString),
    DivByZero,
    Default(&'static str),
}

use LispError::*;

type ThrowsError<A> = Result<A, LispError>;

impl LispInterpreter {
    pub(crate) fn new() -> LispInterpreter {
        LispInterpreter {}
    }

    pub fn eval(&mut self, lisp_expr: &LispVal) -> ThrowsError<LispVal> {
        match lisp_expr {
            String(_) => Ok(lisp_expr.clone()),
            Number(_) => Ok(lisp_expr.clone()),
            Bool(_) => Ok(lisp_expr.clone()),
            List(items) => items
                .split_first()
                .ok_or(BadSpecialForm(
                    "Empty function application",
                    lisp_expr.clone(),
                ))
                .bind_mut(|(func, rest)| self.apply_form(lisp_expr, func, rest)),
            Atom(name) => apply_atom(name).map(PrimitiveFn),
            DottedList(func, args_list) =>
                self.eval_dotted_list(lisp_expr, func, args_list),
            PrimitiveFn(_) => Ok(lisp_expr.clone()),
        }
    }

    fn apply_form(
        &mut self,
        val: &LispVal,
        func: &LispVal,
        args: &[LispVal],
    ) -> ThrowsError<LispVal> {
        match func {
            Atom(x) => match x.as_ref() {
                "quote" => {
                    if args.len() == 1 {
                        Ok(args[0].clone())
                    } else {
                        Err(BadSpecialForm("quote form takes 1 argument", val.clone()))
                    }
                }
                "if" => {
                    if args.len() == 3 {
                        self.eval(&args[0])
                            .bind(|lisp_val| unpack_bool(&lisp_val))
                            .bind_mut(|predicate| {
                                self.eval(if predicate { &args[1] } else { &args[2] })
                            })
                    } else {
                        Err(BadSpecialForm("if form takes 3 arguments", val.clone()))
                    }
                }
                name => {
                    let mut eval_args: Vec<LispVal> = vec![];

                    for lisp_val in args.iter() {
                        match self.eval(lisp_val) {
                            Ok(arg) => eval_args.push(arg),
                            Err(lisp_error) => {
                                return Err(lisp_error);
                            }
                        }
                    }

                    apply_atom(name)
                        .bind(|x| apply_fn(x, &eval_args))
                }
            },
            PrimitiveFn(prim_fn) => {
                let mut eval_args: Vec<LispVal> = vec![];

                for lisp_val in args.iter() {
                    match self.eval(lisp_val) {
                        Ok(arg) => eval_args.push(arg),
                        Err(lisp_error) => {
                            return Err(lisp_error);
                        }
                    }
                }

                apply_fn(prim_fn.clone(), &eval_args)
            },
            _ => unimplemented!(),
        }
    }

    fn eval_dotted_list(
        &mut self,
        val: &LispVal,
        func: &LispVal,
        args_list: &LispVal,
    ) -> ThrowsError<LispVal> {
        self.eval(func).bind_mut(|func| {
            self.eval_dotted_list_tail(vec![], args_list.to_owned())
                .bind_mut(|args| self.apply_form(val, &func, &args))
        })
    }

    fn eval_dotted_list_tail(
        &mut self,
        mut ls: Vec<LispVal>,
        tail: LispVal,
    ) -> ThrowsError<Vec<LispVal>> {
        match tail {
            List(xs) => {
                //TODO
                ls.extend(
                    xs.into_iter()
                        .map(|x| self.eval(&x))
                        .map(extract_value)
                        .collect::<Vec<LispVal>>(),
                );
                Ok(ls)
            }
            DottedList(head, tail) => match self.eval(head.as_ref()) {
                Ok(x) => {
                    ls.push(x);
                    self.eval_dotted_list_tail(ls, *tail)
                }
                Err(e) => Err(e),
            },
            _ => Err(Default("non args")),
        }
    }
}

pub fn extract_value<A>(throws_error: ThrowsError<A>) -> A {
    throws_error.expect("")
}

fn read_expr(source: &str) -> ThrowsError<LispVal> {
    match expr().parse(source) {
        Ok((x, _)) => Ok(x),
        Err(y) => Err(ParseError(y.to_owned())),
    }
}

fn apply_fn(prim_fn: PrimFn, ls: &[LispVal]) -> ThrowsError<LispVal> {
    match prim_fn {
        NumAdd => num_bin_op(ls, |x, y| Ok(Number(x + y))),
        NumSub => num_bin_op(ls, |x, y| Ok(Number(x - y))),
        NumMul => num_bin_op(ls, |x, y| Ok(Number(x * y))),
        NumDiv => num_bin_op(ls, |x, y| {
            if y != 0 { Ok(Number(x / y)) } else { Err(DivByZero) }
        }),
        NumMod => num_bin_op(ls, |x, y| {
            if y != 0 { Ok(Number(x % y)) } else { Err(DivByZero) }
        }),
        NumQuot => num_bin_op(ls, |x, y| {
            if y != 0 { Ok(Number(x / y)) } else { Err(DivByZero) }
        }),
        NumRem => num_bin_op(ls, |x, y| {
            if y != 0 { Ok(Number(x % y)) } else { Err(DivByZero) }
        }),
        NumEq => num_bin_op(ls, |x, y| Ok(Bool(x == y))),
        NumLt => num_bin_op(ls, |x, y| Ok(Bool(x < y))),
        NumGt => num_bin_op(ls, |x, y| Ok(Bool(x > y))),
        NumNe => num_bin_op(ls, |x, y| Ok(Bool(x != y))),
        NumGte => num_bin_op(ls, |x, y| Ok(Bool(x >= y))),
        NumLte => num_bin_op(ls, |x, y| Ok(Bool(x <= y))),
        BoolAnd => bool_bin_op(ls, |x, y| Ok(Bool(x && y))),
        BoolOr => bool_bin_op(ls, |x, y| Ok(Bool(x || y))),
        StringEq => string_bin_op(ls, |x, y| Ok(Bool(x == y))),
        StringLt => string_bin_op(ls, |x, y| Ok(Bool(x < y))),
        StringGt => string_bin_op(ls, |x, y| Ok(Bool(x > y))),
        StringLte => string_bin_op(ls, |x, y| Ok(Bool(x <= y))),
        StringGte => string_bin_op(ls, |x, y| Ok(Bool(x >= y))),
        Cons => bin_op(ls, |head, tail| Ok(DottedList(Box::new(head.clone()), Box::new(tail.clone())))),
        Car => unary_op(ls, |x| {
            unpack_dotted_list(x)
                .map(|(head, _)| head.clone())
                .or_else(|_| {
                    unpack_list_ref(x).bind(|ls| {
                        if ls.len() == 1 {
                            Ok(ls[0].clone())
                        } else {
                            Err(NumArgs(1, ls.to_vec()))
                        }
                    })
                })
        }),
        Cdr => unary_op(ls, |x| {
            unpack_dotted_list(x)
                .map(|(_, tail)| tail.clone())
                .or_else(|_| {
                    unpack_list_ref(x).bind(|ls| {
                        if ls.len() == 1 {
                            Ok(ls[0].clone())
                        } else {
                            Err(NumArgs(1, ls.to_vec()))
                        }
                    })
                })
        }),
    }
}

fn show_error(error: LispError) -> OwnedString {
    match error {
        NumArgs(expected, found) => "Expected "
            .chars()
            .chain(format!("{}", expected).chars())
            .chain(" args; found values ".chars())
            .chain(lisp_vals_to_string(&found).chars())
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
        Default(err) => err.to_owned(),
    }
}

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
    expr().bind(|head| {
        spaces()
            .next(pchar('.'))
            .next(spaces())
            .next(expr())
            .fmap(Box::new)
            .fmap(move |tail| DottedList(Box::new(head.clone()), tail))
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

fn lisp_vals_to_string(items: &[LispVal]) -> OwnedString {
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
            .chain(lisp_vals_to_string(items).chars())
            .chain(")".chars())
            .collect(),
        DottedList(head, tail) => "("
            .chars()
            .chain(show_val(head).chars())
            .chain(" . ".chars())
            .chain(show_val(tail).chars())
            .chain(")".chars())
            .collect(),
        PrimitiveFn(prim_fn) => format!("primitive function {:?}", prim_fn),
    }
}

fn apply_atom(name: &str) -> ThrowsError<PrimFn> {
    match name {
        "+" => Ok(NumAdd),
        "-" => Ok(NumSub),
        "*" => Ok(NumMul),
        "/" => Ok(NumDiv),
        "mod" => Ok(NumMod),
        "" => Ok(NumQuot),
        "remainder" => Ok(NumRem),
        "=" => Ok(NumEq),
        "<" => Ok(NumLt),
        ">" => Ok(NumGt),
        "/=" => Ok(NumNe),
        ">=" => Ok(NumGte),
        "<=" => Ok(NumLte),
        "&&" => Ok(BoolAnd),
        "||" => Ok(BoolOr),
        "string=?" => Ok(StringEq),
        "string<?" => Ok(StringLt),
        "string>?" => Ok(StringGt),
        "string<=?" => Ok(StringLte),
        "string>=?" => Ok(StringGte),
        "cons" => Ok(Cons),
        "car" => Ok(Car),
        "cdr" => Ok(Cdr),
        _ => Err(NotFunction("not a function", name.to_owned())),
    }
}

fn unary_op<F>(args: &[LispVal], f: F) -> ThrowsError<LispVal>
    where
        F: Fn(&LispVal) -> ThrowsError<LispVal>,
{
    if args.len() == 1 {
        f(&args[0])
    } else {
        Err(NumArgs(2, args.to_vec()))
    }
}

fn bin_op<F>(args: &[LispVal], f: F) -> ThrowsError<LispVal>
    where
        F: Fn(&LispVal, &LispVal) -> ThrowsError<LispVal>,
{
    if args.len() == 2 {
        f(&args[0], &args[1])
    } else {
        Err(NumArgs(2, args.to_vec()))
    }
}

fn num_bin_op<F>(args: &[LispVal], f: F) -> ThrowsError<LispVal>
    where
        F: Fn(i64, i64) -> ThrowsError<LispVal>,
{
    if args.len() == 2 {
        unpack_num(&args[0]).bind(|x| {
            unpack_num(&args[1]).bind(|y| {
                f(x, y)
            })
        })
    } else {
        Err(NumArgs(2, args.to_vec()))
    }
}

fn bool_bin_op<F>(args: &[LispVal], f: F) -> ThrowsError<LispVal>
    where
        F: Fn(bool, bool) -> ThrowsError<LispVal>,
{
    if args.len() == 2 {
        unpack_bool(&args[0]).bind(|x| {
            unpack_bool(&args[1]).bind(|y| {
                f(x,y)
            })
        })
    } else {
        Err(NumArgs(2, args.to_vec()))
    }
}

fn string_bin_op<F>(args: &[LispVal], f: F) -> ThrowsError<LispVal>
    where
        F: Fn(&str, &str) -> ThrowsError<LispVal>,
{
    if args.len() == 2 {
        unpack_str(&args[0]).bind(|x| {
            unpack_str(&args[1]).bind(|y| {
                f(x, y)
            })
        })
    } else {
        Err(NumArgs(2, args.to_vec()))
    }
}

fn unpack_atom(lisp_val: &LispVal) -> ThrowsError<OwnedString> {
    match lisp_val {
        Atom(name) => Ok(name.clone()),
        not_atom => Err(TypeMismatch("atom", not_atom.clone())),
    }
}

fn unpack_num(lisp_val: &LispVal) -> ThrowsError<i64> {
    match lisp_val {
        Number(x) => Ok(x.clone()),
        not_num => Err(TypeMismatch("number", not_num.clone())),
    }
}

fn unpack_bool(lisp_val: &LispVal) -> ThrowsError<bool> {
    match lisp_val {
        Bool(x) => Ok(x.clone()),
        not_bool => Err(TypeMismatch("bool", not_bool.clone())),
    }
}

fn unpack_list(lisp_val: LispVal) -> ThrowsError<Vec<LispVal>> {
    match lisp_val {
        List(ls) => Ok(ls),
        not_dotted_list => Err(TypeMismatch("dotted list", not_dotted_list.clone())),
    }
}

fn unpack_list_ref(lisp_val: &LispVal) -> ThrowsError<&[LispVal]> {
    match lisp_val {
        List(ls) => Ok(ls),
        not_dotted_list => Err(TypeMismatch("dotted list", not_dotted_list.clone())),
    }
}

fn unpack_dotted_list(lisp_val: &LispVal) -> ThrowsError<(&LispVal, &LispVal)> {
    match lisp_val {
        DottedList(head, tail) => Ok((head, tail)),
        not_dotted_list => Err(TypeMismatch("dotted list", not_dotted_list.clone())),
    }
}

fn unpack_str(lisp_val: &LispVal) -> ThrowsError<&str> {
    match lisp_val {
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
