use crate::parse::*;
use crate::{Bind, BindMut};

type OwnedString = std::string::String;

pub struct LispInterpreter {}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LispVal {
    Atom(OwnedString),
    List(Vec<LispVal>),
    DottedList(Box<LispVal>, Box<LispVal>),
    Number(i64),
    String(OwnedString),
    Bool(bool),
}

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
    pub fn new() -> LispInterpreter {
        LispInterpreter {}
    }

    pub fn eval(&mut self, lisp_expr: &LispVal) -> ThrowsError<LispVal> {
        match lisp_expr {
            String(_) => Ok(lisp_expr.clone()),
            Number(_) => Ok(lisp_expr.clone()),
            Bool(_) => Ok(lisp_expr.clone()),
            List(items) => items
                .split_first()
                .ok_or_else(|| BadSpecialForm("Empty function application", lisp_expr.clone()))
                .bind_mut(|(func, rest)| {
                    self.eval(func)
                        .bind_mut(|func: LispVal| self.apply_form(lisp_expr, &func, rest))
                }),
            Atom(name) => apply_atom(name),
            //(quote . ((+ . (1 . (2 . ()))) . ()))
            DottedList(func, args_list) => self
                .eval(func)
                .bind_mut(|func| self.eval_dotted_list(lisp_expr, &func, args_list)),
        }
    }

    fn apply_form(
        &mut self,
        lisp_expr: &LispVal,
        func: &LispVal,
        args: &[LispVal],
    ) -> ThrowsError<LispVal> {
        match func {
            Atom(x) => match x.as_ref() {
                "quote" => {
                    if args.len() == 1 {
                        Ok(args[0].clone())
                    } else {
                        Err(BadSpecialForm("quote form takes 1 argument", lisp_expr.clone()))
                    }
                }
                "if" => {
                    if args.len() == 3 {
                        self.eval(&args[0])
                            .bind(|lisp_val: LispVal| unpack_bool(&lisp_val))
                            .bind_mut(|predicate: bool| {
                                self.eval(if predicate { &args[1] } else { &args[2] })
                            })
                    } else {
                        Err(BadSpecialForm("if form takes 3 arguments", lisp_expr.clone()))
                    }
                }
                name => self
                    .eval_args(args)
                    .bind(|eval_args: Vec<LispVal>| apply_fn( name, &eval_args)),
            }
            List(func) => {
                let mut l = func.clone();
                l.extend(args.into_iter().cloned());
                self.eval(&List(l))
            }
            _ => {
                unimplemented!()
            }
        }
    }

    fn eval_dotted_list(
        &mut self,
        lisp_expr: &LispVal,
        func: &LispVal,
        args_list: &LispVal,
    ) -> ThrowsError<LispVal> {
        match self.eval_dotted_list_tail(vec![], args_list.to_owned()) {
            Ok(x) => {
                self.apply_form(lisp_expr, func, &x)
            }
            Err(e) => {
                Err(e)
            }
        }
    }

    fn eval_dotted_list_tail(
        &mut self,
        mut ls: Vec<LispVal>,
        tail: LispVal,
    ) -> ThrowsError<Vec<LispVal>> {
        match tail {
            List(xs) => {
                ls.extend(xs);
                Ok(ls)
            }
            DottedList(head, tail) => {
                ls.push(*head);
                self.eval_dotted_list_tail(ls, *tail)
            }
            _ => Err(Default("non args")),
        }
    }

    fn eval_args(&mut self, args: &[LispVal]) -> ThrowsError<Vec<LispVal>> {
        let mut eval_args: Vec<LispVal> = vec![];

        for lisp_val in args.iter() {
            match self.eval(lisp_val) {
                Ok(arg) => eval_args.push(arg),
                Err(lisp_error) => {
                    return Err(lisp_error);
                }
            }
        }

        Ok(eval_args)
    }
}

pub fn read_expr(source: &str) -> ThrowsError<LispVal> {
    expr()
        .parse(source)
        .map_err(|rest| ParseError(rest.to_owned()))
        .bind(|(lisp_expr, rest)| {
            if rest.is_empty() {
                Ok(lisp_expr)
            } else {
                Err(ParseError(rest.to_owned()))
            }
        })
}

fn div_op<F>(f: F) -> impl Fn(i64, i64) -> ThrowsError<LispVal>
where
    F: Fn(i64, i64) -> i64,
{
    move |x: i64, y: i64| {
        if y != 0 {
            Ok(Number(f(x, y)))
        } else {
            Err(DivByZero)
        }
    }
}

fn pack_num_op<F, A, B>(f: F) -> impl Fn(A, B) -> ThrowsError<LispVal>
where
    F: Fn(A, B) -> i64,
{
    move |x: A, y: B| Ok(Number(f(x, y)))
}

fn pack_bool_op<F, A, B>(f: F) -> impl Fn(A, B) -> ThrowsError<LispVal>
where
    F: Fn(A, B) -> bool,
{
    move |x: A, y: B| Ok(Bool(f(x, y)))
}

fn unpack_num_op<F>(f: F) -> impl Fn(&LispVal, &LispVal) -> ThrowsError<LispVal>
where
    F: Fn(i64, i64) -> ThrowsError<LispVal>,
{
    move |x: &LispVal, y: &LispVal| {
        unpack_num(x).bind(|x: i64| unpack_num(y).bind(|y: i64| f(x, y)))
    }
}

fn unpack_bool_op<F>(f: F) -> impl Fn(&LispVal, &LispVal) -> ThrowsError<LispVal>
where
    F: Fn(bool, bool) -> ThrowsError<LispVal>,
{
    move |x: &LispVal, y: &LispVal| {
        unpack_bool(x).bind(|x: bool| unpack_bool(y).bind(|y: bool| f(x, y)))
    }
}

fn apply_fn(func: &str, args: &[LispVal]) -> ThrowsError<LispVal> {
    match func {
        "+" => binary_op(unpack_num_op(pack_num_op(|x: i64, y: i64| x + y)))(func, args),
        "-" => binary_op(unpack_num_op(pack_num_op(|x: i64, y: i64| x - y)))(func, args),
        "*" => binary_op(unpack_num_op(pack_num_op(|x: i64, y: i64| x * y)))(func, args),
        "/" => binary_op(unpack_num_op(div_op(|x: i64, y: i64| x / y)))(func, args),
        "%" => binary_op(unpack_num_op(div_op(|x: i64, y: i64| x % y)))(func, args),
        "quotient" => binary_op(unpack_num_op(div_op(|x: i64, y: i64| x / y)))(func, args),
        "remainder" => binary_op(unpack_num_op(div_op(|x: i64, y: i64| x % y)))(func, args),
        "=" => binary_op(unpack_num_op(pack_bool_op(|x: i64, y: i64| x == y)))(func, args),
        "<" => binary_op(unpack_num_op(pack_bool_op(|x: i64, y: i64| x < y)))(func, args),
        ">" => binary_op(unpack_num_op(pack_bool_op(|x: i64, y: i64| x > y)))(func, args),
        "/=" => binary_op(unpack_num_op(pack_bool_op(|x: i64, y: i64| x != y)))(func, args),
        ">=" => binary_op(unpack_num_op(pack_bool_op(|x: i64, y: i64| x >= y)))(func, args),
        "<=" => binary_op(unpack_num_op(pack_bool_op(|x: i64, y: i64| x <= y)))(func, args),
        "&&" => binary_op(unpack_bool_op(pack_bool_op(|x: bool, y: bool| x && y)))(func, args),
        "||" => binary_op(unpack_bool_op(pack_bool_op(|x: bool, y: bool| x || y)))(func, args),
        "string=?" => binary_op(string_to_bool_op(|x: &str, y: &str| x == y))(func, args),
        "string<?" => binary_op(string_to_bool_op(|x: &str, y: &str| x < y))(func, args),
        "string>?" => binary_op(string_to_bool_op(|x: &str, y: &str| x > y))(func, args),
        "string<=?" => binary_op(string_to_bool_op(|x: &str, y: &str| x <= y))(func, args),
        "string>=?" => binary_op(string_to_bool_op(|x: &str, y: &str| x >= y))(func, args),
        "cons" => binary_op(|head: &LispVal, tail: &LispVal| {
            Ok(DottedList(Box::new(head.clone()), Box::new(tail.clone())))
        })(func, args),
        "car" => unary_op(list_op(|(head, _)| head.clone()))(args),
        "cdr" => unary_op(list_op(|(_, tail)| tail.clone()))(args),
        _ => Err(NotFunction("", func.to_owned())),
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
            .fmap(move |tail: Box<LispVal>| DottedList(Box::new(head.clone()), tail))
    })
}

pub fn quoted() -> impl Parser<LispVal> {
    pchar('\'')
        .next(expr())
        .fmap(|x: LispVal| List(vec![Atom("quote".to_owned()), x]))
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
    }
}

fn apply_atom(name: &str) -> ThrowsError<LispVal> {
    match name {
        "quote" => Ok(()),
        "if" => Ok(()),
        "+" => Ok(()),
        "-" => Ok(()),
        "*" => Ok(()),
        "/" => Ok(()),
        "mod" => Ok(()),
        "" => Ok(()),
        "remainder" => Ok(()),
        "=" => Ok(()),
        "<" => Ok(()),
        ">" => Ok(()),
        "/=" => Ok(()),
        ">=" => Ok(()),
        "<=" => Ok(()),
        "&&" => Ok(()),
        "||" => Ok(()),
        "string=?" => Ok(()),
        "string<?" => Ok(()),
        "string>?" => Ok(()),
        "string<=?" => Ok(()),
        "string>=?" => Ok(()),
        "cons" => Ok(()),
        "car" => Ok(()),
        "cdr" => Ok(()),
        _ => Err(NotFunction("not a function", name.to_owned())),
    }
    .map(|_| Atom(name.to_owned()))
}

fn list_op<F>(f: F) -> impl Fn(&LispVal) -> ThrowsError<LispVal>
where
    F: Fn((&LispVal, &LispVal)) -> LispVal + Copy,
{
    move |x: &LispVal| {
        unpack_dotted_list(x).map(f).or_else(|_| {
            unpack_list_ref(x).bind(|ls: &[LispVal]| {
                if ls.len() == 1 {
                    Ok(ls[0].clone())
                } else {
                    Err(NumArgs(1, ls.to_vec()))
                }
            })
        })
    }
}

fn unary_op<F>(f: F) -> impl Fn(&[LispVal]) -> ThrowsError<LispVal>
where
    F: Fn(&LispVal) -> ThrowsError<LispVal>,
{
    move |args: &[LispVal]| {
        if args.len() == 1 {
            f(&args[0])
        } else {
            Err(NumArgs(1, args.to_vec()))
        }
    }
}

fn binary_op<F>(f: F) -> impl Fn(&str, &[LispVal]) -> ThrowsError<LispVal>
where
    F: Fn(&LispVal, &LispVal) -> ThrowsError<LispVal>,
{
    move |func, args: &[LispVal]| {
        if args.len() > 2 {
            Err(NumArgs(2, args.to_vec()))
        }
        else if args.len() == 2 {
            f(&args[0], &args[1])
        } else {
            let mut ls = vec![Atom(func.to_owned())];
            ls.extend(args.into_iter().cloned());
            Ok(List(ls))
        }
    }
}

fn string_to_bool_op<F>(f: F) -> impl Fn(&LispVal, &LispVal) -> ThrowsError<LispVal>
where
    F: Fn(&str, &str) -> bool,
{
    move |x: &LispVal, y: &LispVal| {
        unpack_string(x).bind(|x: &str| unpack_string(y).bind(|y: &str| Ok(Bool(f(x, y)))))
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

fn unpack_string(lisp_val: &LispVal) -> ThrowsError<&str> {
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
