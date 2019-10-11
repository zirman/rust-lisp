use std::borrow::Borrow;
use std::collections::BTreeMap;
use std::mem::swap;
use std::rc::Rc;

use crate::bind::{Bind, BindMut};
use crate::lisp_parse::expr;
use crate::parse::Parser;

use LispError::*;
use LispVal::*;

#[derive(Clone, Debug, PartialEq)]
pub enum LispVal {
    Atom(OwnedString),
    List(Vec<LispVal>),
    DottedList(Box<LispVal>, Box<LispVal>),
    Number(i64),
    String(OwnedString),
    Bool(bool),
    LispFn(
        Rc<LispContext>,
        OwnedString,
        Vec<OwnedString>,
        Rc<Vec<LispVal>>,
    ),
}

#[derive(Clone, Debug, PartialEq)]
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

pub type ThrowsError<A> = Result<A, LispError>;

pub type OwnedString = std::string::String;

// TODO: break apart and cleanup
// TODO: let special form isolation
// TODO: infix notation
// TODO: optimization
// TODO: Stack Machine

// TODO: tail-call optimization
// TODO: def-macro
// TODO: product type
// TODO: sum type
// TODO: pattern matching
// TODO: type checking
// TODO: polymorphism
// TODO: Input/Output
// [+]
// (let x 10)
// [+, 10]
// (let y (fn (x y) (+ x y)))
// [+, 10, (fn (x y) (+ x y))]
// (let z (y x x))
// [+, 10, (fn (x y) (+ x y)), 20]

// > (let q 10)
// [10] #[q => s[0]]
// > (let r (fn r (x y) (+ r q)))
// [10, (fn r (x) (+ s[-1] s[0]))] #[q => s[0]]
// [(if (<= s[1] 0) x (s[0] (- s[1] 1))))] s=0
// > (r 2) => (s[0] 2) => [(if (<= s[1] 0) x (s[0] (- s[1] 1)))), (if (<= s[1] 0) x (s[0] (- s[1] 1)))), 2] eval s[1]
// [(if (<= s[1] 0) x (s[0] (- s[1] 1)))), (if (<= s[1] 0) x (s[0] (- s[1] 1)))), 2]
// [(fn r (x) (if (<= x 0) x (r (- x 1)))), 2] eval (if (if (<= x 0) x (r (-x 1))))

#[derive(Clone, Debug, PartialEq)]
pub enum LispContext {
    Void,
    Scope {
        map: BTreeMap<OwnedString, LispVal>,
        next: Rc<LispContext>,
    },
}

impl LispContext {
    pub fn new() -> LispContext {
        LispContext::Scope {
            map: BTreeMap::new(),
            next: Rc::new(LispContext::Void),
        }
    }
    pub fn new_next(next: Rc<LispContext>) -> LispContext {
        LispContext::Scope {
            map: BTreeMap::new(),
            next,
        }
    }
    fn lookup(&self, name: &str) -> Option<&LispVal> {
        let mut lexical = self;
        loop {
            match lexical {
                LispContext::Void => {
                    return None;
                }
                LispContext::Scope { map, next } => {
                    if let Some(lisp_val) = map.get(name) {
                        return Some(lisp_val);
                    } else {
                        lexical = next.borrow();
                    }
                }
            }
        }
    }
    fn add(&mut self, name: OwnedString, lisp_val: LispVal) {
        match self {
            LispContext::Void => {
                unimplemented!();
            }
            LispContext::Scope { map, next: _ } => {
                map.insert(name, lisp_val);
            }
        }
    }
    fn push(&mut self) -> Rc<LispContext> {
        // TODO: not use double swap
        let mut lexical = LispContext::Void;
        swap(self, &mut lexical);
        let next = Rc::new(lexical);
        swap(self, &mut LispContext::new_next(next.clone()));
        next
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
            Atom(name) => self.apply_atom(name),
            DottedList(func, args_list) => self
                .eval(func)
                .bind_mut(|func| collapse_dotted_list(func, args_list).bind_mut(|x| self.eval(&x))),
            LispFn(_, _, _, _) => Ok(lisp_expr.clone()),
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
                        self.apply_quote(&args[0])
                    } else {
                        Err(BadSpecialForm(
                            "quote form takes 1 argument",
                            lisp_expr.clone(),
                        ))
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
                        Err(BadSpecialForm(
                            "if form takes 3 arguments",
                            lisp_expr.clone(),
                        ))
                    }
                }
                "let" => {
                    if args.len() == 2 {
                        unpack_atom(&args[0]).bind_mut(|name| {
                            self.eval(&args[1]).bind_mut(|lisp_val| {
                                self.add(name.to_owned(), lisp_val);
                                Ok(List(vec![]))
                            })
                        })
                    } else {
                        Err(BadSpecialForm(
                            "let form takes 2 arguments",
                            lisp_expr.clone(),
                        ))
                    }
                }
                "fn" => {
                    if args.len() >= 3 {
                        unpack_atom(&args[0]).bind_mut(|name| {
                            unpack_list_ref(&args[1])
                                .bind(unpack_list_of_atoms)
                                .bind_mut(|parameter_names| {
                                    Ok(LispFn(
                                        self.push(),
                                        name.to_owned(),
                                        parameter_names,
                                        Rc::new(args[2..].to_vec()),
                                    ))
                                })
                        })
                    } else {
                        Err(BadSpecialForm(
                            "fn form takes 2 or more arguments",
                            lisp_expr.clone(),
                        ))
                    }
                }
                name => map_err_mut(args, |x| self.eval(x))
                    .bind(|eval_args: Vec<LispVal>| apply_fn(name, &eval_args)),
            },
            List(func) => {
                let mut ls = func.clone();
                ls.extend(args.into_iter().cloned());
                self.eval(&List(ls))
            }
            // (let fib (fn fib (x) (if (<= x 1) x (+ (fib (- x 2)) (fib (- x 1))))))
            LispFn(scope, name, parameter_names, body) => {
                if args.len() > parameter_names.len() {
                    Err(Default("too many args"))
                } else {
                    map_err_mut(args, |x| self.eval(x)).bind_mut(|args| {
                        let mut lexical = LispContext::new_next(scope.clone());
                        lexical.add(
                            name.clone(),
                            LispFn(
                                scope.clone(),
                                name.clone(),
                                parameter_names.clone(),
                                body.clone(),
                            ),
                        );
                        for i in 0..args.len() {
                            lexical.add(parameter_names[i].clone(), args[i].clone());
                        }
                        if args.len() < parameter_names.len() {
                            Ok(LispFn(
                                Rc::new(lexical),
                                name.to_owned(),
                                parameter_names[args.len()..].to_vec(),
                                body.clone(),
                            ))
                        } else {
                            if body.len() > 0 {
                                let mut i = 0;
                                loop {
                                    let result = lexical.eval(&body[i]);
                                    if i >= body.len() - 1 || result.is_err() {
                                        break result;
                                    }
                                    i += 1;
                                }
                            } else {
                                Err(Default("no body"))
                            }
                        }
                    })
                }
            }
            _ => unimplemented!(),
        }
    }

    fn apply_quote(&mut self, lisp_val: &LispVal) -> ThrowsError<LispVal> {
        match lisp_val {
            List(ls) => {
                if ls.len() > 0 {
                    match &ls[0] {
                        Atom(name) => {
                            if name == "unquote" {
                                if ls.len() == 2 {
                                    self.eval(&ls[1])
                                } else {
                                    Err(BadSpecialForm(
                                        "unquote takes 1 argument",
                                        lisp_val.clone(),
                                    ))
                                }
                            } else {
                                map_err_mut(ls, |x| self.apply_quote(&x)).map(|x| List(x))
                            }
                        }
                        _ => map_err_mut(ls, |x| self.apply_quote(&x)).map(|x| List(x)),
                    }
                } else {
                    Ok(lisp_val.clone())
                }
            }
            DottedList(head, tail) => collapse_dotted_list(*head.clone(), tail)
                .bind_mut(|lisp_val| self.apply_quote(&lisp_val)),
            _ => Ok(lisp_val.clone()),
        }
    }

    fn apply_atom(&mut self, name: &str) -> ThrowsError<LispVal> {
        if let Some(lisp_val) = self.lookup(name) {
            Ok(lisp_val.clone())
        } else {
            // primitive functions atoms eval to their name
            match name {
                "quote" => Ok(()),
                "if" => Ok(()),
                "let" => Ok(()),
                "fn" => Ok(()),
                "+" => Ok(()),
                "-" => Ok(()),
                "*" => Ok(()),
                "/" => Ok(()),
                "%" => Ok(()),
                "quotient" => Ok(()),
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
    }
}

fn collapse_dotted_list(head: LispVal, mut tail: &LispVal) -> ThrowsError<LispVal> {
    let mut ls: Vec<LispVal> = vec![head];
    loop {
        match tail {
            List(lisp_vals) => {
                ls.extend(lisp_vals.clone());
                return Ok(List(ls));
            }
            DottedList(head, next_tail) => {
                ls.push(*head.clone());
                tail = &next_tail;
            }
            _ => {
                return Err(Default("non args"));
            }
        }
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
        let args_len = args.len();
        if args_len == 2 {
            f(&args[0], &args[1])
        } else if args_len < 2 {
            let mut ls = vec![Atom(func.to_owned())];
            ls.extend(args.into_iter().cloned());
            Ok(List(ls))
        } else {
            Err(NumArgs(2, args.to_vec()))
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

fn unpack_atom(lisp_val: &LispVal) -> ThrowsError<&str> {
    match lisp_val {
        Atom(name) => Ok(name),
        not_bool => Err(TypeMismatch("bool", not_bool.clone())),
    }
}

fn unpack_list_ref(lisp_val: &LispVal) -> ThrowsError<&[LispVal]> {
    match lisp_val {
        List(ls) => Ok(ls),
        not_dotted_list => Err(TypeMismatch("dotted list", not_dotted_list.clone())),
    }
}

fn unpack_list_of_atoms(lisp_vals: &[LispVal]) -> ThrowsError<Vec<OwnedString>> {
    let mut v = vec![];
    for lisp_val in lisp_vals {
        match lisp_val {
            Atom(name) => v.push(name.to_owned()),
            _ => return Err(Default("only atoms allowed")),
        }
    }
    Ok(v)
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

// TODO: Rollback changes on error
fn map_err_mut<F>(iter: &[LispVal], mut f: F) -> ThrowsError<Vec<LispVal>>
where
    F: FnMut(&LispVal) -> ThrowsError<LispVal>,
{
    let mut lisp_vals = vec![];
    for lisp_val in iter {
        match f(&lisp_val) {
            Ok(result) => lisp_vals.push(result),
            Err(err) => return Err(err),
        }
    }
    Ok(lisp_vals)
}

pub fn lisp_vals_to_string(items: &[LispVal]) -> OwnedString {
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
        LispFn(_scope, _name, _head, _body) => {
            // TODO: make prettier
            "#fn ".chars().collect()
        }
    }
}
