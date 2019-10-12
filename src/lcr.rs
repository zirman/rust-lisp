use crate::lisp::{LispError, LispVal, OwnedString, ThrowsError};
use std::alloc::{dealloc, Layout};
use std::borrow::{Borrow, BorrowMut};
use std::collections::BTreeMap;
use std::ptr;
use std::rc::Rc;

type LcrContext = Vec<LcrPrimitive>;

#[derive(Clone, Debug, PartialEq)]
enum LcrPrimitive {
    // base types
    Bool(bool),
    I32(i32),
    String(Rc<OwnedString>),
    Closure(LcrContext, usize, Rc<LCR>),
    RecClosure(usize, Rc<LCR>),
    // built in functions
    Add,
    CurryAdd(i32),
    Sub,
    CurrySub(i32),
    Mul,
    CurryMul(i32),
    Div,
    CurryDiv(i32),
    Mod,
    CurryMod(i32),
    Eq,
    CurryEq(i32),
    LT,
    CurryLT(i32),
    GT,
    CurryGT(i32),
    NEq,
    CurryNEq(i32),
    GTE,
    CurryGTE(i32),
    LTE,
    CurryLTE(i32),
    And,
    CurryAnd(bool),
    Or,
    CurryOr(bool),
    Not,
    StrEq,
    CurryStrEq(Rc<OwnedString>),
    StrLT,
    CurryStrLT(Rc<OwnedString>),
    StrGT,
    CurryStrGT(Rc<OwnedString>),
    StrLTE,
    CurryStrLTE(Rc<OwnedString>),
    StrGTE,
    CurryStrGTE(Rc<OwnedString>),
}

#[derive(Clone, Debug, PartialEq)]
enum LCR {
    // TODO: Make If take vectors of predicates and consequences
    If(Box<LCR>, Box<LCR>, Box<LCR>),
    // TODO: Make Apply take vectors of arguments
    Apply(Box<LCR>, Box<LCR>),
    Recur(Box<LCR>),
    Primitive(LcrPrimitive),
    Lookup(usize),
}

fn eval(lcr: &LCR, context: &LcrContext) -> ThrowsError<LcrPrimitive> {
    match lcr {
        LCR::If(predicate, consequent, alternative) => match eval(predicate, context) {
            Ok(x) => match x {
                LcrPrimitive::Bool(p) => {
                    if p {
                        eval(consequent, context)
                    } else {
                        eval(alternative, context)
                    }
                }
                _ => Err(LispError::Default("expected bool")),
            },
            Err(e) => Err(e),
        },
        LCR::Apply(function, argument) => {
            match eval(function, context) {
                Ok(mut function) => {
                    match eval(argument, context) {
                        Ok(argument) => {
                            match function {
                                LcrPrimitive::Closure(mut closure, free, body) => {
                                    closure.push(argument);
                                    if closure.len() == free {
                                        eval(&body, &closure)
                                    } else {
                                        Ok(LcrPrimitive::Closure(closure, free, body))
                                    }
                                }
                                LcrPrimitive::RecClosure(free, body) => {
                                    let mut v = Vec::with_capacity(free);
                                    v.push(argument);
                                    if v.len() == free {
                                        eval(&body, &v)
                                    } else {
                                        Ok(LcrPrimitive::Closure(v, free, body))
                                    }
                                }
                                LcrPrimitive::Add => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryAdd(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::Sub => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurrySub(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::Mul => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryMul(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::Div => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryDiv(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::Mod => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryMod(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::Eq => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryEq(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::LT => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryLT(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::GT => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryGT(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::NEq => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryNEq(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::GTE => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryGTE(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::LTE => match argument {
                                    LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryLTE(x)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::And => match argument {
                                    LcrPrimitive::Bool(x) => Ok(LcrPrimitive::CurryAnd(x)),
                                    _ => Err(LispError::Default("expected bool")),
                                },
                                LcrPrimitive::Or => match argument {
                                    LcrPrimitive::Bool(x) => Ok(LcrPrimitive::CurryOr(x)),
                                    _ => Err(LispError::Default("expected bool")),
                                },
                                LcrPrimitive::Not => match argument {
                                    LcrPrimitive::Bool(x) => Ok(LcrPrimitive::Bool(!x)),
                                    _ => Err(LispError::Default("expected bool")),
                                },
                                LcrPrimitive::StrEq => match argument {
                                    LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrEq(x)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::StrLT => match argument {
                                    LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrLT(x)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::StrGT => match argument {
                                    LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrGT(x)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::StrLTE => match argument {
                                    LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrLTE(x)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::StrGTE => match argument {
                                    LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrGTE(x)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::CurryAdd(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::I32(x + y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurrySub(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::I32(x - y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurryMul(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::I32(x * y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurryDiv(x) => {
                                    match argument {
                                        LcrPrimitive::I32(y) => {
                                            // TODO: Handle div by zero
                                            Ok(LcrPrimitive::I32(x / y))
                                        }
                                        _ => Err(LispError::Default("expected i32")),
                                    }
                                }
                                LcrPrimitive::CurryMod(x) => {
                                    match argument {
                                        LcrPrimitive::I32(y) => {
                                            // TODO: Handle div by zero
                                            Ok(LcrPrimitive::I32(x % y))
                                        }
                                        _ => Err(LispError::Default("expected i32")),
                                    }
                                }
                                LcrPrimitive::CurryEq(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x == y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurryLT(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x < y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurryGT(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x > y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurryNEq(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x != y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurryGTE(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x >= y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurryLTE(x) => match argument {
                                    LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x <= y)),
                                    _ => Err(LispError::Default("expected i32")),
                                },
                                LcrPrimitive::CurryAnd(x) => match argument {
                                    LcrPrimitive::Bool(y) => Ok(LcrPrimitive::Bool(x && y)),
                                    _ => Err(LispError::Default("expected bool")),
                                },
                                LcrPrimitive::CurryOr(x) => match argument {
                                    LcrPrimitive::Bool(y) => Ok(LcrPrimitive::Bool(x || y)),
                                    _ => Err(LispError::Default("expected bool")),
                                },
                                LcrPrimitive::CurryStrEq(x) => match argument {
                                    LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x == y)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::CurryStrLT(x) => match argument {
                                    LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x < y)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::CurryStrGT(x) => match argument {
                                    LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x > y)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::CurryStrLTE(x) => match argument {
                                    LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x <= y)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                LcrPrimitive::CurryStrGTE(x) => match argument {
                                    LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x >= y)),
                                    _ => Err(LispError::Default("expected string")),
                                },
                                _ => Err(LispError::Default("unexpected")),
                            }
                        }
                        Err(e) => Err(e),
                    }
                }
                Err(e) => Err(e),
            }
        }
        LCR::Recur(argument) => match eval(argument, context) {
            Ok(argument) => {
                let mut x = &context[0];
                match x {
                    LcrPrimitive::RecClosure(free, body) => {
                        let mut closure: Vec<LcrPrimitive> = Vec::with_capacity(*free);
                        closure.push(LcrPrimitive::RecClosure(*free, body.clone()));
                        closure.push(argument);
                        if closure.len() == *free {
                            eval(&body, &closure)
                        } else {
                            Ok(LcrPrimitive::Closure(closure, *free, body.clone()))
                        }
                    }
                    _ => Err(LispError::Default("expected closure")),
                }
            }
            Err(e) => Err(e),
        },
        LCR::Primitive(x) => Ok(x.clone()),
        LCR::Lookup(i) => match context.get(*i) {
            None => Err(LispError::Default("cannot find")),
            Some(x) => Ok(x.clone()),
        },
    }
}

fn eval_tco(mut lcr: *const LCR, mut context: *const LcrContext) -> ThrowsError<LcrPrimitive> {
    let mut cleanup = false;
    let mut lcr_root: *const LCR = lcr;
    loop {
        let x = match unsafe { &*lcr } {
            LCR::If(predicate, consequent, alternative) => {
                match eval_tco(predicate.borrow(), context) {
                    Ok(x) => match x {
                        LcrPrimitive::Bool(p) => {
                            if !cleanup {
                                cleanup = true;
                                lcr_root = lcr;
                            }
                            lcr = if p { consequent } else { alternative }.borrow();
                            continue;
                        }
                        _ => Err(LispError::Default("expected bool")),
                    },
                    Err(e) => Err(e),
                }
            }
            LCR::Apply(function, argument) => {
                match eval_tco(function.borrow(), context) {
                    Ok(mut function) => {
                        match eval_tco(argument.borrow(), context) {
                            Ok(argument) => {
                                match function {
                                    LcrPrimitive::Closure(mut closure, free, body) => {
                                        closure.push(argument);
                                        if closure.len() == free {
                                            let lcr_tmp = Rc::into_raw(body);
                                            let context_tmp = Box::into_raw(Box::new(closure));
                                            if cleanup {
                                                unsafe {
                                                    Rc::from_raw(lcr_root);
                                                    Box::from_raw(context as *mut LcrContext);
                                                }
                                            } else {
                                                cleanup = true;
                                            }
                                            lcr_root = lcr_tmp;
                                            lcr = lcr_tmp;
                                            context = context_tmp;
                                            continue;
                                        } else {
                                            Ok(LcrPrimitive::Closure(closure, free, body))
                                        }
                                    }
                                    LcrPrimitive::RecClosure(free, body) => {
                                        let mut v = Vec::with_capacity(free);
                                        v.push(argument);
                                        if v.len() == free {
                                            let lcr_tmp = Rc::into_raw(body);
                                            let context_tmp = Box::into_raw(Box::new(vec![]));
                                            if cleanup {
                                                unsafe {
                                                    Rc::from_raw(lcr_root);
                                                    Box::from_raw(context as *mut LcrContext);
                                                }
                                            } else {
                                                cleanup = true;
                                            }
                                            lcr_root = lcr_tmp;
                                            lcr = lcr_tmp;
                                            context = context_tmp;
                                            continue;
                                        } else {
                                            Ok(LcrPrimitive::Closure(v, free, body))
                                        }
                                    }
                                    LcrPrimitive::Add => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryAdd(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::Sub => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurrySub(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::Mul => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryMul(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::Div => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryDiv(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::Mod => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryMod(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::Eq => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryEq(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::LT => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryLT(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::GT => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryGT(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::NEq => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryNEq(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::GTE => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryGTE(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::LTE => match argument {
                                        LcrPrimitive::I32(x) => Ok(LcrPrimitive::CurryLTE(x)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::And => match argument {
                                        LcrPrimitive::Bool(x) => Ok(LcrPrimitive::CurryAnd(x)),
                                        _ => Err(LispError::Default("expected bool")),
                                    },
                                    LcrPrimitive::Or => match argument {
                                        LcrPrimitive::Bool(x) => Ok(LcrPrimitive::CurryOr(x)),
                                        _ => Err(LispError::Default("expected bool")),
                                    },
                                    LcrPrimitive::Not => match argument {
                                        LcrPrimitive::Bool(x) => Ok(LcrPrimitive::Bool(!x)),
                                        _ => Err(LispError::Default("expected bool")),
                                    },
                                    LcrPrimitive::StrEq => match argument {
                                        LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrEq(x)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::StrLT => match argument {
                                        LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrLT(x)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::StrGT => match argument {
                                        LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrGT(x)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::StrLTE => match argument {
                                        LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrLTE(x)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::StrGTE => match argument {
                                        LcrPrimitive::String(x) => Ok(LcrPrimitive::CurryStrGTE(x)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::CurryAdd(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::I32(x + y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurrySub(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::I32(x - y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurryMul(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::I32(x * y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurryDiv(x) => {
                                        match argument {
                                            LcrPrimitive::I32(y) => {
                                                // TODO: Handle div by zero
                                                Ok(LcrPrimitive::I32(x / y))
                                            }
                                            _ => Err(LispError::Default("expected i32")),
                                        }
                                    }
                                    LcrPrimitive::CurryMod(x) => {
                                        match argument {
                                            LcrPrimitive::I32(y) => {
                                                // TODO: Handle div by zero
                                                Ok(LcrPrimitive::I32(x % y))
                                            }
                                            _ => Err(LispError::Default("expected i32")),
                                        }
                                    }
                                    LcrPrimitive::CurryEq(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x == y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurryLT(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x < y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurryGT(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x > y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurryNEq(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x != y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurryGTE(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x >= y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurryLTE(x) => match argument {
                                        LcrPrimitive::I32(y) => Ok(LcrPrimitive::Bool(x <= y)),
                                        _ => Err(LispError::Default("expected i32")),
                                    },
                                    LcrPrimitive::CurryAnd(x) => match argument {
                                        LcrPrimitive::Bool(y) => Ok(LcrPrimitive::Bool(x && y)),
                                        _ => Err(LispError::Default("expected bool")),
                                    },
                                    LcrPrimitive::CurryOr(x) => match argument {
                                        LcrPrimitive::Bool(y) => Ok(LcrPrimitive::Bool(x || y)),
                                        _ => Err(LispError::Default("expected bool")),
                                    },
                                    LcrPrimitive::CurryStrEq(x) => match argument {
                                        LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x == y)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::CurryStrLT(x) => match argument {
                                        LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x < y)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::CurryStrGT(x) => match argument {
                                        LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x > y)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::CurryStrLTE(x) => match argument {
                                        LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x <= y)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    LcrPrimitive::CurryStrGTE(x) => match argument {
                                        LcrPrimitive::String(y) => Ok(LcrPrimitive::Bool(x >= y)),
                                        _ => Err(LispError::Default("expected string")),
                                    },
                                    _ => Err(LispError::Default("unexpected")),
                                }
                            }
                            Err(e) => Err(e),
                        }
                    }
                    Err(e) => Err(e),
                }
            }
            LCR::Recur(argument) => match eval_tco(argument.borrow(), context) {
                Ok(argument) => {
                    let mut x = unsafe { &(*context)[0] };
                    match x {
                        LcrPrimitive::RecClosure(free, body) => {
                            let mut closure: Vec<LcrPrimitive> = Vec::with_capacity(*free);
                            closure.push(LcrPrimitive::RecClosure(*free, body.clone()));
                            closure.push(argument);
                            if closure.len() == *free {
                                let lcr_tmp = Rc::into_raw(body.clone());
                                let context_tmp = Box::into_raw(Box::new(closure));
                                if cleanup {
                                    unsafe {
                                        Rc::from_raw(lcr_root);
                                        Box::from_raw(context as *mut LcrContext);
                                    }
                                } else {
                                    cleanup = true;
                                }
                                lcr_root = lcr_tmp;
                                lcr = lcr_tmp;
                                context = context_tmp;
                                continue;
                            } else {
                                Ok(LcrPrimitive::Closure(closure, *free, body.clone()))
                            }
                        }
                        _ => Err(LispError::Default("expected closure")),
                    }
                }
                Err(e) => Err(e),
            },
            LCR::Primitive(x) => Ok(x.clone()),
            LCR::Lookup(i) => match unsafe { &*context }.get(*i) {
                None => Err(LispError::Default("cannot find ")),
                Some(x) => Ok(x.clone()),
            },
        };
        if cleanup {
            unsafe {
                Rc::from_raw(lcr_root);
                Box::from_raw(context as *mut LcrContext);
            }
        }
        return x;
    }
}

pub fn foo() {
    //    let closure = LCR::Primitive(LcrPrimitive::RecClosure(
    //        2,
    //        Rc::new(LCR::If(
    //            Box::new(LCR::Apply(
    //                Box::new(LCR::Primitive(LcrPrimitive::CurryLTE(9999999))),
    //                Box::new(LCR::Lookup(1)),
    //            )),
    //            Box::new(LCR::Lookup(1)),
    //            Box::new(LCR::Recur(Box::new(LCR::Apply(
    //                Box::new(LCR::Primitive(LcrPrimitive::CurryAdd(1))),
    //                Box::new(LCR::Lookup(1)),
    //            )))),
    //        )),
    //    ));
    //    println!("{:?}", unsafe {
    //        eval_tco(
    //            &LCR::Apply(
    //                Box::new(LCR::Apply(Box::new(closure.clone()), Box::new(closure))),
    //                Box::new(LCR::Primitive(LcrPrimitive::I32(0))),
    //            ),
    //            &Vec::with_capacity(0),
    //        )
    //    });
    //    (fn fib (x) (if (<= x 1) x (+ x (fib (- x 2)) (fib (- x 1)))))
    let closure = LCR::Primitive(LcrPrimitive::RecClosure(
        2,
        Rc::new(LCR::If(
            Box::new(LCR::Apply(
                Box::new(LCR::Primitive(LcrPrimitive::CurryGTE(1))),
                Box::new(LCR::Lookup(1)),
            )),
            Box::new(LCR::Lookup(1)),
            Box::new(LCR::Apply(
                Box::new(LCR::Apply(
                    Box::new(LCR::Primitive(LcrPrimitive::Add)),
                    Box::new(LCR::Recur(Box::new(LCR::Apply(
                        Box::new(LCR::Primitive(LcrPrimitive::CurryAdd(-2))),
                        Box::new(LCR::Lookup(1)),
                    )))),
                )),
                Box::new(LCR::Recur(Box::new(LCR::Apply(
                    Box::new(LCR::Primitive(LcrPrimitive::CurryAdd(-1))),
                    Box::new(LCR::Lookup(1)),
                )))),
            )),
        )),
    ));
    println!(
        "{:?}",
        eval_tco(
            &LCR::Apply(
                Box::new(LCR::Apply(Box::new(closure.clone()), Box::new(closure))),
                Box::new(LCR::Primitive(LcrPrimitive::I32(34))),
            ),
            &Vec::with_capacity(0),
        )
    );
}
