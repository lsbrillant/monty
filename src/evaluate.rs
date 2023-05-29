use std::borrow::Cow;

use crate::object::Object;
use crate::run::RunResult;
use crate::types::{Builtins, CmpOperator, Expr, ExprLoc, Function, Kwarg, Operator};

pub(crate) struct Evaluator<'a> {
    namespace: &'a [Object],
}

impl<'a> Evaluator<'a> {
    pub fn new(namespace: &'a [Object]) -> Self {
        Self { namespace }
    }

    pub fn evaluate(&self, expr_loc: &'a ExprLoc) -> RunResult<Cow<'a, Object>> {
        match &expr_loc.expr {
            Expr::Constant(object) => Ok(Cow::Borrowed(object)),
            Expr::Name(ident) => {
                if let Some(object) = self.namespace.get(ident.id) {
                    match object {
                        Object::Undefined => {
                            Err(format!("({}) name '{}' is not defined", expr_loc.position, ident.name).into())
                        }
                        _ => Ok(Cow::Borrowed(object)),
                    }
                } else {
                    Err(format!("({}) name '{}' is not defined", expr_loc.position, ident.name).into())
                }
            }
            Expr::Call { func, args, kwargs } => self.call_function(func, args, kwargs),
            Expr::Op { left, op, right } => self.op(left, op, right),
            Expr::CmpOp { left, op, right } => Ok(Cow::Owned(self.cmp_op(left, op, right)?.into())),
            Expr::List(elements) => {
                let objects = elements
                    .iter()
                    .map(|e| self.evaluate(e).map(|ob| ob.into_owned()))
                    .collect::<RunResult<_>>()?;
                Ok(Cow::Owned(Object::List(objects)))
            }
        }
    }

    pub fn evaluate_bool(&self, expr_loc: &ExprLoc) -> RunResult<bool> {
        match &expr_loc.expr {
            Expr::CmpOp { left, op, right } => self.cmp_op(left, op, right),
            _ => self.evaluate(expr_loc)?.as_ref().bool(),
        }
    }

    fn op(&self, left: &'a ExprLoc, op: &'a Operator, right: &'a ExprLoc) -> RunResult<Cow<'a, Object>> {
        let left_object = self.evaluate(left)?;
        let right_object = self.evaluate(right)?;
        let op_object: Option<Object> = match op {
            Operator::Add => left_object.add(&right_object),
            Operator::Sub => left_object.sub(&right_object),
            Operator::Mod => left_object.modulo(&right_object),
            _ => return Err(format!("Operator {op:?} not yet implemented").into()),
        };
        match op_object {
            Some(object) => Ok(Cow::Owned(object)),
            None => Err(format!(
                "({}) Cannot apply operator {left} {op} {right}",
                left.position.extend(&right.position)
            )
            .into()),
        }
    }

    fn cmp_op(&self, left: &ExprLoc, op: &CmpOperator, right: &ExprLoc) -> RunResult<bool> {
        let left_object = self.evaluate(left)?;
        let right_object = self.evaluate(right)?;
        let op_object: Option<bool> = match op {
            CmpOperator::Eq => left_object.as_ref().eq(&right_object),
            CmpOperator::NotEq => left_object.as_ref().eq(&right_object).map(|object| !object),
            CmpOperator::Gt => Some(left_object.gt(&right_object)),
            CmpOperator::GtE => Some(left_object.ge(&right_object)),
            CmpOperator::Lt => Some(left_object.lt(&right_object)),
            CmpOperator::LtE => Some(left_object.le(&right_object)),
            _ => return Err(format!("CmpOperator {op} not yet implemented").into()),
        };
        match op_object {
            Some(object) => Ok(object),
            None => Err(format!(
                "({}) Cannot apply comparison operator {left} {op} {right}",
                left.position.extend(&right.position)
            )
            .into()),
        }
    }

    pub fn call_function(
        &self,
        function: &'a Function,
        args: &'a [ExprLoc],
        _kwargs: &'a [Kwarg],
    ) -> RunResult<Cow<'a, Object>> {
        let builtin = match function {
            Function::Builtin(builtin) => builtin,
            Function::Ident(_) => return Err("User defined functions not yet implemented".into()),
        };
        match builtin {
            Builtins::Print => {
                for (i, arg) in args.iter().enumerate() {
                    let object = self.evaluate(arg)?;
                    if i == 0 {
                        print!("{object}");
                    } else {
                        print!(" {object}");
                    }
                }
                println!();
                Ok(Cow::Owned(Object::None))
            }
            Builtins::Range => {
                if args.len() != 1 {
                    Err("range() takes exactly one argument".into())
                } else {
                    let object = self.evaluate(&args[0])?;
                    match object.as_ref() {
                        Object::Int(size) => Ok(Cow::Owned(Object::Range(*size))),
                        _ => Err("range() argument must be an integer".into()),
                    }
                }
            }
            Builtins::Len => {
                if args.len() != 1 {
                    Err(format!("len() takes exactly exactly one argument ({} given)", args.len()).into())
                } else {
                    let object = self.evaluate(&args[0])?;
                    match object.len() {
                        Some(len) => Ok(Cow::Owned(Object::Int(len as i64))),
                        None => Err(format!("Object of type {object} has no len()").into()),
                    }
                }
            }
        }
    }
}
