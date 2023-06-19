use std::borrow::Cow;

use crate::exceptions::{internal_err, ExcType, Exception, InternalRunError};
use crate::expressions::{Expr, ExprLoc, Function, Identifier, Kwarg};
use crate::object::{Attr, Object};
use crate::operators::{CmpOperator, Operator};
use crate::run::RunResult;

pub(crate) struct Evaluator<'d> {
    namespace: &'d [Object],
}

impl<'d> Evaluator<'d> {
    pub fn new(namespace: &'d [Object]) -> Self {
        Self { namespace }
    }

    pub fn evaluate<'c>(&self, expr_loc: &'d ExprLoc<'c>) -> RunResult<'c, Cow<'d, Object>> {
        match &expr_loc.expr {
            Expr::Constant(object) => Ok(Cow::Borrowed(object)),
            Expr::Name(ident) => {
                if let Some(object) = self.namespace.get(ident.id) {
                    match object {
                        Object::Undefined => Err(InternalRunError::Undefined(ident.name.clone().into()).into()),
                        _ => Ok(Cow::Borrowed(object)),
                    }
                } else {
                    let name = ident.name.clone();

                    Err(Exception::new(name, ExcType::NameError)
                        .with_position(expr_loc.position)
                        .into())
                }
            }
            Expr::Call { func, args, kwargs } => Ok(self.call_function(func, args, kwargs)?),
            Expr::AttrCall {
                object,
                attr,
                args,
                kwargs,
            } => Ok(self.attr_call(expr_loc, object, attr, args, kwargs)?),
            // Expr::AttrCall { .. } => todo!(),
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

    pub fn evaluate_bool<'c>(&self, expr_loc: &'d ExprLoc<'c>) -> RunResult<'c, bool> {
        match &expr_loc.expr {
            Expr::CmpOp { left, op, right } => self.cmp_op(left, op, right),
            _ => Ok(self.evaluate(expr_loc)?.as_ref().bool()),
        }
    }

    fn op<'c>(
        &self,
        left: &'d ExprLoc<'c>,
        op: &'d Operator,
        right: &'d ExprLoc<'c>,
    ) -> RunResult<'c, Cow<'d, Object>> {
        let left_object = self.evaluate(left)?;
        let right_object = self.evaluate(right)?;
        let op_object: Option<Object> = match op {
            Operator::Add => left_object.add(&right_object),
            Operator::Sub => left_object.sub(&right_object),
            Operator::Mod => left_object.modulus(&right_object),
            _ => return internal_err!(InternalRunError::TodoError; "Operator {op:?} not yet implemented"),
        };
        match op_object {
            Some(object) => Ok(Cow::Owned(object)),
            None => Exception::operand_type_error(left, op, right, left_object, right_object),
        }
    }

    fn cmp_op<'c>(&self, left: &'d ExprLoc<'c>, op: &'d CmpOperator, right: &'d ExprLoc<'c>) -> RunResult<'c, bool> {
        let left_object = self.evaluate(left)?;
        let right_object = self.evaluate(right)?;
        match op {
            CmpOperator::Eq => Ok(left_object.as_ref().py_eq(&right_object)),
            CmpOperator::NotEq => Ok(!left_object.as_ref().py_eq(&right_object)),
            CmpOperator::Gt => Ok(left_object.gt(&right_object)),
            CmpOperator::GtE => Ok(left_object.ge(&right_object)),
            CmpOperator::Lt => Ok(left_object.lt(&right_object)),
            CmpOperator::LtE => Ok(left_object.le(&right_object)),
            CmpOperator::ModEq(v) => match left_object.as_ref().modulus_eq(&right_object, *v) {
                Some(b) => Ok(b),
                None => Exception::operand_type_error(left, Operator::Mod, right, left_object, right_object),
            },
            _ => internal_err!(InternalRunError::TodoError; "Operator {op:?} not yet implemented"),
        }
    }

    fn call_function<'c>(
        &self,
        function: &'d Function,
        args: &'d [ExprLoc<'c>],
        kwargs: &'d [Kwarg],
    ) -> RunResult<'c, Cow<'d, Object>> {
        let builtin = match function {
            Function::Builtin(builtin) => builtin,
            Function::Ident(_) => {
                return internal_err!(InternalRunError::TodoError; "User defined functions not yet implemented")
            }
        };
        builtin.call_function(self, args, kwargs)
    }

    fn attr_call<'c>(
        &self,
        expr_loc: &'d ExprLoc<'c>,
        object_ident: &Identifier<'c>,
        attr: &Attr,
        args: &'d [ExprLoc<'c>],
        _kwargs: &'d [Kwarg],
    ) -> RunResult<'c, Cow<'d, Object>> {
        let object = if let Some(object) = self.namespace.get(object_ident.id) {
            match object {
                Object::Undefined => return Err(InternalRunError::Undefined(object_ident.name.clone().into()).into()),
                _ => object,
            }
        } else {
            let name = object_ident.name.clone();

            return Err(Exception::new(name, ExcType::NameError)
                .with_position(expr_loc.position)
                .into());
        };
        let args: Vec<Cow<Object>> = args.iter().map(|a| self.evaluate(a)).collect::<RunResult<_>>()?;
        object.attr_call(attr, args)
    }
}
