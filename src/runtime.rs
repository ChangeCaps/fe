use std::{cell::RefCell, rc::Rc};

use crate::variant::{Ref, Variant};

#[derive(Clone, Debug)]
pub enum RuntimeError {
    InvalidMemoryLocation(usize),
    InvalidAccess,
    InvalidDeref,
    InvalidOp,
    ExpectedBool,
    ExpectedFunc,
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Clone, Debug)]
pub enum Func {
    Func(Expr),
}

#[derive(Clone, Debug, Default)]
pub struct Stack {
    pub entries: Vec<Ref>,
}

impl Stack {
    #[inline]
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, variant: Variant) {
        self.entries.push(Ref::Owned(variant));
    }

    #[inline]
    pub fn pop(&mut self, len: usize) {
        self.entries.truncate(self.entries.len() - len);
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline]
    pub fn print(&self) {
        for (i, var) in self.entries.iter().enumerate() {
            var.map(|var| {
                println!("[{}] = {}", i, var);
            });
        }
    }
}

#[derive(Clone, Debug)]
pub struct Runtime<'a> {
    pub stack: Stack,
    pub program: &'a Program,
}

impl<'a> Runtime<'a> {
    #[inline]
    pub fn new(program: &'a Program) -> Self {
        Self {
            stack: Stack::default(),
            program,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Program {
    pub instructions: Vec<Instruction>,
    pub functions: Vec<Func>,
}

impl Program {
    #[inline]
    pub fn eval(&self, runtime: &mut Runtime) -> RuntimeResult<()> {
        for instruction in &self.instructions {
            instruction.eval(runtime)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Push(Expr),
    PushVariant(Variant),
    Pop(usize),
    Expr(Expr),
}

impl Instruction {
    #[inline]
    pub fn eval(&self, runtime: &mut Runtime) -> RuntimeResult<()> {
        match self {
            Self::Push(expr) => {
                let var = expr.eval(runtime)?;

                runtime.stack.push(var);

                Ok(())
            }
            Self::PushVariant(var) => {
                runtime.stack.push(var.clone());

                Ok(())
            }
            Self::Pop(len) => {
                runtime.stack.pop(*len);

                Ok(())
            }
            Self::Expr(expr) => {
                expr.eval(runtime)?;

                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Access {
    Variable(usize),
}

impl Access {
    #[inline]
    pub fn access<O>(
        &self,
        runtime: &mut Runtime,
        f: impl FnOnce(&mut Ref) -> RuntimeResult<O>,
    ) -> RuntimeResult<O> {
        match self {
            Access::Variable(idx) => f(&mut runtime.stack.entries[*idx]),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Literal(Variant),
    Variable(usize),
    Assign(Access, Box<Expr>),
    Ref(Access),
    RefOwned(Box<Expr>),
    Deref(Box<Expr>),
    Block(Vec<Instruction>, Option<Box<Expr>>, usize),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(usize, Vec<Expr>),
    CallExpr(Box<Expr>, Vec<Expr>),
    // binary operators
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    GtEq(Box<Expr>, Box<Expr>),
    LtEq(Box<Expr>, Box<Expr>),
    AndAnd(Box<Expr>, Box<Expr>),
    OrOr(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    NotEq(Box<Expr>, Box<Expr>),
}

impl Expr {
    #[inline]
    pub fn eval(&self, runtime: &mut Runtime) -> RuntimeResult<Variant> {
        macro_rules! math_op {
            ($lhs:ident $op:tt $rhs:ident) => {{
                let lhs = $lhs.eval(runtime)?;
                let rhs = $rhs.eval(runtime)?;

                match lhs {
                    Variant::I32(lhs) => {
                        if let Variant::I32(rhs) = rhs {
                            Ok(Variant::I32(lhs $op rhs))
                        } else {
                            Err(RuntimeError::InvalidOp)
                        }
                    }
                    Variant::F32(lhs) => {
                        if let Variant::F32(rhs) = rhs {
                            Ok(Variant::F32(lhs $op rhs))
                        } else {
                            Err(RuntimeError::InvalidOp)
                        }
                    }
                    _ => Err(RuntimeError::InvalidOp)
                }
            }};
        }

        macro_rules! cmp_op {
            ($lhs:ident $op:tt $rhs:ident) => {{
                let lhs = $lhs.eval(runtime)?;
                let rhs = $rhs.eval(runtime)?;

                match lhs {
                    Variant::I32(lhs) => {
                        if let Variant::I32(rhs) = rhs {
                            Ok(Variant::Bool(lhs $op rhs))
                        } else {
                            Err(RuntimeError::InvalidOp)
                        }
                    }
                    Variant::F32(lhs) => {
                        if let Variant::F32(rhs) = rhs {
                            Ok(Variant::Bool(lhs $op rhs))
                        } else {
                            Err(RuntimeError::InvalidOp)
                        }
                    }
                    _ => Err(RuntimeError::InvalidOp)
                }
            }};
        }

        match self {
            Expr::Literal(var) => Ok(var.clone()),
            Expr::Variable(idx) => Ok(runtime.stack.entries[*idx].cloned()),
            Expr::Assign(lhs, rhs) => {
                let rhs = rhs.eval(runtime)?;

                lhs.access(runtime, |ref_var| {
                    ref_var.map_mut(|var| *var = rhs);
                    Ok(Variant::Unit)
                })
            }
            Expr::Ref(access) => access.access(runtime, |ref_var| {
                match ref_var {
                    Ref::Owned(var) => *ref_var = Ref::Ref(Rc::new(RefCell::new(var.clone()))),
                    _ => {}
                };

                Ok(Variant::Ref(Box::new(ref_var.clone())))
            }),
            Expr::RefOwned(expr) => Ok(Variant::Ref(Box::new(Ref::Owned(expr.eval(runtime)?)))),
            Expr::Deref(expr) => {
                let var = expr.eval(runtime)?;

                if let Variant::Ref(var) = var {
                    Ok(var.cloned())
                } else {
                    Err(RuntimeError::InvalidDeref)
                }
            }
            Expr::Block(instructions, expr, pop) => {
                for instruction in instructions {
                    instruction.eval(runtime)?;
                }

                if let Some(expr) = expr {
                    let res = expr.eval(runtime);

                    runtime.stack.pop(*pop);

                    res
                } else {
                    runtime.stack.pop(*pop);

                    Ok(Variant::Unit)
                }
            }
            Expr::If(check, true_block, false_block) => {
                let check = check.eval(runtime)?;

                if let Variant::Bool(check) = check {
                    if check {
                        true_block.eval(runtime)
                    } else {
                        false_block.eval(runtime)
                    }
                } else {
                    Err(RuntimeError::ExpectedBool)
                }
            }
            Expr::Call(idx, expr_args) => {
                let mut stack = Stack::default();

                for arg in expr_args {
                    stack.push(arg.eval(runtime)?);
                }

                let mut runtime = Runtime {
                    stack,
                    program: runtime.program,
                };

                let func = &runtime.program.functions[*idx];

                let Func::Func(func) = func;

                func.eval(&mut runtime)
            }
            Expr::CallExpr(expr, expr_args) => {
                let var = expr.eval(runtime)?;

                let idx = if let Variant::Func(idx) = var {
                    idx
                } else {
                    return Err(RuntimeError::ExpectedFunc);
                };

                let mut stack = Stack::default();

                for arg in expr_args {
                    stack.push(arg.eval(runtime)?);
                }

                let mut runtime = Runtime {
                    stack,
                    program: runtime.program,
                };

                let func = &runtime.program.functions[idx];

                let Func::Func(func) = func;

                func.eval(&mut runtime)
            }
            Expr::Add(lhs, rhs) => math_op!(lhs + rhs),
            Expr::Sub(lhs, rhs) => math_op!(lhs - rhs),
            Expr::Mul(lhs, rhs) => math_op!(lhs * rhs),
            Expr::Div(lhs, rhs) => math_op!(lhs / rhs),
            Expr::Gt(lhs, rhs) => cmp_op!(lhs > rhs),
            Expr::Lt(lhs, rhs) => cmp_op!(lhs < rhs),
            Expr::GtEq(lhs, rhs) => cmp_op!(lhs >= rhs),
            Expr::LtEq(lhs, rhs) => cmp_op!(lhs <= rhs),
            Expr::AndAnd(lhs, rhs) => {
                if let Variant::Bool(lhs) = lhs.eval(runtime)? {
                    if let Variant::Bool(rhs) = rhs.eval(runtime)? {
                        Ok(Variant::Bool(lhs && rhs))
                    } else {
                        Err(RuntimeError::ExpectedBool)
                    }
                } else {
                    Err(RuntimeError::ExpectedBool)
                }
            }
            Expr::OrOr(lhs, rhs) => {
                if let Variant::Bool(lhs) = lhs.eval(runtime)? {
                    if let Variant::Bool(rhs) = rhs.eval(runtime)? {
                        Ok(Variant::Bool(lhs || rhs))
                    } else {
                        Err(RuntimeError::ExpectedBool)
                    }
                } else {
                    Err(RuntimeError::ExpectedBool)
                }
            }
            Expr::Eq(lhs, rhs) => {
                let lhs = lhs.eval(runtime)?;
                let rhs = rhs.eval(runtime)?;

                Ok(Variant::Bool(lhs == rhs))
            }
            Expr::NotEq(lhs, rhs) => {
                let lhs = lhs.eval(runtime)?;
                let rhs = rhs.eval(runtime)?;

                Ok(Variant::Bool(lhs != rhs))
            }
        }
    }
}
