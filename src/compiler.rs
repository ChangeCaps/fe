use crate::{
    ast::{self, BinOp, Ident, TypeLiteral, UnaryOp},
    runtime::{self, Instruction, Program},
    spanned::Spanned,
    variant::{Ref, Variant, VariantType},
};

#[derive(Clone, Debug)]
pub enum CompileError {
    Undefined(Ident),
    TypeUnspecified,
    ExpectedConst,
    DefaultUndefined,
    InvalidArgCount,
    InvalidAccess,
    InvalidDeref(VariantType),
    InvalidType(VariantType),
}

impl CompileError {
    pub fn msg(&self) -> String {
        match self {
            Self::Undefined(ident) => format!("not defined '{}'", ident),
            Self::TypeUnspecified => format!("type not specified"),
            Self::ExpectedConst => format!("could not evaluate at compile time"),
            Self::DefaultUndefined => format!("default not defined"),
            Self::InvalidArgCount => format!("invalid arg count"),
            Self::InvalidAccess => format!("invalid access"),
            Self::InvalidDeref(ty) => format!("invalid deref target '{}'", ty),
            Self::InvalidType(ty) => format!("invalid type '{}'", ty),
        }
    }
}

pub type CompileResult<T> = Result<T, Spanned<CompileError>>;

#[derive(Clone, Debug)]
pub struct Variable {
    pub ident: Ident,
    pub ty: VariantType,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub idx: usize,
    pub ident: Ident,
    pub args: Vec<VariantType>,
    pub return_type: VariantType,
}

#[derive(Clone, Debug, Default)]
pub struct Stack {
    pub variables: Vec<Option<Variable>>,
    pub funcs: Vec<Func>,
    pub classes: Vec<(Ident, usize)>,
}

impl Stack {
    #[inline]
    pub fn cleared(&self) -> Self {
        Self {
            classes: self.classes.clone(),
            funcs: self.funcs.clone(),
            variables: Vec::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, variable: Variable) {
        self.variables.push(Some(variable));
    }

    #[inline]
    pub fn replace(&mut self, idx: usize, variable: Variable) {
        self.variables[idx] = Some(variable);
    }

    #[inline]
    pub fn push_unit(&mut self) {
        self.variables.push(None);
    }

    #[inline]
    pub fn get(&self, ident: &Ident) -> Option<(usize, &Variable)> {
        self.variables
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(i, v)| v.as_ref().map(|v| (i, v)))
            .find(|(_, var)| var.ident == *ident)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.variables.len()
    }

    #[inline]
    pub fn push_func(&mut self, func: Func) {
        self.funcs.push(func);
    }

    #[inline]
    pub fn get_func(&self, ident: &Ident) -> Option<&Func> {
        self.funcs.iter().rev().find(|var| var.ident == *ident)
    }

    #[inline]
    pub fn push_class(&mut self, ident: Ident, idx: usize) {
        self.classes.push((ident, idx));
    }

    #[inline]
    pub fn get_class(&self, ident: &Ident) -> Option<usize> {
        self.classes.iter().rev().find_map(
            |(var, idx)| {
                if *var == *ident {
                    Some(*idx)
                } else {
                    None
                }
            },
        )
    }
}

#[derive(Clone, Debug)]
pub struct Class {
    pub ident: Ident,
    pub fields: Vec<Field>,
}

#[derive(Clone, Debug)]
pub struct Field {
    pub ident: Ident,
    pub ty: VariantType,
    pub variant: Variant,
}

#[derive(Debug)]
pub struct Compiler<'a> {
    pub stack: Stack,
    pub instructions: &'a mut Vec<Instruction>,
    pub funcs: &'a mut Vec<runtime::Func>,
    pub classes: &'a mut Vec<Class>,
}

impl<'a> Compiler<'a> {
    #[inline]
    pub fn scope(this: &'a mut Compiler<'_>) -> Compiler<'a> {
        Self {
            stack: this.stack.clone(),
            instructions: this.instructions,
            funcs: this.funcs,
            classes: this.classes,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompiledExpr {
    pub expr: runtime::Expr,
    pub ty: VariantType,
}

#[derive(Clone, Debug)]
pub struct CompiledAccess {
    pub access: runtime::Access,
    pub ty: VariantType,
}

#[inline]
pub fn compile_type(
    ty: &Spanned<TypeLiteral>,
    compiler: &mut Compiler,
) -> CompileResult<VariantType> {
    match ty.as_ref() {
        TypeLiteral::I32 => Ok(VariantType::I32),
        TypeLiteral::F32 => Ok(VariantType::F32),
        TypeLiteral::Bool => Ok(VariantType::Bool),
        TypeLiteral::Func(args, return_type) => Ok(VariantType::Func(
            {
                let mut compiled_args = Vec::with_capacity(args.len());

                for arg in args {
                    compiled_args.push(compile_type(arg, compiler)?);
                }

                compiled_args
            },
            Box::new(compile_type(return_type, compiler)?),
        )),
        TypeLiteral::Ref(ty) => Ok(VariantType::Ref(Box::new(compile_type(ty, compiler)?))),
        TypeLiteral::Class(ident) => {
            if let Some(idx) = compiler.stack.get_class(ident) {
                Ok(VariantType::Class(idx))
            } else {
                Err(Spanned::new(
                    CompileError::Undefined(ident.clone()),
                    ty.span.clone(),
                ))
            }
        }
        TypeLiteral::Type => Ok(VariantType::Type),
        TypeLiteral::Unit => Ok(VariantType::Unit),
    }
}

#[inline]
pub fn compile_access(
    expr: &Spanned<ast::Expr>,
    compiler: &mut Compiler,
) -> CompileResult<CompiledAccess> {
    match expr.as_ref() {
        ast::Expr::Variable(path) => {
            if path.segments.len() == 1 {
                let ident = &path.segments[0];

                if let Some((idx, variable)) = compiler.stack.get(ident) {
                    Ok(CompiledAccess {
                        access: runtime::Access::Variable(idx),
                        ty: variable.ty.clone(),
                    })
                } else {
                    Err(Spanned::new(
                        CompileError::Undefined(ident.clone()),
                        path.span.clone(),
                    ))
                }
            } else {
                todo!()
            }
        }
        _ => Err(Spanned::new(CompileError::InvalidAccess, expr.span.clone())),
    }
}

#[inline]
pub fn compile_expr(expr: &ast::Expr, compiler: &mut Compiler) -> CompileResult<CompiledExpr> {
    match expr {
        ast::Expr::Paren(expr) => compile_expr(expr, compiler),
        ast::Expr::Literal(var) => Ok(CompiledExpr {
            expr: runtime::Expr::Literal(var.variant()),
            ty: var.ty(),
        }),
        ast::Expr::Variable(path) => {
            if path.segments.len() == 1 {
                let ident = &path.segments[0];

                if let Some((idx, variable)) = compiler.stack.get(ident) {
                    Ok(CompiledExpr {
                        expr: runtime::Expr::Variable(idx),
                        ty: variable.ty.clone(),
                    })
                } else if let Some(func) = compiler.stack.get_func(ident) {
                    Ok(CompiledExpr {
                        expr: runtime::Expr::Literal(Variant::Func(func.idx)),
                        ty: VariantType::Func(
                            func.args.clone(),
                            Box::new(func.return_type.clone()),
                        ),
                    })
                } else {
                    Err(Spanned::new(
                        CompileError::Undefined(ident.clone()),
                        path.span.clone(),
                    ))
                }
            } else {
                todo!()
            }
        }
        ast::Expr::Type(ty) => Ok(CompiledExpr {
            expr: runtime::Expr::Literal(Variant::Type(compile_type(ty, compiler)?)),
            ty: VariantType::Type,
        }),
        ast::Expr::Assign(lhs, rhs) => {
            let compiled_lhs = compile_access(lhs, compiler)?;
            let compiled_rhs = compile_expr(rhs, compiler)?;

            if compiled_lhs.ty == compiled_rhs.ty {
                Ok(CompiledExpr {
                    expr: runtime::Expr::Assign(compiled_lhs.access, Box::new(compiled_rhs.expr)),
                    ty: compiled_lhs.ty,
                })
            } else {
                Err(Spanned::new(
                    CompileError::InvalidType(compiled_rhs.ty),
                    rhs.span.clone(),
                ))
            }
        }
        ast::Expr::UnaryOp(op, expr) => match op.as_ref() {
            UnaryOp::Ref => {
                if let Ok(access) = compile_access(expr, compiler) {
                    Ok(CompiledExpr {
                        expr: runtime::Expr::Ref(access.access),
                        ty: VariantType::Ref(Box::new(access.ty)),
                    })
                } else {
                    let expr = compile_expr(expr, compiler)?;

                    Ok(CompiledExpr {
                        expr: runtime::Expr::RefOwned(Box::new(expr.expr)),
                        ty: VariantType::Ref(Box::new(expr.ty)),
                    })
                }
            }
            UnaryOp::Deref => {
                let compiled_expr = compile_expr(expr, compiler)?;

                if let VariantType::Ref(ty) = compiled_expr.ty {
                    Ok(CompiledExpr {
                        expr: runtime::Expr::Deref(Box::new(compiled_expr.expr)),
                        ty: *ty,
                    })
                } else {
                    Err(Spanned::new(
                        CompileError::InvalidDeref(compiled_expr.ty),
                        expr.span.clone(),
                    ))
                }
            }
            _ => todo!(),
        },
        ast::Expr::BinOp(lhs, op, rhs) => {
            let compiled_lhs = compile_expr(lhs, compiler)?;
            let compiled_rhs = compile_expr(rhs, compiler)?;

            macro_rules! math_op {
                ($op_name:ident, $op:tt) => {{
                    match compiled_lhs.ty {
                        VariantType::I32 | VariantType::F32 => {
                            if compiled_lhs.ty == compiled_rhs.ty {
                                Ok(CompiledExpr {
                                    expr: runtime::Expr::$op_name(
                                        Box::new(compiled_lhs.expr),
                                        Box::new(compiled_rhs.expr),
                                    ),
                                    ty: compiled_lhs.ty,
                                })
                            } else {
                                Err(Spanned::new(
                                    CompileError::InvalidType(compiled_rhs.ty),
                                    rhs.span.clone(),
                                ))
                            }
                        }
                        _ => Err(Spanned::new(
                            CompileError::InvalidType(compiled_lhs.ty),
                            lhs.span.clone(),
                        )),
                    }
                }};
            }

            macro_rules! cmp_op {
                ($op_name:ident, $op:tt) => {{
                    match compiled_lhs.ty {
                        VariantType::I32 | VariantType::F32 => {
                            if compiled_lhs.ty == compiled_rhs.ty {
                                Ok(CompiledExpr {
                                    expr: runtime::Expr::$op_name(
                                        Box::new(compiled_lhs.expr),
                                        Box::new(compiled_rhs.expr),
                                    ),
                                    ty: VariantType::Bool,
                                })
                            } else {
                                Err(Spanned::new(
                                    CompileError::InvalidType(compiled_rhs.ty),
                                    rhs.span.clone(),
                                ))
                            }
                        }
                        _ => Err(Spanned::new(
                            CompileError::InvalidType(compiled_lhs.ty),
                            lhs.span.clone(),
                        )),
                    }
                }};
            }

            match op.as_ref() {
                BinOp::Add => math_op!(Add, +),
                BinOp::Sub => math_op!(Sub, -),
                BinOp::Mul => math_op!(Mul, *),
                BinOp::Div => math_op!(Div, /),
                BinOp::Gt => cmp_op!(Gt, >),
                BinOp::AndAnd => {
                    if compiled_lhs.ty == compiled_rhs.ty && compiled_lhs.ty == VariantType::Bool {
                        Ok(CompiledExpr {
                            expr: runtime::Expr::AndAnd(
                                Box::new(compiled_lhs.expr),
                                Box::new(compiled_rhs.expr),
                            ),
                            ty: VariantType::Bool,
                        })
                    } else {
                        Err(Spanned::new(
                            CompileError::InvalidType(compiled_rhs.ty),
                            rhs.span.clone(),
                        ))
                    }
                }
                BinOp::OrOr => {
                    if compiled_lhs.ty == compiled_rhs.ty && compiled_lhs.ty == VariantType::Bool {
                        Ok(CompiledExpr {
                            expr: runtime::Expr::OrOr(
                                Box::new(compiled_lhs.expr),
                                Box::new(compiled_rhs.expr),
                            ),
                            ty: VariantType::Bool,
                        })
                    } else {
                        Err(Spanned::new(
                            CompileError::InvalidType(compiled_rhs.ty),
                            rhs.span.clone(),
                        ))
                    }
                }
                BinOp::EqEq => {
                    if compiled_lhs.ty == compiled_rhs.ty {
                        Ok(CompiledExpr {
                            expr: runtime::Expr::Eq(
                                Box::new(compiled_lhs.expr),
                                Box::new(compiled_rhs.expr),
                            ),
                            ty: VariantType::Bool,
                        })
                    } else {
                        Err(Spanned::new(
                            CompileError::InvalidType(compiled_rhs.ty),
                            rhs.span.clone(),
                        ))
                    }
                }
                BinOp::NotEq => {
                    if compiled_lhs.ty == compiled_rhs.ty {
                        Ok(CompiledExpr {
                            expr: runtime::Expr::NotEq(
                                Box::new(compiled_lhs.expr),
                                Box::new(compiled_rhs.expr),
                            ),
                            ty: VariantType::Bool,
                        })
                    } else {
                        Err(Spanned::new(
                            CompileError::InvalidType(compiled_rhs.ty),
                            rhs.span.clone(),
                        ))
                    }
                }
                _ => Err(Spanned::new(
                    CompileError::InvalidType(compiled_rhs.ty),
                    lhs.span.clone() + rhs.span.clone(),
                )),
            }
        }
        ast::Expr::Call(expr, args) => {
            let compiled_expr = compile_expr(expr, compiler)?;

            if let VariantType::Func(func_args, return_type) = compiled_expr.ty {
                if args.len() != args.len() {
                    return Err(Spanned::new(
                        CompileError::InvalidArgCount,
                        expr.span.clone(),
                    ));
                }

                let mut expr_args = Vec::with_capacity(args.len());

                for (arg, ty) in args.iter().zip(func_args) {
                    let expr = compile_expr(arg, compiler)?;

                    if expr.ty != ty {
                        return Err(Spanned::new(
                            CompileError::InvalidType(expr.ty),
                            arg.span.clone(),
                        ));
                    }

                    expr_args.push(expr.expr);
                }

                Ok(CompiledExpr {
                    expr: runtime::Expr::CallExpr(Box::new(compiled_expr.expr), expr_args),
                    ty: *return_type,
                })
            } else if let ast::Expr::Variable(path) = expr.as_ref().as_ref() {
                let ident = &path.segments[0];

                if let Some(func) = compiler.stack.get_func(ident).cloned() {
                    if args.len() != func.args.len() {
                        return Err(Spanned::new(
                            CompileError::InvalidArgCount,
                            expr.span.clone(),
                        ));
                    }

                    let mut expr_args = Vec::with_capacity(args.len());

                    for (arg, ty) in args.iter().zip(func.args) {
                        let expr = compile_expr(arg, compiler)?;

                        if expr.ty != ty {
                            return Err(Spanned::new(
                                CompileError::InvalidType(expr.ty),
                                arg.span.clone(),
                            ));
                        }

                        expr_args.push(expr.expr);
                    }

                    Ok(CompiledExpr {
                        expr: runtime::Expr::Call(func.idx, expr_args),
                        ty: func.return_type.clone(),
                    })
                } else {
                    Err(Spanned::new(
                        CompileError::Undefined(ident.clone()),
                        expr.span.clone(),
                    ))
                }
            } else {
                Err(Spanned::new(CompileError::InvalidAccess, expr.span.clone()))
            }
        }
        ast::Expr::Block(block) => {
            let mut instructions = Vec::new();

            let len = compiler.stack.len();

            let mut compiler = Compiler {
                stack: compiler.stack.clone(),
                instructions: &mut instructions,
                funcs: compiler.funcs,
                classes: compiler.classes,
            };

            compile_stmts(&block.block.stmts, &mut compiler)?;

            let pop = compiler.stack.len() - len;

            if let Some(block) = &block.expr {
                let expr = compile_expr(block, &mut compiler)?;

                Ok(CompiledExpr {
                    expr: runtime::Expr::Block(instructions, Some(Box::new(expr.expr)), pop),
                    ty: expr.ty,
                })
            } else {
                Ok(CompiledExpr {
                    expr: runtime::Expr::Block(instructions, None, pop),
                    ty: VariantType::Unit,
                })
            }
        }
        ast::Expr::If(check, true_expr, false_expr) => {
            let compiled_check = compile_expr(check, compiler)?;
            let compiled_true_expr = compile_expr(true_expr, compiler)?;
            let compiled_false_expr = compile_expr(false_expr, compiler)?;

            if let VariantType::Bool = compiled_check.ty {
                if compiled_true_expr.ty == compiled_false_expr.ty {
                    Ok(CompiledExpr {
                        expr: runtime::Expr::If(
                            Box::new(compiled_check.expr),
                            Box::new(compiled_true_expr.expr),
                            Box::new(compiled_false_expr.expr),
                        ),
                        ty: compiled_true_expr.ty,
                    })
                } else {
                    Err(Spanned::new(
                        CompileError::InvalidType(compiled_false_expr.ty),
                        false_expr.span.clone(),
                    ))
                }
            } else {
                Err(Spanned::new(
                    CompileError::InvalidType(compiled_check.ty),
                    check.span.clone(),
                ))
            }
        }
    }
}

#[inline]
pub fn eval_expr(
    expr: &Spanned<ast::Expr>,
    compiler: &mut Compiler,
) -> CompileResult<(Variant, VariantType)> {
    match expr.as_ref() {
        ast::Expr::Literal(lit) => Ok((lit.variant(), lit.ty())),
        _ => Err(Spanned::new(CompileError::ExpectedConst, expr.span.clone())),
    }
}

#[inline]
pub fn default(ty: &Spanned<VariantType>, compiler: &mut Compiler) -> CompileResult<Variant> {
    match ty.as_ref() {
        VariantType::I32 => Ok(Variant::I32(0)),
        VariantType::F32 => Ok(Variant::F32(0.0)),
        VariantType::Bool => Ok(Variant::Bool(false)),
        VariantType::Ref(ref_ty) => Ok(Variant::Ref(Box::new(Ref::Owned(default(
            &Spanned::new(*ref_ty.clone(), ty.span.clone()),
            compiler,
        )?)))),
        VariantType::Unit => Ok(Variant::Unit),
        _ => Err(Spanned::new(
            CompileError::DefaultUndefined,
            ty.span.clone(),
        )),
    }
}

#[inline]
pub fn compile_func(func: &ast::Func, idx: usize, compiler: &mut Compiler) -> CompileResult<()> {
    let mut stack = compiler.stack.cleared();

    for arg in &func.args {
        let variable = Variable {
            ident: arg.ident.inner.clone(),
            ty: compile_type(&arg.ty, compiler)?,
        };

        stack.push(variable);
    }

    let return_type = func
        .return_type
        .as_ref()
        .map_or(Ok(VariantType::Unit), |v| compile_type(v, compiler))?;

    let mut func_compiler = Compiler {
        stack,
        instructions: compiler.instructions,
        funcs: compiler.funcs,
        classes: compiler.classes,
    };

    let expr = compile_expr(&func.expr, &mut func_compiler)?;

    if expr.ty != return_type {
        return Err(Spanned::new(
            CompileError::InvalidType(expr.ty),
            func.expr.span.clone(),
        ));
    }

    compiler.funcs[idx] = runtime::Func::Func(expr.expr);

    Ok(())
}

#[inline]
pub fn compile_stmts(stmts: &[Spanned<ast::Stmt>], compiler: &mut Compiler) -> CompileResult<()> {
    let mut funcs = Vec::new();

    for stmt in stmts {
        match stmt.as_ref() {
            ast::Stmt::Func(func) => {
                let mut args = Vec::with_capacity(func.args.len());

                for arg in &func.args {
                    args.push(compile_type(&arg.ty, compiler)?);
                }

                let return_type = func
                    .return_type
                    .as_ref()
                    .map_or(Ok(VariantType::Unit), |v| compile_type(v, compiler))?;

                let idx = compiler.funcs.len();
                compiler
                    .funcs
                    .push(runtime::Func::Func(runtime::Expr::Literal(Variant::Unit)));

                let f = Func {
                    idx,
                    ident: func.ident.inner.clone(),
                    args,
                    return_type,
                };

                compiler.stack.push_func(f);
                funcs.push(idx);
            }
            ast::Stmt::Class(class) => {
                let mut fields = Vec::new();

                for stmt in &class.stmts {
                    match stmt {
                        ast::ClassStmt::Field(ident, ty, expr) => {
                            if let Some(expr) = expr {
                                let (var, var_ty) = eval_expr(expr, compiler)?;

                                if let Some(ty) = ty {
                                    if var_ty != compile_type(ty, compiler)? {
                                        return Err(Spanned::new(
                                            CompileError::InvalidType(var_ty),
                                            expr.span.clone(),
                                        ));
                                    }
                                }

                                fields.push(Field {
                                    ident: ident.inner.clone(),
                                    ty: var_ty,
                                    variant: var,
                                });
                            } else if let Some(ty) = ty {
                                let compiled_ty = compile_type(ty, compiler)?;

                                let variant = default(
                                    &Spanned::new(compiled_ty.clone(), ty.span.clone()),
                                    compiler,
                                )?;

                                fields.push(Field {
                                    ident: ident.inner.clone(),
                                    ty: compiled_ty,
                                    variant,
                                });
                            } else {
                                return Err(Spanned::new(
                                    CompileError::TypeUnspecified,
                                    ident.span.clone(),
                                ));
                            }
                        }
                        _ => {}
                    }
                }

                let idx = compiler.classes.len();
                compiler.classes.push(Class {
                    ident: class.ident.inner.clone(),
                    fields,
                });
                compiler.stack.push_class(class.ident.inner.clone(), idx);
            }
            _ => {}
        }
    }

    let mut funcs = funcs.into_iter();

    for stmt in stmts {
        match stmt.as_ref() {
            ast::Stmt::Let(ident, expr) => {
                let compiled_expr = compile_expr(expr, compiler)?;

                let variable = Variable {
                    ident: ident.inner.clone(),
                    ty: compiled_expr.ty,
                };

                compiler.stack.push(variable);
                compiler
                    .instructions
                    .push(Instruction::Push(compiled_expr.expr));
            }
            ast::Stmt::Expr(expr) => {
                let compiled_expr = compile_expr(expr, compiler)?;

                compiler
                    .instructions
                    .push(Instruction::Expr(compiled_expr.expr));
            }
            ast::Stmt::Func(func) => {
                compile_func(func, funcs.next().unwrap(), compiler)?;
            }
            _ => {}
        }
    }

    Ok(())
}

#[inline]
pub fn compile_program(input: &ast::Program) -> CompileResult<runtime::Program> {
    let mut instructions = Vec::new();
    let mut funcs = Vec::new();
    let mut classes = Vec::new();
    let mut compiler = Compiler {
        stack: Stack::default(),
        instructions: &mut instructions,
        funcs: &mut funcs,
        classes: &mut classes,
    };

    compile_stmts(&input.stmts, &mut compiler)?;

    println!("{:#?}", classes);

    Ok(Program {
        instructions,
        functions: funcs,
    })
}
