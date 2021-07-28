use std::{
    collections::{HashMap, HashSet},
    ops::IndexMut,
    sync::Arc,
};

use crate::{
    ast::{self, BinOp, Ident, Path, TypeLiteral, UnaryOp},
    compiler::Compiler,
    func,
    program::Program,
    runtime::{self, Instruction},
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
    CyclicReference,
    LetNotAllowed,
    DuplicateFieldSet(Ident),
    FieldNotCovered(Ident),
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
            Self::CyclicReference => format!("cyclic class reference"),
            Self::DuplicateFieldSet(ident) => format!("field was set more than once '{}'", ident),
            Self::LetNotAllowed => format!("let not allowed in module"),
            Self::FieldNotCovered(ident) => format!("field not covered '{}'", ident),
            Self::InvalidDeref(ty) => format!("invalid deref target '{}'", ty),
            Self::InvalidType(ty) => format!("invalid type '{}'", ty),
        }
    }
}

pub type CompileResult<T> = Result<T, Spanned<CompileError>>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
pub struct Id(pub usize);

#[derive(Clone, Debug)]
pub struct Map<V> {
    inner: Vec<Option<V>>,
    next_id: Id,
}

impl<V> Default for Map<V> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            next_id: Default::default(),
        }
    }
}

impl<V> Map<V> {
    #[inline]
    pub fn next_id(&mut self) -> Id {
        let id = self.next_id;
        self.next_id.0 += 1;
        id
    }

    #[inline]
    pub fn next(&mut self) -> Id {
        self.inner.push(None);
        self.next_id()
    }

    #[inline]
    pub fn insert(&mut self, id: Id, v: V) {
        self.inner[id.0] = Some(v);
    }

    #[inline]
    pub fn push(&mut self, v: V) -> Id {
        self.inner.push(Some(v));
        self.next_id()
    }

    #[inline]
    pub fn get(&self, id: &Id) -> Option<&V> {
        self.inner[id.0].as_ref()
    }

    #[inline]
    pub fn get_mut(&mut self, id: &Id) -> Option<&mut V> {
        self.inner[id.0].as_mut()
    }

    #[inline]
    pub fn to_vec(self) -> Option<Vec<V>> {
        let mut vec = Vec::with_capacity(self.inner.len());

        for v in self.inner {
            vec.push(v?);
        }

        Some(vec)
    }
}

#[derive(Clone, Debug)]
struct Variable {
    pub ident: Ident,
    pub ty: VariantType,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub id: Id,
    pub ident: Ident,
    pub args: Vec<VariantType>,
    pub return_type: VariantType,
}

#[derive(Clone, Debug, Default)]
pub struct Module {
    pub persist: bool,
    pub instructions: Vec<Instruction>,
    pub funcs: HashMap<Ident, Func>,
    pub types: HashMap<Ident, VariantType>,
    pub modules: HashMap<Ident, Id>,
}

impl Module {
    #[inline]
    pub fn persist(&self, data: &ProgramData) -> Self {
        let mut modules = HashMap::new();

        for (ident, id) in &self.modules {
            let module = data.modules.get(id).unwrap();

            if module.persist {
                modules.insert(ident.clone(), *id);
            }
        }

        Self {
            modules,
            ..self.clone()
        }
    }

    #[inline]
    pub fn push_func(&mut self, func: Func) {
        self.funcs.insert(func.ident.clone(), func);
    }

    #[inline]
    pub fn get_func(&self, ident: &Ident) -> Option<&Func> {
        self.funcs.get(ident)
    }

    #[inline]
    pub fn push_type(&mut self, ident: Ident, ty: VariantType) {
        self.types.insert(ident, ty);
    }

    #[inline]
    pub fn get_type(&self, ident: &Ident) -> Option<&VariantType> {
        self.types.get(ident)
    }

    #[inline]
    pub fn push_mod(&mut self, ident: Ident, id: Id) {
        self.modules.insert(ident, id);
    }

    #[inline]
    pub fn get_mod(&self, ident: &Ident) -> Option<&Id> {
        self.modules.get(ident)
    }
}

#[derive(Clone, Debug, Default)]
pub struct ProgramData {
    pub funcs: Map<runtime::Func>,
    pub classes: Map<Class>,
    pub modules: Map<Module>,
    pub methods: HashMap<VariantType, Methods>,
}

#[derive(Clone, Debug, Default)]
struct Stack {
    pub variables: Vec<Option<Variable>>,
}

impl Stack {
    #[inline]
    pub fn push(&mut self, variable: Variable) {
        self.variables.push(Some(variable));
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
}

#[derive(Clone, Debug)]
pub struct Class {
    pub id: Id,
    pub fields: Vec<Field>,
}

impl Class {
    #[inline]
    pub fn get_field(&self, ident: &Ident) -> Option<(usize, &Field)> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, f)| f.ident == *ident)
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub ident: Ident,
    pub ty: VariantType,
    pub variant: Option<Variant>,
}

#[derive(Clone, Debug)]
pub enum SelfArg {
    Owned,
    Ref,
}

#[derive(Clone, Debug)]
pub struct Method {
    pub id: Id,
    pub ident: Ident,
    pub self_arg: SelfArg,
    pub args: Vec<VariantType>,
    pub return_type: VariantType,
}

#[derive(Clone, Debug, Default)]
pub struct Methods {
    pub methods: Vec<Method>,
}

impl Methods {
    #[inline]
    pub fn get_method(&self, ident: &str) -> Option<&Method> {
        self.methods.iter().find(|method| &*method.ident == ident)
    }
}

#[derive(Debug)]
struct CompilerState<'a> {
    pub stack: Stack,
    pub module: Id,
    pub data: &'a mut ProgramData,
}

impl<'a> CompilerState<'a> {
    #[inline]
    pub fn scope(this: &'a mut CompilerState<'_>) -> CompilerState<'a> {
        Self {
            stack: this.stack.clone(),
            module: this.module,
            data: this.data,
        }
    }

    #[inline]
    pub fn cleared(this: &'a mut CompilerState<'_>) -> CompilerState<'a> {
        Self {
            stack: Stack::default(),
            module: this.module,
            data: this.data,
        }
    }

    #[inline]
    pub fn map_stack(this: &'a mut CompilerState<'_>, stack: Stack) -> CompilerState<'a> {
        Self {
            stack,
            module: this.module,
            data: this.data,
        }
    }

    #[inline]
    pub fn module(&self) -> &Module {
        self.data.modules.get(&self.module).unwrap()
    }

    #[inline]
    pub fn module_mut(&mut self) -> &mut Module {
        self.data.modules.get_mut(&self.module).unwrap()
    }

    #[inline]
    pub fn get_module(&self, path: &ast::Path) -> CompileResult<&Module> {
        let mut module_id = self.module;

        for segment in &path.segments[..path.segments.len() - 1] {
            let module = self.data.modules.get(&module_id).unwrap();

            if let Some(module) = module.get_mod(segment) {
                module_id = *module;
            } else {
                return Err(Spanned::new(
                    CompileError::Undefined(segment.inner.clone()),
                    segment.span.clone(),
                ));
            }
        }

        Ok(self.data.modules.get(&module_id).unwrap())
    }
}

#[derive(Clone, Debug)]
struct CompiledExpr {
    pub expr: runtime::Expr,
    pub ty: VariantType,
}

#[derive(Clone, Debug)]
struct CompiledAccess {
    pub access: runtime::Access,
    pub ty: VariantType,
}

#[inline]
fn compile_type(
    ty: &Spanned<TypeLiteral>,
    compiler: &mut CompilerState,
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
        TypeLiteral::Class(path) => {
            let module = compiler.get_module(path)?;

            let ident = path.segments.last().unwrap();

            if let Some(ty) = module.get_type(ident) {
                Ok(ty.clone())
            } else {
                Err(Spanned::new(
                    CompileError::Undefined(ident.inner.clone()),
                    ty.span.clone(),
                ))
            }
        }
        TypeLiteral::Type => Ok(VariantType::Type),
        TypeLiteral::String => Ok(VariantType::String),
        TypeLiteral::Unit => Ok(VariantType::Unit),
        TypeLiteral::Any => Ok(VariantType::Any),
    }
}

#[inline]
fn compile_access(
    expr: &Spanned<ast::Expr>,
    compiler: &mut CompilerState,
) -> CompileResult<CompiledAccess> {
    match expr.as_ref() {
        ast::Expr::Paren(expr) => compile_access(expr, compiler),
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
                        CompileError::Undefined(ident.inner.clone()),
                        path.span.clone(),
                    ))
                }
            } else {
                todo!()
            }
        }
        ast::Expr::Field(expr, ident) => {
            let mut compiled_access = compile_access(expr, compiler)?;

            if let VariantType::Ref(ty) = compiled_access.ty {
                compiled_access.access = runtime::Access::Deref(Box::new(compiled_access.access));
                compiled_access.ty = *ty;
            }

            if let VariantType::Class(id) = compiled_access.ty {
                let class = compiler.data.classes.get(&id).unwrap();

                if let Some((idx, field)) = class.get_field(ident) {
                    Ok(CompiledAccess {
                        access: runtime::Access::Field(Box::new(compiled_access.access), idx),
                        ty: field.ty.clone(),
                    })
                } else {
                    Err(Spanned::new(
                        CompileError::InvalidAccess,
                        ident.span.clone(),
                    ))
                }
            } else {
                Err(Spanned::new(
                    CompileError::InvalidType(compiled_access.ty),
                    expr.span.clone(),
                ))
            }
        }
        ast::Expr::UnaryOp(op, expr) => match op.as_ref() {
            ast::UnaryOp::Deref => {
                let compiled_access = compile_access(expr, compiler)?;

                if let VariantType::Ref(ty) = compiled_access.ty {
                    Ok(CompiledAccess {
                        access: runtime::Access::Deref(Box::new(compiled_access.access)),
                        ty: *ty,
                    })
                } else {
                    Err(Spanned::new(CompileError::InvalidAccess, expr.span.clone()))
                }
            }
            _ => Err(Spanned::new(CompileError::InvalidAccess, expr.span.clone())),
        },
        ast::Expr::Comment(expr, _) => compile_access(expr, compiler),
        _ => Err(Spanned::new(CompileError::InvalidAccess, expr.span.clone())),
    }
}

#[inline]
fn compile_cast(
    expr: &Spanned<ast::Expr>,
    ty: &VariantType,
    compiler: &mut CompilerState<'_>,
) -> CompileResult<CompiledExpr> {
    let compiled_expr = compile_expr(expr, compiler)?;

    if compiled_expr.ty == *ty {
        return Ok(compiled_expr);
    }

    match ty {
        VariantType::F32 => {
            if let VariantType::I32 = compiled_expr.ty {
                Ok(CompiledExpr {
                    expr: runtime::Expr::I32ToF32(Box::new(compiled_expr.expr)),
                    ty: VariantType::F32,
                })
            } else {
                Err(Spanned::new(
                    CompileError::InvalidType(compiled_expr.ty),
                    expr.span.clone(),
                ))
            }
        }
        VariantType::Any => Ok(CompiledExpr {
            expr: compiled_expr.expr,
            ty: VariantType::Any,
        }),
        _ => Err(Spanned::new(
            CompileError::InvalidType(compiled_expr.ty),
            expr.span.clone(),
        )),
    }
}

#[inline]
fn compile_expr(
    expr: &Spanned<ast::Expr>,
    compiler: &mut CompilerState,
) -> CompileResult<CompiledExpr> {
    match expr.as_ref() {
        ast::Expr::Paren(expr) => compile_expr(expr, compiler),
        ast::Expr::Literal(var) => Ok(CompiledExpr {
            expr: runtime::Expr::Literal(var.variant()),
            ty: var.ty(),
        }),
        ast::Expr::Variable(path) => {
            let module = compiler.get_module(path)?;

            let ident = path.segments.last().unwrap();

            if let Some((idx, variable)) = compiler.stack.get(ident) {
                Ok(CompiledExpr {
                    expr: runtime::Expr::Variable(idx),
                    ty: variable.ty.clone(),
                })
            } else if let Some(func) = module.get_func(ident) {
                Ok(CompiledExpr {
                    expr: runtime::Expr::Literal(Variant::Func(func.id)),
                    ty: VariantType::Func(func.args.clone(), Box::new(func.return_type.clone())),
                })
            } else {
                Err(Spanned::new(
                    CompileError::Undefined(ident.inner.clone()),
                    path.span.clone(),
                ))
            }
        }
        ast::Expr::Type(ty) => Ok(CompiledExpr {
            expr: runtime::Expr::Literal(Variant::Type(compile_type(ty, compiler)?)),
            ty: VariantType::Type,
        }),
        ast::Expr::Assign(lhs, rhs) => {
            let compiled_lhs = compile_access(lhs, compiler)?;
            let compiled_rhs = compile_cast(rhs, &compiled_lhs.ty, compiler)?;

            Ok(CompiledExpr {
                expr: runtime::Expr::Assign(compiled_lhs.access, Box::new(compiled_rhs.expr)),
                ty: compiled_lhs.ty,
            })
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

            macro_rules! math_op {
                ($op_name:ident) => {{
                    match compiled_lhs.ty {
                        VariantType::I32 | VariantType::F32 => {
                            let compiled_rhs = compile_cast(rhs, &compiled_lhs.ty, compiler)?;

                            Ok(CompiledExpr {
                                expr: runtime::Expr::$op_name(
                                    Box::new(compiled_lhs.expr),
                                    Box::new(compiled_rhs.expr),
                                ),
                                ty: compiled_lhs.ty,
                            })
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
                            let compiled_rhs = compile_cast(rhs, &compiled_lhs.ty, compiler)?;

                            Ok(CompiledExpr {
                                expr: runtime::Expr::$op_name(
                                    Box::new(compiled_lhs.expr),
                                    Box::new(compiled_rhs.expr),
                                ),
                                ty: VariantType::Bool,
                            })
                        }
                        _ => Err(Spanned::new(
                            CompileError::InvalidType(compiled_lhs.ty),
                            lhs.span.clone(),
                        )),
                    }
                }};
            }

            match op.as_ref() {
                BinOp::Add => match &compiled_lhs.ty {
                    VariantType::I32 | VariantType::F32 => {
                        let compiled_rhs = compile_cast(rhs, &compiled_lhs.ty, compiler)?;

                        Ok(CompiledExpr {
                            expr: runtime::Expr::Add(
                                Box::new(compiled_lhs.expr),
                                Box::new(compiled_rhs.expr),
                            ),
                            ty: compiled_lhs.ty,
                        })
                    }
                    VariantType::String => {
                        let compiled_rhs = compile_expr(rhs, compiler)?;

                        Ok(CompiledExpr {
                            expr: runtime::Expr::Add(
                                Box::new(compiled_lhs.expr),
                                Box::new(compiled_rhs.expr),
                            ),
                            ty: compiled_lhs.ty,
                        })
                    }
                    _ => Err(Spanned::new(
                        CompileError::InvalidType(compiled_lhs.ty),
                        lhs.span.clone(),
                    )),
                },
                BinOp::Sub => math_op!(Sub),
                BinOp::Mul => math_op!(Mul),
                BinOp::Div => math_op!(Div),
                BinOp::Mod => math_op!(Mod),
                BinOp::Gt => cmp_op!(Gt, >),
                BinOp::AndAnd => {
                    let compiled_rhs = compile_cast(rhs, &compiled_lhs.ty, compiler)?;
                    Ok(CompiledExpr {
                        expr: runtime::Expr::AndAnd(
                            Box::new(compiled_lhs.expr),
                            Box::new(compiled_rhs.expr),
                        ),
                        ty: VariantType::Bool,
                    })
                }
                BinOp::OrOr => {
                    let compiled_rhs = compile_cast(rhs, &compiled_lhs.ty, compiler)?;

                    Ok(CompiledExpr {
                        expr: runtime::Expr::OrOr(
                            Box::new(compiled_lhs.expr),
                            Box::new(compiled_rhs.expr),
                        ),
                        ty: VariantType::Bool,
                    })
                }
                BinOp::EqEq => {
                    let compiled_rhs = compile_cast(rhs, &compiled_lhs.ty, compiler)?;

                    Ok(CompiledExpr {
                        expr: runtime::Expr::Eq(
                            Box::new(compiled_lhs.expr),
                            Box::new(compiled_rhs.expr),
                        ),
                        ty: VariantType::Bool,
                    })
                }
                BinOp::NotEq => {
                    let compiled_rhs = compile_cast(rhs, &compiled_lhs.ty, compiler)?;

                    Ok(CompiledExpr {
                        expr: runtime::Expr::NotEq(
                            Box::new(compiled_lhs.expr),
                            Box::new(compiled_rhs.expr),
                        ),
                        ty: VariantType::Bool,
                    })
                }
                _ => Err(Spanned::new(
                    CompileError::InvalidType(compiled_lhs.ty),
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
                    let expr = compile_cast(arg, &ty, compiler)?;

                    expr_args.push(expr.expr);
                }

                Ok(CompiledExpr {
                    expr: runtime::Expr::CallExpr(Box::new(compiled_expr.expr), expr_args),
                    ty: *return_type,
                })
            } else if let ast::Expr::Variable(path) = expr.as_ref().as_ref() {
                let module = compiler.get_module(path)?;

                let ident = path.segments.last().unwrap();

                if let Some(func) = module.get_func(ident).cloned() {
                    if args.len() != func.args.len() {
                        return Err(Spanned::new(
                            CompileError::InvalidArgCount,
                            expr.span.clone(),
                        ));
                    }

                    let mut expr_args = Vec::with_capacity(args.len());

                    for (arg, ty) in args.iter().zip(func.args) {
                        let expr = compile_cast(arg, &ty, compiler)?;

                        expr_args.push(expr.expr);
                    }

                    Ok(CompiledExpr {
                        expr: runtime::Expr::Call(func.id, expr_args),
                        ty: func.return_type.clone(),
                    })
                } else {
                    Err(Spanned::new(
                        CompileError::Undefined(ident.inner.clone()),
                        ident.span.clone(),
                    ))
                }
            } else {
                Err(Spanned::new(CompileError::InvalidAccess, expr.span.clone()))
            }
        }
        ast::Expr::MethodCall(expr, method, args) => {
            let mut compiled_expr = compile_expr(expr, compiler)?;

            if let VariantType::Ref(ty) = compiled_expr.ty {
                compiled_expr.expr = runtime::Expr::Deref(Box::new(compiled_expr.expr));
                compiled_expr.ty = *ty;
            }

            if let Some(methods) = compiler.data.methods.get(&compiled_expr.ty).cloned() {
                let method = methods.get_method(method.inner.as_ref()).ok_or_else(|| {
                    Spanned::new(
                        CompileError::Undefined(method.inner.clone()),
                        method.span.clone(),
                    )
                })?;

                if args.len() != method.args.len() {
                    return Err(Spanned::new(
                        CompileError::InvalidArgCount,
                        expr.span.clone(),
                    ));
                }

                let mut expr_args = Vec::with_capacity(args.len());

                if let SelfArg::Owned = &method.self_arg {
                    expr_args.push(compiled_expr.expr);
                } else {
                    if let Ok(mut access) = compile_access(expr, compiler) {
                        if let VariantType::Ref(ty) = access.ty {
                            access.access = runtime::Access::Deref(Box::new(access.access));
                            access.ty = *ty;
                        }

                        expr_args.push(runtime::Expr::Ref(access.access));
                    } else {
                        expr_args.push(runtime::Expr::RefOwned(Box::new(compiled_expr.expr)));
                    }
                }

                for (arg, ty) in args.iter().zip(&method.args) {
                    let expr = compile_cast(arg, &ty, compiler)?;

                    expr_args.push(expr.expr);
                }

                Ok(CompiledExpr {
                    expr: runtime::Expr::Call(method.id, expr_args),
                    ty: method.return_type.clone(),
                })
            } else {
                Err(Spanned::new(
                    CompileError::Undefined(method.inner.clone()),
                    method.span.clone(),
                ))
            }
        }
        ast::Expr::Field(expr, ident) => {
            let mut compiled_expr = compile_expr(expr, compiler)?;

            if let VariantType::Ref(ty) = compiled_expr.ty {
                compiled_expr.expr = runtime::Expr::Deref(Box::new(compiled_expr.expr));
                compiled_expr.ty = *ty;
            }

            if let VariantType::Class(id) = compiled_expr.ty {
                let class = &compiler.data.classes.get(&id).unwrap();

                if let Some((idx, field)) = class.get_field(ident) {
                    Ok(CompiledExpr {
                        expr: runtime::Expr::Field(Box::new(compiled_expr.expr), idx),
                        ty: field.ty.clone(),
                    })
                } else {
                    Err(Spanned::new(
                        CompileError::InvalidAccess,
                        ident.span.clone(),
                    ))
                }
            } else {
                Err(Spanned::new(
                    CompileError::InvalidType(compiled_expr.ty),
                    expr.span.clone(),
                ))
            }
        }
        ast::Expr::Block(block) => {
            let mut module = compiler.module().clone();

            module.instructions = Vec::new();

            let module = compiler.data.modules.push(module);

            let len = compiler.stack.len();

            let mut compiler = CompilerState {
                stack: compiler.stack.clone(),
                module,
                data: compiler.data,
            };

            let state = type_compile_stmts(&block.block.stmts, &mut compiler)?;
            let state = pre_compile_stmts(&block.block.stmts, state, &mut compiler)?;
            compile_stmts(&block.block.stmts, state, &mut compiler)?;

            let pop = compiler.stack.len() - len;

            if let Some(block) = &block.expr {
                let expr = compile_expr(block, &mut compiler)?;

                Ok(CompiledExpr {
                    expr: runtime::Expr::Block(
                        compiler.module().instructions.clone(),
                        Some(Box::new(expr.expr)),
                        pop,
                    ),
                    ty: expr.ty,
                })
            } else {
                Ok(CompiledExpr {
                    expr: runtime::Expr::Block(compiler.module().instructions.clone(), None, pop),
                    ty: VariantType::Unit,
                })
            }
        }
        ast::Expr::ClassInit(ty, fields) => {
            let mut fields = fields.clone();
            let compiled_ty = compile_type(ty, compiler)?;

            if let VariantType::Class(id) = compiled_ty {
                let class = compiler.data.classes.get(&id).unwrap().clone();

                let mut class_fields = Vec::new();

                for field in &class.fields {
                    let field_expr = fields
                        .iter()
                        .enumerate()
                        .find(|(_, (f, _))| **f == field.ident);

                    if let Some((i, (_, field_expr))) = field_expr {
                        let compiled_expr = compile_cast(field_expr, &field.ty, compiler)?;

                        fields.remove(i);

                        class_fields.push(compiled_expr.expr);
                    } else {
                        return Err(Spanned::new(
                            CompileError::FieldNotCovered(field.ident.clone()),
                            expr.span.clone(),
                        ));
                    }
                }

                if let Some((ident, field)) = fields.first() {
                    return Err(Spanned::new(
                        CompileError::DuplicateFieldSet(ident.inner.clone()),
                        ident.span.clone() + field.span.clone(),
                    ));
                }

                Ok(CompiledExpr {
                    expr: runtime::Expr::ClassInit(class_fields),
                    ty: compiled_ty,
                })
            } else {
                Err(Spanned::new(
                    CompileError::InvalidType(compiled_ty),
                    ty.span.clone(),
                ))
            }
        }
        ast::Expr::If(check, true_expr, false_expr) => {
            let compiled_check = compile_cast(check, &VariantType::Bool, compiler)?;
            let compiled_true_expr = compile_expr(true_expr, compiler)?;
            let compiled_false_expr = compile_cast(false_expr, &compiled_true_expr.ty, compiler)?;

            Ok(CompiledExpr {
                expr: runtime::Expr::If(
                    Box::new(compiled_check.expr),
                    Box::new(compiled_true_expr.expr),
                    Box::new(compiled_false_expr.expr),
                ),
                ty: compiled_true_expr.ty,
            })
        }
        ast::Expr::Comment(expr, _) => compile_expr(expr, compiler),
    }
}

#[inline]
fn eval_expr(
    expr: &Spanned<ast::Expr>,
    compiler: &mut CompilerState,
) -> CompileResult<(Variant, VariantType)> {
    match expr.as_ref() {
        ast::Expr::Literal(lit) => Ok((lit.variant(), lit.ty())),
        _ => Err(Spanned::new(CompileError::ExpectedConst, expr.span.clone())),
    }
}

#[derive(Debug)]
struct TypeCompileClass {
    id: Id,
}

#[derive(Debug)]
struct TypeCompileModule {
    id: Id,
    state: TypeCompileState,
}

#[derive(Debug, Default)]
struct TypeCompileState {
    classes: Vec<TypeCompileClass>,
    modules: Vec<TypeCompileModule>,
}

#[inline]
fn type_compile_stmts(
    stmts: &[Spanned<ast::Stmt>],
    compiler: &mut CompilerState<'_>,
) -> CompileResult<TypeCompileState> {
    let mut state = TypeCompileState::default();

    for stmt in stmts {
        match stmt.as_ref() {
            ast::Stmt::Class(class) => {
                let id = compiler.data.classes.next();

                compiler
                    .module_mut()
                    .push_type(class.ident.inner.clone(), VariantType::Class(id));

                state.classes.push(TypeCompileClass { id });
            }
            ast::Stmt::Module(module) => {
                let mut new_module = compiler.module().persist(&compiler.data);

                new_module.push_mod(Ident::new(String::from("super")), compiler.module);

                let id = compiler.data.modules.push(new_module);
                compiler.module_mut().push_mod(module.ident.clone(), id);

                let mut module_compiler = CompilerState::cleared(compiler);
                module_compiler.module = id;

                let module = type_compile_stmts(&module.stmts, &mut module_compiler)?;

                state.modules.push(TypeCompileModule { id, state: module });
            }
            _ => {}
        }
    }

    Ok(state)
}

#[derive(Debug)]
struct PreCompileFunc {
    id: Id,
    args: Vec<Variable>,
    return_type: VariantType,
}

#[derive(Debug)]
struct PreCompileClass {
    id: Id,
    method_modules: Vec<Id>,
    methods: Vec<PreCompileFunc>,
}

#[derive(Debug)]
struct PreCompileModule {
    id: Id,
    state: PreCompileState,
    stack: Stack,
}

#[derive(Debug, Default)]
struct PreCompileState {
    funcs: Vec<PreCompileFunc>,
    classes: Vec<PreCompileClass>,
    modules: Vec<PreCompileModule>,
}

#[inline]
fn pre_compile_stmts(
    stmts: &[Spanned<ast::Stmt>],
    state: TypeCompileState,
    compiler: &mut CompilerState<'_>,
) -> CompileResult<PreCompileState> {
    let mut classes = state.classes.into_iter();
    let mut modules = state.modules.into_iter();

    let mut state = PreCompileState::default();

    for stmt in stmts {
        match stmt.as_ref() {
            ast::Stmt::Func(func) => {
                let mut args = Vec::new();
                let mut args_vars = Vec::new();

                for arg in &func.args {
                    let ty = compile_type(&arg.ty, compiler)?;

                    args.push(ty.clone());
                    args_vars.push(Variable {
                        ident: arg.ident.inner.clone(),
                        ty,
                    });
                }

                let return_type = if let Some(ty) = &func.return_type {
                    compile_type(ty, compiler)?
                } else {
                    VariantType::Unit
                };

                let id = compiler.data.funcs.next();

                compiler.module_mut().push_func(Func {
                    id,
                    ident: func.ident.inner.clone(),
                    args,
                    return_type: return_type.clone(),
                });

                state.funcs.push(PreCompileFunc {
                    id,
                    args: args_vars,
                    return_type,
                });
            }
            ast::Stmt::Class(class) => {
                let class_state = classes.next().unwrap();

                let mut fields = Vec::new();
                let mut methods = Vec::new();
                let mut method_modules = Vec::new();
                let mut methods_state = Vec::new();

                for stmt in &class.stmts {
                    match stmt {
                        ast::ClassStmt::Field(ident, ty) => {
                            let ty = compile_type(ty, compiler)?;

                            fields.push(Field {
                                ident: ident.inner.clone(),
                                ty,
                                variant: None,
                            });
                        }
                        ast::ClassStmt::Method(method) => {
                            let mut module = compiler.module().clone();

                            module.push_type(
                                Ident::new(String::from("Self")),
                                VariantType::Class(class_state.id),
                            );

                            let module = compiler.data.modules.push(module);
                            let mut method_compiler = CompilerState::cleared(compiler);
                            method_compiler.module = module;

                            let mut args = Vec::new();
                            let mut args_vars = Vec::new();

                            let self_arg = match &method.self_arg {
                                ast::SelfArg::None => {
                                    todo!()
                                }
                                ast::SelfArg::Owned => {
                                    let this = VariantType::Class(class_state.id);
                                    args_vars.push(Variable {
                                        ident: Ident::new(String::from("self")),
                                        ty: this,
                                    });

                                    SelfArg::Owned
                                }
                                ast::SelfArg::Ref => {
                                    let this = VariantType::Ref(Box::new(VariantType::Class(
                                        class_state.id,
                                    )));
                                    args_vars.push(Variable {
                                        ident: Ident::new(String::from("self")),
                                        ty: this,
                                    });

                                    SelfArg::Ref
                                }
                            };

                            for arg in &method.args {
                                let ty = compile_type(&arg.ty, &mut method_compiler)?;

                                args.push(ty.clone());
                                args_vars.push(Variable {
                                    ident: arg.ident.inner.clone(),
                                    ty,
                                });
                            }

                            let return_type = if let Some(ty) = &method.return_type {
                                compile_type(ty, &mut method_compiler)?
                            } else {
                                VariantType::Unit
                            };

                            let id = compiler.data.funcs.next();

                            methods.push(Method {
                                id,
                                ident: method.ident.inner.clone(),
                                self_arg,
                                args,
                                return_type: return_type.clone(),
                            });

                            method_modules.push(module);

                            methods_state.push(PreCompileFunc {
                                id,
                                args: args_vars,
                                return_type,
                            });
                        }
                        _ => {}
                    }
                }

                state.classes.push(PreCompileClass {
                    id: class_state.id,
                    method_modules,
                    methods: methods_state,
                });

                compiler.data.classes.insert(
                    class_state.id,
                    Class {
                        id: class_state.id,
                        fields,
                    },
                );

                let ty = VariantType::Class(class_state.id);

                compiler
                    .data
                    .methods
                    .insert(ty.clone(), Methods { methods });

                // TODO: duplicate definition of class, should replace instead
                compiler
                    .module_mut()
                    .push_type(class.ident.inner.clone(), ty);
            }
            ast::Stmt::Module(module) => {
                let module_state = modules.next().unwrap();

                let mut module_compiler = CompilerState {
                    stack: Stack::default(),
                    module: module_state.id,
                    data: compiler.data,
                };

                let state_module = PreCompileModule {
                    id: module_state.id,
                    state: pre_compile_stmts(
                        &module.stmts,
                        module_state.state,
                        &mut module_compiler,
                    )?,
                    stack: module_compiler.stack.clone(),
                };

                state.modules.push(state_module);

                compiler
                    .module_mut()
                    .push_mod(module.ident.clone(), module_state.id);
            }
            _ => {}
        }
    }

    Ok(state)
}

#[inline]
fn compile_stmts(
    stmts: &[Spanned<ast::Stmt>],
    state: PreCompileState,
    compiler: &mut CompilerState<'_>,
) -> CompileResult<()> {
    let mut funcs = state.funcs.into_iter();
    let mut classes = state.classes.into_iter();
    let mut modules = state.modules.into_iter();

    for stmt in stmts {
        match stmt.as_ref() {
            ast::Stmt::Let(ident, ty, expr) => {
                let compiled_expr = if let Some(ty) = ty {
                    let ty = compile_type(ty, compiler)?;

                    compile_cast(expr, &ty, compiler)?
                } else {
                    compile_expr(expr, compiler)?
                };

                let variable = Variable {
                    ident: ident.inner.clone(),
                    ty: compiled_expr.ty,
                };

                compiler.stack.push(variable);

                compiler
                    .module_mut()
                    .instructions
                    .push(Instruction::Push(compiled_expr.expr));
            }
            ast::Stmt::If(check, expr, else_expr) => {
                let compiled_check = compile_cast(check, &VariantType::Bool, compiler)?;
                let compiled_expr = compile_expr(expr, compiler)?;

                let else_expr = if let Some(expr) = else_expr {
                    Some(compile_expr(expr, compiler)?.expr)
                } else {
                    None
                };

                compiler.module_mut().instructions.push(Instruction::If(
                    compiled_check.expr,
                    compiled_expr.expr,
                    else_expr,
                ));
            }
            ast::Stmt::While(check, expr) => {
                let compiled_check = compile_cast(check, &VariantType::Bool, compiler)?;
                let compiled_expr = compile_expr(expr, compiler)?;

                compiler
                    .module_mut()
                    .instructions
                    .push(Instruction::While(compiled_check.expr, compiled_expr.expr));
            }
            ast::Stmt::Expr(expr) => {
                let compiled_expr = compile_expr(expr, compiler)?;

                compiler
                    .module_mut()
                    .instructions
                    .push(Instruction::Expr(compiled_expr.expr));
            }
            ast::Stmt::Func(func) => {
                let func_state = funcs.next().unwrap();
                let mut func_compiler = CompilerState::cleared(compiler);

                for arg in func_state.args {
                    func_compiler.stack.push(arg);
                }

                let expr = compile_cast(&func.expr, &func_state.return_type, &mut func_compiler)?;

                compiler
                    .data
                    .funcs
                    .insert(func_state.id, runtime::Func::Func(expr.expr));
            }
            ast::Stmt::Module(module) => {
                let module_state = modules.next().unwrap();

                let mut module_compiler = CompilerState::map_stack(compiler, module_state.stack);
                module_compiler.module = module_state.id;

                compile_stmts(&module.stmts, module_state.state, &mut module_compiler)?;
            }
            ast::Stmt::Class(class) => {
                let class_state = classes.next().unwrap();
                let mut method_modules = class_state.method_modules.into_iter();
                let mut methods = class_state.methods.into_iter();

                for stmt in &class.stmts {
                    // we only care about compiling methods
                    if let ast::ClassStmt::Method(method) = stmt {
                        let method_state = methods.next().unwrap();
                        let mut method_compiler = CompilerState::cleared(compiler);
                        method_compiler.module = method_modules.next().unwrap();

                        for arg in method_state.args {
                            method_compiler.stack.push(arg);
                        }

                        let expr = compile_cast(
                            &method.expr,
                            &method_state.return_type,
                            &mut method_compiler,
                        )?;

                        compiler
                            .data
                            .funcs
                            .insert(method_state.id, runtime::Func::Func(expr.expr));
                    }
                }
            }
            _ => {}
        }
    }

    Ok(())
}

#[inline]
pub fn compile_program(compiler: &Compiler, input: &ast::Program) -> CompileResult<Program> {
    let mut data = compiler.data.clone();
    let module = compiler.module;

    let mut compiler = CompilerState {
        stack: Stack::default(),
        module,
        data: &mut data,
    };

    let state = type_compile_stmts(&input.stmts, &mut compiler)?;
    let state = pre_compile_stmts(&input.stmts, state, &mut compiler)?;
    compile_stmts(&input.stmts, state, &mut compiler)?;

    Ok(Program { data, module })
}
