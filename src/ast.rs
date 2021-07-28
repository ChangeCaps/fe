use crate::{
    spanned::{Span, Spanned},
    variant::{Variant, VariantType},
};
use std::sync::Arc;

pub type Ident = Arc<String>;

#[derive(Clone, Debug)]
pub struct Program {
    pub stmts: Vec<Spanned<Stmt>>,
}

#[derive(Clone, Debug)]
pub struct NamedModule {
    pub ident: Ident,
    pub stmts: Vec<Spanned<Stmt>>,
}

#[derive(Clone, Debug)]
pub struct Path {
    pub segments: Vec<Spanned<Ident>>,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
}

#[derive(Clone, Debug)]
pub struct ExprBlock {
    pub block: Block,
    pub expr: Option<Spanned<Expr>>,
}

#[derive(Clone, Debug)]
pub struct FuncArg {
    pub ident: Spanned<Ident>,
    pub ty: Spanned<TypeLiteral>,
}

#[derive(Clone, Debug)]
pub struct Func {
    pub ident: Spanned<Ident>,
    pub args: Vec<FuncArg>,
    pub return_type: Option<Spanned<TypeLiteral>>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelfArg {
    Owned,
    Ref,
    None,
}

#[derive(Clone, Debug)]
pub struct Method {
    pub ident: Spanned<Ident>,
    pub self_arg: SelfArg,
    pub args: Vec<FuncArg>,
    pub return_type: Option<Spanned<TypeLiteral>>,
    pub expr: Spanned<Expr>,
}

#[derive(Clone, Debug)]
pub struct Class {
    pub ident: Spanned<Ident>,
    pub stmts: Vec<ClassStmt>,
}

#[derive(Clone, Debug)]
pub enum ClassStmt {
    Field(Spanned<Ident>, Spanned<TypeLiteral>),
    Method(Method),
    Comment(String),
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Let(Spanned<Ident>, Option<Spanned<TypeLiteral>>, Spanned<Expr>),
    If(Spanned<Expr>, Spanned<Expr>, Option<Spanned<Expr>>),
    While(Spanned<Expr>, Spanned<Expr>),
    Expr(Spanned<Expr>),
    Func(Func),
    Class(Class),
    Module(NamedModule),
    Comment(String),
}

#[derive(Clone, Debug)]
pub enum UnaryOp {
    Ref,
    Deref,
    Not,
    Neg,
}

#[derive(Clone, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    EqEq,
    NotEq,
    Gt,
    Lt,
    GtEq,
    LtEq,
    AndAnd,
    OrOr,
    Xor,
    And,
    Not,
    Or,
}

#[derive(Clone, Debug)]
pub enum Literal {
    I32(i32),
    F32(f32),
    Bool(bool),
    String(String),
    Unit,
}

impl Literal {
    #[inline]
    pub fn ty(&self) -> VariantType {
        match self {
            Self::I32(_) => VariantType::I32,
            Self::F32(_) => VariantType::F32,
            Self::Bool(_) => VariantType::Bool,
            Self::String(_) => VariantType::String,
            Self::Unit => VariantType::Unit,
        }
    }

    #[inline]
    pub fn variant(&self) -> Variant {
        match self {
            Self::I32(v) => Variant::I32(*v),
            Self::F32(v) => Variant::F32(*v),
            Self::Bool(v) => Variant::Bool(*v),
            Self::String(string) => Variant::String(string.clone()),
            Self::Unit => Variant::Unit,
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeLiteral {
    I32,
    F32,
    Bool,
    Func(Vec<Spanned<TypeLiteral>>, Box<Spanned<TypeLiteral>>),
    Ref(Box<Spanned<TypeLiteral>>),
    Class(Path),
    Type,
    Unit,
    String,
    Any,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Paren(Box<Spanned<Expr>>),
    Literal(Literal),
    Variable(Spanned<Path>),
    Type(Spanned<TypeLiteral>),
    Assign(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    UnaryOp(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    BinOp(Box<Spanned<Expr>>, Spanned<BinOp>, Box<Spanned<Expr>>),
    Call(Box<Spanned<Expr>>, Vec<Spanned<Expr>>),
    MethodCall(Box<Spanned<Expr>>, Spanned<Ident>, Vec<Spanned<Expr>>),
    Field(Box<Spanned<Expr>>, Spanned<Ident>),
    Block(Box<Spanned<ExprBlock>>),
    ClassInit(Spanned<TypeLiteral>, Vec<(Spanned<Ident>, Spanned<Expr>)>),
    If(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Comment(Box<Spanned<Expr>>, String),
}
