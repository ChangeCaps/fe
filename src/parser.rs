use crate::{
    ast::{
        BinOp, Block, Class, ClassStmt, Expr, ExprBlock, Func, FuncArg, Ident, Literal, Method,
        NamedModule, Path, Program, SelfArg, Stmt, TypeLiteral, UnaryOp,
    },
    spanned::{self, Span, Spanned},
};

pub type ParseResult<T> = Result<T, Spanned<ParseError>>;

#[derive(Clone, Debug)]
pub enum ParseError {
    ParseError,
    InvalidIdent,
    UnexpectedEof,
    ExpectedToken(&'static str),
    ExpectedTokens(&'static [&'static str]),
    Expected(&'static [&'static str]),
}

impl ParseError {
    #[inline]
    pub fn msg(&self) -> String {
        match self {
            Self::ParseError => format!("parse error"),
            Self::InvalidIdent => format!("invalid identifier"),
            Self::UnexpectedEof => format!("unexpected end of file"),
            Self::ExpectedToken(token) => format!("expected token: '{}'", token),
            Self::ExpectedTokens(tokens) => format!("expected one of tokens: {:?}", tokens),
            Self::Expected(expected) => format!("expected following: {:?}", expected),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Parser<'a> {
    pub lo: usize,
    pub source: &'a str,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(source: &'a str) -> Self {
        Self { lo: 0, source }
    }

    #[inline]
    pub fn complete(&self) -> bool {
        self.lo == self.source.len()
    }

    #[inline]
    pub fn position(&self) -> Span {
        Span::new(self.lo, self.lo)
    }

    #[inline]
    pub fn rest(&self) -> ParseResult<&str> {
        if self.lo < self.source.len() {
            Ok(&self.source[self.lo..])
        } else {
            Err(Spanned::new(ParseError::UnexpectedEof, self.position()))
        }
    }

    #[inline]
    pub fn token(&mut self, token: &'static str) -> ParseResult<Span> {
        if self.rest()?.starts_with(token) {
            let hi = self.lo + token.len();

            let span = Span::new(self.lo, hi);
            self.lo = hi;

            Ok(span)
        } else {
            let span = Span::new(self.lo, self.lo + token.len());

            Err(Spanned::new(ParseError::ExpectedToken(token), span))
        }
    }

    #[inline]
    pub fn token_peek(&mut self, token: &'static str) -> ParseResult<Span> {
        if self.rest()?.starts_with(token) {
            let hi = self.lo + token.len();

            let span = Span::new(self.lo, hi);

            Ok(span)
        } else {
            let span = Span::new(self.lo, self.lo + token.len());

            Err(Spanned::new(ParseError::ExpectedToken(token), span))
        }
    }

    #[inline]
    pub fn peek<O>(&mut self, f: impl FnOnce(&mut Self) -> ParseResult<O>) -> ParseResult<O> {
        let mut parser = self.clone();

        let res = f(&mut parser);

        if res.is_ok() {
            *self = parser;
        }

        res
    }

    #[inline]
    pub fn take(&mut self, len: usize) -> ParseResult<&'a str> {
        let hi = self.lo + len;

        if hi >= self.source.len() {
            return Err(Spanned::new(
                ParseError::UnexpectedEof,
                Span::new(self.lo, hi),
            ));
        }

        Ok(&self.source[self.lo..hi])
    }

    #[inline]
    pub fn take_char(&mut self) -> ParseResult<char> {
        if self.lo < self.source.len() {
            let res = self.source.chars().nth(self.lo).unwrap();

            self.lo += 1;

            Ok(res)
        } else {
            Err(Spanned::new(ParseError::UnexpectedEof, self.position()))
        }
    }

    #[inline]
    pub fn take_while0(&mut self, f: impl Fn(char) -> bool) -> ParseResult<&'a str> {
        let hi = self.lo
            + self
                .rest()?
                .find(|c: char| !f(c))
                .ok_or_else(|| Spanned::new(ParseError::UnexpectedEof, self.position()))?;

        let res = &self.source[self.lo..hi];

        self.lo = hi;

        Ok(res)
    }

    #[inline]
    pub fn take_while1(&mut self, f: impl Fn(char) -> bool) -> ParseResult<&'a str> {
        let res = spanned(self, |p| p.take_while0(f))?;

        if res.len() > 0 {
            Ok(*res)
        } else {
            Err(Spanned::new(ParseError::ParseError, res.span))
        }
    }

    #[inline]
    pub fn whitespace0(&mut self) -> ParseResult<()> {
        if let Ok(rest) = self.rest() {
            let hi = rest
                .find(|c: char| !c.is_whitespace())
                .map(|i| self.lo + i)
                .unwrap_or_else(|| self.source.len());

            self.lo = hi;
        }

        Ok(())
    }

    #[inline]
    pub fn whitespace1(&mut self) -> ParseResult<()> {
        self.take_while1(|c| c.is_whitespace())?;

        Ok(())
    }
}

macro_rules! alt {
    ($expr:expr $(=> $res:expr)? $(,)?) => {{
        let res = $expr?;

        $(
            let res = $res(res)?;
        )?

        Ok(res)
    }};
    ($expr:expr $(=> $res:expr)?, $($rest:tt)*) => {
        if let Ok(res) = $expr {
            $(
                let res = $res(res)?;
            )?

            Ok(res)
        } else {
            alt!($($rest)*)
        }
    };
}

#[inline]
fn opt<O>(parser: &mut Parser<'_>, f: impl FnOnce(&mut Parser<'_>) -> ParseResult<O>) -> Option<O> {
    match parser.peek(f) {
        Ok(v) => Some(v),
        Err(_e) => None,
    }
}

#[inline]
fn spanned<'a, O>(
    parser: &mut Parser<'a>,
    f: impl FnOnce(&mut Parser<'a>) -> ParseResult<O>,
) -> ParseResult<Spanned<O>> {
    let start = parser.position();

    let res = f(parser)?;

    let end = parser.position();

    Ok(Spanned::new(res, start + end))
}

#[inline]
fn ident(parser: &mut Parser<'_>) -> ParseResult<Ident> {
    let first = parser.take_char()?;

    if first.is_alphabetic() || first == '_' {
        let last = parser.take_while0(|c| c.is_alphanumeric() || c == '_')?;

        Ok(Ident::new(String::from(String::from(first) + last)))
    } else {
        Err(Spanned::new(ParseError::InvalidIdent, parser.position()))
    }
}

#[inline]
fn number(parser: &mut Parser<'_>) -> ParseResult<Literal> {
    let first = parser.take_while1(|c| c.is_numeric())?;

    if let Ok(lit) = parser.peek(|parser| {
        parser.token(".")?;
        let last = parser.take_while1(|c| c.is_numeric())?;

        Ok(Literal::F32(
            (first.to_owned() + "." + last).parse().unwrap(),
        ))
    }) { 
        Ok(lit)
    } else {
        Ok(Literal::I32(first.parse().unwrap()))
    }
}

#[inline]
fn variant_type(parser: &mut Parser<'_>) -> ParseResult<TypeLiteral> {
    alt!(
        parser.token("&") => |_| {
            let ty = spanned(parser, variant_type)?;

            Ok(TypeLiteral::Ref(Box::new(ty)))
        },
        parser.token("i32") => |_| Ok(TypeLiteral::I32),
        parser.token("f32") => |_| Ok(TypeLiteral::F32),
        parser.token("bool") => |_| Ok(TypeLiteral::Bool),
        parser.token("type") => |_| Ok(TypeLiteral::Type),
        parser.token("()") => |_| Ok(TypeLiteral::Unit),
        parser.token("any") => |_| Ok(TypeLiteral::Any),
        parser.token("str") => |_| Ok(TypeLiteral::String),
        path(parser) => |ident| Ok(TypeLiteral::Class(ident))
    )
}

#[inline]
fn path(parser: &mut Parser<'_>) -> ParseResult<Path> {
    let mut segments = Vec::new();

    segments.push(spanned(parser, ident)?);

    loop {
        if parser.token("::").is_ok() {
            segments.push(spanned(parser, ident)?);
        } else {
            break;
        }
    }

    Ok(Path { segments })
}

#[inline]
fn typed(parser: &mut Parser<'_>) -> ParseResult<TypeLiteral> {
    parser.token(":")?;
    parser.whitespace0()?;
    variant_type(parser)
}

#[inline]
fn expr_block(parser: &mut Parser<'_>) -> ParseResult<ExprBlock> {
    parser.token("{")?;
    parser.whitespace0()?;

    let mut stmts = Vec::new();

    loop {
        if parser.token_peek("}").is_ok() {
            parser.token("}")?;

            let block = Block { stmts };

            break Ok(ExprBlock { block, expr: None });
        } else if let Ok(stmt) = parser.peek(|parser| spanned(parser, stmt)) {
            stmts.push(stmt);

            parser.whitespace0()?;
        } else {
            let expr = spanned(parser, expr)?;
            parser.whitespace0()?;
            parser.token("}")?;

            let block = Block { stmts };

            break Ok(ExprBlock {
                block,
                expr: Some(expr),
            });
        }
    }
}

#[inline]
fn block_expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    let block = spanned(parser, expr_block)?;

    Ok(Expr::Block(Box::new(block)))
}

#[inline]
fn class_init_expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    let ty = spanned(parser, variant_type)?;
    parser.whitespace0()?;
    parser.token("{")?;

    let mut fields = Vec::new();

    loop {
        parser.whitespace0()?;
        if parser.token("}").is_ok() {
            break;
        }

        let ident = spanned(parser, ident)?;
        parser.whitespace0()?;
        parser.token(":")?;
        parser.whitespace0()?;
        let expr = spanned(parser, expr)?;
        parser.whitespace0()?;
        parser.token(",")?;

        fields.push((ident, expr));
    }

    Ok(Expr::ClassInit(ty, fields))
}

#[inline]
fn term_expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    alt!(
        parser.token("type") => |_| {
            parser.whitespace1()?;
            let ty = spanned(parser, variant_type)?;

            Ok(Expr::Type(ty))
        },
        parser.token("if") => |_| {
            parser.whitespace1()?;
            let check = spanned(parser, expr)?;
            parser.whitespace0()?;
            let block = spanned(parser, block_expr)?;
            parser.whitespace0()?;
            parser.token("else")?;
            parser.whitespace0()?;
            let else_block = spanned(parser, block_expr)?;

            Ok(Expr::If(Box::new(check), Box::new(block), Box::new(else_block)))
        },
        parser.token("true") => |_| Ok(Expr::Literal(Literal::Bool(true))),
        parser.token("false") => |_| Ok(Expr::Literal(Literal::Bool(false))),
        parser.token("()") => |_| Ok(Expr::Literal(Literal::Unit)),
        parser.peek(|parser| class_init_expr(parser)),
        parser.peek(|parser| spanned(parser, path)).map(|path| Expr::Variable(path)),
        number(parser) => |variant| Ok(Expr::Literal(variant)),
        parser.token_peek("{") => |_| block_expr(parser),
        parser.token("\"") => |_| {
            let string = parser.take_while0(|c| c != '"')?;
            parser.token("\"")?;

            let mut string = string.replace(r#"\n"#, "\n");
            string = string.replace(r#"\t"#, "\t");
            string = string.replace(r#"\r"#, "\r");

            Ok(Expr::Literal(Literal::String(string)))
        },
        parser.token("(") => |_| {
            parser.whitespace0()?;
            let expr = spanned(parser, expr)?;
            parser.whitespace0()?;
            parser.token(")")?;

            Ok(Expr::Paren(Box::new(expr)))
        },
        Err(Spanned::new(ParseError::Expected(
            &[
                "integer",
                "float",
                "variable",
                "{",
                "(",
                "type",
                "if",
                "true",
                "false",
                "()",
            ]),
            parser.position())
        ),
    )
}

#[inline]
fn call_expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    let expr = spanned(parser, term_expr)?;

    alt!(
        parser.token("(") => |_| {
            let mut args = Vec::new();

            loop {
                if parser.token(")").is_ok() {
                    break;
                }

                if args.len() > 0 {
                    parser.token(",")?;
                    parser.whitespace0()?;
                }

                let arg = spanned(parser, self::expr)?;

                args.push(arg);
            }

            Ok(Expr::Call(Box::new(expr), args))
        },
        Ok(expr.inner),
    )
}

#[inline]
fn access_expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    let mut lhs = spanned(parser, call_expr)?;

    loop {
        if parser.token(".").is_ok() {
            let ident = spanned(parser, ident)?;

            if parser.token("(").is_ok() {
                let mut args = Vec::new();

                loop {
                    parser.whitespace0()?;
                    if parser.token(")").is_ok() {
                        break;
                    }

                    if args.len() > 0 {
                        parser.token(",")?;
                    }

                    parser.whitespace0()?;
                    let arg = spanned(parser, expr)?;

                    args.push(arg);
                }

                let span = lhs.span.clone() + ident.span.clone();
                lhs = Spanned::new(Expr::MethodCall(Box::new(lhs), ident, args), span);
            } else {
                let span = lhs.span.clone() + ident.span.clone();
                lhs = Spanned::new(Expr::Field(Box::new(lhs), ident), span);
            }
        } else {
            break;
        }
    }

    Ok(lhs.inner)
}

#[inline]
fn unary_op(parser: &mut Parser<'_>) -> ParseResult<UnaryOp> {
    alt!(
        parser.token("&") => |_| Ok(UnaryOp::Ref),
        parser.token("*") => |_| Ok(UnaryOp::Deref),
        parser.token("!") => |_| Ok(UnaryOp::Not),
        parser.token("-") => |_| Ok(UnaryOp::Neg),
    )
}

#[inline]
fn unary_op_expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    if let Ok(op) = spanned(parser, unary_op) {
        let expr = spanned(parser, unary_op_expr)?;

        Ok(Expr::UnaryOp(op, Box::new(expr)))
    } else {
        access_expr(parser)
    }
}

#[inline]
fn comment_expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    let expr = spanned(parser, unary_op_expr)?;

    parser.whitespace0()?;

    if parser.token("//").is_ok() {
        let comment = parser.take_while0(|c| c != '\n')?;

        Ok(Expr::Comment(Box::new(expr), String::from(comment)))
    } else {
        Ok(expr.inner)
    }
}

#[inline]
fn mul_op(parser: &mut Parser<'_>) -> ParseResult<BinOp> {
    alt!(
        parser.token("*") => |_| Ok(BinOp::Mul),
        parser.token("/") => |_| Ok(BinOp::Div),
        parser.token("%") => |_| Ok(BinOp::Mod),
    )
}

#[inline]
fn mul_expr(parser: &mut Parser<'_>, lhs: Spanned<Expr>) -> ParseResult<Expr> {
    parser.whitespace0()?;

    alt!(
        spanned(parser, mul_op) => |op| {
            parser.whitespace0()?;

            let rhs = spanned(parser, comment_expr)?;

            let span = lhs.span.clone() + rhs.span.clone();

            let lhs = Spanned::new(Expr::BinOp(
                Box::new(lhs),
                op,
                Box::new(rhs),
            ), span);

            mul_expr(parser, lhs)
        },
        Ok(lhs.inner),
    )
}

#[inline]
fn add_op(parser: &mut Parser<'_>) -> ParseResult<BinOp> {
    alt!(
        parser.token("+") => |_| Ok(BinOp::Add),
        parser.token("-") => |_| Ok(BinOp::Sub),
    )
}

#[inline]
fn add_expr(parser: &mut Parser<'_>, lhs: Spanned<Expr>) -> ParseResult<Expr> {
    let lhs = spanned(parser, |parser| mul_expr(parser, lhs))?;
    parser.whitespace0()?;

    alt!(
        spanned(parser, add_op) => |op| {
            parser.whitespace0()?;

            let rhs = spanned(parser, comment_expr)?;
            let rhs = spanned(parser, |parser| mul_expr(parser, rhs))?;

            let span = lhs.span.clone() + rhs.span.clone();

            let lhs = Spanned::new(Expr::BinOp(
                Box::new(lhs),
                op,
                Box::new(rhs),
            ), span);

            add_expr(parser, lhs)
        },
        Ok(lhs.inner.clone()),
    )
}

#[inline]
fn cmp_op(parser: &mut Parser<'_>) -> ParseResult<BinOp> {
    alt!(
        parser.token("==") => |_| Ok(BinOp::EqEq),
        parser.token("!=") => |_| Ok(BinOp::NotEq),
        parser.token(">") => |_| Ok(BinOp::Gt),
        parser.token("<") => |_| Ok(BinOp::Lt),
        parser.token(">=") => |_| Ok(BinOp::GtEq),
        parser.token("<=") => |_| Ok(BinOp::LtEq),
    )
}

#[inline]
fn cmp_expr(parser: &mut Parser<'_>, lhs: Spanned<Expr>) -> ParseResult<Expr> {
    let lhs = spanned(parser, |parser| add_expr(parser, lhs))?;
    parser.whitespace0()?;

    alt!(
        spanned(parser, cmp_op) => |op| {
            parser.whitespace0()?;

            let rhs = spanned(parser, comment_expr)?;
            let rhs = spanned(parser, |parser| add_expr(parser, rhs))?;

            let span = lhs.span.clone() + rhs.span.clone();

            let lhs = Spanned::new(Expr::BinOp(
                Box::new(lhs),
                op,
                Box::new(rhs),
            ), span);

            cmp_expr(parser, lhs)
        },
        Ok(lhs.inner),
    )
}

#[inline]
fn bool_op(parser: &mut Parser<'_>) -> ParseResult<BinOp> {
    alt!(
        parser.token("&&") => |_| Ok(BinOp::AndAnd),
        parser.token("||") => |_| Ok(BinOp::OrOr),
    )
}

#[inline]
fn bool_expr(parser: &mut Parser<'_>, lhs: Spanned<Expr>) -> ParseResult<Expr> {
    let lhs = spanned(parser, |parser| cmp_expr(parser, lhs))?;
    parser.whitespace0()?;

    alt!(
        spanned(parser, bool_op) => |op| {
            parser.whitespace0()?;

            let rhs = spanned(parser, comment_expr)?;
            let rhs = spanned(parser, |parser| cmp_expr(parser, rhs))?;

            let span = lhs.span.clone() + rhs.span.clone();

            let lhs = Spanned::new(Expr::BinOp(
                Box::new(lhs),
                op,
                Box::new(rhs),
            ), span);

            bool_expr(parser, lhs)
        },
        Ok(lhs.inner),
    )
}

#[inline]
fn assign_expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    let lhs = spanned(parser, comment_expr)?;
    let lhs = spanned(parser, |parser| bool_expr(parser, lhs))?;

    alt!(
        parser.token("=") => |_| {
            parser.whitespace0()?;

            let rhs = Box::new(spanned(parser, assign_expr)?);

            Ok(Expr::Assign(Box::new(lhs), rhs))
        },
        Ok(lhs.inner),
    )
}

#[inline]
pub fn expr(parser: &mut Parser<'_>) -> ParseResult<Expr> {
    assign_expr(parser)
}

#[inline]
fn let_stmt(parser: &mut Parser<'_>) -> ParseResult<Stmt> {
    parser.token("let")?;
    parser.whitespace1()?;
    let ident = spanned(parser, ident)?;
    parser.whitespace0()?;
    let ty = opt(parser, |parser| spanned(parser, typed));
    parser.whitespace0()?;
    parser.token("=")?;
    parser.whitespace0()?;
    let expr = spanned(parser, expr)?;
    parser.whitespace0()?;
    parser.token(";")?;

    Ok(Stmt::Let(ident, ty, expr))
}

#[inline]
fn if_stmt(parser: &mut Parser<'_>) -> ParseResult<Stmt> {
    parser.token("if")?;
    parser.whitespace1()?;
    let check = spanned(parser, expr)?;
    parser.whitespace0()?;
    let expr = spanned(parser, block_expr)?;
    parser.whitespace0()?;

    let else_expr = if parser.token_peek("else").is_ok() {
        parser.whitespace0()?;

        Some(spanned(parser, block_expr)?)
    } else {
        None
    };

    Ok(Stmt::If(check, expr, else_expr))
}

#[inline]
fn func(parser: &mut Parser<'_>) -> ParseResult<Func> {
    parser.token("fn")?;
    parser.whitespace1()?;
    let ident = spanned(parser, ident)?;
    parser.whitespace0()?;

    parser.token("(")?;

    let mut args = Vec::new();

    loop {
        if parser.token(")").is_ok() {
            if args.len() > 0 {
                opt(parser, |parser| parser.token(","));
            }

            break;
        }

        if args.len() > 0 {
            parser.token(",")?;
            parser.whitespace0()?;
        }

        let ident = spanned(parser, self::ident)?;
        parser.whitespace0()?;
        parser.token(":")?;
        parser.whitespace0()?;
        let ty = spanned(parser, variant_type)?;

        args.push(FuncArg { ident, ty });
    }

    let return_type = opt(parser, |parser| {
        parser.whitespace0()?;
        parser.token("->")?;
        parser.whitespace0()?;
        spanned(parser, variant_type)
    });

    parser.whitespace0()?;

    let expr = spanned(parser, block_expr)?;

    Ok(Func {
        ident,
        args,
        return_type,
        expr,
    })
}

#[inline]
fn self_arg(parser: &mut Parser<'_>) -> ParseResult<SelfArg> {
    alt!(
        parser.token("self") => |_| Ok(SelfArg::Owned),
        parser.token("&self") => |_| Ok(SelfArg::Ref),
    )
}

#[inline]
fn method(parser: &mut Parser<'_>) -> ParseResult<Method> {
    parser.token("fn")?;
    parser.whitespace1()?;
    let ident = spanned(parser, ident)?;
    parser.whitespace0()?;

    parser.token("(")?;

    let self_arg = if let Ok(self_arg) = self_arg(parser) {
        parser.whitespace0()?;

        self_arg
    } else {
        SelfArg::None
    };

    let mut args = Vec::new();

    loop {
        if parser.token(")").is_ok() {
            if args.len() > 0 {
                opt(parser, |parser| parser.token(","));
            }

            break;
        }

        if args.len() > 0 || self_arg != SelfArg::None {
            parser.token(",")?;
            parser.whitespace0()?;
        }

        let ident = spanned(parser, self::ident)?;
        parser.whitespace0()?;
        parser.token(":")?;
        parser.whitespace0()?;
        let ty = spanned(parser, variant_type)?;

        args.push(FuncArg { ident, ty });
    }

    let return_type = opt(parser, |parser| {
        parser.whitespace0()?;
        parser.token("->")?;
        parser.whitespace0()?;
        spanned(parser, variant_type)
    });

    parser.whitespace0()?;

    let expr = spanned(parser, block_expr)?;

    Ok(Method {
        ident,
        self_arg,
        args,
        return_type,
        expr,
    })
}

#[inline]
fn class_stmt(parser: &mut Parser<'_>) -> ParseResult<ClassStmt> {
    alt!(
        parser.token_peek("fn") => |_| Ok(ClassStmt::Method(method(parser)?)),
        parser.token("//") => |_| {
            let comment = parser.take_while0(|c| c != '\n')?;

            Ok(ClassStmt::Comment(String::from(comment)))
        },
        spanned(parser, ident) => |ident| {
            parser.whitespace0()?;
            let ty = spanned(parser, typed)?;
            parser.whitespace0()?;
            parser.token(",")?;

            Ok(ClassStmt::Field(ident, ty))
        }
    )
}

#[inline]
fn class(parser: &mut Parser<'_>) -> ParseResult<Class> {
    parser.token("class")?;
    parser.whitespace1()?;
    let ident = spanned(parser, ident)?;
    parser.whitespace0()?;

    parser.token("{")?;
    parser.whitespace0()?;

    let mut stmts = Vec::new();

    loop {
        parser.whitespace0()?;

        if parser.token("}").is_ok() {
            break;
        }

        let stmt = class_stmt(parser)?;

        stmts.push(stmt);
    }

    Ok(Class { ident, stmts })
}

#[inline]
fn named_module(parser: &mut Parser<'_>) -> ParseResult<NamedModule> {
    parser.token("mod")?;
    parser.whitespace1()?;
    let ident = ident(parser)?;
    parser.whitespace0()?;
    parser.token("{")?;

    let mut stmts = Vec::new();

    loop {
        parser.whitespace0()?;

        if parser.token("}").is_ok() {
            break;
        }

        let stmt = spanned(parser, stmt)?;

        stmts.push(stmt);
    }

    Ok(NamedModule { ident, stmts })
}

#[inline]
pub fn stmt(parser: &mut Parser<'_>) -> ParseResult<Stmt> {
    alt!(
        parser.token_peek("let") => |_| let_stmt(parser),
        parser.token_peek("fn") => |_| Ok(Stmt::Func(func(parser)?)),
        parser.token_peek("class") => |_| Ok(Stmt::Class(class(parser)?)),
        parser.token_peek("mod") => |_| Ok(Stmt::Module(named_module(parser)?)),
        parser.token_peek("if") => |_| if_stmt(parser),
        parser.token("while") => |_| {
            parser.whitespace1()?;
            let check = spanned(parser, expr)?;
            parser.whitespace0()?;
            let expr = spanned(parser, block_expr)?;

            Ok(Stmt::While(check, expr))
        },
        parser.token("//") => |_| {
            let comment = parser.take_while0(|c| c != '\n')?;

            Ok(Stmt::Comment(String::from(comment)))
        },
        spanned(parser, expr) => |expr| {
            parser.token(";")?;

            Ok(Stmt::Expr(expr))
        },
        Err(Spanned::new(ParseError::Expected(&["let", "mod", "fn", "class", "{", "expression"]), parser.position()))
    )
}

#[inline]
pub fn program(parser: &mut Parser<'_>) -> ParseResult<Program> {
    parser.whitespace0()?;

    let mut stmts = Vec::new();

    loop {
        let stmt = spanned(parser, stmt)?;
        parser.whitespace0()?;

        stmts.push(stmt);

        if parser.complete() {
            break;
        }
    }

    Ok(Program { stmts })
}
