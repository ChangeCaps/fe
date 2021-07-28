use std::fmt::Result as FmtResult;
use std::fmt::Write;

use crate::ast::BinOp;
use crate::ast::Expr;
use crate::ast::Program;
use crate::ast::Stmt;
use crate::ast::UnaryOp;
use crate::variant::Variant;

pub struct Formatter<'a, W: Write> {
    pub write: &'a mut W,
    pub indent: usize,
}

impl<'a, W: Write> Formatter<'a, W> {
    #[inline]
    pub fn new(write: &'a mut W) -> Self {
        Self { write, indent: 0 }
    }

    #[inline]
    pub fn write(&mut self, to_write: impl AsRef<str>) -> FmtResult {
        self.write.write_str(to_write.as_ref())
    }

    #[inline]
    pub fn write_indent(&mut self) -> FmtResult {
        let indent: String = vec![' '; self.indent].iter().collect();

        self.write.write_str(&indent)
    }

    #[inline]
    pub fn writeln(&mut self, to_write: impl AsRef<str>) -> FmtResult {
        self.write.write_str(to_write.as_ref())
    }

    #[inline]
    pub fn indent<O>(&mut self, f: impl FnOnce(&mut Self) -> O) -> O {
        self.indent += 4;

        let res = f(self);

        self.indent -= 4;

        res
    }
}

#[inline]
pub fn format_variant<W: Write>(variant: &Variant, f: &mut Formatter<'_, W>) -> FmtResult {
    f.write(format!("{}", variant))
}

#[inline]
pub fn format_unary_op<W: Write>(op: &UnaryOp, f: &mut Formatter<'_, W>) -> FmtResult {
    match op {
        UnaryOp::Ref => f.write("&"),
        UnaryOp::Deref => f.write("*"),
        UnaryOp::Not => f.write("!"),
        UnaryOp::Neg => f.write("-"),
    }
}

#[inline]
pub fn format_bin_op<W: Write>(op: &BinOp, f: &mut Formatter<'_, W>) -> FmtResult {
    match op {
        BinOp::Add => f.write("+"),
        BinOp::Sub => f.write("-"),
        BinOp::Mul => f.write("*"),
        BinOp::Div => f.write("/"),
        BinOp::Mod => f.write("%"),
        BinOp::EqEq => f.write("=="),
        BinOp::NotEq => f.write("!="),
        BinOp::Gt => f.write(">"),
        BinOp::Lt => f.write("<"),
        BinOp::GtEq => f.write(">="),
        BinOp::LtEq => f.write("<"),
        BinOp::AndAnd => f.write("&&"),
        BinOp::OrOr => f.write("||"),
        BinOp::Xor => f.write("^"),
        BinOp::And => f.write("&"),
        BinOp::Not => f.write("~"),
        BinOp::Or => f.write("|"),
    }
}

#[inline]
pub fn format_expr<W: Write>(expr: &Expr, f: &mut Formatter<'_, W>) -> FmtResult {
    match expr {
        Expr::Paren(expr) => {
            f.write("(")?;
            format_expr(expr, f)?;
            f.write(")")?;

            Ok(())
        }
        Expr::Literal(variant) => format_variant(&variant.variant(), f),
        //Expr::Variable(ident) => f.write(ident.as_ref()),
        Expr::Type(ty) => {
            f.write("type ")
            //f.write(format!("{}", ty.ty()));
        }
        Expr::Assign(lhs, rhs) => {
            format_expr(lhs, f)?;
            f.write(" = ")?;
            format_expr(rhs, f)
        }
        Expr::UnaryOp(op, expr) => {
            format_unary_op(op, f)?;
            format_expr(expr, f)
        }
        Expr::BinOp(lhs, op, rhs) => {
            format_expr(lhs, f)?;
            f.write(" ")?;
            format_bin_op(op, f)?;
            f.write(" ")?;
            format_expr(rhs, f)
        }
        Expr::Block(expr_block) => {
            f.write("{\n")?;

            f.indent(|f| {
                for stmt in &expr_block.block.stmts {
                    f.write_indent()?;
                    format_stmt(stmt, f)?;
                    f.write("\n")?;
                }

                if let Some(expr) = &expr_block.expr {
                    f.write_indent()?;
                    format_expr(expr, f)?;
                    f.write("\n")
                } else {
                    Ok(())
                }
            })?;

            f.write_indent()?;
            f.write("}")
        }
        Expr::If(check, block, else_block) => {
            f.write("if ")?;

            format_expr(check, f)?;
            f.indent(|f| {
                f.write_indent()?;
                format_expr(block, f)
            })?;
            f.write(" else ")?;

            f.indent(|f| {
                f.write_indent()?;
                format_expr(else_block, f)
            })
        }
        _ => todo!(),
    }
}

#[inline]
pub fn format_stmt<W: Write>(stmt: &Stmt, f: &mut Formatter<'_, W>) -> FmtResult {
    match stmt {
        Stmt::Expr(expr) => {
            format_expr(expr, f)?;
            f.write(";")
        }
        _ => todo!(),
    }
}

#[inline]
pub fn format_program<W: Write>(program: &Program, f: &mut Formatter<'_, W>) -> FmtResult {
    for stmt in &program.stmts {
        format_stmt(stmt, f)?;
        f.write("\n")?;
    }

    Ok(())
}
