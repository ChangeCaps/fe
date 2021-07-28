use crate::{
    as_variant::AsVariant,
    ast::Ident,
    compilation::{self, Class, Id, ProgramData, SelfArg},
    func::{FromFunc, Func},
    runtime::{Expr, Runtime, RuntimeError, RuntimeResult},
    variant::{Ref, Variant, VariantType},
};

/// A compiled fe program.
#[derive(Debug)]
pub struct Program {
    pub(crate) data: ProgramData,
    pub(crate) module: Id,
}

impl Program {
    /// Get the primary module.
    #[inline]
    pub fn module(&self) -> Module<'_> {
        Module {
            data: &self.data,
            id: &self.module,
        }
    }

    /// Run the primary module.
    #[inline]
    pub fn run(&self) -> RuntimeResult<()> {
        let mut runtime = Runtime::new(self);
        let module = self.data.modules.get(&self.module).unwrap();

        for instruction in &module.instructions {
            instruction.eval(&mut runtime)?;
        }

        Ok(())
    }
}

#[inline]
fn default_variant(ty: &VariantType, program: &Program) -> Option<Variant> {
    match ty {
        VariantType::I32 => Some(Variant::I32(0)),
        VariantType::F32 => Some(Variant::F32(0.0)),
        VariantType::Bool => Some(Variant::Bool(false)),
        VariantType::String => Some(Variant::String(String::new())),
        VariantType::Class(class) => {
            let class = program.data.classes.get(class)?;

            let mut fields = Vec::with_capacity(class.fields.len());

            for field in &class.fields {
                fields.push(Ref::Owned(default_variant(&field.ty, program)?));
            }

            Some(Variant::Class(fields))
        }
        VariantType::Unit => Some(Variant::Unit),
        _ => None,
    }
}

impl Class {
    #[inline]
    pub fn instance(&self, program: &Program) -> Option<ClassInstance> {
        let mut fields = Vec::with_capacity(self.fields.len());

        for field in &self.fields {
            fields.push(Ref::Owned(default_variant(&field.ty, program)?));
        }

        Some(ClassInstance {
            id: self.id,
            variant: Ref::Owned(Variant::Class(fields)),
        })
    }
}

/// A representation of a fe module.
pub struct Module<'a> {
    data: &'a ProgramData,
    id: &'a Id,
}

impl<'a> Module<'a> {
    #[inline]
    fn inner(&self) -> Option<&compilation::Module> {
        self.data.modules.get(self.id)
    }

    #[inline]
    pub fn get_class(&self, ident: &str) -> Option<&Class> {
        let ty = self.inner()?.get_type(&Ident::new(String::from(ident)))?;

        if let VariantType::Class(id) = ty {
            self.data.classes.get(id)
        } else {
            None
        }
    }
}

/// An rust instance of a fe class.
pub struct ClassInstance {
    pub id: Id,
    pub variant: Ref,
}

impl ClassInstance {
    /// Get a rust function mapped on to a method of a fe class.
    #[inline]
    pub fn get_method<'a, F: FromFunc<'a>>(&self, program: &'a Program, method: &str) -> Option<F> {
        let methods = program.data.methods.get(&VariantType::Class(self.id))?;

        let method = methods.get_method(method)?;

        if method.args == F::args() && method.return_type == F::return_type() {
            let self_arg = match &method.self_arg {
                SelfArg::Owned => Expr::Literal(self.variant.cloned()),
                SelfArg::Ref => Expr::Literal(Variant::Ref(Box::new(self.variant.clone()))),
            };

            let func = program.data.funcs.get(&method.id).unwrap();

            Some(F::from_fn(move |args| {
                let mut expr_args = Vec::new();

                expr_args.push(self_arg.clone());

                for arg in args {
                    expr_args.push(Expr::Literal(arg));
                }

                let mut runtime = Runtime::new(&program);

                func.eval(&expr_args, &mut runtime).unwrap()
            }))
        } else {
            None
        }
    }
}
