use std::{any::TypeId, sync::Arc};

use crate::{
    add_std::add_std,
    as_variant::AsVariant,
    ast::{Ident, SelfArg},
    class::Class,
    compilation::{self, compile_program, CompileResult, Func, Id, ProgramData},
    func::{self, Method, SelfArgOf},
    parser::{program, Parser},
    prelude::Program,
    runtime,
    ty::Type,
    variant::VariantType,
};

pub struct RegisterType<'a, T: Type> {
    _marker: std::marker::PhantomData<*const T>,
    module: Id,
    data: &'a mut ProgramData,
}

impl<'a, T: Type> RegisterType<'a, T> {
    #[inline]
    fn module_mut(&mut self) -> &mut compilation::Module {
        self.data.modules.get_mut(&self.module).unwrap()
    }

    #[inline]
    pub fn register_method<S: SelfArgOf<T>, M: Method<S, A, R>, A, R>(
        &mut self,
        ident: impl Into<String>,
        method: M,
    ) {
        let id = self.data.funcs.next();

        self.data
            .methods
            .entry(VariantType::Custom(TypeId::of::<T>()))
            .or_default()
            .methods
            .push(compilation::Method {
                id,
                ident: Ident::new(ident.into()),
                self_arg: method.self_arg(),
                args: method.args(),
                return_type: method.return_type(),
            });

        self.data
            .funcs
            .insert(id, runtime::Func::Embedded(Arc::new(method.to_fn())))
    }
}

impl<'a, T: Type> Drop for RegisterType<'a, T> {
    #[inline]
    fn drop(&mut self) {
        self.module_mut().push_type(
            Ident::new(String::from(T::ident())),
            VariantType::Custom(TypeId::of::<T>()),
        );
    }
}

pub struct Module<'a> {
    id: Id,
    data: &'a mut ProgramData,
}

impl<'a> Module<'a> {
    #[inline]
    fn module_mut(&mut self) -> &mut compilation::Module {
        self.data.modules.get_mut(&self.id).unwrap()
    }

    #[inline]
    pub fn register_fn<F: func::Func<A, R>, A, R>(&mut self, ident: impl Into<String>, func: F) {
        let id = self.data.funcs.next();

        self.module_mut().push_func(Func {
            id,
            ident: Ident::new(ident.into()),
            args: func.args(),
            return_type: func.return_type(),
        });

        let func = func.to_fn();
        self.data
            .funcs
            .insert(id, runtime::Func::Embedded(Arc::new(func)));
    }

    #[inline]
    pub fn register_class<C: Class>(&mut self) {
        let id = self.data.classes.next();

        self.module_mut()
            .push_mod(Ident::new(String::from(C::ident())), id);

        let class = compilation::Class {
            id,
            fields: C::fields(),
        };

        self.data.classes.insert(id, class);
    }

    #[inline]
    pub fn register_type<T: Type>(&mut self) -> RegisterType<'_, T> {
        RegisterType {
            _marker: Default::default(),
            module: self.id,
            data: self.data,
        }
    }

    #[inline]
    pub fn module(&mut self, ident: impl Into<String>) -> Module<'_> {
        let id = self.data.modules.push(compilation::Module::default());

        self.module_mut().push_mod(Ident::new(ident.into()), id);

        Module {
            id,
            data: &mut self.data,
        }
    }
}

pub struct Compiler {
    pub(crate) data: ProgramData,
    pub(crate) module: Id,
}

impl Compiler {
    #[inline]
    pub fn new() -> Self {
        let mut data = ProgramData::default();
        let module = data.modules.push(compilation::Module::default());

        Self { data, module }
    }

    #[inline]
    pub fn compile(&self, source: &str) -> CompileResult<Program> {
        let mut parser = Parser::new(source);
        let ast = program(&mut parser).unwrap();
        compile_program(self, &ast)
    }

    #[inline]
    pub fn add_std(&mut self) {
        add_std(self)
    }

    #[inline]
    fn module_mut(&mut self) -> &mut compilation::Module {
        self.data.modules.get_mut(&self.module).unwrap()
    }

    #[inline]
    pub fn register_fn<F: func::Func<A, R>, A, R>(&mut self, ident: impl Into<String>, func: F) {
        let id = self.data.funcs.next();

        self.module_mut().push_func(Func {
            id,
            ident: Ident::new(ident.into()),
            args: func.args(),
            return_type: func.return_type(),
        });

        let func = func.to_fn();
        self.data
            .funcs
            .insert(id, runtime::Func::Embedded(Arc::new(func)));
    }

    #[inline]
    pub fn register_class<C: Class>(&mut self) {
        let id = self.data.classes.next();

        self.module_mut()
            .push_mod(Ident::new(String::from(C::ident())), id);

        let class = compilation::Class {
            id,
            fields: C::fields(),
        };

        self.data.classes.insert(id, class);
    }

    #[inline]
    pub fn register_type<T: Type>(&mut self) -> RegisterType<'_, T> {
        RegisterType {
            _marker: Default::default(),
            module: self.module,
            data: &mut self.data,
        }
    }

    #[inline]
    pub fn register_method<S: AsVariant + 'static, A, R, M: Method<S, A, R>>(
        &mut self,
        ident: impl Into<String>,
        method: M,
    ) {
        let id = self.data.funcs.next();

        let mut ty = S::ty();

        if let VariantType::Ref(var) = ty {
            ty = *var;
        }

        self.data
            .methods
            .entry(ty)
            .or_default()
            .methods
            .push(compilation::Method {
                id,
                ident: Ident::new(ident.into()),
                self_arg: method.self_arg(),
                args: method.args(),
                return_type: method.return_type(),
            });

        self.data
            .funcs
            .insert(id, runtime::Func::Embedded(Arc::new(method.to_fn())))
    }

    #[inline]
    pub fn module(&mut self, ident: impl Into<String>) -> Module<'_> {
        let mut module = compilation::Module::default();
        module.persist = true;
        let id = self.data.modules.push(module);

        self.module_mut().push_mod(Ident::new(ident.into()), id);

        Module {
            id,
            data: &mut self.data,
        }
    }
}
