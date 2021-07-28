use std::time::{Duration, Instant};

use crate::{
    class::Var,
    compiler::{Compiler, Module},
    ty::Type,
};

impl Type for Instant {
    fn ident() -> &'static str
    where
        Self: Sized,
    {
        "Instant"
    }

    fn eq(&self, other: &Self) -> bool
    where
        Self: Sized,
    {
        self == other
    }
}

impl Type for Duration {
    fn ident() -> &'static str
    where
        Self: Sized,
    {
        "Duration"
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }

    fn eq(&self, other: &Self) -> bool
    where
        Self: Sized,
    {
        self == other
    }
}

pub fn add_std(compiler: &mut Compiler) {
    compiler.register_method("sub", |s: Var<String>, start: i32, end: i32| {
        s.map(|s| String::from(&s[start as usize..end as usize]))
    });

    add_i32_methods(compiler);
    add_f32_methods(compiler);

    let mut module = compiler.module("std");

    add_time(&mut module);
    add_i32(&mut module);
    add_f32(&mut module);
}

pub fn add_time(module: &mut Module<'_>) {
    let mut module = module.module("time");

    module
        .register_type::<Instant>()
        .register_method("duration_since", |x: Instant, y: Instant| {
            x.duration_since(y)
        });
    module.register_type::<Duration>();

    module.register_fn("now", || Instant::now());
}

pub fn add_i32_methods(compiler: &mut Compiler) {
    compiler.register_method("min", |x: i32, y: i32| x.min(y));
    compiler.register_method("max", |x: i32, y: i32| x.max(y));
}

pub fn add_f32_methods(compiler: &mut Compiler) {
    compiler.register_method("min", |x: f32, y: f32| x.min(y));
    compiler.register_method("max", |x: f32, y: f32| x.max(y));
}

pub fn add_i32(module: &mut Module<'_>) {
    let mut module = module.module("i32");

    module.register_fn("min", |x: i32, y: i32| x.min(y));
    module.register_fn("max", |x: i32, y: i32| x.max(y));
}

pub fn add_f32(module: &mut Module<'_>) {
    let mut module = module.module("f32");

    module.register_fn("min", |x: f32, y: f32| x.min(y));
    module.register_fn("max", |x: f32, y: f32| x.max(y));
}
