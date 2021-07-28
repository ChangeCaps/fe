use fe::{
    add_std::add_std,
    compilation::compile_program,
    compiler::Compiler,
    error::error_message,
    parser::{program, Parser},
    prelude::*,
    variant::Variant,
};
use std::fs::read_to_string;

#[derive(Class)]
struct Range {
    start: Var<i32>,
    end: Var<i32>,
}

fn main() -> Result<(), std::io::Error> {
    let source = read_to_string("source.fe")?;

    let mut parser = Parser::new(&source);
    let program = match program(&mut parser) {
        Ok(p) => p,
        Err(e) => {
            println!("{}", error_message(&source, e.span.clone(), &e.msg()));

            return Ok(());
        }
    };

    let mut compiler = Compiler::new();

    compiler.register_class::<Range>();

    add_std(&mut compiler);

    compiler.register_fn("println", |v: Variant| println!("{}", v));

    let program = match compile_program(&compiler, &program) {
        Ok(p) => p,
        Err(e) => {
            println!("{}", error_message(&source, e.span.clone(), &e.msg()));

            return Ok(());
        }
    };

    program.run().unwrap();

    Ok(())
}
