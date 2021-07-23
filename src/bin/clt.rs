use fe::{
    compiler::compile_program,
    error::error_message,
    parser::{program, Parser},
    runtime::Runtime,
};
use std::fs::read_to_string;

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

    let program = match compile_program(&program) {
        Ok(p) => p,
        Err(e) => {
            println!("{}", error_message(&source, e.span.clone(), &e.msg()));

            return Ok(());
        }
    };

    println!("{:#?}", program);

    let mut runtime = Runtime::new(&program);

    program.eval(&mut runtime).unwrap();

    runtime.stack.print();

    Ok(())
}
