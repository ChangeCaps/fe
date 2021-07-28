//! A scripting language written in pure rust with no dependencies.
//! # Sample code
//! ```fe
//! class Foo {
//! 	// this is a field
//! 	bar: i32,
//!
//!		// this is a method
//! 	fn bar(&self) -> f32 {
//!			self.bar
//! 	}
//! }
//!
//! let b = Bar { bar: 0, };
//! println(b.bar());
//! ```
//!
//! # Example
//! ```rust
//! # use fe::prelude::*;
//! let code = r#"
//!		let x = 2;
//! 	let y = x * 3 + 2;
//! "#;
//!
//! let compiler = Compiler::new();
//! let program = compiler.compile(code).unwrap();
//! program.run().unwrap();
//! ```

pub mod add_std;
pub mod as_variant;
pub mod ast;
pub mod class;
pub mod compilation;
pub mod compiler;
pub mod error;
pub mod format;
pub mod func;
pub mod parser;
pub mod program;
pub mod runtime;
pub mod spanned;
pub mod ty;
pub mod variant;

pub mod prelude {
    pub use crate::class::{Class, Var};
    pub use crate::compiler::Compiler;
    pub use crate::program::Program;

    #[cfg(feature = "derive")]
    pub use fe_derive::Class;
}
