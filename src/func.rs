use crate::as_variant::AsVariant;
use crate::class::Var;
use crate::compilation::SelfArg;
use crate::variant::{Variant, VariantType};

/// Used to convert from rust function to fe function.
pub trait Func<A, R> {
    fn args(&self) -> Vec<VariantType>;

    fn return_type(&self) -> VariantType;

    fn to_fn(self) -> Box<dyn Fn(Vec<Variant>) -> Option<Variant>>;
}

pub trait SelfArgOf<T>: AsVariant {}

impl<T: AsVariant> SelfArgOf<T> for T {}
impl<T: AsVariant> SelfArgOf<T> for Var<T> {}

/// Used to convert from rust function to fe method.
pub trait Method<S: AsVariant, A, R> {
    fn self_arg(&self) -> SelfArg {
        match S::ty() {
            VariantType::Ref(_) => SelfArg::Ref,
            _ => SelfArg::Owned,
        }
    }

    fn args(&self) -> Vec<VariantType>;

    fn return_type(&self) -> VariantType;

    fn to_fn(self) -> Box<dyn Fn(Vec<Variant>) -> Option<Variant>>;
}

/// Used to convert from fe function to rust function.
pub trait FromFunc<'a>: Sized {
    fn args() -> Vec<VariantType>;

    fn return_type() -> VariantType;

    fn from_fn<F: Fn(Vec<Variant>) -> Variant + 'a>(f: F) -> Self;
}

macro_rules! impl_func {
    ($($arg:ident),*) => {
		#[allow(unused_parens)]
		impl<$($arg: AsVariant,)* R: AsVariant, Fun: Fn($($arg),*) -> R + 'static> Func<($($arg,)*), R> for Fun {
			fn args(&self) -> Vec<VariantType> {
				vec![$($arg::ty()),*]
			}

			fn return_type(&self) -> VariantType {
				R::ty()
			}

			#[allow(non_snake_case)]
			fn to_fn(self) -> Box<dyn Fn(Vec<Variant>) -> Option<Variant>> {
				Box::new(move |args| {
					#[allow(unused)]
					let mut args = args.into_iter();

					$(
						let $arg = $arg::from_variant(args.next()?)?;
					)*

					Some(self($($arg),*).to_variant())
				})
			}
		}

		#[allow(unused_parens)]
		impl<S: AsVariant, $($arg: AsVariant,)* R: AsVariant, Fun: Fn(S, $($arg),*) -> R + 'static>
			Method<S, ($($arg,)*), R> for Fun
		{
			fn args(&self) -> Vec<VariantType> {
				vec![$($arg::ty()),*]
			}

			fn return_type(&self) -> VariantType {
				R::ty()
			}

			#[allow(non_snake_case)]
			fn to_fn(self) -> Box<dyn Fn(Vec<Variant>) -> Option<Variant>> {
				Box::new(move |args| {
					#[allow(unused)]
					let mut args = args.into_iter();

					let _self = S::from_variant(args.next()?)?;

					$(
						let $arg = $arg::from_variant(args.next()?)?;
					)*

					Some(self(_self, $($arg),*).to_variant())
				})
			}
		}

		#[allow(unused_parens, non_snake_case)]
		impl<'a, $($arg: AsVariant,)* R: AsVariant> FromFunc<'a> for Box<dyn Fn($($arg),*) -> R + 'a> {
			fn args() -> Vec<VariantType> {
				vec![$($arg::ty()),*]
			}

			fn return_type() -> VariantType {
				R::ty()
			}

			fn from_fn<Func: Fn(Vec<Variant>) -> Variant + 'a>(f: Func) -> Self {
				Box::new(move |$($arg: $arg),*| {
					R::from_variant(f(vec![$($arg.to_variant()),*])).unwrap()
				})
			}
		}
	};
}

impl_func!();
impl_func!(A);
impl_func!(A, B);
impl_func!(A, B, C);
impl_func!(A, B, C, D);
impl_func!(A, B, C, D, E);
impl_func!(A, B, C, D, E, F);
impl_func!(A, B, C, D, E, F, G);
impl_func!(A, B, C, D, E, F, G, H);
impl_func!(A, B, C, D, E, F, G, H, I);
impl_func!(A, B, C, D, E, F, G, H, I, J);
