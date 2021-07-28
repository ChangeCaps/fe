use crate::{
    class::Var,
    variant::{Variant, VariantType},
};

pub trait AsVariant: Sized {
    fn ty() -> VariantType;

    fn from_variant_ref<O>(variant: &Variant, f: impl FnOnce(Option<&Self>) -> O) -> O;

    fn from_variant_mut<O>(variant: &mut Variant, f: impl FnOnce(Option<&mut Self>) -> O) -> O;

    fn from_variant(variant: Variant) -> Option<Self>;

    fn to_variant(self) -> Variant;
}

impl<T: AsVariant> AsVariant for Var<T> {
    fn ty() -> VariantType {
        VariantType::Ref(Box::new(T::ty()))
    }

    fn from_variant_ref<O>(variant: &Variant, f: impl FnOnce(Option<&Self>) -> O) -> O {
        if let Variant::Ref(ref_var) = variant {
            f(Some(&Var::new(ref_var.as_ref().clone())))
        } else {
            f(None)
        }
    }

    fn from_variant_mut<O>(variant: &mut Variant, f: impl FnOnce(Option<&mut Self>) -> O) -> O {
        if let Variant::Ref(ref_var) = variant {
            f(Some(&mut Var::new(ref_var.as_ref().clone())))
        } else {
            f(None)
        }
    }

    fn from_variant(variant: Variant) -> Option<Self> {
        if let Variant::Ref(ref_var) = variant {
            Some(Var::new(*ref_var))
        } else {
            None
        }
    }

    fn to_variant(self) -> Variant {
        Variant::Ref(Box::new(self.inner))
    }
}

macro_rules! impl_as_variant {
    ($ty:ty, $ident:ident) => {
        impl AsVariant for $ty {
            fn ty() -> VariantType {
                VariantType::$ident
            }

            fn from_variant_ref<O>(variant: &Variant, f: impl FnOnce(Option<&Self>) -> O) -> O {
                if let Variant::$ident(v) = variant {
                    f(Some(v))
                } else {
                    f(None)
                }
            }

            fn from_variant_mut<O>(
                variant: &mut Variant,
                f: impl FnOnce(Option<&mut Self>) -> O,
            ) -> O {
                if let Variant::$ident(v) = variant {
                    f(Some(v))
                } else {
                    f(None)
                }
            }

            fn from_variant(variant: Variant) -> Option<Self> {
                if let Variant::$ident(v) = variant {
                    Some(v)
                } else {
                    None
                }
            }

            fn to_variant(self) -> Variant {
                Variant::$ident(self)
            }
        }
    };
}

impl_as_variant!(i32, I32);
impl_as_variant!(f32, F32);
impl_as_variant!(bool, Bool);
impl_as_variant!(VariantType, Type);
impl_as_variant!(String, String);

impl AsVariant for () {
    fn ty() -> VariantType {
        VariantType::Unit
    }

    fn from_variant_ref<O>(_: &Variant, f: impl FnOnce(Option<&Self>) -> O) -> O {
        f(None)
    }

    fn from_variant_mut<O>(_: &mut Variant, f: impl FnOnce(Option<&mut Self>) -> O) -> O {
        f(None)
    }

    fn from_variant(variant: Variant) -> Option<Self> {
        if let Variant::Unit = variant {
            Some(())
        } else {
            None
        }
    }

    fn to_variant(self) -> Variant {
        Variant::Unit
    }
}

impl AsVariant for Variant {
    fn ty() -> VariantType {
        VariantType::Any
    }

    fn from_variant_ref<O>(variant: &Variant, f: impl FnOnce(Option<&Self>) -> O) -> O {
        f(Some(variant))
    }

    fn from_variant_mut<O>(variant: &mut Variant, f: impl FnOnce(Option<&mut Self>) -> O) -> O {
        f(Some(variant))
    }

    fn from_variant(variant: Variant) -> Option<Self> {
        Some(variant)
    }

    fn to_variant(self) -> Variant {
        self
    }
}
