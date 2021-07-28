use std::any::{type_name, Any, TypeId};

use crate::{
    as_variant::AsVariant,
    variant::{Variant, VariantType},
};

#[derive(Clone, Debug)]
pub struct CustomType(Box<dyn Type>);

impl PartialEq for CustomType {
    fn eq(&self, other: &Self) -> bool {
        self.0.dyn_eq(other.0.as_ref())
    }
}

impl AsRef<dyn Type> for CustomType {
    fn as_ref(&self) -> &dyn Type {
        self.0.as_ref()
    }
}

impl std::ops::Deref for CustomType {
    type Target = dyn Type;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl std::ops::DerefMut for CustomType {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut()
    }
}

pub trait TypeExt: Any {
    fn type_name(&self) -> &'static str;

    fn box_clone(&self) -> Box<dyn Type>;

    fn any(&self) -> &dyn Any;

    fn any_mut(&mut self) -> &mut dyn Any;

    fn dyn_eq(&self, other: &dyn Type) -> bool;
}

impl<T: Any + Clone + Type + Sized> TypeExt for T {
    #[inline]
    fn type_name(&self) -> &'static str {
        type_name::<Self>()
    }

    #[inline]
    fn box_clone(&self) -> Box<dyn Type> {
        Box::new(self.clone())
    }

    #[inline]
    fn any(&self) -> &dyn Any {
        self
    }

    #[inline]
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }

    #[inline]
    fn dyn_eq(&self, other: &dyn Type) -> bool {
        if let Some(other) = other.any().downcast_ref() {
            self.eq(other)
        } else {
            false
        }
    }
}

pub trait Type: TypeExt {
    fn ident() -> &'static str
    where
        Self: Sized;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[custom:{{{}}}]", self.type_name())
    }

    fn eq(&self, _other: &Self) -> bool
    where
        Self: Sized,
    {
        false
    }
}

impl Clone for Box<dyn Type> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

impl std::fmt::Debug for Box<dyn Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name())
    }
}

impl<T: Type> AsVariant for T {
    #[inline]
    fn ty() -> VariantType {
        VariantType::Custom(TypeId::of::<Self>())
    }

    #[inline]
    fn from_variant_ref<O>(
        variant: &crate::variant::Variant,
        f: impl FnOnce(Option<&Self>) -> O,
    ) -> O {
        if let Variant::Custom(ty) = variant {
            f((**ty).any().downcast_ref())
        } else {
            f(None)
        }
    }

    #[inline]
    fn from_variant_mut<O>(
        variant: &mut crate::variant::Variant,
        f: impl FnOnce(Option<&mut Self>) -> O,
    ) -> O {
        if let Variant::Custom(ty) = variant {
            f((**ty).any_mut().downcast_mut())
        } else {
            f(None)
        }
    }

    #[inline]
    fn from_variant(variant: crate::variant::Variant) -> Option<Self> {
        if let Variant::Custom(ty) = variant {
            let b = ty.0;

            if b.as_ref().any().type_id() == TypeId::of::<Self>() {
                // SAFETY: we have just checked that types are equal, so casting is safe

                unsafe {
                    let raw: *mut dyn Type = Box::into_raw(b);
                    let t = std::ptr::read(raw as *const Self);
                    std::mem::forget(raw);
                    Some(t)
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    #[inline]
    fn to_variant(self) -> crate::variant::Variant {
        Variant::Custom(CustomType(Box::new(self)))
    }
}
