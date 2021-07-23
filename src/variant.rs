use std::{cell::RefCell, rc::Rc};

#[derive(Clone, Debug)]
pub enum Ref {
    Owned(Variant),
    Ref(Rc<RefCell<Variant>>),
}

impl PartialEq for Ref {
    fn eq(&self, other: &Self) -> bool {
        self.map(|lhs| other.map(|rhs| lhs == rhs))
    }
}

impl Ref {
    #[inline]
    pub fn cloned(&self) -> Variant {
        match self {
            Self::Owned(variant) => variant.clone(),
            Self::Ref(ref_variant) => ref_variant.borrow().clone(),
        }
    }

    #[inline]
    pub fn map<O>(&self, f: impl FnOnce(&Variant) -> O) -> O {
        match self {
            Self::Owned(variant) => f(variant),
            Self::Ref(ref_variant) => f(&ref_variant.borrow()),
        }
    }

    #[inline]
    pub fn map_mut<O>(&mut self, f: impl FnOnce(&mut Variant) -> O) -> O {
        match self {
            Self::Owned(variant) => f(variant),
            Self::Ref(ref_variant) => f(&mut ref_variant.borrow_mut()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Variant {
    I32(i32),
    F32(f32),
    Bool(bool),
    Ref(Box<Ref>),
    Type(VariantType),
    Func(usize),
    Unit,
}

impl std::fmt::Display for Variant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32(v) => write!(f, "{:?}", v),
            Self::F32(v) => write!(f, "{:?}", v),
            Self::Bool(v) => write!(f, "{}", v),
            Self::Ref(v) => write!(f, "&{:?}", v),
            Self::Type(v) => write!(f, "<{}>", v),
            Self::Func(v) => write!(f, "[func:{}]", v),
            Self::Unit => write!(f, "()"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariantType {
    I32,
    F32,
    Bool,
    Ref(Box<VariantType>),
    Type,
    Func(Vec<VariantType>, Box<VariantType>),
    Class(usize),
    Unit,
}

impl std::fmt::Display for VariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "i32"),
            Self::F32 => write!(f, "f32"),
            Self::Bool => write!(f, "bool"),
            Self::Ref(ty) => write!(f, "&{}", ty),
            Self::Type => write!(f, "type"),
            Self::Func(args, return_type) => {
                write!(f, "fn(")?;

                for (i, ty) in args.iter().enumerate() {
                    if i < args.len() {
                        write!(f, "{}, ", ty)?;
                    } else {
                        write!(f, "{}", ty)?;
                    }
                }

                write!(f, ") -> {}", return_type)
            }
            Self::Class(idx) => write!(f, "[class:{}]", idx),
            Self::Unit => write!(f, "()"),
        }
    }
}
