use crate::{
    as_variant::AsVariant,
    compilation::{Field, Id},
    program::ClassInstance,
    variant::{Ref, Variant, VariantType},
};

pub struct Var<V> {
    pub inner: Ref,
    _marker: std::marker::PhantomData<V>,
}

impl<V: AsVariant> From<V> for Var<V> {
    #[inline]
    fn from(v: V) -> Self {
        Self {
            inner: Ref::Owned(v.to_variant()),
            _marker: Default::default(),
        }
    }
}

impl<V: AsVariant> Var<V> {
    #[inline]
    pub fn new(inner: Ref) -> Self {
        Self {
            inner,
            _marker: Default::default(),
        }
    }

    #[inline]
    pub fn ty() -> VariantType {
        V::ty()
    }

    #[inline]
    pub fn map<O>(&self, f: impl FnOnce(&V) -> O) -> O {
        self.inner
            .map(|var| V::from_variant_ref(var, |v| f(v.unwrap())))
    }

    #[inline]
    pub fn map_mut<O>(&mut self, f: impl FnOnce(&mut V) -> O) -> O {
        self.inner
            .map_mut(|var| V::from_variant_mut(var, |v| f(v.unwrap())))
    }
}

pub trait Class {
    fn ident() -> &'static str
    where
        Self: Sized;

    fn fields() -> Vec<Field>
    where
        Self: Sized;

    fn field_mut(&mut self, idx: usize) -> &mut Ref;

    fn to_variant(self) -> Variant;

    fn instance(self, id: Id) -> ClassInstance
    where
        Self: Sized,
    {
        ClassInstance {
            id,
            variant: Ref::Owned(self.to_variant()),
        }
    }
}
