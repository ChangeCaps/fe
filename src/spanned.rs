#[derive(Clone, Debug)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    #[inline]
    pub fn new(lo: usize, hi: usize) -> Self {
        Span { lo, hi }
    }
}

impl std::ops::Add for Span {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            lo: self.lo.min(rhs.lo),
            hi: self.hi.max(rhs.hi),
        }
    }
}

impl std::ops::AddAssign for Span {
    #[inline]
    fn add_assign(&mut self, rhs: Self) {
        self.lo = self.lo.min(rhs.lo);
        self.hi = self.hi.max(rhs.hi);
    }
}

#[derive(Clone)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    #[inline]
    pub fn new(inner: T, span: Span) -> Self {
        Self { inner, span }
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        self.inner.fmt(f)
    }
}
