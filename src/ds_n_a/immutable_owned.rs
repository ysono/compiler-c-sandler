/// A wrapper type that exposes immutable accesses to the inner type.
///
/// Use case: marking a struct member as immutable while other members are mutable.
pub struct ImmutableOwned<T>(T);
impl<T> From<T> for ImmutableOwned<T> {
    fn from(t: T) -> Self {
        Self(t)
    }
}
impl<T> ImmutableOwned<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}
impl<T> core::ops::Deref for ImmutableOwned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
