use core::{
    fmt::{self, Debug, Formatter},
    marker::PhantomData,
};

/// This type informs that an instance of the given type was present at some time in the past.
pub struct Witness<T>(PhantomData<T>);
impl<T> Witness<T> {
    pub fn new(_: &T) -> Self {
        Self(PhantomData)
    }
}
impl<T> Clone for Witness<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Witness<T> {}
impl<T> Debug for Witness<T> {
    fn fmt(&self, _: &mut Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}
