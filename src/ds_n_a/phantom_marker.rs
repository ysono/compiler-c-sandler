use core::fmt::{self, Debug, Formatter};
use core::marker::PhantomData;

/// This type helps ensure the presence of an instance of `T`, for sanity-check purposes.
/// The struct takes no space at runtime.
pub struct PhantomMarker<T>(PhantomData<T>);
impl<T> PhantomMarker<T> {
    pub fn new(_: &T) -> Self {
        Self(PhantomData)
    }
}
impl<T> Clone for PhantomMarker<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for PhantomMarker<T> {}
impl<T> Debug for PhantomMarker<T> {
    fn fmt(&self, _: &mut Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}
