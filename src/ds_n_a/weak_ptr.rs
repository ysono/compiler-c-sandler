use core::{
    borrow::Borrow,
    hash::{Hash, Hasher},
};

/// An unsafe weak pointer to an arbitrary object.
///
/// This type differs from [`std::rc::Weak`] by exposing unsafe operations that involve dereferencing the pointer.
///
/// Operations that satisfy traits ([`Borrow`], [`Hash`], [`PartialEq`]) are _not_ marked as `unsafe`, but in fact they are all `unsafe`!
#[derive(Debug)]
pub struct WeakPtr<T>(*const T);
impl<T> WeakPtr<T> {
    pub fn new(t: &T) -> Self {
        Self(t as *const _)
    }
}
impl<T> Borrow<T> for WeakPtr<T> {
    fn borrow(&self) -> &T {
        unsafe { &*(self.0) }
    }
}
impl<T: Hash> Hash for WeakPtr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let r = unsafe { &*self.0 };
        r.hash(state);
    }
}
impl<T: PartialEq> PartialEq for WeakPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        let slf_r = unsafe { &*self.0 };
        let oth_r = unsafe { &*other.0 };
        slf_r == oth_r
    }
}
impl<T: Eq> Eq for WeakPtr<T> {}
