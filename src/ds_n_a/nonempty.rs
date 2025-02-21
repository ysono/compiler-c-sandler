//! Note, in a different library, [`NonEmpty::from_vec()`](https://docs.rs/nonempty/0.10.0/nonempty/struct.NonEmpty.html#method.from_vec) calls `Vec::remove(0)`, which is O(n).

use core::{fmt::Debug, ops::Deref};

pub struct NonEmpty<T> {
    vec: Vec<T>,
}
impl<T> NonEmpty<T> {
    pub fn new(elem: T) -> Self {
        Self { vec: vec![elem] }
    }
    pub fn from_vec(vec: Vec<T>) -> Option<Self> {
        if vec.is_empty() {
            None
        } else {
            Some(Self { vec })
        }
    }

    pub fn first(&self) -> &T {
        &self.vec[0]
    }

    pub fn ref_map<F, U>(&self, f: F) -> NonEmpty<U>
    where
        F: Fn(&T) -> U,
    {
        let vec = self.vec.iter().map(f).collect::<Vec<_>>();
        NonEmpty { vec }
    }
}
impl<T> Deref for NonEmpty<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}
impl<T> IntoIterator for NonEmpty<T> {
    type Item = T;
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.vec.into_iter()
    }
}
impl<T: Debug> Debug for NonEmpty<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NonEmpty").field("vec", &self.vec).finish()
    }
}
