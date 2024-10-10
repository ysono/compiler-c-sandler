#[cfg(test)]
mod test;

use core::borrow::Borrow;
use core::hash::{Hash, Hasher};
use core::ops::Deref;
use owning_ref::{CloneStableAddress, StableAddress};
use std::collections::HashSet;
use std::rc::Rc;

/// This container stores distinct instances of `T`.
///
/// "Singleton" refers to each of these instances of `T`; there may be multiple instances of `T`.
pub struct SingletonRepository<T> {
    repo: HashSet<SingletonKey<T>>,
}
impl<T> Default for SingletonRepository<T> {
    /// [`SingletonRepository`] is `Default` regardless of whether `T` is `Default`.
    fn default() -> Self {
        Self { repo: HashSet::default() }
    }
}
impl<T: Hash + Eq> SingletonRepository<T> {
    pub fn get_or_new(&mut self, elem: T) -> Singleton<T> {
        /*
        1) Calculate `elem.hash()`: `T::hash()`. The digest determines the bucket.
        2) In the bucket, for each existing_key, evaluate `existing_key.borrow() == &elem`: `SingletonKey::borrow() == &T`.
        In the special case where `T` recursively contains a `Singleton<U>` member,
            `T::hash()` and `T::eq()` both ought to calculate non-recursively.
        */
        let existing_key = self.repo.get(&elem);

        match existing_key {
            None => {
                let singleton = Singleton { rc: Rc::new(elem) };
                let singleton_ret = singleton.clone();
                let new_key = SingletonKey { singleton };

                /*
                1) Calculate `new_key.hash()`: `SingletonKey::hash()`, which in turn calculates `T::hash()`. The digest determines the bucket.
                2) In the bucket, for each existing_key, evaluate `&existing_key == &new_key`: `&SingletonKey == &SingletonKey`.
                    Each existing key is expected to be non-equal to the new key.
                */
                let did_insert = self.repo.insert(new_key);
                debug_assert!(did_insert);

                singleton_ret
            }
            Some(SingletonKey { singleton: node }) => node.clone(),
        }
    }
}

/// The key type of a hash table that's queryable by `T`.
///
/// In order to enable querying a hash table by `T`,
/// + [`SingletonKey`] is `impl Borrow<T>`.
/// + [`SingletonKey::hash()`] and `T::hash()` derive the same digest.
///
/// This type is constructable within this mod only.
/// [`SingletonRepository`] constructs exactly one instance of [`SingletonKey`] per distinct `T`.
/// In addition, [`SingletonKey`] is non-`Clone`.
/// Hence, all instances of [`SingletonKey`] are non-equal to one another.
///
/// It is possibly also correct for [`SingletonKey::eq()`] to return `false` statically.
/// [`SingletonKey::eq()`] must compare by pointer value if any of:
/// + An instance is compared against itself during some (unknown by us) internal mechanism of `HashMap`.
/// + If [`SingletonKey`] were `Clone`. -- We might make it `Clone` if we choose to make this type public.
///
/// Caveat: [`SingletonKey::eq()`] works as expected solely on two instances that were constructed by the same [`SingletonRepository`].
#[derive(Debug)]
struct SingletonKey<T> {
    singleton: Singleton<T>,
}
impl<T> Borrow<T> for SingletonKey<T> {
    fn borrow(&self) -> &T {
        self.singleton.rc.as_ref()
    }
}
impl<T: Hash> Hash for SingletonKey<T> {
    /// (This `impl` is the same as `#[derive(Hash)]`; we choose to be explicit.)
    fn hash<H: Hasher>(&self, state: &mut H) {
        let t: &T = self.singleton.rc.as_ref();
        t.hash(state);
    }
}
impl<T> PartialEq<Self> for SingletonKey<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.singleton.rc, &other.singleton.rc)
    }
}
impl<T> Eq for SingletonKey<T> {}

/// A shared pointer to a uniquely-constructed distinct instance of `T`.
///
/// This type is constructable within this mod only.
/// [`SingletonRepository`] constructs exactly one instance of [`Singleton`] per distinct `T`.
/// In addition, [`Singleton`] is `Clone`.
/// Hence, the equality between 2 instances of [`Singleton`] is determined exactly by the equality of the pointer values.
///
/// Both [`Singleton::hash()`] and [`Singleton::eq()`] evaluate the pointer value, and not the content of `T`, so that
///     in case `T` recursively contains [`Singleton<U>`] member(s),
///     the default `T::hash()` and `T::eq()`, as provided by `#[derive(Hash, PartialEq)] struct T{...}`,
///     calculate non-recursively, without evaluating nested `U`(s).
///
/// [`Singleton::hash()`] differs from `T::hash()`.
/// In order to prevent [`Singleton`] from being used as the key type of a hash table that's queryable by `T`,
/// [`Singleton`] is _not_ `impl Borrow<T>`.
///
/// Caveat: [`Singleton::eq()`] works as expected solely on two instances that were constructed by the same [`SingletonRepository`].
#[derive(Debug)]
pub struct Singleton<T> {
    rc: Rc<T>,
}
impl<T> Clone for Singleton<T> {
    /// [`Singleton`] is `Clone` regardless of whether `T` is `Clone`.
    fn clone(&self) -> Self {
        Self { rc: Rc::clone(&self.rc) }
    }
}
impl<T> AsRef<T> for Singleton<T> {
    fn as_ref(&self) -> &T {
        self.rc.as_ref()
    }
}
impl<T> Deref for Singleton<T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.rc.as_ref()
    }
}
impl<T: Hash> Hash for Singleton<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.rc);
        ptr.hash(state);
    }
}
impl<T> PartialEq<Self> for Singleton<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
    }
}
impl<T> Eq for Singleton<T> {}
unsafe impl<T> StableAddress for Singleton<T> {}
unsafe impl<T> CloneStableAddress for Singleton<T> {}
