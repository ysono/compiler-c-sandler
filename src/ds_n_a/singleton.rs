use derive_more::Deref;
use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;
use std::rc::Rc;

#[derive(Deref, PartialEq, Eq, Hash, Debug)]
pub struct Singleton<T>(Rc<T>);
impl<T> Clone for Singleton<T> {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}
impl<T> Borrow<T> for Singleton<T> {
    fn borrow(&self) -> &T {
        &self.0
    }
}

pub struct SingletonRepository<T> {
    repo: HashSet<Singleton<T>>,
}
impl<T> Default for SingletonRepository<T> {
    fn default() -> Self {
        Self { repo: Default::default() }
    }
}
impl<T> SingletonRepository<T>
where
    T: Hash + Eq,
{
    pub fn get_or_new(&mut self, elem: T) -> Singleton<T> {
        match self.repo.get(&elem) {
            None => {
                let elem = Singleton(Rc::new(elem));
                self.repo.insert(elem.clone());
                elem
            }
            Some(singleton) => singleton.clone(),
        }
    }
}
