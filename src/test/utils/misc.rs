use std::hash::{DefaultHasher, Hash, Hasher};

pub fn hash<T: Hash>(t: T) -> u64 {
    let mut hasher = DefaultHasher::default();
    t.hash(&mut hasher);
    hasher.finish()
}

macro_rules! fail {
    ( $($arg:tt),* ) => {
        panic!($($arg)*)
    };
}
pub(crate) use fail;
