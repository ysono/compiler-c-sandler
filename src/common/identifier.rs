use derivative::Derivative;
use derive_more::{Constructor, Deref};
use std::borrow::Borrow;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Constructor, Deref, PartialEq, Eq, Hash, Debug)]
pub struct RawIdentifier(String);

#[derive(Derivative, Debug)]
#[derivative(PartialEq, Eq, Hash)]
pub enum SymbolIdentifier {
    Exact(Rc<RawIdentifier>),
    Generated {
        id: UniqueId,

        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        descr: (),
        /* We might let `descr` member be an `Option<String>`, hence this identifier type is non-`Clone`. */
    },
}
impl SymbolIdentifier {
    pub fn new_generated() -> Self {
        Self::Generated { id: UniqueId::new(), descr: () }
    }
}

#[derive(Debug)]
pub struct JumpLabel {
    pub id: UniqueId,
    pub descr1: &'static str,
    pub descr2: &'static str,
}
impl JumpLabel {
    /// @arg `descr2s` elements must be all distinct. (We ought to `static_assert` this.)
    pub fn create<Uid, const LEN: usize, Out>(
        id: Uid,
        descr1: &'static str,
        descr2s: [&'static str; LEN],
    ) -> [Out; LEN]
    where
        Uid: Borrow<UniqueId>,
        Out: From<Self> + std::fmt::Debug,
    {
        descr2s
            .into_iter()
            .map(|descr2| {
                Self {
                    id: id.borrow().privately_clone(),
                    descr1,
                    descr2,
                }
                .into()
            })
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct UniqueId(u64);
impl UniqueId {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static NEXT_ID: AtomicU64 = AtomicU64::new(0);
        let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
        Self(curr_id)
    }
    fn privately_clone(&self) -> Self {
        Self(self.0)
    }
    pub fn as_int(&self) -> u64 {
        self.0
    }
}
