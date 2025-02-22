use derivative::Derivative;
use derive_more::{Constructor, Deref};
use std::{
    rc::Rc,
    sync::atomic::{AtomicU64, Ordering},
};

#[derive(Constructor, Deref, Hash, PartialEq, Eq, Debug)]
pub struct RawIdentifier(String);

#[derive(Derivative, Debug)]
#[derivative(Hash, PartialEq, Eq)]
pub enum SymbolIdentifier {
    Exact(Rc<RawIdentifier>),
    Generated {
        id: UniqueId,

        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        descr: (),
        /* We might let `descr` member be an `Option<String>`, hence this identifier type is non-`Clone`. */
    },
}
impl SymbolIdentifier {
    pub fn new_generated() -> Self {
        Self::Generated { id: UniqueId::new(), descr: () }
    }
}
#[cfg(debug_assertions)]
impl PartialOrd for SymbolIdentifier {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
#[cfg(debug_assertions)]
impl Ord for SymbolIdentifier {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering as CmpOrd;

        match (self, other) {
            (Self::Exact(l_ident), Self::Exact(r_ident)) => {
                let l_str: &str = &l_ident.as_ref().0;
                let r_str: &str = &r_ident.as_ref().0;
                l_str.cmp(r_str)
            }
            (Self::Exact(_), Self::Generated { .. }) => CmpOrd::Less,
            (Self::Generated { .. }, Self::Exact(_)) => CmpOrd::Greater,
            (Self::Generated { id: l_id, .. }, Self::Generated { id: r_id, .. }) => {
                l_id.as_int().cmp(&r_id.as_int())
            }
        }
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
    pub fn create<const LEN: usize>(
        custom_id: Option<&UniqueId>,
        descr1: &'static str,
        descr2s: [&'static str; LEN],
    ) -> [Self; LEN] {
        let id = match custom_id {
            None => UniqueId::new(),
            Some(id) => id.privately_clone(),
        };

        descr2s.map(|descr2| Self {
            id: id.privately_clone(),
            descr1,
            descr2,
        })
    }
}

#[derive(Hash, PartialEq, Eq, Debug)]
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
