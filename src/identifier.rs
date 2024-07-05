use crate::stage1_lex::tokens::Identifier;
use derivative::Derivative;
use std::rc::Rc;
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Derivative, Debug)]
#[derivative(PartialEq, Eq, Hash)]
pub enum UniqueIdentifier {
    Exact(Rc<Identifier>),
    Generated {
        id: IdentifierId,

        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        descr: Option<String>,
    },
}
impl UniqueIdentifier {
    pub fn new_generated(descr: Option<String>) -> Self {
        Self::Generated { id: IdentifierId::new(), descr }
    }

    pub fn id(&self) -> Option<&IdentifierId> {
        match self {
            Self::Generated { id, .. } => Some(id),
            Self::Exact(..) => None,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct IdentifierId(u64);
impl IdentifierId {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static NEXT_ID: AtomicU64 = AtomicU64::new(0);
        let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
        Self(curr_id)
    }
    pub fn as_int(&self) -> u64 {
        self.0
    }
}
