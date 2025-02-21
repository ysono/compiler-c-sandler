use super::*;
use std::sync::atomic::{AtomicUsize, Ordering};

/// LinkedList node.
///
/// Using the default `impl` for `Hash` and `PartialEq`.
#[derive(Hash, PartialEq, Eq, Debug)]
enum LLNode {
    Terminal(Datum),
    Recursive(Datum, Singleton<Self>),
}

/// Using the default `impl` for `Hash` and `PartialEq`.
#[derive(Hash, PartialEq, Eq, Debug)]
struct Datum {
    content: i32,
    stats: Rc<Stats>,
}
impl Datum {
    fn new(content: i32) -> (Self, Rc<Stats>) {
        let stats = Rc::new(Stats::default());
        let stats_ret = Rc::clone(&stats);
        let slf = Self { content, stats };
        (slf, stats_ret)
    }
}

#[derive(Default, Debug)]
struct Stats {
    hash_ct: AtomicUsize,
    eq_ct: AtomicUsize,
}
impl Hash for Stats {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash_ct.fetch_add(1, Ordering::SeqCst);

        /* Don't update the Hasher. */
        let _ = state;
    }
}
impl PartialEq<Self> for Stats {
    fn eq(&self, other: &Self) -> bool {
        self.eq_ct.fetch_add(1, Ordering::SeqCst);
        other.eq_ct.fetch_add(1, Ordering::SeqCst);

        /* No interference with the equality calculation. */
        true
    }
}
impl Eq for Stats {}
impl Stats {
    fn hash_ct_since_last(&self) -> usize {
        self.hash_ct.swap(0, Ordering::SeqCst)
    }
    fn eq_ct_since_last(&self) -> usize {
        self.eq_ct.swap(0, Ordering::SeqCst)
    }
}

#[derive(Debug)]
struct ExpectedMethodCalls {
    hash: bool,
    eq: bool,
}
fn assert_method_calls(stats: &Stats, exp: ExpectedMethodCalls) {
    let hash_ct = stats.hash_ct_since_last();
    let eq_ct = stats.eq_ct_since_last();
    let ok = ((hash_ct > 0) == exp.hash) && ((eq_ct > 0) == exp.eq);
    assert!(ok, "{:#?}", (hash_ct, eq_ct, exp));
}

#[test]
fn simple_elem() {
    const CONTENT: i32 = 123;
    let new_datum = || Datum::new(CONTENT);

    let mut repo = SingletonRepository::<Datum>::default();

    /*
    ; SingletonKey
    ;   |
    ; Singleton
    ;   |
    ; Datum         Datum
    ;   |             |
    ; Stats         Stats
    */

    {
        /* Construct an instance of `Datum`. */
        let (datum, input_stats) = new_datum();
        assert_method_calls(&input_stats, ExpectedMethodCalls { hash: false, eq: false });

        /* Expect new singleton. */
        assert_eq!(repo.repo.len(), 0);
        let singleton = repo.get_or_new(datum);
        assert_eq!(repo.repo.len(), 1);
        assert_eq!(Rc::ptr_eq(&singleton.stats, &input_stats), true);
        assert_method_calls(
            &input_stats,
            ExpectedMethodCalls {
                hash: true, // 1) `Datum::hash()` determines the bucket.
                eq: false,  // 2) No existing `SingletonKey::borrow()` to compare against.
            },
        );
    }

    {
        /* Construct a non-distinct instance of `Datum`. */
        let (datum, input_stats) = new_datum();
        assert_method_calls(&input_stats, ExpectedMethodCalls { hash: false, eq: false });

        /* Expect _no_ new singleton. */
        assert_eq!(repo.repo.len(), 1);
        let singleton = repo.get_or_new(datum);
        assert_eq!(repo.repo.len(), 1);
        assert_eq!(Rc::ptr_eq(&singleton.stats, &input_stats), false);
        assert_method_calls(
            &input_stats,
            ExpectedMethodCalls {
                hash: true, // 1) `Datum::hash()` determines the bucket.
                eq: true,   // 2) `Datum::eq()` between `SingletonKey::borrow()` and `&Datum`.
            },
        );
        assert_method_calls(
            &singleton.stats,
            ExpectedMethodCalls {
                hash: false, // Didn't calc `SingletonKey::hash()` on the existing `SingletonKey`.
                eq: true,    // 2) `Datum::eq()` between `SingletonKey::borrow()` and `&Datum`.
            },
        );
    }
}

#[test]
fn recursive_elem() {
    const CONTENT: i32 = 123;
    let new_terminal_node = || {
        let (datum, datum_stats) = Datum::new(CONTENT);
        let term_node = LLNode::Terminal(datum);
        (term_node, datum_stats)
    };
    let new_recursive_node = |nested_node: Singleton<LLNode>| {
        let (datum, datum_stats) = Datum::new(CONTENT);
        let rec_node = LLNode::Recursive(datum, nested_node);
        (rec_node, datum_stats)
    };

    let mut repo = SingletonRepository::<LLNode>::default();

    /*
    ; SingletonKey
    ;   |
    ; Singleton
    ;   |
    ; LLNode::Terminal      LLNode::Terminal
    ;   |                     |
    ; Datum                 Datum
    ;   |                     |
    ; Stats                 Stats
    */

    {
        /* Construct a `LLNode::Terminal`. */
        let (term_node, input_stats) = new_terminal_node();
        assert_method_calls(&input_stats, ExpectedMethodCalls { hash: false, eq: false });

        /* Expect new singleton. */
        assert_eq!(repo.repo.len(), 0);
        let singleton = repo.get_or_new(term_node);
        assert_eq!(repo.repo.len(), 1);
        match singleton.as_ref() {
            LLNode::Terminal(stored) => {
                assert_eq!(Rc::ptr_eq(&stored.stats, &input_stats), true);
                assert_method_calls(&input_stats, ExpectedMethodCalls { hash: true, eq: false });
                /* Same situation as the SingletonRepository<Datum> test case. */
            }
            LLNode::Recursive(..) => unreachable!(),
        }
    }

    let (stored_term, stored_term_stats);
    {
        /* Construct a non-distinct instance of `LLNode::Terminal`. */
        let (term_node, input_stats) = new_terminal_node();
        assert_method_calls(&input_stats, ExpectedMethodCalls { hash: false, eq: false });

        /* Expect _no_ new singleton. */
        assert_eq!(repo.repo.len(), 1);
        let singleton = repo.get_or_new(term_node);
        assert_eq!(repo.repo.len(), 1);
        match singleton.as_ref() {
            LLNode::Terminal(stored) => {
                assert_eq!(Rc::ptr_eq(&stored.stats, &input_stats), false);
                assert_method_calls(&input_stats, ExpectedMethodCalls { hash: true, eq: true });
                assert_method_calls(&stored.stats, ExpectedMethodCalls { hash: false, eq: true });
                /* Same situation as the SingletonRepository<Datum> test case. */

                stored_term_stats = stored.stats.clone();
            }
            LLNode::Recursive(..) => unreachable!(),
        }

        stored_term = singleton;
    };

    /*
    ; SingletonKey
    ;   |
    ; Singleton
    ;   |
    ; LLNode::Recursive   LLNode::Recursive
    ;   |         \         /         |
    ; Datum        Singleton        Datum
    ;   |             |               |
    ; Stats     LLNode::Terminal    Stats
    ;                 |
    ;               Datum
    ;                 |
    ;               Stats
    */

    {
        /* Construct a `LLNode::Recursive`. */
        let (rec_node, input_stats) = new_recursive_node(stored_term.clone());
        assert_method_calls(&input_stats, ExpectedMethodCalls { hash: false, eq: false });
        assert_method_calls(
            &stored_term_stats,
            ExpectedMethodCalls { hash: false, eq: false },
        );

        /* Expect new singleton. */
        assert_eq!(repo.repo.len(), 1);
        let singleton = repo.get_or_new(rec_node);
        assert_eq!(repo.repo.len(), 2);
        match singleton.as_ref() {
            LLNode::Terminal(..) => unreachable!(),
            LLNode::Recursive(stored_rec, _nested_node) => {
                assert_eq!(Rc::ptr_eq(&stored_rec.stats, &input_stats), true);
                assert_method_calls(
                    &input_stats,
                    ExpectedMethodCalls {
                        hash: true, // 1) `LLNode::hash()` determines the bucket.
                        eq: false, // 2) No existing `SingletonKey::borrow()` containing `LLNode::Recursive` to compare against.
                    },
                ); // Same situation as the SingletonRepository<Datum> test case.
                assert_method_calls(
                    &stored_term_stats,
                    ExpectedMethodCalls {
                        hash: false, // 1) Calc'ing `LLNode::hash()` entails calc'ing `Singleton::hash()`, which doesn't recurse into calc'ing `Datum::hash()`.
                        eq: false, // 2) No existing `SingletonKey::borrow()` containing `LLNode::Recursive` to compare against.
                    },
                ); // Newly pertinent in the SingletonRepository<LLNode> test case.
            }
        }
    }

    {
        /* Construct a non-distinct instance of `LLNode::Recursive`. */
        let (rec_node, input_stats) = new_recursive_node(stored_term.clone());
        assert_method_calls(&input_stats, ExpectedMethodCalls { hash: false, eq: false });
        assert_method_calls(
            &stored_term_stats,
            ExpectedMethodCalls { hash: false, eq: false },
        );

        /* Expect _no_ new singleton. */
        assert_eq!(repo.repo.len(), 2);
        let singleton = repo.get_or_new(rec_node);
        assert_eq!(repo.repo.len(), 2);
        match singleton.as_ref() {
            LLNode::Terminal(..) => unreachable!(),
            LLNode::Recursive(stored_rec, _nested_node) => {
                assert_eq!(Rc::ptr_eq(&stored_rec.stats, &input_stats), false);
                assert_method_calls(
                    &input_stats,
                    ExpectedMethodCalls {
                        hash: true, // 1) `LLNode::hash()` determines the bucket.
                        eq: true, // 2) `LLNode::eq()` between `SingletonKey::borrow()` and `&LLNode`.
                    },
                ); // Same situation as the SingletonRepository<Datum> test case.
                assert_method_calls(
                    &stored_rec.stats,
                    ExpectedMethodCalls {
                        hash: false, // Didn't calc `SingletonKey::hash()` on the existing `SingletonKey`.
                        eq: true, // 2) `LLNode::eq()` between `SingletonKey::borrow()` and `&LLNode`.
                    },
                ); // Same situation as the SingletonRepository<Datum> test case.
                assert_method_calls(
                    &stored_term_stats,
                    ExpectedMethodCalls {
                        hash: false, // 1) Calc'ing `LLNode::hash()` entails calc'ing `Singleton::hash()`, which doesn't recurse into calc'ing `Datum::hash()`.
                        eq: false,   // 2) Calc'ing `LLNode::eq()` ... ditto ... doesn't recurse.
                    },
                ); // Newly pertinent in the SingletonRepository<LLNode> test case.
            }
        }
    }
}
