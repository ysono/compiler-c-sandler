macro_rules! fail {
    ( $($arg:tt),* ) => {
        panic!($($arg)*)
    };
}
pub(crate) use fail;
