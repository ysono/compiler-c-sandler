#![allow(rustdoc::private_intra_doc_links)]

mod common;
mod driver;
mod ds_n_a;
mod stage1_lex;
mod stage2_parse;
mod stage3_tacky;
mod stage4_asm_gen;
mod stage5_asm_emit;

pub use driver::{Driver, config::CliArgs};

mod utils {
    macro_rules! noop {
        ( $($comment:tt),* ) => {{} /* Empty expression */};
    }
    pub(crate) use noop;
}

#[cfg(test)]
mod test;
