#![allow(rustdoc::private_intra_doc_links)]

mod common;
pub mod driver;
mod ds_n_a;
mod stage1_lex;
mod stage2_parse;
mod stage3_tacky;
mod stage4_asm_gen;
mod stage5_asm_emit;

mod utils {
    macro_rules! noop {
        ($($comment:literal)?) => {{}}; // Empty expression
    }

    pub(crate) use noop;
}
