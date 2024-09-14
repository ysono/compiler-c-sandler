#![allow(rustdoc::private_intra_doc_links)]

mod common;
pub mod driver;
mod stage1_lex;
mod stage2_parse;
mod stage3_tacky;
mod stage4_asm_gen;
mod stage5_asm_emit;
