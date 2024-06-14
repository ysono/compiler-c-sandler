#![allow(rustdoc::private_intra_doc_links)]

pub mod driver;
pub mod files;
pub mod stage1_lex;
pub mod stage2_parse;
pub mod stage3_tacky;
pub mod stage4_asm_gen;
pub mod stage5_asm_emit;
pub mod symbol_table;
