#![doc = include_str!("./functions.md")]

pub mod asm_ast;
mod generate;
mod phase1_generate;
mod phase2_finalize;
mod phase3_fix;

pub use generate::AsmCodeGenerator;
pub use phase2_finalize::FinalizedAsmAst;
