mod data;
mod fun;
mod label;

use crate::{
    common::symbol_table_backend::BackendSymbolTable,
    ds_n_a::immutable_owned::ImmutableOwned,
    stage4_asm_gen::{asm_ast::*, FinalizedAsmAst},
};
use regex::Regex;
use std::io::{self, Write};

const TAB: &str = "\t";

pub struct AsmCodeEmitter<W> {
    label_bad_char: Regex,

    backend_symtab: ImmutableOwned<BackendSymbolTable>,

    w: W,
}
impl<W: Write> AsmCodeEmitter<W> {
    pub fn new(backend_symtab: ImmutableOwned<BackendSymbolTable>, w: W) -> Self {
        Self {
            label_bad_char: Regex::new(r"[^a-zA-Z0-9._]").unwrap(),
            backend_symtab,
            w,
        }
    }

    pub fn emit_program(
        mut self,
        Program { static_vars, static_consts, funs }: Program<FinalizedAsmAst>,
    ) -> Result<(), io::Error> {
        for static_var in static_vars {
            self.write_static_var(static_var)?;
        }

        for static_const in static_consts {
            self.write_static_const(static_const)?;
        }

        for fun in funs {
            self.write_fun(fun)?;
        }

        if cfg!(target_os = "linux") {
            writeln!(&mut self.w, "{TAB}.section	.note.GNU-stack,\"\",@progbits")?;
        }

        self.w.flush()?;
        Ok(())
    }
}
