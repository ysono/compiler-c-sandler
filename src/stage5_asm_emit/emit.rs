mod data;
mod fun;
mod label;

use crate::{
    common::symbol_table_backend::BackendSymbolTable,
    ds_n_a::immutable_owned::ImmutableOwned,
    stage4_asm_gen::{asm_ast::*, FinalizedAsmAst},
};
use regex::Regex;
use std::{
    io::{self, Write},
    rc::Rc,
};

const TAB: &str = "\t";

pub struct AsmCodeEmitter<W> {
    label_bad_char: Regex,

    /// We use Rc in order to work around rust's mutability checks.
    /// Note, `Rc` alone makes the inner type immutable, but we choose to be explicit with [`ImmutableOwned`].
    backend_symtab: Rc<ImmutableOwned<BackendSymbolTable>>,

    w: W,
}
impl<W: Write> AsmCodeEmitter<W> {
    pub fn new(backend_symtab: ImmutableOwned<BackendSymbolTable>, w: W) -> Self {
        Self {
            label_bad_char: Regex::new(r"[^a-zA-Z0-9._]").unwrap(),
            backend_symtab: Rc::new(backend_symtab),
            w,
        }
    }

    pub fn emit_program(
        mut self,
        Program { funs }: Program<FinalizedAsmAst>,
    ) -> Result<(), io::Error> {
        self.write_static_objs()?;

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
