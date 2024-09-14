use super::{AsmCodeEmitter, TAB};
use crate::common::{
    identifier::{JumpLabel, SymbolIdentifier},
    symbol_table_frontend::StaticVisibility,
};
use std::io::{self, Write};

const IDENT_PFX_NONLOCAL: &str = if cfg!(target_os = "macos") { "_" } else { "" };
const IDENT_PFX_LOCAL: &str = if cfg!(target_os = "macos") {
    "L."
} else {
    ".L."
};

impl<W: Write> AsmCodeEmitter<W> {
    pub(super) fn write_symbol_visibility(
        &mut self,
        ident: &SymbolIdentifier,
        visibility: StaticVisibility,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        match visibility {
            StaticVisibility::Global => {
                write!(&mut self.w, "{TAB}.globl{TAB}")?;
                self.write_symbol_name(ident, locality)?;
                writeln!(&mut self.w)?;
            }
            StaticVisibility::NonGlobal => { /* No-op. */ }
        }
        Ok(())
    }
    pub(super) fn write_symbol_decl(
        &mut self,
        ident: &SymbolIdentifier,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        self.write_symbol_name(ident, locality)?;
        writeln!(&mut self.w, ":")?;
        Ok(())
    }
    pub(super) fn write_symbol_name(
        &mut self,
        ident: &SymbolIdentifier,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        let pfx = match locality {
            LabelLocality::InObjSymTab => IDENT_PFX_NONLOCAL,
            LabelLocality::Local => IDENT_PFX_LOCAL,
        };
        match ident {
            SymbolIdentifier::Exact(ident) => {
                let name = ident as &str;
                write!(&mut self.w, "{pfx}{name}")
            }
            SymbolIdentifier::Generated { id, descr: () } => {
                /* ANON_NAME ought to be a non-empty str that cannot be a substring within any raw identifier string in the C src code. */
                const ANON_NAME: &str = "anon.";
                let id = id.as_int();
                write!(&mut self.w, "{pfx}{ANON_NAME}{id:x}")
            }
        }
    }

    pub(super) fn write_jump_decl(&mut self, lbl: &JumpLabel) -> Result<(), io::Error> {
        self.write_jump_name(lbl)?;
        writeln!(&mut self.w, ":")?;
        Ok(())
    }
    pub(super) fn write_jump_name(
        &mut self,
        JumpLabel { id, descr1, descr2 }: &JumpLabel,
    ) -> Result<(), io::Error> {
        let pfx = IDENT_PFX_LOCAL;
        let id = id.as_int();
        let descr1 = self.label_bad_char.replace_all(descr1, "_");
        let descr2 = self.label_bad_char.replace_all(descr2, "_");
        write!(&mut self.w, "{pfx}{descr1}.{id}.{descr2}")?;
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub enum LabelLocality {
    InObjSymTab, // Non-local, ie assembler will include this label in the object file's symbol table.
    Local,       // Ditto not include.
}
impl LabelLocality {
    pub const OF_FUN: Self = Self::InObjSymTab;
    pub const OF_STATIC_VAR: Self = Self::InObjSymTab;
    pub const OF_STATIC_CONST: Self = Self::Local;
}
