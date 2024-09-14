use super::{AsmCodeEmitter, TAB};
use crate::common::{identifier::UniqueIdentifier, symbol_table_frontend::StaticVisibility};
use std::io::{self, Write};

impl<W: Write> AsmCodeEmitter<W> {
    /* Asm label */

    pub(super) fn write_label_visibility(
        &mut self,
        ident: &UniqueIdentifier,
        visibility: StaticVisibility,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        match visibility {
            StaticVisibility::Global => {
                write!(&mut self.w, "{TAB}.globl{TAB}")?;
                self.write_label_name(ident, locality)?;
                writeln!(&mut self.w)?;
            }
            StaticVisibility::NonGlobal => { /* No-op. */ }
        }
        Ok(())
    }
    pub(super) fn write_label_instance(
        &mut self,
        ident: &UniqueIdentifier,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        self.write_label_name(ident, locality)?;
        writeln!(&mut self.w, ":")?;
        Ok(())
    }
    pub(super) fn write_label_name(
        &mut self,
        ident: &UniqueIdentifier,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        const IDENT_PFX_NONLOCAL: &str = if cfg!(target_os = "macos") { "_" } else { "" };
        const IDENT_PFX_LOCAL: &str = if cfg!(target_os = "macos") {
            "L."
        } else {
            ".L."
        };
        let ident_pfx = match locality {
            LabelLocality::InObjSymTab => IDENT_PFX_NONLOCAL,
            LabelLocality::Local => IDENT_PFX_LOCAL,
        };
        write!(&mut self.w, "{ident_pfx}")?;
        self.write_ident(ident)?;
        Ok(())
    }
    fn write_ident(&mut self, ident: &UniqueIdentifier) -> Result<(), io::Error> {
        match ident {
            UniqueIdentifier::Exact(ident) => {
                let name = ident as &str;
                write!(&mut self.w, "{name}")
            }
            UniqueIdentifier::Generated { id, descr } => {
                /* DELIM must be, and NAME_DEFAULT ought to be,
                a non-empty str that cannot be a substring within any original identifier string in the C src code. */
                const DELIM: char = '.';
                const NAME_DEFAULT: &str = "anon.";
                let name = descr
                    .as_ref()
                    .map(|descr| descr as &str)
                    .unwrap_or(NAME_DEFAULT);
                let name = self.label_bad_char.replace_all(name, "_");
                let id = id.as_int();
                write!(&mut self.w, "{name}{DELIM}{id:x}")
            }
        }
    }
}

#[derive(Clone, Copy)]
pub enum LabelLocality {
    InObjSymTab, // Non-local, ie assembler will include this label in the object file's symbol table.
    Local,       // Ditto not include.
}
impl LabelLocality {
    pub fn of_fun() -> Self {
        Self::InObjSymTab
    }
    pub fn of_static_var() -> Self {
        Self::InObjSymTab
    }
    pub fn of_static_const() -> Self {
        Self::Local
    }
    pub fn of_jump() -> Self {
        Self::Local
    }
}
