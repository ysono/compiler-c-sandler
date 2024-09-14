use super::{label::LabelLocality, AsmCodeEmitter, TAB};
use crate::{
    common::{
        identifier::UniqueIdentifier,
        symbol_table_frontend::StaticVisibility,
        types_backend::{Alignment, OperandByteLen},
        types_frontend::Const,
    },
    stage4_asm_gen::asm_ast::*,
};
use std::io::{self, Write};

impl<W: Write> AsmCodeEmitter<W> {
    pub(super) fn write_static_var(
        &mut self,
        StaticVariable {
            ident,
            visibility,
            alignment,
            init,
        }: StaticVariable,
    ) -> Result<(), io::Error> {
        let section = match init {
            Const::Int(0) | Const::Long(0) | Const::UInt(0) | Const::ULong(0) => ".bss",
            Const::Int(_) | Const::Long(_) | Const::UInt(_) | Const::ULong(_) => ".data",
            Const::Double(_) => ".data", // Even if val==0, we don't write to `.bss`.
        };
        self.write_static_datum(
            &ident,
            visibility,
            LabelLocality::of_static_var(),
            section,
            alignment,
            init,
        )
    }
    pub(super) fn write_static_const(
        &mut self,
        StaticConstant { ident, alignment, init }: StaticConstant,
    ) -> Result<(), io::Error> {
        let section = if cfg!(target_os = "macos") {
            match alignment {
                Alignment::B4 => ".literal4",
                Alignment::B8 => ".literal8",
                Alignment::B16 => ".literal16",
            }
        } else {
            ".section .rodata"
        };
        self.write_static_datum(
            &ident,
            StaticVisibility::NonGlobal,
            LabelLocality::of_static_const(),
            section,
            alignment,
            init,
        )
    }
    fn write_static_datum(
        &mut self,
        ident: &UniqueIdentifier,
        visibility: StaticVisibility,
        locality: LabelLocality,
        section: &'static str,
        alignment: Alignment,
        init: Const,
    ) -> Result<(), io::Error> {
        self.write_label_visibility(ident, visibility, locality)?;
        writeln!(&mut self.w, "{TAB}{section}")?;
        writeln!(&mut self.w, "{TAB}.balign {}", alignment as u8)?;
        self.write_label_instance(ident, locality)?;
        if section == ".bss" {
            let bytelen = OperandByteLen::from(init.var_type()) as u8;
            writeln!(&mut self.w, "{TAB}.zero {bytelen}")?;
        } else {
            match init {
                Const::Int(i) => writeln!(&mut self.w, "{TAB}.long {i}")?,
                Const::UInt(i) => writeln!(&mut self.w, "{TAB}.long {i}")?,
                Const::Long(i) => writeln!(&mut self.w, "{TAB}.quad {i}")?,
                Const::ULong(i) => writeln!(&mut self.w, "{TAB}.quad {i}")?,
                Const::Double(f) => writeln!(&mut self.w, "{TAB}.quad {}", f.to_bits())?,
            }
        }
        Ok(())
    }
}
