use super::{label::LabelLocality, AsmCodeEmitter, TAB};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        symbol_table_frontend::{InitializerItem, StaticVisibility},
        types_backend::{Alignment, OperandByteLen},
    },
    stage4_asm_gen::asm_ast::*,
    utils::noop,
};
use std::io::{self, Write};

impl<W: Write> AsmCodeEmitter<W> {
    pub(super) fn write_static_var(
        &mut self,
        StaticVariable {
            ident,
            visibility,
            alignment,
            inits,
        }: StaticVariable,
    ) -> Result<(), io::Error> {
        let init = match inits.first() {
            Some(InitializerItem::Single(konst)) => *konst,
            Some(InitializerItem::Zero(bytelen)) => match bytelen.as_int() {
                4 => Const::Int(0),
                8 => Const::Long(0),
                _ => todo!(),
            },
            _ => todo!(),
        };
        let section = match init {
            Const::Int(0) | Const::Long(0) | Const::UInt(0) | Const::ULong(0) => ".bss",
            Const::Int(_) | Const::Long(_) | Const::UInt(_) | Const::ULong(_) => ".data",
            Const::Double(_) => ".data", // Even if val==0.0, we don't write to `.bss`.
        };
        self.write_static_datum(
            &ident,
            visibility,
            LabelLocality::OF_STATIC_VAR,
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
            LabelLocality::OF_STATIC_CONST,
            section,
            alignment,
            init,
        )?;
        if cfg!(target_os = "macos") {
            self.write_fillin_data(init, alignment)?;
        }
        Ok(())
    }
    fn write_static_datum(
        &mut self,
        ident: &SymbolIdentifier,
        visibility: StaticVisibility,
        locality: LabelLocality,
        section: &'static str,
        alignment: Alignment,
        init: Const,
    ) -> Result<(), io::Error> {
        self.write_symbol_visibility(ident, visibility, locality)?;
        writeln!(&mut self.w, "{TAB}{section}")?;
        writeln!(&mut self.w, "{TAB}.balign {}", alignment as u8)?;
        self.write_symbol_decl(ident, locality)?;
        if section == ".bss" {
            let bytelen = OperandByteLen::from(init.arithmetic_type()) as u8;
            writeln!(&mut self.w, "{TAB}.zero {bytelen}")?;
        } else {
            /* Supported formats include:
                + hexadecimal floating-point: `.double 0x2.8p+3` (LLVM supports it; GAS doesn't)
                + decimal floating-point: `.double 20.0`, `2e3`
                + decimal
            We choose to emit as decimal. */
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
    fn write_fillin_data(&mut self, init: Const, alignment: Alignment) -> Result<(), io::Error> {
        let declared_bytelen = OperandByteLen::from(init.arithmetic_type());
        match (declared_bytelen, alignment) {
            (blen, ali) if (blen as u8) == (ali as u8) => noop!(),
            (OperandByteLen::B4, Alignment::B8) => {
                writeln!(&mut self.w, "{TAB}.long 0")?;
            }
            (OperandByteLen::B4, Alignment::B16) => {
                writeln!(&mut self.w, "{TAB}.long 0")?;
                writeln!(&mut self.w, "{TAB}.quad 0")?;
            }
            (OperandByteLen::B8, Alignment::B16) => {
                writeln!(&mut self.w, "{TAB}.quad 0")?;
            }
            _ => unreachable!(),
        }
        Ok(())
    }
}
