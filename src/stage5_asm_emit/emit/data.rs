use super::{label::LabelLocality, AsmCodeEmitter, TAB};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        symbol_table_backend::{
            AsmObj, AsmObjAttrs, StaticReadWriteAsmObjAttrs, StaticReadonlyAsmObjAttrs,
        },
        symbol_table_frontend::{InitializerItem, StaticVisibility},
        types_backend::{Alignment, OperandByteLen},
    },
    utils::noop,
};
use std::{
    io::{self, Write},
    rc::Rc,
};

impl<W: Write> AsmCodeEmitter<W> {
    pub(super) fn write_static_objs(&mut self) -> Result<(), io::Error> {
        let backend_symtab = Rc::clone(&self.backend_symtab);
        for (ident, obj) in backend_symtab.ident_to_obj().iter() {
            let AsmObj { asm_type: _, asm_attrs } = obj;
            match asm_attrs {
                AsmObjAttrs::Stack => noop!(),
                AsmObjAttrs::StaticRW(attrs) => self.write_static_readwrite(ident, attrs)?,
                AsmObjAttrs::StaticRO(attrs) => self.write_static_readonly(ident, attrs)?,
            }
        }
        Ok(())
    }
    fn write_static_readwrite(
        &mut self,
        ident: &SymbolIdentifier,
        StaticReadWriteAsmObjAttrs {
            visibility,
            alignment,
            initializer,
        }: &StaticReadWriteAsmObjAttrs,
    ) -> Result<(), io::Error> {
        if let Some(items) = initializer {
            let locality = LabelLocality::OF_STATIC_RW_OBJ;
            let section = match &items[..] {
                [] | [InitializerItem::Zero(_)] => ".bss",
                _ => ".data",
            };
            self.write_static_datum(ident, *visibility, locality, section, *alignment, items)?;
        }
        Ok(())
    }
    fn write_static_readonly(
        &mut self,
        ident: &SymbolIdentifier,
        StaticReadonlyAsmObjAttrs { alignment, initializer }: &StaticReadonlyAsmObjAttrs,
    ) -> Result<(), io::Error> {
        let visibility = StaticVisibility::NonGlobal;
        let locality = LabelLocality::OF_STATIC_RO_OBJ;
        let section = if cfg!(target_os = "macos") {
            match alignment {
                Alignment::B4 => ".literal4",
                Alignment::B8 => ".literal8",
                Alignment::B16 => ".literal16",
            }
        } else {
            ".section .rodata"
        };
        let items = &[InitializerItem::Single(*initializer)];
        self.write_static_datum(ident, visibility, locality, section, *alignment, items)?;
        if cfg!(target_os = "macos") {
            self.write_fillin_data(*initializer, *alignment)?;
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
        inits: &[InitializerItem<Const>],
    ) -> Result<(), io::Error> {
        self.write_symbol_visibility(ident, visibility, locality)?;
        writeln!(&mut self.w, "{TAB}{section}")?;
        writeln!(&mut self.w, "{TAB}.balign {}", alignment as u8)?;
        self.write_symbol_decl(ident, locality)?;
        for init in inits {
            match init {
                InitializerItem::Zero(bytelen) => {
                    let bytelen = bytelen.as_int();
                    writeln!(&mut self.w, "{TAB}.zero {bytelen}")?;
                }
                InitializerItem::String { .. } => todo!(),
                InitializerItem::Pointer(_) => todo!(),
                InitializerItem::Single(konst) => {
                    /* Supported formats include:
                        + hexadecimal floating-point: `.double 0x2.8p+3` (LLVM supports it; GAS doesn't)
                        + decimal floating-point: `.double 20.0`, `2e3`
                        + decimal
                    We choose to emit as decimal. */
                    match konst {
                        Const::Char(_) | Const::UChar(_) => todo!(),
                        Const::Int(i) => writeln!(&mut self.w, "{TAB}.long {i}")?,
                        Const::UInt(i) => writeln!(&mut self.w, "{TAB}.long {i}")?,
                        Const::Long(i) => writeln!(&mut self.w, "{TAB}.quad {i}")?,
                        Const::ULong(i) => writeln!(&mut self.w, "{TAB}.quad {i}")?,
                        Const::Double(f) => writeln!(&mut self.w, "{TAB}.quad {}", f.to_bits())?,
                    }
                }
            }
        }
        writeln!(&mut self.w)?;
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
