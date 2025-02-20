use super::{AsmCodeEmitter, TAB, label::LabelLocality};
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
    array, ascii,
    io::{self, Write},
    rc::Rc,
};

impl<W: Write> AsmCodeEmitter<W> {
    pub(super) fn write_static_objs(&mut self) -> Result<(), io::Error> {
        if cfg!(debug_assertions) {
            self.write_static_objs_sorted()
        } else {
            self.write_static_objs_nonsorted()
        }
    }

    fn write_static_objs_sorted(&mut self) -> Result<(), io::Error> {
        let mut static_rw_idents = vec![];
        let mut static_ro_idents = vec![];
        for (ident, obj) in self.backend_symtab.ident_to_obj().iter() {
            let AsmObj { asm_type: _, asm_attrs } = obj;
            match asm_attrs {
                AsmObjAttrs::Stack => noop!(),
                AsmObjAttrs::StaticRW(_) => static_rw_idents.push(Rc::clone(ident)),
                AsmObjAttrs::StaticRO(_) => static_ro_idents.push(Rc::clone(ident)),
            }
        }

        #[cfg(debug_assertions)]
        {
            static_rw_idents.sort();
            static_ro_idents.sort();
        }

        let backend_symtab = Rc::clone(&self.backend_symtab);
        for ident in static_rw_idents {
            let AsmObj { asm_attrs, .. } = backend_symtab.ident_to_obj().get(&ident).unwrap();
            match asm_attrs {
                AsmObjAttrs::StaticRW(attrs) => self.write_static_readwrite(&ident, attrs)?,
                _ => unreachable!(),
            }
        }
        for ident in static_ro_idents {
            let AsmObj { asm_attrs, .. } = backend_symtab.ident_to_obj().get(&ident).unwrap();
            match asm_attrs {
                AsmObjAttrs::StaticRO(attrs) => self.write_static_readonly(&ident, attrs)?,
                _ => unreachable!(),
            }
        }

        Ok(())
    }
    fn write_static_objs_nonsorted(&mut self) -> Result<(), io::Error> {
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
            match initializer {
                InitializerItem::String { .. } => ".cstring",
                _ => match alignment {
                    Alignment::B1 => ".literal1",
                    Alignment::B4 => ".literal4",
                    Alignment::B8 => ".literal8",
                    Alignment::B16 => ".literal16",
                },
            }
        } else {
            ".section .rodata"
        };
        let items = array::from_ref(initializer);
        self.write_static_datum(ident, visibility, locality, section, *alignment, items)?;
        if cfg!(target_os = "macos") {
            self.write_fillin_data(initializer, *alignment)?;
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
        if alignment != Alignment::B1 {
            writeln!(&mut self.w, "{TAB}.balign {}", alignment as u8)?;
        }
        self.write_symbol_decl(ident, locality)?;
        for init in inits {
            match init {
                InitializerItem::Zero(bytelen) => {
                    let bytelen = bytelen.as_int();
                    writeln!(&mut self.w, "{TAB}.zero {bytelen}")?;
                }
                InitializerItem::String { chars, zeros_sfx_bytelen } => {
                    let mut zeros_sfx_bytelen = zeros_sfx_bytelen.as_int();

                    let ascii_directive;
                    if zeros_sfx_bytelen >= 1 {
                        zeros_sfx_bytelen -= 1;
                        ascii_directive = ".asciz";
                    } else {
                        ascii_directive = ".ascii";
                    }

                    write!(&mut self.w, "{TAB}{ascii_directive} \"")?;
                    for byte in chars {
                        if "\"\\\n".as_bytes().contains(byte) {
                            write!(&mut self.w, "{}", ascii::escape_default(*byte))?;
                        } else if *byte == b' ' || byte.is_ascii_graphic() {
                            write!(&mut self.w, "{}", char::from(*byte))?;
                            /* Eg on MacOS, the single-quote character must _not_ be escaped. */
                        } else {
                            write!(&mut self.w, "\\{byte:03o}")?;
                        }
                    }
                    writeln!(&mut self.w, "\"")?;

                    if zeros_sfx_bytelen > 0 {
                        writeln!(&mut self.w, "{TAB}.zero {zeros_sfx_bytelen}")?;
                    }
                }
                InitializerItem::Pointer(ident) => {
                    let AsmObj { asm_attrs, .. } =
                        self.backend_symtab.ident_to_obj().get(ident).unwrap();
                    let locality = match asm_attrs {
                        AsmObjAttrs::Stack => unreachable!(),
                        AsmObjAttrs::StaticRW(_) => LabelLocality::OF_STATIC_RW_OBJ,
                        AsmObjAttrs::StaticRO(_) => LabelLocality::OF_STATIC_RO_OBJ,
                    };

                    write!(&mut self.w, "{TAB}.quad ")?;
                    self.write_symbol_name(ident, locality)?;
                    writeln!(&mut self.w)?;
                }
                InitializerItem::Single(konst) => {
                    /* Supported formats include:
                        + hexadecimal floating-point: `.double 0x2.8p+3` (LLVM supports it; GAS doesn't)
                        + decimal floating-point: `.double 20.0`, `2e3`
                        + decimal
                    We choose to emit as decimal. */
                    match konst {
                        Const::Char(i) => writeln!(&mut self.w, "{TAB}.byte {i}")?,
                        Const::UChar(i) => writeln!(&mut self.w, "{TAB}.byte {i}")?,
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
    fn write_fillin_data(
        &mut self,
        init: &InitializerItem<Const>,
        alignment: Alignment,
    ) -> Result<(), io::Error> {
        if let InitializerItem::Single(konst) = init {
            let declared_bytelen = OperandByteLen::from(konst.arithmetic_type());
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
        }
        Ok(())
    }
}
