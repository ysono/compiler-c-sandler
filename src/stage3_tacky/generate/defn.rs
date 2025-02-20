use super::FunInstrsGenerator;
use crate::{
    common::{
        identifier::SymbolIdentifier, primitive::Const, symbol_table_frontend::InitializerItem,
        types_backend::ByteLen, types_frontend::ScalarType,
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::{cmp, rc::Rc};

impl FunInstrsGenerator<'_> {
    pub(super) fn gen_var_defn(
        &mut self,
        c::VariableDefinition { ident, init }: c::VariableDefinition<TypeCheckedCAst>,
    ) {
        let mut cur_offset = ByteLen::new(0);
        for item in init {
            match item {
                InitializerItem::Single(typed_exp) => {
                    self.gen_var_init_single(&ident, &mut cur_offset, typed_exp);
                }
                InitializerItem::String { chars, zeros_sfx_bytelen } => {
                    self.gen_var_init_string(&ident, &mut cur_offset, chars, zeros_sfx_bytelen);
                }
                InitializerItem::Pointer(_) => unreachable!(
                    "The pointer initializer item is used to initialize static objs only."
                ),
                InitializerItem::Zero(zeros_bytelen) => {
                    self.gen_var_init_zero(&ident, &mut cur_offset, zeros_bytelen);
                }
            }
        }
    }

    fn gen_var_init_single(
        &mut self,
        ident: &Rc<SymbolIdentifier>,
        cur_offset: &mut ByteLen,
        typed_exp: c::TypedExp<ScalarType>,
    ) {
        let single_bytelen = typed_exp.typ().bytelen();

        let val = self.gen_exp_and_get_value(typed_exp);

        self.instrs.push(new_cto(val, ident, *cur_offset));

        *cur_offset += single_bytelen;
    }

    fn gen_var_init_string(
        &mut self,
        ident: &Rc<SymbolIdentifier>,
        cur_offset: &mut ByteLen,
        chars: Vec<u8>,
        zeros_sfx_bytelen: ByteLen,
    ) {
        let mut rem_bytelen = chars.len() as u64 + zeros_sfx_bytelen.as_int();

        let mut cur_chars_sfx: &[u8] = &chars;

        fn derive_window_bytes<const LEN: usize>(chars_sfx: &[u8]) -> ([u8; LEN], usize) {
            let mut buf = [0u8; LEN];

            let copy_bytelen = cmp::min(chars_sfx.len(), LEN);
            buf[0..copy_bytelen].copy_from_slice(&chars_sfx[0..copy_bytelen]);

            (buf, copy_bytelen)
        }

        while rem_bytelen >= 8 {
            let (buf, copy_bytelen) = derive_window_bytes::<8>(cur_chars_sfx);

            let val = Value::Constant(Const::ULong(u64::from_le_bytes(buf)));
            self.instrs.push(new_cto(val, ident, *cur_offset));

            cur_chars_sfx = &cur_chars_sfx[copy_bytelen..];
            *cur_offset += ByteLen::new(8);
            rem_bytelen -= 8;
        }
        if rem_bytelen >= 4 {
            let (buf, copy_bytelen) = derive_window_bytes::<4>(cur_chars_sfx);

            let val = Value::Constant(Const::UInt(u32::from_le_bytes(buf)));
            self.instrs.push(new_cto(val, ident, *cur_offset));

            cur_chars_sfx = &cur_chars_sfx[copy_bytelen..];
            *cur_offset += ByteLen::new(4);
            rem_bytelen -= 4;
        }
        while rem_bytelen >= 1 {
            let (buf, copy_bytelen) = derive_window_bytes::<1>(cur_chars_sfx);

            let val = Value::Constant(Const::UChar(u8::from_le_bytes(buf)));
            self.instrs.push(new_cto(val, ident, *cur_offset));

            cur_chars_sfx = &cur_chars_sfx[copy_bytelen..];
            *cur_offset += ByteLen::new(1);
            rem_bytelen -= 1;
        }
    }

    fn gen_var_init_zero(
        &mut self,
        ident: &Rc<SymbolIdentifier>,
        cur_offset: &mut ByteLen,
        zeros_bytelen: ByteLen,
    ) {
        let mut rem_bytelen = zeros_bytelen.as_int();

        while rem_bytelen >= 8 {
            let val = Value::Constant(Const::ULong(0));
            self.instrs.push(new_cto(val, ident, *cur_offset));

            *cur_offset += ByteLen::new(8);
            rem_bytelen -= 8;
        }
        if rem_bytelen >= 4 {
            let val = Value::Constant(Const::UInt(0));
            self.instrs.push(new_cto(val, ident, *cur_offset));

            *cur_offset += ByteLen::new(4);
            rem_bytelen -= 4;
        }
        while rem_bytelen >= 1 {
            let val = Value::Constant(Const::UChar(0));
            self.instrs.push(new_cto(val, ident, *cur_offset));

            *cur_offset += ByteLen::new(1);
            rem_bytelen -= 1;
        }
    }
}

/// Note, [`Instruction::CopyToOffset`] is compatible with any alignment at the destination.
fn new_cto(src: Value, ident: &Rc<SymbolIdentifier>, offset: ByteLen) -> Instruction {
    Instruction::CopyToOffset(CopyToOffset {
        src,
        dst_obj: Rc::clone(ident),
        offset,
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        common::{
            symbol_table_frontend::SymbolTable,
            types_frontend::{ArithmeticType, ObjType, SubObjType},
        },
        ds_n_a::singleton::SingletonRepository,
        test::utils::{fail, TypeBuilder},
    };
    use c::{RExp, TypedExp, TypedRExp, VariableDefinition};
    use owning_ref::OwningRef;

    fn in_single_int(i: i32, typ: SubObjType<ScalarType>) -> InitializerItem<TypedExp<ScalarType>> {
        InitializerItem::Single(TypedExp::R(TypedRExp {
            exp: RExp::Const(Const::Int(i)),
            typ,
        }))
    }

    fn emit_initializer(
        initializer: Vec<InitializerItem<TypedExp<ScalarType>>>,
    ) -> Vec<CopyToOffset> {
        let mut symtab = SymbolTable::default();
        let mut tacky_gen = FunInstrsGenerator::new(&mut symtab);

        let var_defn = VariableDefinition {
            ident: Rc::new(SymbolIdentifier::new_generated()),
            init: initializer,
        };
        tacky_gen.gen_var_defn(var_defn);

        tacky_gen
            .instrs
            .into_iter()
            .map(|instr| match instr {
                Instruction::CopyToOffset(cto) => cto,
                _ => fail!(),
            })
            .collect()
    }

    #[test]
    fn gen_var_defn_bytes() {
        let mut obj_typ_repo = SingletonRepository::<ObjType>::default();
        let mut typ_bld = TypeBuilder::new(&mut obj_typ_repo);

        let int_typ_as_obj_typ = typ_bld.build_obj_type(&[], ArithmeticType::Int);
        let int_typ_as_sca_typ =
            OwningRef::new(int_typ_as_obj_typ.clone()).map(|obj_typ| match obj_typ {
                ObjType::Scalar(s) => s,
                _ => unreachable!(),
            });

        let in_items = vec![
            InitializerItem::Zero(ByteLen::new(8 + 4 + 3)),
            in_single_int(0x0202_0101, int_typ_as_sca_typ.clone()),
            in_single_int(0x0404_0303, int_typ_as_sca_typ.clone()),
            InitializerItem::String {
                chars: "aabbccddeef".into(),
                zeros_sfx_bytelen: ByteLen::new(((8 * 2) - "aabbccddeef".len() as u64) + (4 + 3)),
            },
        ]; // Note, this series of items is unrealistic as an output of the typechecker.

        let instrs = emit_initializer(in_items);

        let mut actual_instrs = instrs.into_iter();
        let mut expected_offset = 0;
        {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::ULong(0x0000_0000_0000_0000)));
            expected_offset += 8;
        }
        {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::UInt(0x0000_0000)));
            expected_offset += 4;
        }
        for _ in 0..3 {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::UChar(0x00)));
            expected_offset += 1;
        }
        {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::Int(0x0202_0101)));
            expected_offset += 4;
        }
        {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::Int(0x0404_0303)));
            expected_offset += 4;
        }
        {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::ULong(0x6464_6363_6262_6161)));
            expected_offset += 8;
        }
        {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::ULong(0x0000_0000_0066_6565)));
            expected_offset += 8;
        }
        {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::UInt(0x0000_0000)));
            expected_offset += 4;
        }
        for _ in 0..3 {
            let CopyToOffset { src, dst_obj: _, offset } = actual_instrs.next().unwrap();
            assert_eq!(offset, ByteLen::new(expected_offset));
            assert_eq!(src, Value::Constant(Const::UChar(0x00)));
            expected_offset += 1;
        }
        assert!(actual_instrs.next().is_none())
    }
}
