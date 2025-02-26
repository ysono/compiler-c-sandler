use super::FunInstrsGenerator;
use crate::{
    common::{
        identifier::SymbolIdentifier, primitive::Const, symbol_table_frontend::InitializerString,
        types_backend::ByteLen,
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::{cmp, rc::Rc};

impl FunInstrsGenerator<'_> {
    /// We emit
    /// {evaluations of sub-expressions} and
    /// {initializations (ie `[CopyToOffset]` instructions)}
    /// interleaved.
    pub(super) fn gen_var_defn(
        &mut self,
        c::VariableDefinition { ident, init }: c::VariableDefinition<TypeCheckedCAst>,
    ) {
        let mut cto_gen = CtoGenerator::new(ident);

        for item in init {
            match item {
                c::RuntimeInitializerItem::Single(typed_exp) => {
                    cto_gen.flush(&mut self.instrs);

                    let single_bytelen = typed_exp.typ().bytelen();

                    let val = self.gen_exp_and_get_value(typed_exp);

                    cto_gen.push(val, single_bytelen, &mut self.instrs);
                }
                c::RuntimeInitializerItem::String(InitializerString {
                    chars,
                    zeros_sfx_bytelen,
                }) => cto_gen.gen_init_string(chars, zeros_sfx_bytelen, &mut self.instrs),
                c::RuntimeInitializerItem::Pointer(()) => unreachable!(
                    "The pointer initializer item is used to initialize static objs only."
                ),
                c::RuntimeInitializerItem::Zero(zeros_bytelen) => {
                    cto_gen.gen_init_zero(zeros_bytelen, &mut self.instrs)
                }
            }
        }

        cto_gen.flush(&mut self.instrs);
    }
}

struct CtoGenerator {
    buf: [u8; 8],
    buf_dirty_len: usize,

    ident: Rc<SymbolIdentifier>,
    dst_offset: ByteLen,
}
/// Invariant: after each call to any `gen_*()` method, 0 <= `buf_dirty_len` < 8
impl CtoGenerator {
    fn new(ident: Rc<SymbolIdentifier>) -> Self {
        Self {
            buf: [0u8; 8],
            buf_dirty_len: 0,

            ident,
            dst_offset: ByteLen::new(0),
        }
    }

    fn gen_init_string(
        &mut self,
        chars: Vec<u8>,
        zeros_sfx_bytelen: ByteLen,
        instrs: &mut Vec<Instruction>,
    ) {
        let mut rem_chars: &[u8] = &chars;

        while rem_chars.len() != 0 {
            let filled_len = self.fill_buf_and_flush_full(rem_chars, instrs);
            rem_chars = &rem_chars[filled_len..];
        }

        self.gen_init_zero(zeros_sfx_bytelen, instrs);
    }

    fn gen_init_zero(&mut self, zeros_bytelen: ByteLen, instrs: &mut Vec<Instruction>) {
        const BATCH: [u8; 8] = [0u8; 8];

        let mut rem_bytelen = zeros_bytelen.as_int() as usize;

        let fill_max_len = cmp::min(8, rem_bytelen);
        let filled_len = self.fill_buf_and_flush_full(&BATCH[..fill_max_len], instrs);
        rem_bytelen -= filled_len;

        while rem_bytelen >= 8 {
            let val = Value::Constant(Const::ULong(0));
            self.push(val, ByteLen::new(8), instrs);

            rem_bytelen -= 8;
        }

        if rem_bytelen != 0 {
            self.buf[..rem_bytelen].fill(0);
            self.buf_dirty_len = rem_bytelen;
        }
    }

    fn fill_buf_and_flush_full(&mut self, bytes: &[u8], instrs: &mut Vec<Instruction>) -> usize {
        let copy_bytelen = cmp::min(8 - self.buf_dirty_len, bytes.len());
        let new_dirty_len = self.buf_dirty_len + copy_bytelen;

        self.buf[self.buf_dirty_len..new_dirty_len].copy_from_slice(&bytes[..copy_bytelen]);

        if new_dirty_len < 8 {
            self.buf_dirty_len = new_dirty_len;
        } else {
            let val = Value::Constant(Const::ULong(u64::from_le_bytes(self.buf)));
            self.push(val, ByteLen::new(8), instrs);

            self.buf_dirty_len = 0;
        }

        copy_bytelen
    }

    fn flush(&mut self, instrs: &mut Vec<Instruction>) {
        let mut int = u64::from_le_bytes(self.buf);

        if self.buf_dirty_len >= 4 {
            let val = Value::Constant(Const::UInt(int as u32));
            self.push(val, ByteLen::new(4), instrs);

            self.buf_dirty_len -= 4;
            int >>= 32;
        }
        while self.buf_dirty_len >= 1 {
            let val = Value::Constant(Const::UChar(int as u8));
            self.push(val, ByteLen::new(1), instrs);

            self.buf_dirty_len -= 1;
            int >>= 8;
        }
    }

    /// Note, [`Instruction::CopyToOffset`] is compatible with any alignment at the destination.
    fn push(&mut self, src: Value, delta_bytelen: ByteLen, instrs: &mut Vec<Instruction>) {
        let cto = CopyToOffset {
            src,
            dst_obj: Rc::clone(&self.ident),
            offset: self.dst_offset,
        };
        let instr = Instruction::CopyToOffset(cto);
        instrs.push(instr);

        self.dst_offset += delta_bytelen;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        common::{
            symbol_table_frontend::FrontendSymbolTable,
            types_frontend::{ArithmeticType, ObjType, ScalarType, SubObjType},
        },
        ds_n_a::singleton::SingletonRepository,
        test::utils::{TypeBuilder, fail},
    };
    use owning_ref::OwningRef;
    use std::collections::VecDeque;

    /* Creating inputs. */

    fn in_single_constexpr_int(i: i32, typ: SubObjType<ScalarType>) -> c::RuntimeInitializerItem {
        c::RuntimeInitializerItem::Single(c::TypedExp::R(c::TypedRExp {
            exp: c::RExp::Const(Const::Int(i)),
            typ,
        }))
    }
    fn in_single_rtexpr_int(
        i: i32,
        j: i32,
        typ: SubObjType<ScalarType>,
    ) -> c::RuntimeInitializerItem {
        let lhs = c::TypedExp::R(c::TypedRExp {
            exp: c::RExp::Const(Const::Int(i)),
            typ: typ.clone(),
        });
        let rhs = c::TypedExp::R(c::TypedRExp {
            exp: c::RExp::Const(Const::Int(j)),
            typ: typ.clone(),
        });
        let exp = c::TypedExp::R(c::TypedRExp {
            exp: c::RExp::Binary(c::Binary {
                op: c::ArithmeticBinaryOperator::Add.into(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
            typ,
        });
        c::RuntimeInitializerItem::Single(exp)
    }

    /* Converting inputs to outputs. */

    fn emit_initializer(initializer: Vec<c::RuntimeInitializerItem>) -> Vec<Instruction> {
        let mut fe_symtab = FrontendSymbolTable::default();
        let mut tacky_gen = FunInstrsGenerator::new(&mut fe_symtab);

        let var_defn = c::VariableDefinition {
            ident: Rc::new(SymbolIdentifier::new_generated()),
            init: initializer,
        };
        tacky_gen.gen_var_defn(var_defn);

        tacky_gen.instrs
    }

    /* Checking outputs. */

    struct Actual {
        actual_instrs: VecDeque<Instruction>,
        expected_offset: u64,
    }
    impl Actual {
        fn expect_cto<V: Into<Value>>(&mut self, expected_val: V, expected_delta_offset: u64) {
            let actual_cto = match self.actual_instrs.pop_front().unwrap() {
                Instruction::CopyToOffset(cto) => cto,
                _ => fail!(),
            };
            let CopyToOffset { src, dst_obj: _, offset } = actual_cto;
            assert_eq!(src, expected_val.into());
            assert_eq!(offset, ByteLen::new(self.expected_offset));
            self.expected_offset += expected_delta_offset;
        }

        fn expect_ctos_zeros(&mut self, ct_8s: u16, ct_4s: u16, ct_1s: u16) {
            for _ in 0..ct_8s {
                self.expect_cto(Const::ULong(0x0000_0000_0000_0000), 8);
            }
            for _ in 0..ct_4s {
                self.expect_cto(Const::UInt(0x0000_0000), 4);
            }
            for _ in 0..ct_1s {
                self.expect_cto(Const::UChar(0x00), 1);
            }
        }
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
            c::RuntimeInitializerItem::Zero(ByteLen::new(8 * 2 + 4 * 1 + 1 * 3)),
            in_single_constexpr_int(0x0202_0101, int_typ_as_sca_typ.clone()),
            c::RuntimeInitializerItem::Zero(ByteLen::new(8 * 2 + 4 * 1 + 1 * 3)),
            in_single_rtexpr_int(0x0404_0000, 0x0000_0303, int_typ_as_sca_typ.clone()),
            c::RuntimeInitializerItem::Zero(ByteLen::new(8 * 2 + 4 * 1 + 1 * 3)),
            c::RuntimeInitializerItem::String(InitializerString {
                chars: "aabbccddeef".into(),
                zeros_sfx_bytelen: ByteLen::new(5),
            }),
        ]; // Note, this series of items is unrealistic as an output of the typechecker.

        let actual_instrs = emit_initializer(in_items);

        let mut actual = Actual {
            actual_instrs: VecDeque::from(actual_instrs),
            expected_offset: 0,
        };

        actual.expect_ctos_zeros(2, 1, 3);
        actual.expect_cto(Const::Int(0x0202_0101), 4);
        actual.expect_ctos_zeros(2, 1, 3);
        let rt_val = {
            let instr = actual.actual_instrs.pop_front().unwrap();
            match instr {
                Instruction::Binary(Binary { dst, .. }) => dst,
                _ => fail!(),
            }
        };
        actual.expect_cto(rt_val, 4);
        actual.expect_ctos_zeros(2, 0, 0);
        actual.expect_cto(Const::ULong(0x6100_0000_0000_0000), 8);
        actual.expect_cto(Const::ULong(0x6564_6463_6362_6261), 8);
        actual.expect_cto(Const::UInt(0x0000_6665), 4);
        actual.expect_ctos_zeros(0, 0, 3);

        assert!(actual.actual_instrs.is_empty())
    }
}
