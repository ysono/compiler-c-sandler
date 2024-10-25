use super::FunInstrsGenerator;
use crate::{
    common::{primitive::Const, symbol_table_frontend::InitializerItem, types_backend::ByteLen},
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
    pub(super) fn gen_var_defn(
        &mut self,
        c::VariableDefinition { ident, typ, init }: c::VariableDefinition<TypeCheckedCAst>,
    ) {
        let single_type = typ.single_type();
        let single_bytelen = ByteLen::from(single_type);
        let single_zero = Const::new_zero_bits(single_type);
        let new_cto = |src: Value, offset: ByteLen| {
            Instruction::CopyToOffset(CopyToOffset {
                src,
                dst_obj: Rc::clone(&ident),
                offset,
            })
        };

        /* Begin instructions */

        let mut offset = ByteLen::new(0);
        for item in init {
            match item {
                InitializerItem::Single(exp) => {
                    let val = self.gen_exp_and_get_value(exp);

                    self.instrs.push(new_cto(val, offset));
                    offset += single_bytelen;
                }
                InitializerItem::String { .. } => todo!(),
                InitializerItem::Pointer(_) => todo!(),
                InitializerItem::Zero(bytelen) => {
                    let final_offset = offset + bytelen;
                    while offset < final_offset {
                        self.instrs
                            .push(new_cto(Value::Constant(single_zero), offset));
                        offset += single_bytelen;
                    }
                    debug_assert_eq!(offset, final_offset);
                }
            }
        }
    }
}
