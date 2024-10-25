use super::FunInstrsGenerator;
use crate::{
    common::{
        identifier::SymbolIdentifier, primitive::Const, symbol_table_frontend::InitializerItem,
        types_backend::ByteLen, types_frontend::ScalarType,
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};
use std::rc::Rc;

impl<'a> FunInstrsGenerator<'a> {
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
                InitializerItem::String { .. } => todo!(),
                InitializerItem::Pointer(_) => todo!(),
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
