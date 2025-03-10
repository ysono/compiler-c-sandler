use super::FunInstrsGenerator;
use crate::{
    common::{
        primitive::Const,
        types_backend::ByteLen,
        types_frontend::{
            ArithmeticType, NonAggrType, NonVoidType, ObjType, PointerType, ScalarType, SubObjType,
        },
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
};

/// Binary pointer arithmetic
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp_binary_ptr(
        &mut self,
        op: c::PointerArithmeticBinaryOperator,
        c::Binary { op: _, lhs, rhs, concrete_typ }: c::Binary<TypeCheckedCAst>,
        ifc_typ: NonAggrType,
    ) -> Value {
        use c::PointerArithmeticBinaryOperator as COP;

        let sca_typ = Self::extract_sca_typ(ifc_typ, concrete_typ);

        match op {
            COP::PointerPlusInteger => self.gen_ptr_plus_integ(*lhs, *rhs, sca_typ),
            COP::PointerMinusInteger => self.gen_ptr_minus_integ(*lhs, *rhs, sca_typ),
            COP::PointerMinusPointer => self.gen_ptr_minus_ptr(*lhs, *rhs, sca_typ),
        }
    }

    fn gen_ptr_plus_integ(
        &mut self,
        ptr: c::ScalarExp,
        idx: c::ScalarExp,
        ptr_typ: SubObjType<ScalarType>,
    ) -> Value {
        let scale = extract_pointee_bytelen(ptr.typ()).unwrap();

        let out_val = self.register_new_value(ptr_typ);

        /* Begin instructions */

        let ptr = self.gen_sca_exp_and_get_value(ptr);
        let idx = self.gen_sca_exp_and_get_value(idx);

        self.instrs.push(Instruction::AddPtr(AddPtr {
            ptr,
            idx,
            scale,
            dst: out_val.clone(),
        }));
        out_val
    }

    fn gen_ptr_minus_integ(
        &mut self,
        ptr: c::ScalarExp,
        idx: c::ScalarExp,
        ptr_typ: SubObjType<ScalarType>,
    ) -> Value {
        let scale = extract_pointee_bytelen(ptr.typ()).unwrap();

        let neg_idx_val = self.register_new_value(idx.typ().clone());
        let out_val = self.register_new_value(ptr_typ);

        /* Begin instructions */

        let ptr = self.gen_sca_exp_and_get_value(ptr);
        let idx = self.gen_sca_exp_and_get_value(idx);

        self.instrs.extend([
            Instruction::Unary(Unary {
                op: NumericUnaryOperator::Negate.into(),
                src: idx,
                dst: neg_idx_val.clone(),
            }),
            Instruction::AddPtr(AddPtr {
                ptr,
                idx: neg_idx_val,
                scale,
                dst: out_val.clone(),
            }),
        ]);

        out_val
    }

    fn gen_ptr_minus_ptr(
        &mut self,
        lhs_ptr: c::ScalarExp,
        rhs_ptr: c::ScalarExp,
        out_typ: SubObjType<ScalarType>,
    ) -> Value {
        let scale = extract_pointee_bytelen(lhs_ptr.typ()).unwrap();

        let diff_val = self.register_new_value(out_typ.clone());
        let out_val = self.register_new_value(out_typ);

        /* Begin instructions */

        let lhs_ptr = self.gen_sca_exp_and_get_value(lhs_ptr);
        let rhs_ptr = self.gen_sca_exp_and_get_value(rhs_ptr);

        self.instrs.extend([
            Instruction::Binary(Binary {
                op: ArithmeticBinaryOperator::Sub.into(),
                lhs: lhs_ptr,
                rhs: rhs_ptr,
                dst: diff_val.clone(),
            }),
            Instruction::Binary(Binary {
                op: DivRemBinaryOperator::Div.into(),
                lhs: diff_val,
                rhs: Value::Constant(Const::ULong(scale.as_int())),
                dst: out_val.clone(),
            }),
        ]);

        out_val
    }
}

/// Subscript
impl FunInstrsGenerator<'_> {
    pub(super) fn gen_exp_subscript<LTyp: Clone + Into<NonVoidType>>(
        &mut self,
        c::Subscript { exp1: ptr_exp, exp2: idx_exp }: c::Subscript<TypeCheckedCAst>,
        ifc_typ: LTyp,
    ) -> Object<LTyp> {
        debug_assert!(extract_pointer_type(ptr_exp.typ()).is_ok());
        debug_assert_eq!(
            extract_arithmetic_type(idx_exp.typ().as_owner()),
            Ok(ArithmeticType::Long)
        );

        let ptr_typ = ptr_exp.typ().clone();
        let addr = self.register_new_value(ptr_typ);

        let scale = ifc_typ.clone().into().bytelen();

        /* Begin instructions */

        let ptr = self.gen_sca_exp_and_get_value(*ptr_exp);
        let idx = self.gen_sca_exp_and_get_value(*idx_exp);

        self.instrs.push(Instruction::AddPtr(AddPtr {
            ptr,
            idx,
            scale,
            dst: addr.clone(),
        }));

        Object::Pointee { addr, typ: ifc_typ }
    }
}

/* Helpers */

fn extract_pointee_bytelen(sca_typ: &ScalarType) -> Result<ByteLen, ()> {
    let PointerType { pointee_type } = extract_pointer_type(sca_typ)?;
    let pointee_type = NonVoidType::try_from(pointee_type.clone()).unwrap();
    Ok(pointee_type.bytelen())
}

fn extract_arithmetic_type(obj_typ: &ObjType) -> Result<ArithmeticType, ()> {
    match obj_typ {
        ObjType::Scalar(ScalarType::Arith(ari_typ)) => Ok(*ari_typ),
        _ => Err(()),
    }
}
fn extract_pointer_type(sca_typ: &ScalarType) -> Result<&PointerType, ()> {
    match sca_typ {
        ScalarType::Ptr(ptr_typ) => Ok(ptr_typ),
        _ => Err(()),
    }
}
