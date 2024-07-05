use crate::types_backend::OperandByteLen;
use std::cmp::Ordering;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum VarType {
    Int,
    Long,
    UInt,
    ULong,
    Double,
}
impl VarType {
    pub fn is_signed(&self) -> bool {
        match self {
            Self::Int | Self::Long => true,
            Self::UInt | Self::ULong => false,
            Self::Double => todo!(),
        }
    }

    pub fn derive_common_type(typ1: VarType, typ2: VarType) -> VarType {
        if typ1 == typ2 {
            typ1
        } else if (typ1 == VarType::Double) || (typ2 == VarType::Double) {
            VarType::Double
        } else {
            let bytelen1 = OperandByteLen::from(typ1);
            let bytelen2 = OperandByteLen::from(typ2);
            match bytelen1.cmp(&bytelen2) {
                Ordering::Equal => {
                    if typ1.is_signed() {
                        typ2
                    } else {
                        typ1
                    }
                }
                Ordering::Greater => typ1,
                Ordering::Less => typ2,
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct FunType {
    pub params: Vec<VarType>,
    pub ret: VarType,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Const {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}
impl Const {
    pub fn new_zero_bits(typ: VarType) -> Const {
        Const::Int(0).cast_to(typ)
    }

    pub fn cast_to(&self, typ: VarType) -> Const {
        macro_rules! new_const {
            ( $out_konst_variant:expr, $out_prim:ty ) => {
                match self {
                    Const::Int(i) => $out_konst_variant(*i as $out_prim),
                    Const::Long(i) => $out_konst_variant(*i as $out_prim),
                    Const::UInt(i) => $out_konst_variant(*i as $out_prim),
                    Const::ULong(i) => $out_konst_variant(*i as $out_prim),
                    Const::Double(f) => $out_konst_variant(*f as $out_prim),
                }
            };
        }

        match typ {
            VarType::Int => new_const!(Const::Int, i32),
            VarType::Long => new_const!(Const::Long, i64),
            VarType::UInt => new_const!(Const::UInt, u32),
            VarType::ULong => new_const!(Const::ULong, u64),
            VarType::Double => new_const!(Const::Double, f64),
        }
    }

    pub fn to_bits(&self) -> u64 {
        match self {
            Const::Int(i) => *i as u64,
            Const::Long(i) => *i as u64,
            Const::UInt(i) => *i as u64,
            Const::ULong(i) => *i,
            Const::Double(f) => f.to_bits(),
        }
    }

    pub fn var_type(&self) -> VarType {
        match self {
            Const::Int(_) => VarType::Int,
            Const::Long(_) => VarType::Long,
            Const::UInt(_) => VarType::UInt,
            Const::ULong(_) => VarType::ULong,
            Const::Double(_) => VarType::Double,
        }
    }
}
