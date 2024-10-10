use crate::ds_n_a::singleton::Singleton;
use derive_more::From;
use owning_ref::OwningRef;
use std::hash::Hash;

pub type SubObjType<SubTyp> = OwningRef<Singleton<ObjType>, SubTyp>;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum ObjType {
    Scalar(ScalarType),
    Array(ArrayType),
}
impl<St: Into<ScalarType>> From<St> for ObjType {
    fn from(sca_typ: St) -> Self {
        Self::Scalar(sca_typ.into())
    }
}

#[derive(From, PartialEq, Eq, Hash, Debug)]
pub enum ScalarType {
    Arith(ArithmeticType),
    Ptr(PointerType),
}
impl ScalarType {
    pub fn effective_arithmetic_type(&self) -> ArithmeticType {
        match self {
            Self::Arith(a) => *a,
            Self::Ptr(_) => ArithmeticType::ULong,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum ArithmeticType {
    Int,
    Long,
    UInt,
    ULong,
    Double,
}
impl ArithmeticType {
    pub fn is_signed(&self) -> bool {
        match self {
            Self::Int | Self::Long | Self::Double => true,
            Self::UInt | Self::ULong => false,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct PointerType {
    pub pointee_type: Singleton<ObjType>, // We don't support function-pointers.
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct ArrayType {
    elem_type: Singleton<ObjType>,
    elem_count: ArrayElementCount,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct ArrayElementCount(u64);

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct FunType {
    pub params: Vec<Singleton<ObjType>>,
    pub ret: Singleton<ObjType>,
}
