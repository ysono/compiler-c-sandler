use crate::ds_n_a::singleton::Singleton;
use derive_more::{Constructor, From};
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
impl From<ArrayType> for ObjType {
    fn from(arr_typ: ArrayType) -> Self {
        Self::Array(arr_typ)
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
    pub elem_type: Singleton<ObjType>,
    pub elem_count: ArrayElementCount,
}
impl ArrayType {
    pub fn as_ptr_to_elem(&self) -> PointerType {
        let pointee_type = self.elem_type.clone();
        PointerType { pointee_type }
    }
}

#[derive(Constructor, PartialEq, Eq, Hash, Debug)]
pub struct ArrayElementCount(u64);
impl ArrayElementCount {
    pub fn as_int(&self) -> u64 {
        self.0
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct FunType {
    pub params: Vec<Singleton<ObjType>>,
    pub ret: Singleton<ObjType>,
}
