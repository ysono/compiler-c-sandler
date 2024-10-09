use crate::ds_n_a::singleton::Singleton;
use derive_more::From;
use std::hash::Hash;

#[derive(From, PartialEq, Eq, Hash, Debug)]
pub enum VarType {
    Arith(ArithmeticType),
    Ptr(PointerType),
}
impl VarType {
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
    pub pointee_type: Singleton<VarType>, // We don't support function-pointers.
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct FunType {
    pub params: Vec<Singleton<VarType>>,
    pub ret: Singleton<VarType>,
}
