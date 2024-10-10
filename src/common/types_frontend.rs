use crate::{common::types_backend::ByteLen, ds_n_a::singleton::Singleton};
use derivative::Derivative;
use derive_more::{Constructor, From};
use getset::Getters;
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
impl ObjType {
    pub fn bytelen(&self) -> ByteLen {
        match self {
            Self::Scalar(s) => ByteLen::from(s.effective_arithmetic_type()),
            Self::Array(a) => *a.bytelen(),
        }
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
    pub fn is_integer(&self) -> bool {
        match self {
            Self::Int | Self::Long | Self::UInt | Self::ULong => true,
            Self::Double => false,
        }
    }
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

#[derive(Getters, Derivative, Debug)]
#[getset(get = "pub")]
#[derivative(PartialEq, Eq, Hash)]
pub struct ArrayType {
    elem_type: Singleton<ObjType>,
    elem_count: ArrayElementCount,

    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    bytelen: ByteLen,
}
impl ArrayType {
    pub fn new(elem_type: Singleton<ObjType>, elem_count: ArrayElementCount) -> Self {
        let bytelen = elem_type.bytelen() * elem_count.as_int();
        Self { elem_type, elem_count, bytelen }
    }

    pub fn as_ptr_to_elem(&self) -> PointerType {
        let pointee_type = self.elem_type.clone();
        PointerType { pointee_type }
    }
}

#[derive(Constructor, Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ArrayElementCount(u64);
impl ArrayElementCount {
    pub fn as_int(&self) -> u64 {
        self.0
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct FunType<Typ> {
    pub params: Vec<Typ>,
    pub ret: Typ,
}
pub type ParsedFunType = FunType<Singleton<ObjType>>;
pub type ScalarFunType = FunType<SubObjType<ScalarType>>;

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::utils::{TestDeclaratorItem as Dec, TypeBuilder};

    #[test]
    fn obj_type_bytelen() {
        let mut typ_bld = TypeBuilder::default();

        assert_eq!(
            typ_bld
                .build_obj_type(&[Dec::Arr(17)], ArithmeticType::Int)
                .bytelen()
                .as_int(),
            4 * 17
        );
        assert_eq!(
            typ_bld
                .build_obj_type(&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Long)
                .bytelen()
                .as_int(),
            8 * 7 * 17
        );
        assert_eq!(
            typ_bld
                .build_obj_type(&[Dec::Ptr], ArithmeticType::Int)
                .bytelen()
                .as_int(),
            8
        );
        assert_eq!(
            typ_bld
                .build_obj_type(&[Dec::Ptr, Dec::Ptr, Dec::Arr(17)], ArithmeticType::UInt)
                .bytelen()
                .as_int(),
            8
        );
    }
}
