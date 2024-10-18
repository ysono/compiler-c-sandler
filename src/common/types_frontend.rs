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
    /// This un-aggregates the self type into a single scalar type.
    /// + In case self is scalar, then self.
    /// + In case self is array, then the leaf-level element type.
    ///
    /// Even though the semantic information is [`ScalarType`], we return [`ArithmeticType`],
    ///     b/c that's all that our use cases need and it's cheaper to represent.
    pub fn single_type(&self) -> ArithmeticType {
        match self {
            Self::Scalar(s) => s.effective_arithmetic_type(),
            Self::Array(a) => *a.single_type(),
        }
    }

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
    single_type: ArithmeticType,

    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    bytelen: ByteLen,
}
impl ArrayType {
    pub fn new(elem_type: Singleton<ObjType>, elem_count: ArrayElementCount) -> Self {
        let single_type = elem_type.single_type();
        let bytelen = elem_type.bytelen() * elem_count.as_int();
        Self {
            elem_type,
            elem_count,
            single_type,
            bytelen,
        }
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
    use crate::{
        ds_n_a::singleton::SingletonRepository,
        test::utils::{TestDeclaratorItem as Dec, TypeBuilder},
    };

    #[test]
    fn obj_type_properties() {
        let mut obj_typ_repo = SingletonRepository::<ObjType>::default();
        let mut typ_bld = TypeBuilder::new(&mut obj_typ_repo);

        let mut get_properties = |items_outward: &[Dec], base_typ: ArithmeticType| {
            let obj_typ = typ_bld.build_obj_type(items_outward, base_typ);
            (obj_typ.single_type(), obj_typ.bytelen().as_int())
        };

        assert_eq!(
            get_properties(&[], ArithmeticType::Int),
            (ArithmeticType::Int, 4)
        );
        assert_eq!(
            get_properties(&[Dec::Ptr], ArithmeticType::Int),
            (ArithmeticType::ULong, 8)
        );
        assert_eq!(
            get_properties(&[Dec::Ptr, Dec::Arr(17), Dec::Ptr], ArithmeticType::Int),
            (ArithmeticType::ULong, 8)
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17)], ArithmeticType::Int),
            (ArithmeticType::Int, 17 * 4)
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Int),
            (ArithmeticType::Int, 17 * 7 * 4)
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17), Dec::Ptr, Dec::Arr(7)], ArithmeticType::Int),
            (ArithmeticType::ULong, 17 * 8)
        );
    }
}
