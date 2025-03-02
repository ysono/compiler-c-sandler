pub use self::{fun_type::*, obj_type::*, obj_type_node::*, obj_type_parsed::*};
use crate::{common::types_backend::ByteLen, ds_n_a::singleton::Singleton};
use anyhow::anyhow;
use derivative::Derivative;
use derive_more::{Constructor, From, TryInto};
use getset::Getters;
use owning_ref::OwningRef;
use std::hash::Hash;

/// Object type variants
mod obj_type {
    use super::*;

    #[derive(Hash, PartialEq, Eq, Debug)]
    pub enum ObjType {
        Void(VoidType),
        Scalar(ScalarType),
        Array(ArrayType),
    }
    impl From<VoidType> for ObjType {
        fn from(void_typ: VoidType) -> Self {
            Self::Void(void_typ)
        }
    }
    impl<St: Into<ScalarType>> From<St> for ObjType {
        fn from(typ: St) -> Self {
            Self::Scalar(typ.into())
        }
    }
    impl From<ArrayType> for ObjType {
        fn from(arr_typ: ArrayType) -> Self {
            Self::Array(arr_typ)
        }
    }

    #[derive(Hash, PartialEq, Eq, Debug)]
    pub struct VoidType;

    #[derive(From, Hash, PartialEq, Eq, Debug)]
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
        pub fn bytelen(&self) -> ByteLen {
            ByteLen::from(self.effective_arithmetic_type())
        }
    }

    #[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
    pub enum ArithmeticType {
        Char,
        SChar,
        UChar,
        Int,
        Long,
        UInt,
        ULong,
        Double,
    }
    impl ArithmeticType {
        pub fn is_character(&self) -> bool {
            match self {
                Self::Char | Self::SChar | Self::UChar => true,
                Self::Int | Self::Long | Self::UInt | Self::ULong | Self::Double => false,
            }
        }
        pub fn is_integer(&self) -> bool {
            match self {
                Self::Char
                | Self::SChar
                | Self::UChar
                | Self::Int
                | Self::Long
                | Self::UInt
                | Self::ULong => true,
                Self::Double => false,
            }
        }
        pub fn is_signed(&self) -> bool {
            match self {
                Self::Char | Self::SChar | Self::Int | Self::Long | Self::Double => true,
                Self::UChar | Self::UInt | Self::ULong => false,
            }
        }
    }

    #[derive(Hash, PartialEq, Eq, Debug)]
    pub struct PointerType {
        pub pointee_type: Singleton<ObjType>, // We don't support function-pointers.
    }

    #[derive(Getters, Derivative, Debug)]
    #[getset(get = "pub")]
    #[derivative(Hash, PartialEq, Eq)]
    pub struct ArrayType {
        elem_type: NonVoidType,
        elem_count: ArrayElementCount,

        /// Even though the semantic information is [`ScalarType`], we encode [`ArithmeticType`],
        ///     b/c that's all that our use cases need and it's cheaper to represent.
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        single_type: ArithmeticType,

        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        bytelen: ByteLen,
    }
    impl ArrayType {
        pub fn new(elem_type: NonVoidType, elem_count: ArrayElementCount) -> Self {
            let single_type = match &elem_type {
                NonVoidType::Scalar(s) => s.effective_arithmetic_type(),
                NonVoidType::Array(a) => a.single_type,
            };
            let bytelen = elem_type.bytelen() * elem_count.as_int();
            Self {
                elem_type,
                elem_count,
                single_type,
                bytelen,
            }
        }

        pub fn as_ptr_to_elem(&self) -> PointerType {
            let pointee_type = self.elem_type.clone().into();
            PointerType { pointee_type }
        }
    }

    #[derive(Constructor, Clone, Copy, Hash, PartialEq, Eq, Debug)]
    pub struct ArrayElementCount(u64);
    impl ArrayElementCount {
        pub fn as_int(&self) -> u64 {
            self.0
        }
    }
}

/// Trie nodes representing object types
mod obj_type_node {
    use super::*;

    pub type SubObjType<SubTyp> = OwningRef<Singleton<ObjType>, SubTyp>;

    enum ObjTypeNodeRef {
        Void(SubObjType<VoidType>),
        Scalar(SubObjType<ScalarType>),
        Array(SubObjType<ArrayType>),
    }
    impl From<Singleton<ObjType>> for ObjTypeNodeRef {
        fn from(o: Singleton<ObjType>) -> Self {
            match o.as_ref() {
                ObjType::Void(v) => {
                    let v = unsafe { &*(v as *const _) };
                    let ownref = OwningRef::new(o).map(|_| v);
                    Self::Void(ownref)
                }
                ObjType::Scalar(s) => {
                    let s = unsafe { &*(s as *const _) };
                    let ownref = OwningRef::new(o).map(|_| s);
                    Self::Scalar(ownref)
                }
                ObjType::Array(a) => {
                    let a = unsafe { &*(a as *const _) };
                    let ownref = OwningRef::new(o).map(|_| a);
                    Self::Array(ownref)
                }
            }
        }
    }
    impl From<ObjTypeNodeRef> for Singleton<ObjType> {
        fn from(o_ref: ObjTypeNodeRef) -> Self {
            match o_ref {
                ObjTypeNodeRef::Void(v) => v.into_owner(),
                ObjTypeNodeRef::Scalar(s) => s.into_owner(),
                ObjTypeNodeRef::Array(a) => a.into_owner(),
            }
        }
    }

    #[derive(From, TryInto, Clone, Debug)]
    pub enum NonAggrType {
        Void(SubObjType<VoidType>),
        Scalar(SubObjType<ScalarType>),
    }
    impl TryFrom<Singleton<ObjType>> for NonAggrType {
        type Error = Singleton<ObjType>;
        fn try_from(o: Singleton<ObjType>) -> Result<Self, Self::Error> {
            let o_ref = ObjTypeNodeRef::from(o);
            match o_ref {
                ObjTypeNodeRef::Void(v) => Ok(Self::Void(v)),
                ObjTypeNodeRef::Scalar(s) => Ok(Self::Scalar(s)),
                ObjTypeNodeRef::Array(_) => Err(o_ref.into()),
            }
        }
    }
    impl NonAggrType {
        pub fn try_into_scalar(self) -> Result<SubObjType<ScalarType>, Self> {
            SubObjType::<ScalarType>::try_from(self).map_err(|e| e.input)
        }
    }

    #[derive(From, Clone, Hash, PartialEq, Eq, Debug)]
    pub enum NonVoidType {
        Scalar(SubObjType<ScalarType>),
        Array(SubObjType<ArrayType>),
    }
    impl TryFrom<Singleton<ObjType>> for NonVoidType {
        type Error = Singleton<ObjType>;
        fn try_from(o: Singleton<ObjType>) -> Result<Self, Self::Error> {
            let o_ref = ObjTypeNodeRef::from(o);
            match o_ref {
                ObjTypeNodeRef::Void(_) => Err(o_ref.into()),
                ObjTypeNodeRef::Scalar(s) => Ok(Self::Scalar(s)),
                ObjTypeNodeRef::Array(a) => Ok(Self::Array(a)),
            }
        }
    }
    impl From<NonVoidType> for Singleton<ObjType> {
        fn from(nonvoid: NonVoidType) -> Self {
            match nonvoid {
                NonVoidType::Scalar(s) => s.into_owner(),
                NonVoidType::Array(a) => a.into_owner(),
            }
        }
    }
    impl NonVoidType {
        pub fn is_nonvoid(o: &ObjType) -> bool {
            match o {
                ObjType::Void(_) => false,
                ObjType::Scalar(_) | ObjType::Array(_) => true,
            }
        }
        pub fn bytelen(&self) -> ByteLen {
            match self {
                Self::Scalar(s) => s.bytelen(),
                Self::Array(a) => *a.bytelen(),
            }
        }
    }
}

/// Intermediary parsing result of object types
mod obj_type_parsed {
    use super::*;

    /// The official tester expects some object type variants to
    ///     be valid at the Parse phase
    ///     and invalid at the Typecheck phase.
    /// We choose to represent object types in their valid forms only,
    ///     and represent delayed invalid results as errors.
    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub struct ParsedObjType(pub Result<Singleton<ObjType>, ParsedObjTypeError>);
    impl ParsedObjType {
        pub fn into_res(self) -> Result<Singleton<ObjType>, ParsedObjTypeError> {
            self.0
        }
        pub fn as_res(&self) -> Result<Singleton<ObjType>, ParsedObjTypeError> {
            self.0.clone()
        }
    }

    #[derive(Clone, Hash, PartialEq, Eq, Debug)]
    pub enum ParsedObjTypeError {
        ArrayElemNonCompletableType,
    }
    impl From<ParsedObjTypeError> for anyhow::Error {
        fn from(err: ParsedObjTypeError) -> Self {
            match err {
                ParsedObjTypeError::ArrayElemNonCompletableType => {
                    anyhow!("In C, an array's elem type must be a completable type.")
                }
            }
        }
    }
}

/// Function type
mod fun_type {
    use super::*;

    /// The official tester expects function types to have component types (params and return) that are
    ///     unchecked at the Parse phase
    ///     and validated at the Typecheck phase.
    #[derive(Hash, PartialEq, Eq, Debug)]
    pub struct FunType<Typ> {
        pub params: Vec<Typ>,
        pub ret: Typ,
    }
    pub type ParsedFunType = FunType<ParsedObjType>;
    pub type TypecheckedFunType = FunType<SubObjType<ScalarType>>;
}

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

        let mut get_properties = |items_baseward: &[Dec], base_typ: ArithmeticType| {
            let obj_typ = typ_bld.build_obj_type(items_baseward, base_typ);

            let single_typ = match obj_typ.as_ref() {
                ObjType::Void(_) => None,
                ObjType::Scalar(_) => None,
                ObjType::Array(a) => Some(*a.single_type()),
            };

            let bytelen = match obj_typ.as_ref() {
                ObjType::Void(_) => None,
                ObjType::Scalar(s) => Some(s.bytelen().as_int()),
                ObjType::Array(a) => Some(a.bytelen().as_int()),
            };

            (single_typ, bytelen)
        };

        assert_eq!(get_properties(&[], ArithmeticType::Int), (None, Some(4)));
        assert_eq!(
            get_properties(&[Dec::Ptr], ArithmeticType::Int),
            (None, Some(8))
        );
        assert_eq!(
            get_properties(&[Dec::Ptr, Dec::Arr(17), Dec::Ptr], ArithmeticType::Int),
            (None, Some(8))
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17)], ArithmeticType::Int),
            (Some(ArithmeticType::Int), Some(17 * 4))
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Int),
            (Some(ArithmeticType::Int), Some(17 * 7 * 4))
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17), Dec::Ptr, Dec::Arr(7)], ArithmeticType::Int),
            (Some(ArithmeticType::ULong), Some(17 * 8))
        );
    }
}
