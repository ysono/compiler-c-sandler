pub use self::{fun_type::*, obj_type::*, obj_type_node::*};
use crate::{common::types_backend::ByteLen, ds_n_a::singleton::Singleton};
use derivative::Derivative;
use derive_more::{Constructor, From};
use getset::Getters;
use owning_ref::OwningRef;
use std::hash::Hash;

/// Object type variants
mod obj_type {
    use super::*;

    #[derive(Hash, PartialEq, Eq, Debug)]
    pub enum ObjType {
        Void,
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
                ObjType::Void => todo!(),
                Self::Scalar(s) => s.bytelen(),
                Self::Array(a) => *a.bytelen(),
            }
        }
    }

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
        elem_type: Singleton<ObjType>,
        elem_count: ArrayElementCount,

        /// Even though the semantic information is [`ScalarType`], we encode [`ArithmeticType`],
        ///     b/c that's all that our use cases need and it's cheaper to represent.
        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        single_type: ArithmeticType,

        #[derivative(Hash = "ignore", PartialEq = "ignore")]
        bytelen: ByteLen,
    }
    impl ArrayType {
        pub fn new(elem_type: Singleton<ObjType>, elem_count: ArrayElementCount) -> Self {
            let single_type = match elem_type.as_ref() {
                ObjType::Void => todo!(),
                ObjType::Scalar(s) => s.effective_arithmetic_type(),
                ObjType::Array(a) => a.single_type,
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
            let pointee_type = self.elem_type.clone();
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
}

/// Function type
mod fun_type {
    use super::*;

    #[derive(Hash, PartialEq, Eq, Debug)]
    pub struct FunType<Typ> {
        pub params: Vec<Typ>,
        pub ret: Typ,
    }
    pub type ParsedFunType = FunType<Singleton<ObjType>>;
    pub type ScalarFunType = FunType<SubObjType<ScalarType>>;
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
                ObjType::Void => todo!(),
                ObjType::Scalar(_) => None,
                ObjType::Array(a) => Some(*a.single_type()),
            };

            let bytelen = obj_typ.bytelen().as_int();

            (single_typ, bytelen)
        };

        assert_eq!(get_properties(&[], ArithmeticType::Int), (None, 4));
        assert_eq!(get_properties(&[Dec::Ptr], ArithmeticType::Int), (None, 8));
        assert_eq!(
            get_properties(&[Dec::Ptr, Dec::Arr(17), Dec::Ptr], ArithmeticType::Int),
            (None, 8)
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17)], ArithmeticType::Int),
            (Some(ArithmeticType::Int), 17 * 4)
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Int),
            (Some(ArithmeticType::Int), 17 * 7 * 4)
        );
        assert_eq!(
            get_properties(&[Dec::Arr(17), Dec::Ptr, Dec::Arr(7)], ArithmeticType::Int),
            (Some(ArithmeticType::ULong), 17 * 8)
        );
    }
}
