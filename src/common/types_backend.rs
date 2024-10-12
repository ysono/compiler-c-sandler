use crate::common::types_frontend::{ArithmeticType, ArrayType, ObjType, ScalarType};
use derive_more::{Add, AddAssign, Constructor, From};
use std::{borrow::Borrow, ops::Mul};

#[derive(From, Debug)]
pub enum AssemblyType {
    Scalar(ScalarAssemblyType),
    ByteArray(ByteArrayAssemblyType),
}
impl<Ot: Borrow<ObjType>> From<Ot> for AssemblyType {
    fn from(obj_typ: Ot) -> Self {
        match obj_typ.borrow() {
            ObjType::Scalar(sca_typ) => Self::Scalar(ScalarAssemblyType::from(sca_typ)),
            ObjType::Array(arr_typ) => Self::ByteArray(ByteArrayAssemblyType::from(arr_typ)),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ScalarAssemblyType {
    Longword, // 32-bit. Aka doubleword.
    Quadword, // 64-bit
    Double,
}
impl From<ArithmeticType> for ScalarAssemblyType {
    fn from(ari_type: ArithmeticType) -> Self {
        use ArithmeticType as AT;
        match ari_type {
            AT::Int | AT::UInt => Self::Longword,
            AT::Long | AT::ULong => Self::Quadword,
            AT::Double => Self::Double,
        }
    }
}
impl<St: Borrow<ScalarType>> From<St> for ScalarAssemblyType {
    fn from(sca_typ: St) -> Self {
        let ari_typ = sca_typ.borrow().effective_arithmetic_type();
        Self::from(ari_typ)
    }
}

#[derive(Debug)]
pub struct ByteArrayAssemblyType(pub ByteLen, pub Alignment);
impl<At: Borrow<ArrayType>> From<At> for ByteArrayAssemblyType {
    fn from(arr_typ: At) -> Self {
        let arr_typ = arr_typ.borrow();

        let bytelen = *arr_typ.bytelen();
        let align = Alignment::of_arr_type(arr_typ);
        Self(bytelen, align)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Alignment {
    B4 = 4,
    B8 = 8,
    B16 = 16,
}
impl Alignment {
    /// The required alignment, in stack and in asm sections (`.data`, `.rodata`, `.literal8`, ...),
    ///   according to System V x64 ABI.
    /// On any individual item, the actual declared alignment may be a multiple of this basic amount,
    ///   eg b/c an instruction accessing it requires the operand to be aligned a certain amount.
    pub fn default_of_scalar<T: Into<ScalarAssemblyType>>(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            ScalarAssemblyType::Longword => Self::B4,
            ScalarAssemblyType::Quadword => Self::B8,
            ScalarAssemblyType::Double => Self::B8,
        }
    }

    pub fn default_of_obj_type(obj_typ: &ObjType) -> Self {
        match obj_typ {
            ObjType::Scalar(sca_typ) => Self::default_of_scalar(ScalarAssemblyType::from(sca_typ)),
            ObjType::Array(arr_typ) => Self::of_arr_type(arr_typ),
        }
    }

    /// This alignment is required for SSE instructions to operate on array elements in parallel.
    /// It's also mandated by the System V ABI, regardless of whether any SSE instructions will operate on a given array.
    /// The requirement applies to array-typed variables, not to nested array-typed elements.
    pub fn of_arr_type(arr_typ: &ArrayType) -> Self {
        if arr_typ.bytelen().as_int() < 16 {
            let single_typ = arr_typ.single_type();
            Self::default_of_scalar(*single_typ)
        } else {
            Self::B16
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OperandByteLen {
    B1 = 1,
    B4 = 4,
    B8 = 8,
}
impl<T: Into<ScalarAssemblyType>> From<T> for OperandByteLen {
    fn from(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            ScalarAssemblyType::Longword => Self::B4,
            ScalarAssemblyType::Quadword => Self::B8,
            ScalarAssemblyType::Double => Self::B8,
        }
    }
}

#[derive(Constructor, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Add, AddAssign, Debug)]
pub struct ByteLen(u64);
impl<T: Into<OperandByteLen>> From<T> for ByteLen {
    fn from(t: T) -> Self {
        let bytelen = t.into() as u64;
        Self(bytelen)
    }
}
impl Mul<u64> for ByteLen {
    type Output = Self;
    fn mul(self, rhs: u64) -> Self {
        Self(self.0 * rhs)
    }
}
impl ByteLen {
    pub fn as_int(&self) -> u64 {
        self.0
    }
}
