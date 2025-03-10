use crate::common::types_frontend::{ArithmeticType, ArrayType, NonVoidType, ScalarType};
use derive_more::{Add, AddAssign, Constructor, From};
use std::ops::Mul;

#[derive(From, Debug)]
pub enum AssemblyType {
    Scalar(ScalarAssemblyType),
    ByteArray(ByteArrayAssemblyType),
}
impl From<&NonVoidType> for AssemblyType {
    fn from(nonvoid_typ: &NonVoidType) -> Self {
        match nonvoid_typ {
            NonVoidType::Scalar(s) => Self::Scalar(ScalarAssemblyType::from(s.as_ref())),
            NonVoidType::Array(a) => Self::ByteArray(ByteArrayAssemblyType::from(a.as_ref())),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ScalarAssemblyType {
    Byte,
    Longword, // 32-bit. Aka doubleword.
    Quadword, // 64-bit
    Double,
}
impl From<ArithmeticType> for ScalarAssemblyType {
    fn from(ari_type: ArithmeticType) -> Self {
        use ArithmeticType as AT;
        match ari_type {
            AT::Char | AT::SChar | AT::UChar => Self::Byte,
            AT::Int | AT::UInt => Self::Longword,
            AT::Long | AT::ULong => Self::Quadword,
            AT::Double => Self::Double,
        }
    }
}
impl From<&ScalarType> for ScalarAssemblyType {
    fn from(sca_typ: &ScalarType) -> Self {
        let ari_typ = sca_typ.effective_arithmetic_type();
        Self::from(ari_typ)
    }
}

#[derive(Debug)]
pub struct ByteArrayAssemblyType(pub ByteLen, pub Alignment);
impl From<&ArrayType> for ByteArrayAssemblyType {
    fn from(arr_typ: &ArrayType) -> Self {
        let bytelen = *arr_typ.bytelen();
        let align = Alignment::of_arr_type(arr_typ);
        Self(bytelen, align)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, Debug)]
pub enum Alignment {
    B1 = 1,
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
            ScalarAssemblyType::Byte => Self::B1,
            ScalarAssemblyType::Longword => Self::B4,
            ScalarAssemblyType::Quadword => Self::B8,
            ScalarAssemblyType::Double => Self::B8,
        }
    }

    pub fn default_of_nonvoid_type(nonvoid_typ: &NonVoidType) -> Self {
        match nonvoid_typ {
            NonVoidType::Scalar(s) => Self::default_of_scalar(ScalarAssemblyType::from(s.as_ref())),
            NonVoidType::Array(a) => Self::of_arr_type(a),
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
            ScalarAssemblyType::Byte => Self::B1,
            ScalarAssemblyType::Longword => Self::B4,
            ScalarAssemblyType::Quadword => Self::B8,
            ScalarAssemblyType::Double => Self::B8,
        }
    }
}

/// The amount of a blob, in the unit of bytes.
///
/// We avoid the overloaded terminology "size".
#[derive(Constructor, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Add, AddAssign, Debug)]
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
