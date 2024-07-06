use crate::types_frontend::VarType;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AssemblyType {
    Longword, // 32-bit. Aka doubleword.
    Quadword, // 64-bit
    Double,
}
impl From<VarType> for AssemblyType {
    fn from(var_type: VarType) -> Self {
        match var_type {
            VarType::Int | VarType::UInt => Self::Longword,
            VarType::Long | VarType::ULong => Self::Quadword,
            VarType::Double => Self::Double,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
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
    pub fn default_of<T: Into<AssemblyType>>(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AssemblyType::Longword => Self::B4,
            AssemblyType::Quadword => Self::B8,
            AssemblyType::Double => Self::B8,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OperandByteLen {
    B1 = 1,
    B4 = 4,
    B8 = 8,
}
impl<T: Into<AssemblyType>> From<T> for OperandByteLen {
    fn from(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AssemblyType::Longword => Self::B4,
            AssemblyType::Quadword => Self::B8,
            AssemblyType::Double => Self::B8,
        }
    }
}
