use crate::types_frontend::VarType;

#[derive(Clone, Copy, Debug)]
pub enum AssemblyType {
    Longword, // 32-bit. Aka doubleword.
    Quadword, // 64-bit
}
impl From<VarType> for AssemblyType {
    fn from(var_type: VarType) -> Self {
        match var_type {
            VarType::Int | VarType::UInt => Self::Longword,
            VarType::Long | VarType::ULong => Self::Quadword,
            VarType::Double => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Alignment {
    B4 = 4,
    B8 = 8,
}
impl<T: Into<AssemblyType>> From<T> for Alignment {
    fn from(t: T) -> Self {
        let asm_type = t.into();
        match asm_type {
            AssemblyType::Longword => Self::B4,
            AssemblyType::Quadword => Self::B8,
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
        }
    }
}
