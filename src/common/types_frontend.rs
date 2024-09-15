use crate::ds_n_a::singleton::Singleton;
use anyhow::{anyhow, Result};
use derive_more::From;
use std::hash::{Hash, Hasher};
use std::mem;

pub enum Type {
    Var(Singleton<VarType>),
    Fun(Singleton<FunType>),
}

#[derive(From, PartialEq, Eq, Hash, Debug)]
pub enum VarType {
    Arithmetic(ArithmeticType),
    Pointer(Singleton<VarType>), // We don't support function-pointers.
}
impl VarType {
    // TOOD check all usages of this method
    pub fn effective_arithmetic_type(&self) -> ArithmeticType {
        match self {
            Self::Arithmetic(a) => *a,
            Self::Pointer(_) => ArithmeticType::ULong,
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
pub struct FunType {
    pub params: Vec<Singleton<VarType>>,
    pub ret: Singleton<VarType>,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Const {
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}
impl Const {
    /// @return a constant whose bitwise representation is `0b000...000`
    pub fn new_zero_bits(typ: &VarType) -> Const {
        Const::Int(0).cast_at_compile_time(typ).unwrap()
    }

    pub fn cast_at_compile_time(&self, typ: &VarType) -> Result<Const> {
        macro_rules! new_const {
            ( $out_konst_variant:expr, $out_prim:ty ) => {
                match self {
                    Const::Int(i) => $out_konst_variant(*i as $out_prim),
                    Const::Long(i) => $out_konst_variant(*i as $out_prim),
                    Const::UInt(i) => $out_konst_variant(*i as $out_prim),
                    Const::ULong(i) => $out_konst_variant(*i as $out_prim),
                    Const::Double(f) => $out_konst_variant(*f as $out_prim),
                }
            };
        }
        let out_konst = match typ {
            VarType::Arithmetic(a) => match a {
                ArithmeticType::Int => new_const!(Const::Int, i32),
                ArithmeticType::Long => new_const!(Const::Long, i64),
                ArithmeticType::UInt => new_const!(Const::UInt, u32),
                ArithmeticType::ULong => new_const!(Const::ULong, u64),
                ArithmeticType::Double => new_const!(Const::Double, f64),
            },
            VarType::Pointer(_) => {
                if self.is_zero_integer() {
                    new_const!(Const::ULong, u64)
                } else {
                    return Err(anyhow!("Cannot cast {self:?} to {typ:?}"));
                }
            }
        };
        Ok(out_konst)
    }

    pub fn as_bits(&self) -> i64 {
        #[allow(clippy::unnecessary_cast)]
        match self {
            Const::Int(i) => *i as i64,
            Const::Long(i) => *i as i64,
            Const::UInt(i) => *i as i64,
            Const::ULong(i) => *i as i64,
            Const::Double(f) => f.to_bits() as i64,
        }
    }

    pub fn arithmetic_type(&self) -> ArithmeticType {
        match self {
            Const::Int(_) => ArithmeticType::Int,
            Const::Long(_) => ArithmeticType::Long,
            Const::UInt(_) => ArithmeticType::UInt,
            Const::ULong(_) => ArithmeticType::ULong,
            Const::Double(_) => ArithmeticType::Double,
        }
    }

    pub fn is_zero_integer(&self) -> bool {
        matches!(
            self,
            Const::Int(0) | Const::Long(0) | Const::UInt(0) | Const::ULong(0)
        )
    }
}
/// Impl'ing `Eq` for `Const` is desirable in the context of HashMap.
/// We ought to write a custom `PartialEq` impl, which compares two `Const::Double(_)` instances by comparing their `f64` values bitwise.
/// But, we omit it, for brevity.
/// 2+ instances of `Const::Double(f64::NAN)` are unequal with one another. All other comparisons work as expected.
impl Eq for Const {}
impl Hash for Const {
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            Const::Int(i) => i.hash(state),
            Const::Long(i) => i.hash(state),
            Const::UInt(i) => i.hash(state),
            Const::ULong(i) => i.hash(state),
            Const::Double(f) => f.to_bits().hash(state),
        }
    }
}
