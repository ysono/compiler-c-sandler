use crate::common::types_frontend::{ArithmeticType, ScalarType};
use std::{
    hash::{Hash, Hasher},
    mem,
};

#[derive(Clone, Copy, Debug)]
pub enum Const {
    Char(i8),
    UChar(u8),
    Int(i32),
    Long(i64),
    UInt(u32),
    ULong(u64),
    Double(f64),
}
impl Const {
    /// @return a constant whose bitwise representation is `0b000...000`
    pub fn new_zero_bits(typ: ArithmeticType) -> Const {
        Const::Int(0).cast_at_compile_time(&typ.into())
    }

    pub fn cast_at_compile_time(&self, typ: &ScalarType) -> Const {
        macro_rules! new_const {
            ( $out_konst_variant:expr, $out_prim:ty ) => {
                match self {
                    Const::Char(i) => $out_konst_variant(*i as $out_prim),
                    Const::UChar(i) => $out_konst_variant(*i as $out_prim),
                    Const::Int(i) => $out_konst_variant(*i as $out_prim),
                    Const::Long(i) => $out_konst_variant(*i as $out_prim),
                    Const::UInt(i) => $out_konst_variant(*i as $out_prim),
                    Const::ULong(i) => $out_konst_variant(*i as $out_prim),
                    Const::Double(f) => $out_konst_variant(*f as $out_prim),
                }
            };
        }
        match typ {
            ScalarType::Arith(a) => match a {
                ArithmeticType::Char => new_const!(Const::Char, i8),
                ArithmeticType::SChar => new_const!(Const::Char, i8),
                ArithmeticType::UChar => new_const!(Const::UChar, u8),
                ArithmeticType::Int => new_const!(Const::Int, i32),
                ArithmeticType::Long => new_const!(Const::Long, i64),
                ArithmeticType::UInt => new_const!(Const::UInt, u32),
                ArithmeticType::ULong => new_const!(Const::ULong, u64),
                ArithmeticType::Double => new_const!(Const::Double, f64),
            },
            ScalarType::Ptr(_) => new_const!(Const::ULong, u64),
        }
    }

    pub fn as_bits(&self) -> i64 {
        #[allow(clippy::unnecessary_cast)]
        match self {
            Const::Char(i) => *i as i64,
            Const::UChar(i) => *i as i64,
            Const::Int(i) => *i as i64,
            Const::Long(i) => *i as i64,
            Const::UInt(i) => *i as i64,
            Const::ULong(i) => *i as i64,
            Const::Double(f) => f.to_bits() as i64,
        }
    }

    pub fn arithmetic_type(&self) -> ArithmeticType {
        match self {
            Const::Char(_) => ArithmeticType::Char,
            Const::UChar(_) => ArithmeticType::UChar,
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
            Const::Char(0)
                | Const::UChar(0)
                | Const::Int(0)
                | Const::Long(0)
                | Const::UInt(0)
                | Const::ULong(0)
        )
    }
}
impl PartialEq<Self> for Const {
    /// Between two Doubles,
    ///     + cause +0.0 and -0.0 to _not_ equal each other.
    ///     + cause two f64::NAN instances having the same bitwise repr, to equal each other.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::UChar(l0), Self::UChar(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Long(l0), Self::Long(r0)) => l0 == r0,
            (Self::UInt(l0), Self::UInt(r0)) => l0 == r0,
            (Self::ULong(l0), Self::ULong(r0)) => l0 == r0,
            (Self::Double(l0), Self::Double(r0)) => l0.to_bits() == r0.to_bits(),
            _ => false,
        }
    }
}
impl Eq for Const {}
impl Hash for Const {
    fn hash<H: Hasher>(&self, state: &mut H) {
        mem::discriminant(self).hash(state);
        match self {
            Const::Char(i) => i.hash(state),
            Const::UChar(i) => i.hash(state),
            Const::Int(i) => i.hash(state),
            Const::Long(i) => i.hash(state),
            Const::UInt(i) => i.hash(state),
            Const::ULong(i) => i.hash(state),
            Const::Double(f) => f.to_bits().hash(state),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test::utils::hash;

    #[test]
    fn const_f64() {
        assert_eq!(0.0f64, -0.0f64);

        assert_ne!(f64::NAN, f64::NAN);

        assert_ne!(Const::Double(0.0), Const::Double(-0.0));
        assert_ne!(hash(Const::Double(0.0)), hash(Const::Double(-0.0)));

        assert_eq!(Const::Double(f64::NAN), Const::Double(f64::NAN));
        assert_eq!(hash(Const::Double(f64::NAN)), hash(Const::Double(f64::NAN)));
    }
}
