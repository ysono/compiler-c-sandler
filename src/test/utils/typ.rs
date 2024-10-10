use crate::common::types_frontend::{ArithmeticType, ObjType, PointerType, ScalarType};

#[derive(PartialEq, Eq, Debug)]
pub struct ProtoType {
    pub items_baseward: Vec<TestDeclaratorItem>,
    pub base_type: ArithmeticType,
}
#[derive(PartialEq, Eq, Debug)]
pub enum TestDeclaratorItem {
    Ptr,
}

/// This decomposition enables [`ObjType`]s to be compared easily.
///
/// For two [`Singleton`]s to compare as equal, they must be produced by the same [`SingletonRepository`].
/// But the repository used by the compiler is private.
pub fn decompose_obj_type(obj_typ: &ObjType) -> ProtoType {
    let mut items_baseward = vec![];
    let base_type = do_decompose_obj_type(obj_typ, &mut items_baseward);
    ProtoType { items_baseward, base_type }
}
fn do_decompose_obj_type(obj_typ: &ObjType, items: &mut Vec<TestDeclaratorItem>) -> ArithmeticType {
    match obj_typ {
        ObjType::Scalar(s) => match s {
            ScalarType::Arith(a) => *a,
            ScalarType::Ptr(PointerType { pointee_type }) => {
                items.push(TestDeclaratorItem::Ptr);
                do_decompose_obj_type(pointee_type, items)
            }
        },
    }
}
