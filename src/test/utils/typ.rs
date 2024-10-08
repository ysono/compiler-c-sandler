use crate::common::types_frontend::{ArithmeticType, VarType};

#[derive(PartialEq, Eq, Debug)]
pub struct ProtoType {
    pub items_baseward: Vec<TestDeclaratorItem>,
    pub base_type: ArithmeticType,
}
#[derive(PartialEq, Eq, Debug)]
pub enum TestDeclaratorItem {
    Ptr,
}

/// This decomposition enables [`VarType`]s to be compared easily.
///
/// For two [`Singleton`]s to compare as equal, they must be produced by the same [`SingletonRepository`].
/// But the repository used by the compiler is private.
pub fn decompose_var_type(var_typ: &VarType) -> ProtoType {
    let mut items_baseward = vec![];
    let base_type = do_decompose_var_type(var_typ, &mut items_baseward);
    ProtoType { items_baseward, base_type }
}
fn do_decompose_var_type(var_typ: &VarType, items: &mut Vec<TestDeclaratorItem>) -> ArithmeticType {
    match var_typ {
        VarType::Arithmetic(a) => *a,
        VarType::Pointer(pointee_type) => {
            items.push(TestDeclaratorItem::Ptr);
            do_decompose_var_type(pointee_type, items)
        }
    }
}
