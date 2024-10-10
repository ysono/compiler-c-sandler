use crate::{
    common::types_frontend::{
        ArithmeticType, ArrayElementCount, ArrayType, ObjType, PointerType, ScalarType,
    },
    ds_n_a::singleton::{Singleton, SingletonRepository},
};

#[derive(PartialEq, Eq, Debug)]
pub struct ProtoType {
    pub items_baseward: Vec<TestDeclaratorItem>,
    pub base_type: ArithmeticType,
}
#[derive(PartialEq, Eq, Debug)]
pub enum TestDeclaratorItem {
    Ptr,
    Arr(u64), // Elem count.
}

#[derive(Default)]
pub struct TypeBuilder {
    obj_typ_repo: SingletonRepository<ObjType>,
}
impl TypeBuilder {
    pub fn build_obj_type(
        &mut self,
        in_items_outward: &[TestDeclaratorItem],
        base_typ: ArithmeticType,
    ) -> Singleton<ObjType> {
        let mut cur_typ = self.obj_typ_repo.get_or_new(base_typ.into());

        for in_item in in_items_outward.iter().rev() {
            match in_item {
                TestDeclaratorItem::Ptr => {
                    cur_typ = self
                        .obj_typ_repo
                        .get_or_new(PointerType { pointee_type: cur_typ }.into());
                }
                TestDeclaratorItem::Arr(elem_count) => {
                    cur_typ = self.obj_typ_repo.get_or_new(
                        ArrayType::new(cur_typ, ArrayElementCount::new(*elem_count)).into(),
                    );
                }
            }
        }

        cur_typ
    }
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
        ObjType::Array(arr_typ) => {
            items.push(TestDeclaratorItem::Arr(arr_typ.elem_count().as_int()));
            do_decompose_obj_type(arr_typ.elem_type(), items)
        }
    }
}
