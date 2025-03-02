use crate::{
    common::types_frontend::{
        ArithmeticType, ArrayElementCount, ArrayType, NonVoidType, ObjType, PointerType, ScalarType,
    },
    ds_n_a::singleton::{Singleton, SingletonRepository},
};
use derive_more::Constructor;

#[derive(Debug)]
pub struct ProtoType {
    pub items_baseward: Vec<TestDeclaratorItem>,
    pub base_type: ArithmeticType,
}
#[derive(Debug)]
pub enum TestDeclaratorItem {
    Ptr,
    Arr(u64), // Elem count.
}

#[derive(Constructor)]
pub struct TypeBuilder<'a> {
    obj_typ_repo: &'a mut SingletonRepository<ObjType>,
}
impl<'a> TypeBuilder<'a> {
    pub fn build_obj_type(
        &mut self,
        in_items_baseward: &[TestDeclaratorItem],
        base_typ: ArithmeticType,
    ) -> Singleton<ObjType> {
        let mut cur_typ = self.obj_typ_repo.get_or_new(base_typ.into());

        for in_item in in_items_baseward.iter().rev() {
            match in_item {
                TestDeclaratorItem::Ptr => {
                    cur_typ = self
                        .obj_typ_repo
                        .get_or_new(PointerType { pointee_type: cur_typ }.into());
                }
                TestDeclaratorItem::Arr(elem_count) => {
                    cur_typ = self.obj_typ_repo.get_or_new(
                        ArrayType::new(
                            NonVoidType::try_from(cur_typ).unwrap(),
                            ArrayElementCount::new(*elem_count),
                        )
                        .into(),
                    );
                }
            }
        }

        cur_typ
    }
}

/// To assert the content of an instance of [`Singleton<ObjType>`] that's constructed by the compiler's private [`SingletonRepository`],
/// compare that [`Singleton<ObjType>`] against a locally-constructed [`ProtoType`].
impl PartialEq<ProtoType> for &ObjType {
    fn eq(&self, proto_typ: &ProtoType) -> bool {
        let mut obj_typ: &ObjType = *self;

        let mut proto_items_sfx = proto_typ.items_baseward.iter();

        loop {
            match (obj_typ, proto_items_sfx.next()) {
                (ObjType::Scalar(ScalarType::Arith(a)), None) => {
                    return *a == proto_typ.base_type;
                }
                (
                    ObjType::Scalar(ScalarType::Ptr(PointerType { pointee_type })),
                    Some(TestDeclaratorItem::Ptr),
                ) => {
                    obj_typ = pointee_type;
                }
                (ObjType::Array(arr_typ), Some(TestDeclaratorItem::Arr(elem_count)))
                    if arr_typ.elem_count().as_int() == *elem_count =>
                {
                    obj_typ = match arr_typ.elem_type() {
                        NonVoidType::Scalar(s) => s.as_owner(),
                        NonVoidType::Array(a) => a.as_owner(),
                    };
                }
                _ => return false,
            }
        }
    }
}
