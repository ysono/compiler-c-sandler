use super::TypeChecker;
use crate::common::{
    identifier::SymbolIdentifier,
    symbol_table_frontend::InitializerString,
    types_backend::ByteLen,
    types_frontend::{ArithmeticType, ArrayElementCount, ArrayType, NonVoidType},
};
use std::rc::Rc;

impl TypeChecker {
    pub(super) fn define_static_readonly_string(
        &mut self,
        chars: Vec<u8>,
    ) -> (Rc<SymbolIdentifier>, NonVoidType) {
        /* Unconditionally include one terminating '\0' char. */
        let zeros_sfx_bytelen = ByteLen::new(1);

        let typ = self.derive_string_type(&chars, zeros_sfx_bytelen);
        let initializer = InitializerString { chars, zeros_sfx_bytelen };

        let ident = self
            .frontend_symtab
            .get_or_new_static_readonly_string(initializer, &typ);

        (ident, typ)
    }
    fn derive_string_type(&mut self, chars: &[u8], tail_bytelen: ByteLen) -> NonVoidType {
        let elem_type = self.obj_type_repo.get_or_new(ArithmeticType::Char.into());
        let elem_type = NonVoidType::try_from(elem_type).unwrap();

        let elem_ct = ArrayElementCount::new(chars.len() as u64 + tail_bytelen.as_int());

        let arr_type = self
            .obj_type_repo
            .get_or_new(ArrayType::new(elem_type, elem_ct).into());
        NonVoidType::try_from(arr_type).unwrap()
    }
}
