use crate::{
    common::{
        identifier::SymbolIdentifier, primitive::Const,
        symbol_table_backend::StaticReadonlyAsmObjAttrs as ROAttrs,
        symbol_table_frontend::InitializerItem, types_backend::Alignment,
    },
    test::utils,
};
use anyhow::Result;

fn is_ident_generated(ident: &SymbolIdentifier) -> bool {
    match ident {
        SymbolIdentifier::Exact(_) => false,
        SymbolIdentifier::Generated { .. } => true,
    }
}

#[test]
fn static_double_objs() -> Result<()> {
    let pp = "
        int main(void) {
            double my_dbl_1 = 3.14;
            double my_dbl_2 = 3.14;

            -my_dbl_1;
            -my_dbl_2;
            -my_dbl_2;

            unsigned long my_ulong = my_dbl_1;
            (unsigned long) my_dbl_2;
        }
        ";

    let (_t_prog, be_symtab) = utils::compile_until_asm_gen(&pp)?;

    let (static_rw_objs, static_ro_objs) = utils::be_symtab_into_static_objs(be_symtab);

    assert!(static_rw_objs.is_empty());

    {
        let mut static_ro_objs = static_ro_objs.into_iter();
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B8);
            assert_eq!(initializer, InitializerItem::Single(Const::Double(3.14)));
        }
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B16);
            assert_eq!(initializer, InitializerItem::Single(Const::Double(-0.0)));
        }
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B8);
            assert_eq!(
                initializer,
                InitializerItem::Single(Const::Double((1u64 << 63) as f64))
            );
        }
        assert!(static_ro_objs.next().is_none());
    }

    Ok(())
}
