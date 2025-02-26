use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        symbol_table_backend::{
            StaticReadWriteAsmObjAttrs as RWAttrs, StaticReadonlyAsmObjAttrs as ROAttrs,
        },
        symbol_table_frontend::{InitializerString, StaticInitializerItem},
        types_backend::{Alignment, ByteLen},
    },
    test::utils,
};
use anyhow::Result;

fn is_ident_exact(ident: &SymbolIdentifier, expected_exact: &str) -> bool {
    match ident {
        SymbolIdentifier::Exact(raw_ident) => {
            let raw_ident = raw_ident.as_ref() as &str;
            raw_ident == expected_exact
        }
        SymbolIdentifier::Generated { .. } => false,
    }
}
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
            assert_eq!(
                initializer,
                StaticInitializerItem::Single(Const::Double(3.14))
            );
        }
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B16);
            assert_eq!(
                initializer,
                StaticInitializerItem::Single(Const::Double(-0.0))
            );
        }
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B8);
            assert_eq!(
                initializer,
                StaticInitializerItem::Single(Const::Double((1u64 << 63) as f64))
            );
        }
        assert!(static_ro_objs.next().is_none());
    }

    Ok(())
}

#[test]
fn static_string_objs() -> Result<()> {
    let pp = "
        char static_arr_ch_05[5]  = \"AABBB\";
        char static_arr_ch_21[21] = \"CCDDD\";

        char *static_ptr_ch_05 = \"aabbb\";
        char *static_ptr_ch_17 = \"ccccddddeeeefffff\";

        int main(void) {
            char rt_arr_ch_05[5]  = \"EEFFF\";
            char rt_arr_ch_21[21] = \"GGHHH\";

            char *rt_ptr_ch_05 = \"eefff\";
            char *rt_ptr_ch_17 = \"gggghhhhiiiijjjjj\";

            \"kklll\"[2];
        }
        ";

    let (_t_prog, be_symtab) = utils::compile_until_asm_gen(&pp)?;

    let (static_rw_objs, static_ro_objs) = utils::be_symtab_into_static_objs(be_symtab);

    {
        let mut static_rw_objs = static_rw_objs.into_iter();
        {
            let (ident, RWAttrs { alignment, initializer, .. }) = static_rw_objs.next().unwrap();
            assert!(is_ident_exact(&ident, "static_arr_ch_05"));
            assert_eq!(alignment, Alignment::B1);
            assert_eq!(
                initializer,
                Some(vec![StaticInitializerItem::String(InitializerString {
                    chars: "AABBB".into(),
                    zeros_sfx_bytelen: ByteLen::new(0)
                })])
            );
        }
        {
            let (ident, RWAttrs { alignment, initializer, .. }) = static_rw_objs.next().unwrap();
            assert!(is_ident_exact(&ident, "static_arr_ch_21"));
            assert_eq!(alignment, Alignment::B16);
            assert_eq!(
                initializer,
                Some(vec![StaticInitializerItem::String(InitializerString {
                    chars: "CCDDD".into(),
                    zeros_sfx_bytelen: ByteLen::new(21 - 5)
                })])
            );
        }
        {
            let (ident, RWAttrs { alignment, initializer, .. }) = static_rw_objs.next().unwrap();
            assert!(is_ident_exact(&ident, "static_ptr_ch_05"));
            assert_eq!(alignment, Alignment::B8);
            assert!(matches!(
                &initializer.unwrap()[..],
                [
                    StaticInitializerItem::Pointer(rhs_ident)
                ] if is_ident_generated(rhs_ident)
            ));
        }
        {
            let (ident, RWAttrs { alignment, initializer, .. }) = static_rw_objs.next().unwrap();
            assert!(is_ident_exact(&ident, "static_ptr_ch_17"));
            assert_eq!(alignment, Alignment::B8);
            assert!(matches!(
                &initializer.unwrap()[..],
                [
                    StaticInitializerItem::Pointer(rhs_ident)
                ] if is_ident_generated(rhs_ident)
            ));
        }
        assert!(static_rw_objs.next().is_none());
    }

    {
        let mut static_ro_objs = static_ro_objs.into_iter();
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B1);
            assert_eq!(
                initializer,
                StaticInitializerItem::String(InitializerString {
                    chars: "aabbb".into(),
                    zeros_sfx_bytelen: ByteLen::new(1)
                }),
            );
        }
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B1);
            assert_eq!(
                initializer,
                StaticInitializerItem::String(InitializerString {
                    chars: "ccccddddeeeefffff".into(),
                    zeros_sfx_bytelen: ByteLen::new(1)
                }),
            );
        }
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B1);
            assert_eq!(
                initializer,
                StaticInitializerItem::String(InitializerString {
                    chars: "eefff".into(),
                    zeros_sfx_bytelen: ByteLen::new(1)
                }),
            );
        }
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B1);
            assert_eq!(
                initializer,
                StaticInitializerItem::String(InitializerString {
                    chars: "gggghhhhiiiijjjjj".into(),
                    zeros_sfx_bytelen: ByteLen::new(1)
                }),
            );
        }
        {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.next().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B1);
            assert_eq!(
                initializer,
                StaticInitializerItem::String(InitializerString {
                    chars: "kklll".into(),
                    zeros_sfx_bytelen: ByteLen::new(1)
                }),
            );
        }
        assert!(static_ro_objs.next().is_none());
    }

    Ok(())
}
