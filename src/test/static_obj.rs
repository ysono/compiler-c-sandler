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
use std::{collections::VecDeque, rc::Rc};

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
        fn expect_ro_double(
            static_ro_objs: &mut VecDeque<(Rc<SymbolIdentifier>, ROAttrs)>,
            expected_alignment: Alignment,
            expected_double: f64,
        ) {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.pop_front().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, expected_alignment);
            assert_eq!(
                initializer,
                StaticInitializerItem::Single(Const::Double(expected_double))
            );
        }

        let mut static_ro_objs = VecDeque::from(static_ro_objs);
        expect_ro_double(&mut static_ro_objs, Alignment::B8, 3.14);
        expect_ro_double(&mut static_ro_objs, Alignment::B16, -0.0);
        expect_ro_double(&mut static_ro_objs, Alignment::B8, (1u64 << 63) as f64);
        assert!(static_ro_objs.is_empty());
    }

    Ok(())
}

#[test]
fn static_string_objs() -> Result<()> {
    let pp = r#"
        char static_arr_char_05[5]  = "AABBB";
        char static_arr_char_21[21] = "CCDDD";

        char *static_ptr_char_05 = "aabbb";
        char *static_ptr_char_17 = "ccccddddeeeefffff";

        int main(void) {
            char rt_arr_char_05[5]  = "EEFFF";
            char rt_arr_char_21[21] = "GGHHH";

            char *rt_ptr_char_05 = "eefff";
            char *rt_ptr_char_17 = "gggghhhhiiiijjjjj";

            "kklll"
            "mmnnn"[2];

            "AABBB"[2];
            "aabbb"[2];
            "EEFFF"[2];
            "eefff"[2];
        }
        "#;

    let (_t_prog, be_symtab) = utils::compile_until_asm_gen(&pp)?;

    let (static_rw_objs, static_ro_objs) = utils::be_symtab_into_static_objs(be_symtab);

    {
        fn expect_rw(
            static_rw_objs: &mut VecDeque<(Rc<SymbolIdentifier>, RWAttrs)>,
            expected_ident: &'static str,
            expected_alignment: Alignment,
        ) -> Vec<StaticInitializerItem> {
            let (ident, RWAttrs { alignment, initializer, .. }) =
                static_rw_objs.pop_front().unwrap();
            assert!(is_ident_exact(&ident, expected_ident));
            assert_eq!(alignment, expected_alignment);
            initializer.unwrap()
        }

        let mut static_rw_objs = VecDeque::from(static_rw_objs);
        {
            let inits = expect_rw(&mut static_rw_objs, "static_arr_char_05", Alignment::B1);
            assert_eq!(
                inits,
                vec![StaticInitializerItem::String(InitializerString {
                    chars: "AABBB".into(),
                    zeros_sfx_bytelen: ByteLen::new(0)
                })]
            );
        }
        {
            let inits = expect_rw(&mut static_rw_objs, "static_arr_char_21", Alignment::B16);
            assert_eq!(
                inits,
                vec![StaticInitializerItem::String(InitializerString {
                    chars: "CCDDD".into(),
                    zeros_sfx_bytelen: ByteLen::new(21 - 5)
                })]
            );
        }
        {
            let inits = expect_rw(&mut static_rw_objs, "static_ptr_char_05", Alignment::B8);
            assert!(matches!(
                &inits[..],
                [
                    StaticInitializerItem::Pointer(rhs_ident)
                ] if is_ident_generated(rhs_ident)
            ));
        }
        {
            let inits = expect_rw(&mut static_rw_objs, "static_ptr_char_17", Alignment::B8);
            assert!(matches!(
                &inits[..],
                [
                    StaticInitializerItem::Pointer(rhs_ident)
                ] if is_ident_generated(rhs_ident)
            ));
        }
        assert!(static_rw_objs.is_empty());
    }

    {
        fn expect_ro(
            static_ro_objs: &mut VecDeque<(Rc<SymbolIdentifier>, ROAttrs)>,
            expected_chars: &'static str,
        ) {
            let (ident, ROAttrs { alignment, initializer }) = static_ro_objs.pop_front().unwrap();
            assert!(is_ident_generated(&ident));
            assert_eq!(alignment, Alignment::B1);
            assert_eq!(
                initializer,
                StaticInitializerItem::String(InitializerString {
                    chars: expected_chars.into(),
                    zeros_sfx_bytelen: ByteLen::new(1)
                })
            );
        }

        let mut static_ro_objs = VecDeque::from(static_ro_objs);
        expect_ro(&mut static_ro_objs, "aabbb");
        expect_ro(&mut static_ro_objs, "ccccddddeeeefffff");
        expect_ro(&mut static_ro_objs, "eefff");
        expect_ro(&mut static_ro_objs, "gggghhhhiiiijjjjj");
        expect_ro(&mut static_ro_objs, "kklllmmnnn");
        expect_ro(&mut static_ro_objs, "AABBB");
        expect_ro(&mut static_ro_objs, "aabbb");
        expect_ro(&mut static_ro_objs, "EEFFF");
        expect_ro(&mut static_ro_objs, "eefff");
        assert!(static_ro_objs.is_empty());
    }

    Ok(())
}
