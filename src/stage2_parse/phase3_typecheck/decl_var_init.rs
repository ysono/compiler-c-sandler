use super::TypeChecker;
use crate::{
    common::{
        symbol_table_frontend::{
            InitializerItem, InitializerString, StaticInitializer, StaticInitializerItem,
        },
        types_backend::ByteLen,
        types_frontend::{NonVoidType, ObjType, ScalarType, SubObjType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{Result, anyhow};
use std::borrow::Cow;

impl TypeChecker {
    pub(super) fn generate_zero_static_initializer(typ: &ObjType) -> StaticInitializer {
        let bytelen = typ.bytelen();
        let item = StaticInitializerItem::Zero(bytelen);
        StaticInitializer::Concrete(vec![item])
    }

    pub(super) fn typecheck_initializer_static(
        &mut self,
        typ: &Singleton<ObjType>,
        init: VariableInitializer<ResolvedCAst>,
    ) -> Result<Vec<StaticInitializerItem>> {
        let mut typecheck_single = |typ: SubObjType<ScalarType>, exp: Expression<ResolvedCAst>| {
            self.typecheck_initializer_static_single(typ, exp)
        };

        let mut inits = vec![];
        Self::typecheck_initializer(typ, init, &mut typecheck_single, &mut inits)?;
        Ok(inits)
    }
    fn typecheck_initializer_static_single(
        &mut self,
        to: SubObjType<ScalarType>,
        from: Expression<ResolvedCAst>,
    ) -> Result<StaticInitializerItem> {
        match &from {
            Expression::R(RExp::Const(_)) => noop!(),
            Expression::L(LExp::String(_)) => noop!(),
            _ => {
                return Err(anyhow!(
                    "Within each static initializer, each single expression must be a constexpr. For each constexpr, only a literal is supported."
                ));
            }
        };

        let from = self.typecheck_exp_and_convert_to_scalar(from)?;

        let () = Self::can_cast_by_assignment(&to, &from)?;

        let item = match from {
            TypedExp::R(TypedRExp { exp, .. }) => match exp {
                RExp::Const(in_konst) => {
                    let out_konst = in_konst.cast_at_compile_time(&to);

                    if out_konst.is_zero_integer() {
                        let bytelen = ByteLen::from(out_konst.arithmetic_type());
                        StaticInitializerItem::Zero(bytelen)
                    } else {
                        /* If double, even if val==0.0, we encode a Single item.
                        This helps inform us later to write it to `.data` instead of `.bss`. */
                        StaticInitializerItem::Single(out_konst)
                    }
                }
                RExp::AddrOf(AddrOf(sub_exp)) => match *sub_exp {
                    TypedLExp {
                        exp: LExp::String(static_obj_ident),
                        ..
                    } => StaticInitializerItem::Pointer(static_obj_ident),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        Ok(item)
    }

    pub(super) fn typecheck_initializer_runtime(
        &mut self,
        typ: &Singleton<ObjType>,
        init: VariableInitializer<ResolvedCAst>,
    ) -> Result<Vec<RuntimeInitializerItem>> {
        let mut typecheck_single = |typ: SubObjType<ScalarType>, exp: Expression<ResolvedCAst>| {
            self.cast_by_assignment(Cow::Owned(typ), exp)
                .map(RuntimeInitializerItem::Single)
        };

        let mut inits = vec![];
        Self::typecheck_initializer(typ, init, &mut typecheck_single, &mut inits)?;
        Ok(inits)
    }

    fn typecheck_initializer<F, Sngl, Ptr>(
        typ: &Singleton<ObjType>,
        init: VariableInitializer<ResolvedCAst>,
        typecheck_single: &mut F,
        out_items: &mut Vec<InitializerItem<Sngl, Ptr>>,
    ) -> Result<()>
    where
        F: FnMut(
            SubObjType<ScalarType>,
            Expression<ResolvedCAst>,
        ) -> Result<InitializerItem<Sngl, Ptr>>,
    {
        match (typ.as_ref(), init) {
            (ObjType::Void, _) => todo!(),
            (ObjType::Scalar(_), VariableInitializer::Single(exp)) => {
                let sca_typ = Self::extract_scalar_type(typ.clone()).unwrap();
                let item = typecheck_single(sca_typ, exp)?;
                Self::push_initializer_item(out_items, item);
            }
            (ObjType::Array(arr_typ), VariableInitializer::Compound(sub_inits)) => {
                let elems_ct = arr_typ.elem_count().as_int();
                let sub_inits_ct = sub_inits.len() as u64;
                if elems_ct < sub_inits_ct {
                    return Err(anyhow!(
                        "Too many initializer elements. {arr_typ:#?} vs {sub_inits:#?}"
                    ));
                }

                for sub_init in sub_inits {
                    let elem_type: Singleton<ObjType> = arr_typ.elem_type().clone().into();
                    Self::typecheck_initializer(&elem_type, sub_init, typecheck_single, out_items)?;
                }

                if elems_ct > sub_inits_ct {
                    let fill_bytelen = arr_typ.elem_type().bytelen() * (elems_ct - sub_inits_ct);
                    let item = InitializerItem::Zero(fill_bytelen);
                    Self::push_initializer_item(out_items, item);
                }
            }
            (
                ObjType::Array(arr_typ),
                VariableInitializer::Single(Expression::L(LExp::String(chars))),
            ) => {
                let ok = match arr_typ.elem_type() {
                    NonVoidType::Scalar(sca_typ) => match sca_typ.as_ref() {
                        ScalarType::Arith(ari_typ) if ari_typ.is_character() => Ok(()),
                        _ => Err(()),
                    },
                    _ => Err(()),
                };
                let () = ok.map_err(|()| {
                    anyhow!("Cannot initialize {arr_typ:#?} using string literal.")
                })?;

                let elems_ct = arr_typ.elem_count().as_int();
                let chars_ct = chars.len() as u64;
                if elems_ct < chars_ct {
                    return Err(anyhow!(
                        "Too many chars in initializer. {arr_typ:#?} vs {chars:#?}"
                    ));
                }

                let zeros_sfx_bytelen = ByteLen::new(elems_ct - chars_ct);
                let item = InitializerItem::String(InitializerString { chars, zeros_sfx_bytelen });
                out_items.push(item);
            }
            (ObjType::Scalar(_), init @ VariableInitializer::Compound(..))
            | (ObjType::Array(_), init @ VariableInitializer::Single(_)) => {
                return Err(anyhow!(
                    "Advanced compound initializers aren't supported. {typ:#?} vs {init:#?}"
                ));
            }
        }
        Ok(())
    }
    fn push_initializer_item<Sngl, Ptr>(
        items: &mut Vec<InitializerItem<Sngl, Ptr>>,
        item: InitializerItem<Sngl, Ptr>,
    ) {
        match (items.last_mut(), item) {
            (Some(InitializerItem::Zero(prv_bytelen)), InitializerItem::Zero(cur_bytelen)) => {
                *prv_bytelen += cur_bytelen;
            }
            (
                Some(InitializerItem::String(InitializerString { zeros_sfx_bytelen, .. })),
                InitializerItem::Zero(cur_bytelen),
            ) => {
                *zeros_sfx_bytelen += cur_bytelen;
            }
            (_, item) => {
                items.push(item);
            }
        }
    }
}

#[cfg(test)]
#[allow(non_snake_case)]
mod test {
    use super::*;
    use crate::{
        common::{primitive::Const, types_backend::ByteLen, types_frontend::ArithmeticType},
        ds_n_a::singleton::SingletonRepository,
        stage2_parse::phase2_resolve::ResolvedCAst,
        test::utils::{TestDeclaratorItem as Dec, TypeBuilder},
    };

    use RuntimeInitializerItem as OutRtItem;
    use StaticInitializerItem as OutStaticItem;
    use VariableInitializer as InNode;

    /* Creating inputs. */

    fn in_single_constexpr_int(int: i32) -> InNode<ResolvedCAst> {
        let exp = Expression::R(RExp::Const(Const::Int(int)));
        InNode::Single(exp)
    }
    fn in_single_constexpr_dbl(dbl: f64) -> InNode<ResolvedCAst> {
        let exp = Expression::R(RExp::Const(Const::Double(dbl)));
        InNode::Single(exp)
    }
    fn in_single_constexpr_str(chars: &'static str) -> InNode<ResolvedCAst> {
        let exp = Expression::L(LExp::String(chars.into()));
        InNode::Single(exp)
    }

    /* Checking outputs. */

    fn match_out_rt_single_const_int(item: &OutRtItem, expected_int: i32) -> bool {
        match item {
            OutRtItem::Single(TypedExp::R(TypedRExp {
                exp: RExp::Const(Const::Int(actual_int)),
                typ,
            })) => {
                (*actual_int == expected_int)
                    && matches!(typ.as_ref(), ScalarType::Arith(ArithmeticType::Int))
            }
            _ => false,
        }
    }
    fn match_out_rt_single_addrof(item: &OutRtItem) -> bool {
        match item {
            OutRtItem::Single(TypedExp::R(TypedRExp { exp: RExp::AddrOf(_), .. })) => true,
            _ => false,
        }
    }
    fn match_out_rt_string(
        item: &OutRtItem,
        expected_chars: &'static str,
        expected_zeros_sfx_bytelen: u64,
    ) -> bool {
        match item {
            OutRtItem::String(InitializerString { chars, zeros_sfx_bytelen }) => {
                (&chars[..] == expected_chars.as_bytes())
                    && (zeros_sfx_bytelen.as_int() == expected_zeros_sfx_bytelen)
            }
            _ => false,
        }
    }
    fn match_out_rt_zero(item: &OutRtItem, expected_bytelen: u64) -> bool {
        match item {
            OutRtItem::Zero(actual_bytelen) => actual_bytelen.as_int() == expected_bytelen,
            _ => false,
        }
    }

    /* Converting inputs to outputs. */

    fn new_typechecker(
        (typ_decls, base_typ): (&[Dec], ArithmeticType),
    ) -> (TypeChecker, Singleton<ObjType>) {
        let obj_type_repo = SingletonRepository::default();
        let mut tc = TypeChecker::new(obj_type_repo);

        let mut typ_bld = TypeBuilder::new(&mut tc.obj_type_repo);
        let typ = typ_bld.build_obj_type(&typ_decls, base_typ);

        (tc, typ)
    }
    fn do_typecheck_static(
        typ: (&[Dec], ArithmeticType),
        init: InNode<ResolvedCAst>,
    ) -> Result<Vec<OutStaticItem>> {
        let (mut tc, typ) = new_typechecker(typ);
        tc.typecheck_initializer_static(&typ, init)
    }
    fn do_typecheck_runtime(
        typ: (&[Dec], ArithmeticType),
        init: InNode<ResolvedCAst>,
    ) -> Result<Vec<OutRtItem>> {
        let (mut tc, typ) = new_typechecker(typ);
        tc.typecheck_initializer_runtime(&typ, init)
    }

    /// ScalarType <- single initializer
    mod scalar_single {
        use super::*;

        #[test]
        fn static_single_nonzero() -> Result<()> {
            let out_items =
                do_typecheck_static((&[], ArithmeticType::UInt), in_single_constexpr_dbl(41.0))?;
            assert_eq!(&out_items, &[OutStaticItem::Single(Const::UInt(41))]);
            Ok(())
        }

        #[test]
        fn static_single_zero_integ() -> Result<()> {
            {
                let out_items =
                    do_typecheck_static((&[], ArithmeticType::UInt), in_single_constexpr_dbl(0.0))?;
                assert_eq!(&out_items, &[OutStaticItem::Zero(ByteLen::new(4))]);
            }
            {
                let out_items = do_typecheck_static(
                    (&[Dec::Ptr], ArithmeticType::Double),
                    in_single_constexpr_int(0),
                )?;
                assert_eq!(&out_items, &[OutStaticItem::Zero(ByteLen::new(8))]);
            }
            Ok(())
        }

        #[test]
        fn static_single_zero_dbl() -> Result<()> {
            let out_items =
                do_typecheck_static((&[], ArithmeticType::Double), in_single_constexpr_dbl(0.0))?;
            assert_eq!(&out_items, &[OutStaticItem::Single(Const::Double(0.0))]); // Not Zero item.
            Ok(())
        }
    }

    /// ArrayType <- compound initializer
    mod array_compound {
        use super::*;

        #[test]
        fn static_compound_empty() -> Result<()> {
            let out_items = do_typecheck_static(
                (&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Int),
                InNode::Compound(vec![]),
            )?;
            assert_eq!(&out_items, &[OutStaticItem::Zero(ByteLen::new(4 * 7 * 17))]);
            Ok(())
            /* Note, our parser grammer forbids an empty initializer, so this scenario is unrealistic. */
        }

        #[test]
        fn static_compound_fill_zeros() -> Result<()> {
            let out_items = do_typecheck_static(
                (&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Int),
                InNode::Compound(vec![
                    InNode::Compound(vec![in_single_constexpr_int(10)]),
                    InNode::Compound(vec![in_single_constexpr_int(0)]),
                    InNode::Compound(vec![]),
                    InNode::Compound(vec![
                        in_single_constexpr_int(0),
                        in_single_constexpr_int(20),
                        in_single_constexpr_int(21),
                    ]),
                    InNode::Compound(vec![in_single_constexpr_int(0), in_single_constexpr_int(0)]),
                ]),
            )?;
            assert_eq!(
                &out_items,
                &[
                    OutStaticItem::Single(Const::Int(10)),
                    OutStaticItem::Zero(ByteLen::new(4 * ((7 - 1) + 7 * 2 + 1))), // arr[0][1:], arr[1:3][:], arr[3][:1]
                    OutStaticItem::Single(Const::Int(20)),
                    OutStaticItem::Single(Const::Int(21)),
                    OutStaticItem::Zero(ByteLen::new(4 * ((7 - 3) + 7 * (17 - 4)))), // arr[3][3:], arr[4:][:]
                ]
            );
            Ok(())
        }

        #[test]
        fn static_compound_fill_zeros_between_dbls() -> Result<()> {
            let out_items = do_typecheck_static(
                (&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Double),
                InNode::Compound(vec![
                    InNode::Compound(vec![in_single_constexpr_dbl(10.0)]),
                    InNode::Compound(vec![]),
                    InNode::Compound(vec![
                        in_single_constexpr_dbl(20.0),
                        in_single_constexpr_dbl(21.0),
                    ]),
                    InNode::Compound(vec![in_single_constexpr_dbl(0.0)]),
                    InNode::Compound(vec![
                        in_single_constexpr_dbl(-0.0),
                        in_single_constexpr_dbl(0.0),
                    ]),
                ]),
            )?;
            assert_eq!(
                &out_items,
                &[
                    OutStaticItem::Single(Const::Double(10.0)),
                    OutStaticItem::Zero(ByteLen::new(8 * ((7 - 1) + 7))), // arr[0][1:], arr[1][:]
                    OutStaticItem::Single(Const::Double(20.0)),
                    OutStaticItem::Single(Const::Double(21.0)),
                    OutStaticItem::Zero(ByteLen::new(8 * (7 - 2))), // arr[2][2:]
                    OutStaticItem::Single(Const::Double(0.0)), // Not combined with neighboring Zero items.
                    OutStaticItem::Zero(ByteLen::new(8 * (7 - 1))), // arr[3][1:]
                    OutStaticItem::Single(Const::Double(-0.0)), // Not combined with neighboring Zero items.
                    OutStaticItem::Single(Const::Double(0.0)), // Not combined with neighboring Zero items.
                    OutStaticItem::Zero(ByteLen::new(8 * ((7 - 2) + 7 * (17 - 5)))), // arr[4][2:], arr[5:][:]
                ]
            );
            Ok(())
        }

        #[test]
        fn runtime_compound_fill_zeros() -> Result<()> {
            let out_items = do_typecheck_runtime(
                (&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Int),
                InNode::Compound(vec![
                    InNode::Compound(vec![in_single_constexpr_int(10)]),
                    InNode::Compound(vec![]),
                    InNode::Compound(vec![
                        in_single_constexpr_int(0),
                        in_single_constexpr_int(20),
                        in_single_constexpr_int(21),
                    ]),
                ]),
            )?;

            assert_eq!(out_items.len(), 6);

            assert!(match_out_rt_single_const_int(&out_items[0], 10));
            assert!(match_out_rt_zero(&out_items[1], 4 * ((7 - 1) + 7))); // arr[0][1:], arr[1][:]
            assert!(match_out_rt_single_const_int(&out_items[2], 0)); // Not combined with neighboring Zero items.
            assert!(match_out_rt_single_const_int(&out_items[3], 20));
            assert!(match_out_rt_single_const_int(&out_items[4], 21));
            assert!(match_out_rt_zero(
                &out_items[5],
                4 * ((7 - 3) + 7 * (17 - 3))
            )); // arr[2][3:], arr[3:][:]

            Ok(())
        }
    }

    /// ScalarType <- string initializer
    mod scalar_string {
        use super::*;

        #[test]
        fn static_lhs_ptrToChar_rhs_singleString() -> Result<()> {
            let out_items = do_typecheck_static(
                (&[Dec::Ptr], ArithmeticType::Char),
                in_single_constexpr_str("aabb"),
            )?;

            assert!(matches!(&out_items[..], &[OutStaticItem::Pointer(_)]));
            Ok(())
        }

        #[test]
        fn runtime_lhs_ptrToChar_rhs_singleString() -> Result<()> {
            let out_items = do_typecheck_runtime(
                (&[Dec::Ptr], ArithmeticType::Char),
                in_single_constexpr_str("aabb"),
            )?;

            assert_eq!(out_items.len(), 1);

            assert!(match_out_rt_single_addrof(&out_items[0]));

            Ok(())
        }
    }

    /// ArrayType <- string initializer
    mod array_string {
        use super::*;

        #[test]
        fn lhs_arrDepth1_rhs_singleString() -> Result<()> {
            for character_typ in [
                ArithmeticType::Char,
                ArithmeticType::SChar,
                ArithmeticType::UChar,
            ] {
                let out_items = do_typecheck_static(
                    (&[Dec::Arr(17)], character_typ),
                    in_single_constexpr_str("aabb"),
                )?;

                assert_eq!(
                    &out_items,
                    &[OutStaticItem::String(InitializerString {
                        chars: "aabb".into(),
                        zeros_sfx_bytelen: ByteLen::new(17 - 4)
                    })]
                );
            }
            for character_typ in [
                ArithmeticType::Char,
                ArithmeticType::SChar,
                ArithmeticType::UChar,
            ] {
                let out_items = do_typecheck_runtime(
                    (&[Dec::Arr(17)], character_typ),
                    in_single_constexpr_str("aabb"),
                )?;

                assert_eq!(out_items.len(), 1);

                assert!(match_out_rt_string(&out_items[0], "aabb", 17 - 4));
            }
            Ok(())
        }

        #[test]
        fn lhs_arrDepth2_rhs_compoundStrings() -> Result<()> {
            for character_typ in [
                ArithmeticType::Char,
                ArithmeticType::SChar,
                ArithmeticType::UChar,
            ] {
                let out_items = do_typecheck_static(
                    (&[Dec::Arr(17), Dec::Arr(7)], character_typ),
                    InNode::Compound(vec![
                        in_single_constexpr_str("aabb"),
                        in_single_constexpr_str("ccddeef"),
                        in_single_constexpr_str("gghh"),
                    ]),
                )?;

                assert_eq!(
                    &out_items,
                    &[
                        OutStaticItem::String(InitializerString {
                            chars: "aabb".into(),
                            zeros_sfx_bytelen: ByteLen::new(7 - 4),
                        }),
                        OutStaticItem::String(InitializerString {
                            chars: "ccddeef".into(),
                            zeros_sfx_bytelen: ByteLen::new(7 - 7),
                        }),
                        OutStaticItem::String(InitializerString {
                            chars: "gghh".into(),
                            zeros_sfx_bytelen: ByteLen::new((7 - 4) + 7 * (17 - 3)),
                        }),
                    ]
                );
            }
            Ok(())
        }
    }

    /// some type <--(invalid)-- string initializer.
    mod invalid_string {
        use super::*;

        #[test]
        fn lhs_invalidType_rhs_singleString() -> Result<()> {
            use ArithmeticType as AT;

            let mut lhs_type_ingredients = vec![];
            for non_char_ari_typ in [AT::Int, AT::SChar, AT::UChar] {
                lhs_type_ingredients.extend([(vec![Dec::Ptr], non_char_ari_typ)]);
            }
            for non_character_typ in [AT::Int] {
                lhs_type_ingredients.extend([(vec![Dec::Arr(17)], non_character_typ)]);
            }
            for any_typ in [AT::Int, AT::Char, AT::SChar, AT::UChar] {
                lhs_type_ingredients.extend([
                    (vec![], any_typ),
                    (vec![Dec::Ptr, Dec::Arr(17)], any_typ),
                    (vec![Dec::Arr(17), Dec::Ptr], any_typ),
                    (vec![Dec::Arr(17), Dec::Arr(7)], any_typ),
                ]);
            }

            for (typ_decls, base_typ) in lhs_type_ingredients {
                let res = do_typecheck_static(
                    (&typ_decls[..], base_typ),
                    in_single_constexpr_str("aabb"),
                );
                assert!(res.is_err(), "{res:#?}");
            }
            Ok(())
        }
    }
}
