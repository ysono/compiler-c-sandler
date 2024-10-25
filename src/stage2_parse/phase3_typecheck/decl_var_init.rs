use super::TypeChecker;
use crate::{
    common::{
        primitive::Const,
        symbol_table_frontend::{InitializerItem, StaticInitializer},
        types_backend::ByteLen,
        types_frontend::{ObjType, ScalarType},
    },
    ds_n_a::singleton::Singleton,
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    utils::noop,
};
use anyhow::{anyhow, Result};

impl TypeChecker {
    pub(super) fn generate_zero_static_initializer(typ: &ObjType) -> StaticInitializer {
        let bytelen = typ.bytelen();
        let item = InitializerItem::Zero(bytelen);
        StaticInitializer::Concrete(vec![item])
    }

    pub(super) fn typecheck_initializer_static(
        &mut self,
        typ: &Singleton<ObjType>,
        init: VariableInitializer<ResolvedCAst>,
    ) -> Result<Vec<InitializerItem<Const>>> {
        let mut typecheck_single = |typ: &Singleton<ObjType>, exp: Expression<ResolvedCAst>| {
            self.typecheck_initializer_static_single(typ, exp)
        };

        let mut inits = vec![];
        Self::typecheck_initializer(typ, init, &mut typecheck_single, &mut inits)?;
        Ok(inits)
    }
    fn typecheck_initializer_static_single(
        &mut self,
        to: &Singleton<ObjType>,
        from: Expression<ResolvedCAst>,
    ) -> Result<InitializerItem<Const>> {
        let to = Self::extract_scalar_type_ref(to)
            .map_err(|typ| anyhow!("Cannot \"convert as if by assignment\" to {typ:?}"))?;

        match &from {
            Expression::R(RExp::Const(_)) => noop!(),
            Expression::L(LExp::String(_)) => noop!(),
            _ => return Err(anyhow!("Within each static initializer, each single expression must be a constexpr. For each constexpr, only a literal is supported."))
        };

        let from = self.typecheck_exp_and_convert_to_scalar(from)?;

        let () = Self::can_cast_by_assignment(to, &from)?;

        let item = match from {
            TypedExp::R(TypedRExp { exp, .. }) => match exp {
                RExp::Const(in_konst) => {
                    let out_konst = in_konst.cast_at_compile_time(to);

                    if out_konst.is_zero_integer() {
                        let bytelen = ByteLen::from(out_konst.arithmetic_type());
                        InitializerItem::Zero(bytelen)
                    } else {
                        /* If double, even if val==0.0, we encode a Single item.
                        This helps inform us later to write it to `.data` instead of `.bss`. */
                        InitializerItem::Single(out_konst)
                    }
                }
                RExp::AddrOf(AddrOf(sub_exp)) => match *sub_exp {
                    TypedLExp {
                        exp: LExp::String(static_obj_ident),
                        ..
                    } => InitializerItem::Pointer(static_obj_ident),
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
    ) -> Result<Vec<InitializerItem<TypedExp<ScalarType>>>> {
        let mut typecheck_single = |typ: &Singleton<ObjType>, exp: Expression<ResolvedCAst>| {
            self.cast_by_assignment(typ.clone(), exp)
                .map(InitializerItem::Single)
        };

        let mut inits = vec![];
        Self::typecheck_initializer(typ, init, &mut typecheck_single, &mut inits)?;
        Ok(inits)
    }

    fn typecheck_initializer<F, Sngl>(
        typ: &Singleton<ObjType>,
        init: VariableInitializer<ResolvedCAst>,
        typecheck_single: &mut F,
        out_items: &mut Vec<InitializerItem<Sngl>>,
    ) -> Result<()>
    where
        F: FnMut(&Singleton<ObjType>, Expression<ResolvedCAst>) -> Result<InitializerItem<Sngl>>,
    {
        match (typ.as_ref(), init) {
            (ObjType::Scalar(_), VariableInitializer::Single(exp)) => {
                let item = typecheck_single(typ, exp)?;
                Self::push_initializer_item(out_items, item);
            }
            (ObjType::Scalar(_), init @ VariableInitializer::Compound(..))
            | (ObjType::Array(_), init @ VariableInitializer::Single(_)) => {
                return Err(anyhow!(
                    "Advanced compound initializers aren't supported. {typ:?} vs {init:?}"
                ));
            }
            (ObjType::Array(arr_typ), VariableInitializer::Compound(sub_inits)) => {
                let elems_ct = arr_typ.elem_count().as_int();
                let sub_inits_ct = sub_inits.len() as u64;
                if elems_ct < sub_inits_ct {
                    return Err(anyhow!(
                        "Too many initializer elements. {arr_typ:?} vs {sub_inits:?}"
                    ));
                }

                for sub_init in sub_inits {
                    Self::typecheck_initializer(
                        arr_typ.elem_type(),
                        sub_init,
                        typecheck_single,
                        out_items,
                    )?;
                }

                if elems_ct > sub_inits_ct {
                    let fill_bytelen = arr_typ.elem_type().bytelen() * (elems_ct - sub_inits_ct);
                    let item = InitializerItem::Zero(fill_bytelen);
                    Self::push_initializer_item(out_items, item);
                }
            }
        }
        Ok(())
    }
    fn push_initializer_item<Sngl>(
        items: &mut Vec<InitializerItem<Sngl>>,
        item: InitializerItem<Sngl>,
    ) {
        match (items.last_mut(), item) {
            (Some(InitializerItem::Zero(prv_bytelen)), InitializerItem::Zero(cur_bytelen)) => {
                *prv_bytelen += cur_bytelen;
            }
            (_, item) => {
                items.push(item);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        common::{types_backend::ByteLen, types_frontend::ArithmeticType},
        ds_n_a::singleton::SingletonRepository,
        stage2_parse::phase2_resolve::ResolvedCAst,
        test::utils::{TestDeclaratorItem as Dec, TypeBuilder},
    };
    use InitializerItem as OutItem;
    use VariableInitializer as InNode;

    fn in_single_constexpr_int(int: i32) -> InNode<ResolvedCAst> {
        let exp = Expression::R(RExp::Const(Const::Int(int)));
        InNode::Single(exp)
    }
    fn in_single_constexpr_dbl(dbl: f64) -> InNode<ResolvedCAst> {
        let exp = Expression::R(RExp::Const(Const::Double(dbl)));
        InNode::Single(exp)
    }

    fn match_out_rt_single_const_int(
        item: &OutItem<TypedExp<ScalarType>>,
        expected_int: i32,
    ) -> bool {
        match item {
            OutItem::Single(TypedExp::R(TypedRExp {
                exp: RExp::Const(Const::Int(actual_int)),
                typ,
            })) => {
                (*actual_int == expected_int)
                    && matches!(typ.as_ref(), ScalarType::Arith(ArithmeticType::Int))
            }
            _ => false,
        }
    }
    fn match_out_rt_zero(item: &OutItem<TypedExp<ScalarType>>, expected_bytelen: u64) -> bool {
        match item {
            OutItem::Zero(actual_bytelen) => actual_bytelen.as_int() == expected_bytelen,
            _ => false,
        }
    }

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
    ) -> Result<Vec<OutItem<Const>>> {
        let (mut tc, typ) = new_typechecker(typ);
        tc.typecheck_initializer_static(&typ, init)
    }
    fn do_typecheck_runtime(
        typ: (&[Dec], ArithmeticType),
        init: InNode<ResolvedCAst>,
    ) -> Result<Vec<OutItem<TypedExp<ScalarType>>>> {
        let (mut tc, typ) = new_typechecker(typ);
        tc.typecheck_initializer_runtime(&typ, init)
    }

    /* ScalarType <- single initializer */

    #[test]
    fn static_single_nonzero() -> Result<()> {
        let out_items =
            do_typecheck_static((&[], ArithmeticType::UInt), in_single_constexpr_dbl(41.0))?;
        assert_eq!(&out_items, &[OutItem::Single(Const::UInt(41))]);
        Ok(())
    }

    #[test]
    fn static_single_zero_integ() -> Result<()> {
        {
            let out_items =
                do_typecheck_static((&[], ArithmeticType::UInt), in_single_constexpr_dbl(0.0))?;
            assert_eq!(&out_items, &[OutItem::Zero(ByteLen::new(4))]);
        }
        {
            let out_items = do_typecheck_static(
                (&[Dec::Ptr], ArithmeticType::Double),
                in_single_constexpr_int(0),
            )?;
            assert_eq!(&out_items, &[OutItem::Zero(ByteLen::new(8))]);
        }
        Ok(())
    }

    #[test]
    fn static_single_zero_dbl() -> Result<()> {
        let out_items =
            do_typecheck_static((&[], ArithmeticType::Double), in_single_constexpr_dbl(0.0))?;
        assert_eq!(&out_items, &[OutItem::Single(Const::Double(0.0))]); // Not Zero item.
        Ok(())
    }

    /* ArrayType <- compound initializer */

    #[test]
    fn static_compound_empty() -> Result<()> {
        let out_items = do_typecheck_static(
            (&[Dec::Arr(17), Dec::Arr(7)], ArithmeticType::Int),
            InNode::Compound(vec![]),
        )?;
        assert_eq!(&out_items, &[OutItem::Zero(ByteLen::new(4 * 7 * 17))]);
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
                OutItem::Single(Const::Int(10)),
                OutItem::Zero(ByteLen::new(4 * ((7 - 1) + 7 * 2 + 1))), // arr[0][1:], arr[1:3][:], arr[3][:1]
                OutItem::Single(Const::Int(20)),
                OutItem::Single(Const::Int(21)),
                OutItem::Zero(ByteLen::new(4 * ((7 - 3) + 7 * (17 - 4)))), // arr[3][3:], arr[4:][:]
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
                OutItem::Single(Const::Double(10.0)),
                OutItem::Zero(ByteLen::new(8 * ((7 - 1) + 7))), // arr[0][1:], arr[1][:]
                OutItem::Single(Const::Double(20.0)),
                OutItem::Single(Const::Double(21.0)),
                OutItem::Zero(ByteLen::new(8 * (7 - 2))), // arr[2][2:]
                OutItem::Single(Const::Double(0.0)), // Not combined with neighboring Zero items.
                OutItem::Zero(ByteLen::new(8 * (7 - 1))), // arr[3][1:]
                OutItem::Single(Const::Double(-0.0)), // Not combined with neighboring Zero items.
                OutItem::Single(Const::Double(0.0)), // Not combined with neighboring Zero items.
                OutItem::Zero(ByteLen::new(8 * ((7 - 2) + 7 * (17 - 5)))), // arr[4][2:], arr[5:][:]
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
