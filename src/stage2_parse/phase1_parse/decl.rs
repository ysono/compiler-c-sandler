use super::Parser;
use crate::{
    common::{
        identifier::RawIdentifier,
        primitive::Const,
        types_frontend::{
            ArithmeticType, ArrayElementCount, ArrayType, ObjType, ParsedFunType, PointerType,
        },
    },
    ds_n_a::singleton::Singleton,
    stage1_lex::tokens as t,
    stage2_parse::c_ast::*,
    utils::noop,
};
use anyhow::{anyhow, Context, Result};
use std::cmp;

impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    /* Non-abstract declarator */

    pub(super) fn parse_specifiers_and_declarator(
        &mut self,
    ) -> Result<Option<(DeclaratorResult, Option<StorageClassSpecifier>)>> {
        let mut inner = || -> Result<_> {
            let (base_type, storage_class) = match self.parse_specifiers()? {
                None => return Ok(None),
                Some(specifiers) => specifiers,
            };

            let declarator = self.parse_declarator()?;

            let declarator_res = self.derive_declared_type(base_type, declarator)?;

            Ok(Some((declarator_res, storage_class)))
        };
        inner().context("[ { <specifier> }+ <declarator> ]")
    }

    fn parse_specifiers(
        &mut self,
    ) -> Result<Option<(ArithmeticType, Option<StorageClassSpecifier>)>> {
        let mut inner = || -> Result<_> {
            let mut typs = vec![];
            let mut scss = vec![];
            while let Some(Ok(t::Token::Type(_) | t::Token::StorageClassSpecifier(_))) =
                self.tokens.peek()
            {
                match self.tokens.next().unwrap().unwrap() {
                    t::Token::Type(t_typ) => typs.push(t_typ),
                    t::Token::StorageClassSpecifier(scs) => scss.push(scs),
                    _ => unreachable!(),
                }
            }
            if typs.is_empty() && scss.is_empty() {
                return Ok(None);
            }

            let expected_max_len = cmp::min(3, typs.len());
            typs[..expected_max_len].sort();

            let ari_type = match &typs[..] {
                [t::Type::Int] => ArithmeticType::Int,
                [t::Type::Int, t::Type::Long] => ArithmeticType::Long,
                [t::Type::Int, t::Type::Long, t::Type::Signed] => ArithmeticType::Long,
                [t::Type::Int, t::Type::Long, t::Type::Unsigned] => ArithmeticType::ULong,
                [t::Type::Int, t::Type::Signed] => ArithmeticType::Int,
                [t::Type::Int, t::Type::Unsigned] => ArithmeticType::UInt,
                [t::Type::Long] => ArithmeticType::Long,
                [t::Type::Long, t::Type::Signed] => ArithmeticType::Long,
                [t::Type::Long, t::Type::Unsigned] => ArithmeticType::ULong,
                [t::Type::Signed] => ArithmeticType::Int,
                [t::Type::Unsigned] => ArithmeticType::UInt,
                [t::Type::Double] => ArithmeticType::Double,
                actual => return Err(anyhow!("Invalid types. {actual:?}")),
                /* Void is not supported yet. */
            };

            let scs = match &scss[..] {
                [] => None,
                [scs] => Some(*scs),
                actual => return Err(anyhow!("Invalid storage class specifiers. {actual:?}")),
            };

            Ok(Some((ari_type, scs)))
        };
        inner().context("{ <specifier> }")
    }

    fn parse_declarator(&mut self) -> Result<Declarator> {
        let mut inner = || -> Result<_> {
            let mut ptr_count = 0;
            while let Some(Ok(t::Token::Operator(t::Operator::Star))) = self.tokens.peek() {
                self.tokens.next();
                ptr_count += 1;
            }

            let mut declarator = self.parse_direct_declarator()?;

            if ptr_count > 0 {
                declarator
                    .items_baseward
                    .push(DeclaratorItem::Ptr(ptr_count));
            }

            Ok(declarator)
        };
        inner().context("<declarator>")
    }
    fn parse_direct_declarator(&mut self) -> Result<Declarator> {
        let mut inner = || -> Result<_> {
            let declarator = self.parse_simple_declarator()?;

            self.parse_declarator_suffx(declarator)
        };
        inner().context("<direct-declarator>")
    }
    fn parse_simple_declarator(&mut self) -> Result<Declarator> {
        let mut inner = || -> Result<_> {
            match self.tokens.next() {
                Some(Ok(t::Token::Identifier(ident))) => Ok(Declarator::new(ident)),
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    let declarator = self.parse_declarator()?;

                    self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                    Ok(declarator)
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<simple-declarator>")
    }
    fn parse_declarator_suffx(&mut self, mut declarator: Declarator) -> Result<Declarator> {
        let inner = || -> Result<_> {
            match self.parse_param_list()? {
                Some(params) => declarator.items_baseward.push(DeclaratorItem::Fun(params)),
                None => {
                    while let Some(elem_count) = self.parse_arr_elem_count()? {
                        declarator
                            .items_baseward
                            .push(DeclaratorItem::Arr(elem_count))
                    }
                }
            }
            Ok(declarator)
        };
        inner().context("[ <declarator-suffix> ]")
    }
    fn parse_param_list(&mut self) -> Result<Option<Vec<Param>>> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    self.tokens.next();
                }
                _ => return Ok(None),
            }

            let params = match self.tokens.peek() {
                Some(Ok(t::Token::Type(t::Type::Void))) => {
                    self.tokens.next();
                    Vec::with_capacity(0)
                }
                _ => {
                    let mut params = vec![];
                    loop {
                        let param_ari_type = match self.parse_specifiers()? {
                            Some((t, None)) => t,
                            actual => return Err(anyhow!("{actual:?}")),
                        };

                        let declarator = self.parse_declarator()?;

                        params.push(Param(param_ari_type, declarator));

                        match self.tokens.peek() {
                            Some(Ok(t::Token::Demarcator(t::Demarcator::Comma))) => {
                                self.tokens.next();
                                continue;
                            }
                            _ => break,
                        }
                    }
                    params
                }
            };

            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

            Ok(Some(params))
        };
        inner().context("[ <param-list> ]")
    }
    fn parse_arr_elem_count(&mut self) -> Result<Option<ArrayElementCount>> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::SquareOpen))) => {
                    self.tokens.next();
                }
                _ => return Ok(None),
            }

            let elem_count = match self.tokens.next() {
                Some(Ok(t::Token::Const(konst))) => match konst {
                    /* Note, the lexer emits integer literals that are >= 0. */
                    Const::Int(i) => i as u64,
                    Const::Long(i) => i as u64,
                    Const::UInt(i) => i as u64,
                    Const::ULong(i) => i,
                    actual => return Err(anyhow!("{actual:?}")),
                },
                actual => return Err(anyhow!("{actual:?}")),
            };

            self.expect_exact(&[t::Demarcator::SquareClose.into()])?;

            Ok(Some(ArrayElementCount::new(elem_count)))
        };
        inner().context(r#"[ "[" <const> "]" ]"#)
    }

    fn derive_declared_type(
        &mut self,
        base_type: ArithmeticType,
        Declarator { ident, mut items_baseward, .. }: Declarator,
    ) -> Result<DeclaratorResult> {
        let mut cur_type = self.obj_type_repo.get_or_new(base_type.into());

        while let Some(item) = items_baseward.pop() {
            match item {
                DeclaratorItem::Ptr(ptr_count) => {
                    for _ in 0..ptr_count {
                        cur_type = self
                            .obj_type_repo
                            .get_or_new(PointerType { pointee_type: cur_type }.into());
                    }
                }
                DeclaratorItem::Arr(elem_count) => {
                    match items_baseward.last() {
                        Some(DeclaratorItem::Fun(_)) => noop!(
                            "In C, a function can't return an array.",
                            "The official tester expects us to assert this at a later phase."
                        ),
                        Some(DeclaratorItem::Ptr(_) | DeclaratorItem::Arr(_)) | None => {
                            noop!("Happy path.")
                        }
                    }
                    cur_type = self
                        .obj_type_repo
                        .get_or_new(ArrayType::new(cur_type, elem_count).into());
                }
                DeclaratorItem::Fun(params) => match items_baseward.last() {
                    Some(DeclaratorItem::Fun(_)) => {
                        return Err(anyhow!("In C, a function can't return a function."))
                    }
                    Some(DeclaratorItem::Ptr(_) | DeclaratorItem::Arr(_)) => {
                        return Err(anyhow!("Function pointers aren't supported."))
                    }
                    None => {
                        let mut param_types = Vec::with_capacity(params.len());
                        let mut param_idents = Vec::with_capacity(params.len());
                        for Param(param_base_type, param_declarator) in params {
                            let param_res =
                                self.derive_declared_type(param_base_type, param_declarator)?;
                            match param_res {
                                DeclaratorResult::Var(param_ident, param_type) => {
                                    param_types.push(param_type);
                                    param_idents.push(param_ident);
                                }
                                DeclaratorResult::Fun(..) => {
                                    return Err(anyhow!(
                                        "Function parameters aren't supported. {param_res:?}"
                                    ))
                                }
                            }
                        }

                        let fun_type = ParsedFunType {
                            params: param_types,
                            ret: cur_type,
                        };
                        let fun_type = self.fun_type_repo.get_or_new(fun_type);

                        return Ok(DeclaratorResult::Fun(ident, fun_type, param_idents));
                    }
                },
            }
        }

        Ok(DeclaratorResult::Var(ident, cur_type))
    }

    /* Abstract declarator */

    pub(super) fn parse_cast_type(&mut self) -> Result<Option<Singleton<ObjType>>> {
        let mut inner = || -> Result<_> {
            let base_type = match self.parse_specifiers()? {
                None => return Ok(None),
                Some((_, storage_class @ Some(_))) => return Err(anyhow!("{storage_class:?}")),
                Some((typ, None)) => typ,
            };

            let declarator = self.parse_abstract_declarator()?;

            let final_type = self.derive_abstract_declared_type(base_type, declarator);
            Ok(Some(final_type))
        };
        inner().context("[ { <type-specifier> }+ [ <abstract-declarator> ] ]")
    }

    fn parse_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        let mut inner = || -> Result<_> {
            let mut ptr_count = 0;
            while let Some(Ok(t::Token::Operator(t::Operator::Star))) = self.tokens.peek() {
                self.tokens.next();
                ptr_count += 1;
            }

            let mut declarator = self.parse_direct_abstract_declarator()?;

            if ptr_count > 0 {
                declarator
                    .items_baseward
                    .push(AbstractDeclaratorItem::Ptr(ptr_count));
            }

            Ok(declarator)
        };
        inner().context("<abstract-declarator>")
    }
    fn parse_direct_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        let mut inner = || -> Result<_> {
            let mut declarator = match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    self.tokens.next();

                    let declarator = self.parse_abstract_declarator()?;
                    if declarator.items_baseward.is_empty() {
                        return Err(anyhow!("Empty parentheses. It means a param-list. Can't cast to a function type."));
                    }

                    self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                    declarator
                }
                _ => AbstractDeclarator::new(),
            };

            while let Some(elem_count) = self.parse_arr_elem_count()? {
                declarator
                    .items_baseward
                    .push(AbstractDeclaratorItem::Arr(elem_count));
            }

            Ok(declarator)
        };
        inner().context("[ <direct-abstract-declarator> ]")
    }

    fn derive_abstract_declared_type(
        &mut self,
        base_type: ArithmeticType,
        AbstractDeclarator { mut items_baseward, .. }: AbstractDeclarator,
    ) -> Singleton<ObjType> {
        let mut cur_type = self.obj_type_repo.get_or_new(base_type.into());

        while let Some(item) = items_baseward.pop() {
            match item {
                AbstractDeclaratorItem::Ptr(ptr_count) => {
                    for _ in 0..ptr_count {
                        cur_type = self
                            .obj_type_repo
                            .get_or_new(PointerType { pointee_type: cur_type }.into());
                    }
                }
                AbstractDeclaratorItem::Arr(elem_count) => {
                    cur_type = self
                        .obj_type_repo
                        .get_or_new(ArrayType::new(cur_type, elem_count).into());
                }
            }
        }

        cur_type
    }
}

/* Non-abstract declarator */

#[derive(Debug)]
struct Declarator {
    ident: RawIdentifier,
    items_baseward: Vec<DeclaratorItem>,
    _dummy: (), // Help clarify where we're constructing.
}
impl Declarator {
    fn new(ident: RawIdentifier) -> Self {
        Self {
            ident,
            items_baseward: vec![],
            _dummy: (),
        }
    }
}
#[derive(Debug)]
enum DeclaratorItem {
    Ptr(u64), // The u64 = repetition.
    Arr(ArrayElementCount),
    Fun(Vec<Param>),
}
#[derive(Debug)]
struct Param(ArithmeticType, Declarator);

#[derive(Debug)]
pub(super) enum DeclaratorResult {
    Var(RawIdentifier, Singleton<ObjType>),
    Fun(RawIdentifier, Singleton<ParsedFunType>, Vec<RawIdentifier>),
}

/* Abstract declarator */

struct AbstractDeclarator {
    items_baseward: Vec<AbstractDeclaratorItem>,
    _dummy: (), // Help clarify where we're constructing.
}
impl AbstractDeclarator {
    pub fn new() -> Self {
        Self {
            items_baseward: vec![],
            _dummy: (),
        }
    }
}

#[derive(Debug)]
enum AbstractDeclaratorItem {
    Ptr(u64), // The u64 = repetition.
    Arr(ArrayElementCount),
}
