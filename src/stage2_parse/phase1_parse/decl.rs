pub(super) use self::non_abstract_declarator::DeclaratorResult;
use self::{abstract_declarator::*, non_abstract_declarator::*};
use super::Parser;
use crate::{
    common::{
        identifier::RawIdentifier,
        primitive::Const,
        types_frontend::{
            ArithmeticType, ArrayElementCount, ArrayType, NonVoidType, ObjType, ParsedFunType,
            ParsedObjType, ParsedObjTypeError, PointerType, VoidType,
        },
    },
    ds_n_a::singleton::Singleton,
    stage1_lex::tokens as t,
    stage2_parse::c_ast::*,
    utils::noop,
};
use anyhow::{Context, Result, anyhow};
use std::cmp;

/// Non-abstract declarator
impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
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

    fn parse_specifiers(&mut self) -> Result<Option<(ObjType, Option<StorageClassSpecifier>)>> {
        use ArithmeticType as AT;
        use t::TypeSpecifier as TS;

        let mut inner = || -> Result<_> {
            let mut typ_specs = vec![];
            let mut sc_specs = vec![];
            loop {
                match self.peek_token()? {
                    t::Token::TypeSpecifier(typ_spec) => {
                        typ_specs.push(*typ_spec);
                        self.tokens.next();
                    }
                    t::Token::StorageClassSpecifier(sc_spec) => {
                        sc_specs.push(*sc_spec);
                        self.tokens.next();
                    }
                    _ => break,
                }
            }
            if typ_specs.is_empty() && sc_specs.is_empty() {
                return Ok(None);
            }

            let expected_max_len = cmp::min(3, typ_specs.len());
            typ_specs[..expected_max_len].sort();

            let base_type = match &typ_specs[..] {
                [TS::Void] => VoidType.into(),
                [TS::Char] => AT::Char.into(),
                [TS::Char, TS::Signed] => AT::SChar.into(),
                [TS::Char, TS::Unsigned] => AT::UChar.into(),
                [TS::Int] => AT::Int.into(),
                [TS::Int, TS::Long] => AT::Long.into(),
                [TS::Int, TS::Long, TS::Signed] => AT::Long.into(),
                [TS::Int, TS::Long, TS::Unsigned] => AT::ULong.into(),
                [TS::Int, TS::Signed] => AT::Int.into(),
                [TS::Int, TS::Unsigned] => AT::UInt.into(),
                [TS::Long] => AT::Long.into(),
                [TS::Long, TS::Signed] => AT::Long.into(),
                [TS::Long, TS::Unsigned] => AT::ULong.into(),
                [TS::Signed] => AT::Int.into(),
                [TS::Unsigned] => AT::UInt.into(),
                [TS::Double] => AT::Double.into(),
                actual => return Err(anyhow!("Invalid types. {actual:#?}")),
            };

            let sc_spec = match &sc_specs[..] {
                [] => None,
                [sc_spec] => Some(*sc_spec),
                actual => return Err(anyhow!("Invalid storage class specifiers. {actual:#?}")),
            };

            Ok(Some((base_type, sc_spec)))
        };
        inner().context("{ <specifier> }")
    }

    fn parse_declarator(&mut self) -> Result<Declarator> {
        let mut inner = || -> Result<_> {
            let mut ptr_count = 0;
            while let t::Token::Operator(t::Operator::Star) = self.peek_token()? {
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

            self.parse_declarator_suffix(declarator)
        };
        inner().context("<direct-declarator>")
    }
    fn parse_simple_declarator(&mut self) -> Result<Declarator> {
        let mut inner = || -> Result<_> {
            match self.next_token()? {
                t::Token::Identifier(ident) => Ok(Declarator::new(ident)),
                t::Token::Demarcator(t::Demarcator::ParenOpen) => {
                    let declarator = self.parse_declarator()?;

                    self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                    Ok(declarator)
                }
                actual => Err(anyhow!("{actual:#?}")),
            }
        };
        inner().context("<simple-declarator>")
    }
    fn parse_declarator_suffix(&mut self, mut declarator: Declarator) -> Result<Declarator> {
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
            match self.peek_token()? {
                t::Token::Demarcator(t::Demarcator::ParenOpen) => {
                    self.tokens.next();
                }
                _ => return Ok(None),
            }

            let mut params = vec![];
            loop {
                let param_base_type = match self.parse_specifiers()? {
                    Some((t, None)) => t,
                    actual => return Err(anyhow!("{actual:#?}")),
                };

                let is_empty_params = params.is_empty()
                    && matches!(param_base_type, ObjType::Void(_))
                    && matches!(
                        self.peek_token()?,
                        t::Token::Demarcator(t::Demarcator::ParenClose)
                    );
                if is_empty_params {
                    break;
                }

                let declarator = self.parse_declarator()?;

                params.push(Param(param_base_type, declarator));

                match self.peek_token()? {
                    t::Token::Demarcator(t::Demarcator::Comma) => {
                        self.tokens.next();
                        continue;
                    }
                    _ => break,
                }
            }

            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

            Ok(Some(params))
        };
        inner().context("[ <param-list> ]")
    }
    fn parse_arr_elem_count(&mut self) -> Result<Option<ArrayElementCount>> {
        let mut inner = || -> Result<_> {
            match self.peek_token()? {
                t::Token::Demarcator(t::Demarcator::SquareOpen) => {
                    self.tokens.next();
                }
                _ => return Ok(None),
            }

            let elem_count = match self.next_token()? {
                t::Token::Const(konst) => match konst {
                    /* Note, the lexer emits integer literals that are >= 0. */
                    Const::Int(i) => i as u64,
                    Const::Long(i) => i as u64,
                    Const::UInt(i) => i as u64,
                    Const::ULong(i) => i,
                    actual => return Err(anyhow!("{actual:#?}")),
                },
                actual => return Err(anyhow!("{actual:#?}")),
            };

            self.expect_exact(&[t::Demarcator::SquareClose.into()])?;

            Ok(Some(ArrayElementCount::new(elem_count)))
        };
        inner().context(r#"[ "[" <const> "]" ]"#)
    }

    fn derive_declared_type(
        &mut self,
        base_type: ObjType,
        Declarator { ident, mut items_baseward, .. }: Declarator,
    ) -> Result<DeclaratorResult> {
        let mut cur_type = self.obj_type_repo.get_or_new(base_type);

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

                    match self.parse_array_type(cur_type, elem_count) {
                        Ok(obj_type) => {
                            cur_type = obj_type;
                        }
                        Err(err) => {
                            let parsed_obj_type = ParsedObjType(Err(err));
                            return Ok(DeclaratorResult::Var(ident, parsed_obj_type));
                        }
                    }
                }
                DeclaratorItem::Fun(params) => match items_baseward.last() {
                    Some(DeclaratorItem::Fun(_)) => {
                        return Err(anyhow!("In C, a function can't return a function."));
                    }
                    Some(DeclaratorItem::Ptr(_) | DeclaratorItem::Arr(_)) => {
                        return Err(anyhow!("Function pointers aren't supported."));
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
                                        "Function parameters aren't supported. {param_res:#?}"
                                    ));
                                }
                            }
                        }

                        let parsed_obj_type = ParsedObjType(Ok(cur_type));
                        let fun_type = ParsedFunType {
                            params: param_types,
                            ret: parsed_obj_type,
                        };
                        let fun_type = self.fun_type_repo.get_or_new(fun_type);

                        return Ok(DeclaratorResult::Fun(ident, fun_type, param_idents));
                    }
                },
            }
        }

        let parsed_obj_type = ParsedObjType(Ok(cur_type));
        Ok(DeclaratorResult::Var(ident, parsed_obj_type))
    }
}

/// Abstract declarator
impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    pub(super) fn parse_type_name(&mut self) -> Result<Option<ParsedObjType>> {
        let mut inner = || -> Result<_> {
            let base_type = match self.parse_specifiers()? {
                None => return Ok(None),
                Some((_, storage_class @ Some(_))) => return Err(anyhow!("{storage_class:#?}")),
                Some((typ, None)) => typ,
            };

            let declarator = self.parse_abstract_declarator()?;

            let final_type = self.derive_abstract_declared_type(base_type, declarator);
            Ok(Some(final_type))
        };
        inner().context("[ <type-name> ]")
    }

    fn parse_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        let mut inner = || -> Result<_> {
            let mut ptr_count = 0;
            while let t::Token::Operator(t::Operator::Star) = self.peek_token()? {
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
        inner().context("[ <abstract-declarator> ]")
    }
    fn parse_direct_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        let mut inner = || -> Result<_> {
            let mut declarator = match self.peek_token()? {
                t::Token::Demarcator(t::Demarcator::ParenOpen) => {
                    self.tokens.next();

                    let declarator = self.parse_abstract_declarator()?;
                    if declarator.items_baseward.is_empty() {
                        return Err(anyhow!(
                            "Empty parentheses. It means a <param-list>. Can't cast to a function type."
                        ));
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
        base_type: ObjType,
        AbstractDeclarator { mut items_baseward, .. }: AbstractDeclarator,
    ) -> ParsedObjType {
        let mut cur_type = self.obj_type_repo.get_or_new(base_type);

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
                    match self.parse_array_type(cur_type, elem_count) {
                        Ok(obj_type) => {
                            cur_type = obj_type;
                        }
                        Err(err) => {
                            return ParsedObjType(Err(err));
                        }
                    }
                }
            }
        }

        ParsedObjType(Ok(cur_type))
    }
}

/// Helpers
impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    /// The official tester requires us to allow invalid array types at the parser stage.
    ///
    /// In addition, any types that are derived from invalid array types must also be unconditionally allowed.
    ///     + pointer_to(array), array_of(array), and any combination thereof, are valid in C.
    ///     + function(return=array) is invalid in C,
    ///         but the parser grammer doesn't validate this, and the official tester requires us to allow these types.
    ///
    /// Hence, at each code site that calls this helper,
    ///     given each invalid array type,
    ///     we immediately return the invalid result,
    ///     without applying further validations using the remaining leafward declarator items.
    fn parse_array_type(
        &mut self,
        elem_type: Singleton<ObjType>,
        elem_count: ArrayElementCount,
    ) -> Result<Singleton<ObjType>, ParsedObjTypeError> {
        match NonVoidType::try_from(elem_type) {
            Err(_) => Err(ParsedObjTypeError::ArrayElemNonCompletableType),
            Ok(nonvoid_type) => {
                let arr_type = ArrayType::new(nonvoid_type, elem_count);
                let arr_type = self.obj_type_repo.get_or_new(arr_type.into());
                Ok(arr_type)
            }
        }
    }
}

mod non_abstract_declarator {
    use super::*;

    #[derive(Debug)]
    pub struct Declarator {
        pub ident: RawIdentifier,
        pub items_baseward: Vec<DeclaratorItem>,
        _dummy: (), // Help clarify where we're constructing.
    }
    impl Declarator {
        pub fn new(ident: RawIdentifier) -> Self {
            Self {
                ident,
                items_baseward: vec![],
                _dummy: (),
            }
        }
    }
    #[derive(Debug)]
    pub enum DeclaratorItem {
        Ptr(u64), // The u64 = repetition.
        Arr(ArrayElementCount),
        Fun(Vec<Param>),
    }
    #[derive(Debug)]
    pub struct Param(pub ObjType, pub Declarator);

    #[derive(Debug)]
    pub enum DeclaratorResult {
        Var(RawIdentifier, ParsedObjType),
        Fun(RawIdentifier, Singleton<ParsedFunType>, Vec<RawIdentifier>),
    }
}

mod abstract_declarator {
    use super::*;

    pub struct AbstractDeclarator {
        pub items_baseward: Vec<AbstractDeclaratorItem>,
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
    pub enum AbstractDeclaratorItem {
        Ptr(u64), // The u64 = repetition.
        Arr(ArrayElementCount),
    }
}
