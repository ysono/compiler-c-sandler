use super::{ParsedCAst, Parser};
use crate::{
    common::{
        identifier::RawIdentifier,
        types_frontend::{ArithmeticType, FunType, Type, VarType},
    },
    ds_n_a::singleton::Singleton,
    stage1_lex::tokens as t,
    stage2_parse::c_ast::*,
    utils::noop,
};
use anyhow::{anyhow, Context, Result};
use std::cmp;

impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    /* Non-abstract declaration */

    pub(super) fn maybe_parse_declaration(&mut self) -> Result<Option<Declaration<ParsedCAst>>> {
        let mut inner = || -> Result<_> {
            let (base_ari_type, storage_class) = match self.maybe_parse_specifiers()? {
                None => return Ok(None),
                Some(specifiers) => specifiers,
            };

            let declarator = self
                .parse_declarator()
                .context("<declarator>, from tokens to intermediary repr")?;

            let base_type = self.var_type_repo.get_or_new(base_ari_type.into());
            let (ident, final_type, param_idents) = self
                .derive_declared_type(base_type, declarator)
                .context("<declarator>, from intermediary repr to final type")?;

            let declaration = match final_type {
                Type::Var(typ) => {
                    let init = self.parse_var_init()?;

                    let decl = VariableDeclaration { ident, typ, storage_class, init };
                    Declaration::Var(decl)
                }
                Type::Fun(typ) => {
                    let body = self.parse_fun_body()?;

                    let decl = FunctionDeclaration {
                        ident,
                        typ,
                        storage_class,
                        param_idents,
                        body,
                    };
                    Declaration::Fun(decl)
                }
            };
            Ok(Some(declaration))
        };
        inner().context("<declaration>")
    }

    fn maybe_parse_specifiers(
        &mut self,
    ) -> Result<Option<(ArithmeticType, Option<StorageClassSpecifier>)>> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Type(_) | t::Token::StorageClassSpecifier(_))) => noop!(),
                _ => return Ok(None),
            }

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
        inner().context("<specifier>+")
    }

    fn parse_declarator(&mut self) -> Result<Declarator> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Operator(t::Operator::Star))) => {
                    self.tokens.next();

                    let sub_declarator = self.parse_declarator()?;

                    Ok(Declarator::Pointer(Box::new(sub_declarator)))
                }
                _ => self.parse_direct_declarator(),
            }
        };
        inner().context("<declarator>")
    }
    fn parse_direct_declarator(&mut self) -> Result<Declarator> {
        let mut inner = || -> Result<_> {
            let mut lhs_declarator = self.parse_simple_declarator()?;

            while let Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) = self.tokens.peek()
            {
                self.tokens.next();

                let params = self.parse_param_list()?;

                self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                lhs_declarator = Declarator::Fun(params, Box::new(lhs_declarator));
            }

            Ok(lhs_declarator)
        };
        inner().context("<direct-declarator>")
    }
    fn parse_simple_declarator(&mut self) -> Result<Declarator> {
        let mut inner = || -> Result<_> {
            match self.tokens.next() {
                Some(Ok(t::Token::Identifier(ident))) => Ok(Declarator::Ident(ident)),
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
    fn parse_param_list(&mut self) -> Result<Vec<Param>> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Type(t::Type::Void))) => {
                    self.tokens.next();
                    Ok(Vec::with_capacity(0))
                }
                _ => {
                    let mut params = vec![];

                    loop {
                        let param_ari_type = match self.maybe_parse_specifiers()? {
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

                    Ok(params)
                }
            }
        };
        inner().context("<param-list>")
    }

    fn derive_declared_type(
        &mut self,
        base_type: Singleton<VarType>,
        declarator: Declarator,
    ) -> Result<(RawIdentifier, Type, Vec<RawIdentifier>)> {
        let inner = || -> Result<_> {
            match declarator {
                Declarator::Pointer(sub_declarator) => {
                    let curr_type = self.var_type_repo.get_or_new(VarType::Pointer(base_type));
                    self.derive_declared_type(curr_type, *sub_declarator)
                }
                Declarator::Fun(params, sub_declarator) => match *sub_declarator {
                    Declarator::Ident(ident) => {
                        let mut param_types = Vec::with_capacity(params.len());
                        let mut param_idents = Vec::with_capacity(params.len());
                        for Param(param_base_ari_type, param_declarator) in params {
                            let param_base_type =
                                self.var_type_repo.get_or_new(param_base_ari_type.into());
                            let (param_ident, param_type, _) =
                                self.derive_declared_type(param_base_type, param_declarator)?;
                            let param_type = match param_type {
                                Type::Var(t) => t,
                                Type::Fun(t) => {
                                    return Err(anyhow!(
                                        "Function parameters aren't supported. {t:?}"
                                    ))
                                }
                            };
                            param_types.push(param_type);
                            param_idents.push(param_ident);
                        }

                        let fun_type = FunType {
                            params: param_types,
                            ret: base_type,
                        };
                        let fun_type = self.fun_type_repo.get_or_new(fun_type);

                        Ok((ident, Type::Fun(fun_type), param_idents))
                    }
                    Declarator::Fun(..) => {
                        Err(anyhow!("In C, a function can't return a function."))
                    }
                    Declarator::Pointer(_) => Err(anyhow!("Function pointers aren't supported.")),
                },
                Declarator::Ident(ident) => {
                    Ok((ident, Type::Var(base_type), Vec::with_capacity(0)))
                }
            }
        };
        inner().context("(declarator intermediary repr)")
    }

    fn parse_var_init(&mut self) -> Result<Option<Expression<ParsedCAst>>> {
        let mut inner = || -> Result<_> {
            match self.tokens.next() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => Ok(None),
                Some(Ok(t::Token::Operator(t::Operator::Assign))) => {
                    let init = self.parse_exp()?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Some(init))
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<variable-declaration> initializer")
    }

    fn parse_fun_body(&mut self) -> Result<Option<Block<ParsedCAst>>> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    self.tokens.next();
                    Ok(None)
                }
                _ => self.parse_block().map(Some),
            }
        };
        inner().context("<function-declaration> body")
    }

    /* Abstract declaration */

    pub(super) fn parse_abstract_declaration(&mut self) -> Result<Option<Singleton<VarType>>> {
        let mut inner = || -> Result<_> {
            let (base_ari_type, storage_class) = match self.maybe_parse_specifiers()? {
                None => return Ok(None),
                Some(specifiers) => specifiers,
            };

            if storage_class.is_some() {
                return Err(anyhow!("{storage_class:?}"));
            }

            let declarator = self
                .parse_abstract_declarator()
                .context("<abstract-declarator>, from tokens to intermediary repr")?;

            let base_type = self.var_type_repo.get_or_new(base_ari_type.into());
            let final_type = self.derive_abstract_declared_type(base_type, declarator);
            Ok(Some(final_type))
        };
        inner().context("<abstract-declarator>")
    }

    fn parse_abstract_declarator(&mut self) -> Result<AbstractDeclarator> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Operator(t::Operator::Star))) => {
                    self.tokens.next();

                    let sub_declarator = self.parse_abstract_declarator()?;

                    let curr_declarator = AbstractDeclarator::Pointer(Box::new(sub_declarator));
                    Ok(curr_declarator)
                }
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    self.tokens.next();

                    let declarator = self.parse_abstract_declarator()?;

                    self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                    Ok(declarator)
                }
                _ => Ok(AbstractDeclarator::Base),
            }
        };
        inner().context("<abstract-declarator>")
    }

    fn derive_abstract_declared_type(
        &mut self,
        base_type: Singleton<VarType>,
        declarator: AbstractDeclarator,
    ) -> Singleton<VarType> {
        match declarator {
            AbstractDeclarator::Pointer(sub_declarator) => {
                let curr_type = self.var_type_repo.get_or_new(VarType::Pointer(base_type));
                self.derive_abstract_declared_type(curr_type, *sub_declarator)
            }
            AbstractDeclarator::Base => base_type,
        }
    }
}

enum Declarator {
    Pointer(Box<Declarator>),
    Fun(Vec<Param>, Box<Declarator>),
    Ident(RawIdentifier),
}
struct Param(ArithmeticType, Declarator);

enum AbstractDeclarator {
    Pointer(Box<AbstractDeclarator>),
    Base,
}
