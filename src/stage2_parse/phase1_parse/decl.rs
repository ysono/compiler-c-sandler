use super::{ParsedCAst, Parser};
use crate::{
    common::types_frontend::{FunType, VarType},
    stage1_lex::tokens as t,
    stage2_parse::c_ast::*,
};
use anyhow::{anyhow, Context, Result};
use std::cmp;
use std::rc::Rc;

impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    pub(super) fn maybe_parse_decl(&mut self) -> Result<Option<Declaration<ParsedCAst>>> {
        let mut inner = || -> Result<_> {
            let (typ, storage_class) = match self.maybe_parse_specifiers()? {
                None => return Ok(None),
                Some((t, sc)) => (t, sc),
            };

            let ident = match self.tokens.next() {
                Some(Ok(t::Token::Identifier(ident))) => ident,
                actual => return Err(anyhow!("{actual:?}")),
            };

            match self.tokens.next() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    Ok(Some(Declaration::VarDecl(VariableDeclaration {
                        ident,
                        init: None,
                        typ,
                        storage_class,
                    })))
                }
                Some(Ok(t::Token::Operator(t::Operator::Assign))) => {
                    let init = self.parse_exp()?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Some(Declaration::VarDecl(VariableDeclaration {
                        ident,
                        init: Some(init),
                        typ,
                        storage_class,
                    })))
                }
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    let (param_typs, param_idents) = self.parse_param_list()?;

                    self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                    let decl = FunctionDeclaration {
                        ident,
                        param_idents,
                        typ: Rc::new(FunType { params: param_typs, ret: typ }),
                        storage_class,
                    };

                    let decl = match self.tokens.peek() {
                        Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                            self.tokens.next();

                            Declaration::FunDecl(decl)
                        }
                        _ => {
                            let body = self.parse_block()?;

                            Declaration::FunDefn(FunctionDefinition { decl, body })
                        }
                    };

                    Ok(Some(decl))
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<declaration>")
    }
    pub(super) fn maybe_parse_specifiers(
        &mut self,
    ) -> Result<Option<(VarType, Option<StorageClassSpecifier>)>> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Type(_) | t::Token::StorageClassSpecifier(_))) => { /* No-op. */ }
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
                    _ => { /* Impossible. */ }
                }
            }

            let expected_max_len = cmp::min(3, typs.len());
            typs[..expected_max_len].sort();

            let var_type = match &typs[..] {
                /* In each pattern below, items must be sorted by the `t::Type` enum type's discriminants. */
                [t::Type::Int] => VarType::Int,
                [t::Type::Int, t::Type::Long] => VarType::Long,
                [t::Type::Int, t::Type::Long, t::Type::Signed] => VarType::Long,
                [t::Type::Int, t::Type::Long, t::Type::Unsigned] => VarType::ULong,
                [t::Type::Int, t::Type::Signed] => VarType::Int,
                [t::Type::Int, t::Type::Unsigned] => VarType::UInt,
                [t::Type::Long] => VarType::Long,
                [t::Type::Long, t::Type::Signed] => VarType::Long,
                [t::Type::Long, t::Type::Unsigned] => VarType::ULong,
                [t::Type::Signed] => VarType::Int,
                [t::Type::Unsigned] => VarType::UInt,
                [t::Type::Double] => VarType::Double,
                actual => return Err(anyhow!("Invalid types. {actual:?}")),
                /* Void is not supported yet. */
            };

            let scs = match &scss[..] {
                [] => None,
                [scs] => Some(*scs),
                actual => return Err(anyhow!("Invalid storage class specifiers. {actual:?}")),
            };

            Ok(Some((var_type, scs)))
        };
        inner().context("<declaration> specifiers")
    }
    fn parse_param_list(&mut self) -> Result<(Vec<VarType>, Vec<t::Identifier>)> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Type(t::Type::Void))) => {
                    self.tokens.next();

                    Ok((Vec::with_capacity(0), Vec::with_capacity(0)))
                }
                _ => {
                    let mut typs = vec![];
                    let mut idents = vec![];

                    loop {
                        match self.maybe_parse_specifiers()? {
                            Some((typ, None)) => typs.push(typ),
                            actual => return Err(anyhow!("{actual:?}")),
                        };

                        match self.tokens.next() {
                            Some(Ok(t::Token::Identifier(ident))) => idents.push(ident),
                            actual => return Err(anyhow!("{actual:?}")),
                        }

                        match self.tokens.peek() {
                            Some(Ok(t::Token::Demarcator(t::Demarcator::Comma))) => {
                                self.tokens.next();
                                continue;
                            }
                            _ => break,
                        }
                    }

                    Ok((typs, idents))
                }
            }
        };
        inner().context("<param-list>")
    }
}
