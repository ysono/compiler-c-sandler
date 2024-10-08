use super::{decl::DeclaratorResult, ParsedCAst, Parser};
use crate::{stage1_lex::tokens as t, stage2_parse::c_ast::*};
use anyhow::{anyhow, Context, Result};

impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    pub(super) fn parse_declaration(&mut self) -> Result<Option<Declaration<ParsedCAst>>> {
        let mut inner = || -> Result<_> {
            let (declarator_res, storage_class) = match self.parse_specifiers_and_declarator()? {
                None => return Ok(None),
                Some(x) => x,
            };

            let declaration = match declarator_res {
                DeclaratorResult::Var(ident, typ) => {
                    let init = self.parse_var_init()?;

                    let decl = VariableDeclaration { ident, typ, storage_class, init };
                    Declaration::Var(decl)
                }
                DeclaratorResult::Fun(ident, typ, param_idents) => {
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
        inner().context("[ <declaration> ]")
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
        inner().context(r#"[ "=" <exp> ] ";""#)
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
        inner().context(r#"<block> | ";""#)
    }
}
