use super::{ParsedCAst, Parser, decl::DeclaratorResult};
use crate::{stage1_lex::tokens as t, stage2_parse::c_ast::*, utils::noop};
use anyhow::{Context, Result, anyhow};

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

    fn parse_var_init(&mut self) -> Result<Option<VariableInitializer<ParsedCAst>>> {
        let mut inner = || -> Result<_> {
            match self.tokens.next() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => Ok(None),
                Some(Ok(t::Token::Operator(t::Operator::Assign))) => {
                    let init = self.parse_initializer()?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Some(init))
                }
                actual => Err(anyhow!("{actual:#?}")),
            }
        };
        inner().context(r#"[ "=" <initializer> ] ";""#)
    }
    fn parse_initializer(&mut self) -> Result<VariableInitializer<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::CurlyOpen))) => {
                    self.tokens.next();

                    let init = self.parse_initializer()?;
                    let mut inits = vec![init];

                    loop {
                        match self.tokens.next() {
                            Some(Ok(t::Token::Demarcator(t::Demarcator::CurlyClose))) => break,
                            Some(Ok(t::Token::Demarcator(t::Demarcator::Comma))) => noop!(),
                            actual => return Err(anyhow!("{actual:#?}")),
                        }

                        match self.tokens.peek() {
                            Some(Ok(t::Token::Demarcator(t::Demarcator::CurlyClose))) => {
                                self.tokens.next();
                                break;
                            }
                            _ => {
                                let init = self.parse_initializer()?;
                                inits.push(init);
                            }
                        }
                    }

                    Ok(VariableInitializer::Compound(inits))
                }
                _ => self.parse_exp().map(VariableInitializer::Single),
            }
        };
        inner().context("<initializer>")
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
