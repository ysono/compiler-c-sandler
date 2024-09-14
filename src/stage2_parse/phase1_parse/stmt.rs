use super::{ParsedCAst, Parser};
use crate::{stage1_lex::tokens as t, stage2_parse::c_ast::*};
use anyhow::{anyhow, Context, Result};

impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    pub(super) fn parse_stmt(&mut self) -> Result<Statement<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    self.tokens.next();

                    Ok(Statement::Null)
                }
                Some(Ok(t::Token::Keyword(t::Keyword::Return))) => {
                    self.tokens.next();

                    let exp = self.parse_exp()?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Return(exp))
                }
                Some(Ok(t::Token::Control(t::Control::If))) => self.parse_stmt_if(),
                Some(Ok(t::Token::Demarcator(t::Demarcator::BraceOpen))) => {
                    let block = self.parse_block()?;
                    Ok(Statement::Compound(block))
                }
                Some(Ok(t::Token::Loop(t::Loop::Break))) => {
                    self.tokens.next();

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Break(()))
                }
                Some(Ok(t::Token::Loop(t::Loop::Continue))) => {
                    self.tokens.next();

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Continue(()))
                }
                Some(Ok(t::Token::Loop(t::Loop::While))) => self.parse_stmt_while(),
                Some(Ok(t::Token::Loop(t::Loop::Do))) => self.parse_stmt_dowhile(),
                Some(Ok(t::Token::Loop(t::Loop::For))) => self.parse_stmt_for(),
                _ => {
                    let exp = self.parse_exp()?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Expression(exp))
                }
            }
        };
        inner().context("<statement>")
    }
    fn parse_stmt_if(&mut self) -> Result<Statement<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Control::If.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp()?;

            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

            let then = self.parse_stmt()?;

            let elze = match self.tokens.peek() {
                Some(Ok(t::Token::Control(t::Control::Else))) => {
                    self.tokens.next();

                    let stmt = self.parse_stmt()?;
                    Some(stmt)
                }
                _ => None,
            };

            Ok(Statement::If(If {
                condition,
                then: Box::new(then),
                elze: elze.map(Box::new),
            }))
        };
        inner().context("<statement> if")
    }
    fn parse_stmt_while(&mut self) -> Result<Statement<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::While.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp()?;

            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

            let body = self.parse_stmt()?;

            Ok(Statement::While(
                (),
                CondBody { condition, body: Box::new(body) },
            ))
        };
        inner().context("<statement> while")
    }
    fn parse_stmt_dowhile(&mut self) -> Result<Statement<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::Do.into()])?;

            let body = self.parse_stmt()?;

            self.expect_exact(&[t::Loop::While.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp()?;

            self.expect_exact(&[
                t::Demarcator::ParenClose.into(),
                t::Demarcator::Semicolon.into(),
            ])?;

            Ok(Statement::DoWhile(
                (),
                CondBody { body: Box::new(body), condition },
            ))
        };
        inner().context("<statement> dowhile")
    }
    fn parse_stmt_for(&mut self) -> Result<Statement<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::For.into(), t::Demarcator::ParenOpen.into()])?;

            let init = match self.maybe_parse_decl()? {
                Some(Declaration::VarDecl(vd)) => ForInit::Decl(vd),
                Some(Declaration::FunDecl(fd)) => return Err(anyhow!("{fd:?}")),
                Some(Declaration::FunDefn(fd)) => return Err(anyhow!("{fd:?}")),
                None => match self.tokens.peek() {
                    Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                        self.tokens.next();

                        ForInit::None
                    }
                    _ => {
                        let exp = self.parse_exp()?;

                        self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                        ForInit::Exp(exp)
                    }
                },
            };

            let condition = match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => None,
                _ => {
                    let exp = self.parse_exp()?;
                    Some(exp)
                }
            };

            self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

            let post = match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenClose))) => None,
                _ => {
                    let exp = self.parse_exp()?;
                    Some(exp)
                }
            };

            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

            let body = self.parse_stmt()?;

            Ok(Statement::For(
                (),
                For {
                    init,
                    condition,
                    post,
                    body: Box::new(body),
                },
            ))
        };
        inner().context("<statement> for")
    }
}
