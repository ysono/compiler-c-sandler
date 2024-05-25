//! ```
//! <program> ::= <function>
//! <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
//! <statement> ::= "return" <exp> ";"
//! <exp> ::= <int>
//! <identifier> ::= ? An identifier token ?
//! <int> ::= ? A constant token ?
//! ```

use crate::lexer::{Const, Demarcator, Identifier, Keyword, Token};
use anyhow::{anyhow, Result};
use std::any;

#[derive(Debug)]
pub struct Program {
    pub func: Function,
}
#[derive(Debug)]
pub struct Function {
    pub ident: Identifier,
    pub stmt: Statement,
}
#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}
#[derive(Debug)]
pub struct Expression {
    pub const_: Const,
}

pub struct Parser<T> {
    tokens: T,
}
impl<T: Iterator<Item = Result<Token>>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Self { tokens }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let f = self.parse_func()?;
        match self.tokens.next() {
            None => {}
            actual => return Err(anyhow!("Expected EOF but found {:?}", actual)),
        }
        Ok(Program { func: f })
    }
    fn parse_func(&mut self) -> Result<Function> {
        match self.tokens.next() {
            Some(Ok(Token::Keyword(Keyword::Int))) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Keyword::Int,
                    actual
                ));
            }
        }
        let ident = match self.tokens.next() {
            Some(Ok(Token::Identifier(ident))) => ident,
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    any::type_name::<Identifier>(),
                    actual
                ));
            }
        };
        match self.tokens.next() {
            Some(Ok(Token::Demarcator(Demarcator::ParenOpen))) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Demarcator::ParenOpen,
                    actual
                ));
            }
        }
        match self.tokens.next() {
            Some(Ok(Token::Keyword(Keyword::Void))) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Keyword::Void,
                    actual
                ));
            }
        }
        match self.tokens.next() {
            Some(Ok(Token::Demarcator(Demarcator::ParenClose))) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Demarcator::ParenClose,
                    actual
                ));
            }
        }
        match self.tokens.next() {
            Some(Ok(Token::Demarcator(Demarcator::BraceOpen))) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Demarcator::BraceOpen,
                    actual
                ));
            }
        }
        let stmt = self.parse_stmt()?;
        match self.tokens.next() {
            Some(Ok(Token::Demarcator(Demarcator::BraceClose))) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Demarcator::BraceClose,
                    actual
                ));
            }
        }
        Ok(Function { ident, stmt })
    }
    fn parse_stmt(&mut self) -> Result<Statement> {
        match self.tokens.next() {
            Some(Ok(Token::Keyword(Keyword::Return))) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Keyword::Return,
                    actual
                ));
            }
        }
        let exp = self.parse_exp()?;
        match self.tokens.next() {
            Some(Ok(Token::Demarcator(Demarcator::Semicolon))) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Demarcator::Semicolon,
                    actual
                ));
            }
        }
        Ok(Statement::Return(exp))
    }
    fn parse_exp(&mut self) -> Result<Expression> {
        let const_ = match self.tokens.next() {
            Some(Ok(Token::Const(const_))) => const_,
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    any::type_name::<Const>(),
                    actual
                ));
            }
        };
        Ok(Expression { const_ })
    }
}
