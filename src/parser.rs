//! ```
//! <program> ::= <function>
//! <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
//! <statement> ::= "return" <exp> ";"
//! <exp> ::= <int>
//! <identifier> ::= ? An identifier token ?
//! <int> ::= ? A constant token ?
//! ```

use crate::lexer::{Const, Identifier, Keyword, Token};
use anyhow::{anyhow, Result};
use std::any;

#[derive(Debug)]
pub struct Program {
    f: Function,
}
#[derive(Debug)]
pub struct Function {
    ident: Identifier,
    stmt: Statement,
}
#[derive(Debug)]
pub struct Statement {
    exp: Expression,
}
#[derive(Debug)]
pub struct Expression {
    const_: Const,
}

pub struct Parser<T> {
    tokens: T,
}
impl<T: Iterator<Item = Result<Token>>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Self { tokens }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let f = self.parse_function()?;
        match self.tokens.next() {
            None => {}
            actual => return Err(anyhow!("Expected EOF but found {:?}", actual)),
        }
        Ok(Program { f })
    }
    fn parse_function(&mut self) -> Result<Function> {
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
            Some(Ok(Token::ParenOpen)) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Token::ParenOpen,
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
            Some(Ok(Token::ParenClose)) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Token::ParenClose,
                    actual
                ));
            }
        }
        match self.tokens.next() {
            Some(Ok(Token::BraceOpen)) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Token::BraceOpen,
                    actual
                ));
            }
        }
        let stmt = self.parse_stmt()?;
        match self.tokens.next() {
            Some(Ok(Token::BraceClose)) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Token::BraceClose,
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
            Some(Ok(Token::Semicolon)) => {}
            actual => {
                return Err(anyhow!(
                    "Expected {:?} but found {:?}",
                    Token::Semicolon,
                    actual
                ));
            }
        }
        Ok(Statement { exp })
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
