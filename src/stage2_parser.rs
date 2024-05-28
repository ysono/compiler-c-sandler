//! ```
//! <program> ::= <function>
//! <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
//! <statement> ::= "return" <exp> ";"
//! <exp> ::= <int> | <unop> <exp> | "(" <exp> ")"
//! <unop> ::= "-" | "~"
//! <identifier> ::= ? An identifier token ?
//! <int> ::= ? A constant token ?
//! ```

use crate::stage1_lexer::tokens::{Demarcator, Identifier, Keyword, Operation, Token};
use anyhow::{anyhow, Result};
use std::any;

pub mod c_ast {
    use crate::stage1_lexer::tokens::{Const, Identifier};
    use derive_more::From;

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
    #[derive(From, Debug)]
    pub enum Expression {
        Const(Const),
        Unary(UnaryOperator, Box<Expression>),
    }
    #[derive(Debug)]
    pub enum UnaryOperator {
        Complement,
        Negate,
    }
}
use c_ast::*;

pub struct Parser<T> {
    tokens: T,
}
impl<T: Iterator<Item = Result<Token>>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Self { tokens }
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let func = self.parse_func()?;

        match self.tokens.next() {
            None => {}
            actual => return Err(anyhow!("Expected EOF but found {:?}", actual)),
        }

        Ok(Program { func })
    }
    fn parse_func(&mut self) -> Result<Function> {
        self.expect_exact([Keyword::Int.into()])?;

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

        self.expect_exact([
            Demarcator::ParenOpen.into(),
            Keyword::Void.into(),
            Demarcator::ParenClose.into(),
            Demarcator::BraceOpen.into(),
        ])?;

        let stmt = self.parse_stmt()?;

        self.expect_exact([Demarcator::BraceClose.into()])?;

        Ok(Function { ident, stmt })
    }
    fn parse_stmt(&mut self) -> Result<Statement> {
        self.expect_exact([Keyword::Return.into()])?;

        let exp = self.parse_exp()?;

        self.expect_exact([Demarcator::Semicolon.into()])?;

        Ok(Statement::Return(exp))
    }
    fn parse_exp(&mut self) -> Result<Expression> {
        match self.tokens.next() {
            Some(Ok(Token::Const(const_))) => return Ok(const_.into()),
            Some(Ok(Token::Operation(token_op))) => {
                let ast_op = match token_op {
                    Operation::Complement => UnaryOperator::Complement,
                    Operation::Negate => UnaryOperator::Negate,
                };
                let exp = self.parse_exp()?;
                let exp = Box::new(exp);
                return Ok(Expression::Unary(ast_op, exp));
            }
            Some(Ok(Token::Demarcator(Demarcator::ParenOpen))) => {
                let exp = self.parse_exp()?;
                self.expect_exact([Demarcator::ParenClose.into()])?;
                return Ok(exp);
            }
            actual => {
                return Err(anyhow!(
                    "Could not parse {} at {:?}",
                    any::type_name::<Expression>(),
                    actual
                ));
            }
        }
    }

    fn expect_exact<const LEN: usize>(&mut self, next_tokens: [Token; LEN]) -> Result<()> {
        for expected in next_tokens {
            match self.tokens.next() {
                Some(Ok(actual)) if expected == actual => {}
                actual => return Err(anyhow!("Expected {:?} but found {:?}", expected, actual)),
            }
        }
        Ok(())
    }
}
