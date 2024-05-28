//! ```
//! <program> ::= <function>
//! <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
//! <statement> ::= "return" <exp> ";"
//! <exp> ::= <factor> | <exp> <binop> <exp>
//! <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
//! <unop> ::= "-" | "~"
//! <binop> ::= "-" | "+" | "*" | "/" | "%"
//! <identifier> ::= ? An identifier token ?
//! <int> ::= ? A constant token ?
//! ```

use crate::stage1_lexer::tokens::{Demarcator, Identifier, Keyword, Operator, Token};
use anyhow::{anyhow, Result};
use std::any;
use std::iter::Peekable;

pub mod c_ast {
    use crate::stage1_lexer::tokens::{Const, Identifier};

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
    pub enum Expression {
        Const(Const),
        Unary(UnaryOperator, Box<Expression>),
        Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    }
    #[derive(Debug)]
    pub enum UnaryOperator {
        Complement,
        Negate,
    }
    #[derive(Debug)]
    pub enum BinaryOperator {
        Sub,
        Add,
        Mul,
        Div,
        Rem,
    }
}
use c_ast::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BinaryOperatorPrecedence(u8);
impl<'a> From<&'a BinaryOperator> for BinaryOperatorPrecedence {
    fn from(op: &'a BinaryOperator) -> Self {
        match op {
            BinaryOperator::Sub => Self(45),
            BinaryOperator::Add => Self(45),
            BinaryOperator::Mul => Self(50),
            BinaryOperator::Div => Self(50),
            BinaryOperator::Rem => Self(50),
        }
    }
}
impl BinaryOperatorPrecedence {
    fn min() -> Self {
        Self(0)
    }
    fn inc1(&self) -> Self {
        Self(self.0 + 1)
    }
}

pub struct Parser<T: Iterator<Item = Result<Token>>> {
    tokens: Peekable<T>,
}
impl<T: Iterator<Item = Result<Token>>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
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

        let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

        self.expect_exact([Demarcator::Semicolon.into()])?;

        Ok(Statement::Return(exp))
    }
    fn parse_exp(&mut self, min_prec: BinaryOperatorPrecedence) -> Result<Expression> {
        let mut lhs: Expression = self.parse_factor()?;

        loop {
            if let Some(op) = self.peek_binary_op() {
                let nxt_prec = BinaryOperatorPrecedence::from(&op);
                if nxt_prec >= min_prec {
                    self.tokens.next();

                    let rhs = self.parse_exp(nxt_prec.inc1())?;

                    lhs = Expression::Binary(op, Box::new(lhs), Box::new(rhs));

                    continue;
                }
            }
            break;
        }

        Ok(lhs)
    }
    fn parse_factor(&mut self) -> Result<Expression> {
        let err =
            |actual: Option<Result<Token>>| Err(anyhow!("Could not parse <factor> at {actual:?}"));
        match self.tokens.next() {
            Some(Ok(Token::Const(const_))) => return Ok(Expression::Const(const_)),
            Some(Ok(Token::Operator(op))) => {
                if let Some(op) = Self::convert_unary_op(&op) {
                    let exp = self.parse_factor()?;
                    return Ok(Expression::Unary(op, Box::new(exp)));
                }
                return err(Some(Ok(Token::Operator(op))));
            }
            Some(Ok(Token::Demarcator(Demarcator::ParenOpen))) => {
                let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                self.expect_exact([Demarcator::ParenClose.into()])?;
                return Ok(exp);
            }
            actual => return err(actual),
        }
    }
    fn peek_binary_op(&mut self) -> Option<BinaryOperator> {
        if let Some(Ok(Token::Operator(op))) = self.tokens.peek() {
            match op {
                Operator::Minus => return Some(BinaryOperator::Sub),
                Operator::Plus => return Some(BinaryOperator::Add),
                Operator::Star => return Some(BinaryOperator::Mul),
                Operator::Slash => return Some(BinaryOperator::Div),
                Operator::Percent => return Some(BinaryOperator::Rem),
                _ => {}
            }
        }
        None
    }
    fn convert_unary_op(op: &Operator) -> Option<UnaryOperator> {
        match op {
            Operator::Tilde => return Some(UnaryOperator::Complement),
            Operator::Minus => return Some(UnaryOperator::Negate),
            _ => {}
        }
        None
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
