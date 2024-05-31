//! ```
//! <program> ::= <function>
//! <function> ::= "int" <identifier> "(" "void" ")" "{" <statement> "}"
//! <statement> ::= "return" <exp> ";"
//! <exp> ::= <factor> | <exp> <binop> <exp>
//! <factor> ::= <int> | <unop> <factor> | "(" <exp> ")"
//! <unop> ::= "-" | "~" | "!"
//! <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">="
//! <identifier> ::= ? An identifier token ?
//! <int> ::= ? A constant token ?
//! ```

use crate::stage1_lexer::tokens::{Demarcator, Identifier, Keyword, Operator, Token};
use anyhow::{anyhow, Context, Result};
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
        Unary(expression::Unary),
        Binary(expression::Binary),
    }
    pub mod expression {
        use super::*;

        #[derive(Debug)]
        pub struct Unary {
            pub op: UnaryOperator,
            pub sub_exp: Box<Expression>,
        }

        #[derive(Debug)]
        pub struct Binary {
            pub op: BinaryOperator,
            pub lhs: Box<Expression>,
            pub rhs: Box<Expression>,
        }
    }

    #[derive(Debug)]
    pub enum UnaryOperator {
        /* -> int */
        Complement,
        Negate,
        /* -> bool */
        Not,
    }

    #[derive(Debug)]
    pub enum BinaryOperator {
        /* -> int */
        Sub,
        Add,
        Mul,
        Div,
        Rem,
        /* -(logic)-> bool */
        And,
        Or,
        /* -(compare)-> bool */
        Eq,
        Neq,
        Lt,
        Lte,
        Gt,
        Gte,
    }
}
use c_ast::expression::*;
use c_ast::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BinaryOperatorPrecedence(u8);
impl<'a> From<&'a BinaryOperator> for BinaryOperatorPrecedence {
    fn from(op: &'a BinaryOperator) -> Self {
        use BinaryOperator as BO;
        match op {
            BO::Mul | BO::Div | BO::Rem => Self(50),
            BO::Sub | BO::Add => Self(45),
            BO::Lt | BO::Lte | BO::Gt | BO::Gte => Self(35),
            BO::Eq | BO::Neq => Self(30),
            BO::And => Self(10),
            BO::Or => Self(5),
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
        let mut inner = || -> Result<Program> {
            let func = self.parse_func()?;

            match self.tokens.next() {
                None => {}
                actual => return Err(anyhow!("Expected end-of-tokens but found {actual:?}")),
            }

            Ok(Program { func })
        };
        inner().context("<program>")
    }
    fn parse_func(&mut self) -> Result<Function> {
        let mut inner = || -> Result<Function> {
            self.expect_exact([Keyword::Int.into()])?;

            let ident = self.parse_ident()?;

            self.expect_exact([
                Demarcator::ParenOpen.into(),
                Keyword::Void.into(),
                Demarcator::ParenClose.into(),
                Demarcator::BraceOpen.into(),
            ])?;

            let stmt = self.parse_stmt()?;

            self.expect_exact([Demarcator::BraceClose.into()])?;

            Ok(Function { ident, stmt })
        };
        inner().context("<function>")
    }
    fn parse_ident(&mut self) -> Result<Identifier> {
        let mut inner = || -> Result<Identifier> {
            match self.tokens.next() {
                Some(Ok(Token::Identifier(ident))) => Ok(ident),
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<identifier>")
    }
    fn parse_stmt(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<Statement> {
            self.expect_exact([Keyword::Return.into()])?;

            let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

            self.expect_exact([Demarcator::Semicolon.into()])?;

            Ok(Statement::Return(exp))
        };
        inner().context("<statement>")
    }
    fn parse_exp(&mut self, min_prec: BinaryOperatorPrecedence) -> Result<Expression> {
        let mut inner = || -> Result<Expression> {
            let mut lhs: Expression = self.parse_factor()?;

            loop {
                if let Some(c_op) = self.peek_binary_op() {
                    let nxt_prec = BinaryOperatorPrecedence::from(&c_op);
                    if nxt_prec >= min_prec {
                        self.tokens.next();

                        let rhs = self.parse_exp(nxt_prec.inc1())?;

                        lhs = Expression::Binary(Binary {
                            op: c_op,
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs),
                        });

                        continue;
                    }
                }
                break;
            }

            Ok(lhs)
        };
        inner().context("<exp>")
    }
    fn parse_factor(&mut self) -> Result<Expression> {
        let mut inner = || -> Result<Expression> {
            match self.tokens.next() {
                Some(Ok(Token::Const(konst))) => return Ok(Expression::Const(konst)),
                Some(Ok(Token::Operator(t_op))) => match Self::convert_unary_op(&t_op) {
                    Some(c_op) => {
                        let exp = self.parse_factor()?;
                        return Ok(Expression::Unary(Unary {
                            op: c_op,
                            sub_exp: Box::new(exp),
                        }));
                    }
                    None => {
                        let actual: Option<Result<Token>> = Some(Ok(Token::Operator(t_op)));
                        return Err(anyhow!("{actual:?}"));
                    }
                },
                Some(Ok(Token::Demarcator(Demarcator::ParenOpen))) => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                    self.expect_exact([Demarcator::ParenClose.into()])?;
                    return Ok(exp);
                }
                actual => return Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<factor>")
    }
    fn peek_binary_op(&mut self) -> Option<BinaryOperator> {
        if let Some(Ok(Token::Operator(op))) = self.tokens.peek() {
            match op {
                Operator::Minus => return Some(BinaryOperator::Sub),
                Operator::Plus => return Some(BinaryOperator::Add),
                Operator::Star => return Some(BinaryOperator::Mul),
                Operator::Slash => return Some(BinaryOperator::Div),
                Operator::Percent => return Some(BinaryOperator::Rem),
                Operator::And => return Some(BinaryOperator::And),
                Operator::Or => return Some(BinaryOperator::Or),
                Operator::Eq => return Some(BinaryOperator::Eq),
                Operator::Neq => return Some(BinaryOperator::Neq),
                Operator::Lt => return Some(BinaryOperator::Lt),
                Operator::Lte => return Some(BinaryOperator::Lte),
                Operator::Gt => return Some(BinaryOperator::Gt),
                Operator::Gte => return Some(BinaryOperator::Gte),
                _ => {}
            }
        }
        None
    }
    fn convert_unary_op(t_op: &Operator) -> Option<UnaryOperator> {
        match t_op {
            Operator::Tilde => Some(UnaryOperator::Complement),
            Operator::Minus => Some(UnaryOperator::Negate),
            Operator::Not => Some(UnaryOperator::Not),
            _ => None,
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
