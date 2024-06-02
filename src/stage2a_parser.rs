//! ```ebnf
//! <program> ::= <function>
//! <function> ::= "int" <identifier> "(" "void" ")" "{" { <block-item> } "}"
//! <block-item> ::= <statement> | <declaration>
//! <declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
//! <statement> ::= "return" <exp> ";"
//!               | <exp> ";"
//!               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
//!               | ";"
//! <exp> ::= <factor> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
//! <factor> ::= <int> | <identifier> | <unop> <factor> | "(" <exp> ")"
//! <unop> ::= "-" | "~" | "!"
//! <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
//! <identifier> ::= ? An identifier token ?
//! <int> ::= ? A constant token ?
//! ```

pub mod c_ast {
    pub use self::expression::*;
    pub use crate::stage1_lexer::tokens::{Const, Identifier};

    #[derive(Debug)]
    pub struct Program {
        pub func: Function,
    }

    #[derive(Debug)]
    pub struct Function {
        pub ident: Identifier,
        pub body: Vec<BlockItem>,
    }

    #[derive(Debug)]
    pub enum BlockItem {
        Declaration(Declaration),
        Statement(Statement),
    }

    #[derive(Debug)]
    pub struct Declaration {
        pub ident: Identifier,
        pub init: Option<Expression>,
    }

    #[derive(Debug)]
    pub enum Statement {
        Return(Expression),
        Expression(Expression),
        If(If),
        Null,
    }

    #[derive(Debug)]
    pub enum Expression {
        Const(Const),
        Var(Identifier),
        Unary(Unary),
        Binary(Binary),
        Assignment(Assignment),
        Conditional(Conditional),
    }
    mod expression {
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

        #[derive(Debug)]
        pub struct Assignment {
            pub lhs: Box<Expression>,
            pub rhs: Box<Expression>,
        }

        #[derive(Debug)]
        pub struct Conditional {
            pub condition: Box<Expression>,
            pub then: Box<Expression>,
            pub elze: Box<Expression>,
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

    #[derive(Debug)]
    pub struct If {
        pub condition: Expression,
        pub then: Box<Statement>,
        pub elze: Option<Box<Statement>>,
    }
}

use self::c_ast::*;
use crate::stage1_lexer::tokens::{self, Control, Demarcator, Keyword, Operator, Token};
use anyhow::{anyhow, Context, Result};
use std::iter::Peekable;

enum BinaryOperatorInfo {
    Generic(BinaryOperator),
    ControlQuestion,
    Assign,
}
impl BinaryOperatorInfo {
    fn from(t_op: &tokens::Operator) -> Option<Self> {
        use c_ast::BinaryOperator as CBO;
        use tokens::Operator as TO;
        match t_op {
            TO::Minus => Some(Self::Generic(CBO::Sub)),
            TO::Plus => Some(Self::Generic(CBO::Add)),
            TO::Star => Some(Self::Generic(CBO::Mul)),
            TO::Slash => Some(Self::Generic(CBO::Div)),
            TO::Percent => Some(Self::Generic(CBO::Rem)),
            TO::And => Some(Self::Generic(CBO::And)),
            TO::Or => Some(Self::Generic(CBO::Or)),
            TO::Eq => Some(Self::Generic(CBO::Eq)),
            TO::Neq => Some(Self::Generic(CBO::Neq)),
            TO::Lt => Some(Self::Generic(CBO::Lt)),
            TO::Lte => Some(Self::Generic(CBO::Lte)),
            TO::Gt => Some(Self::Generic(CBO::Gt)),
            TO::Gte => Some(Self::Generic(CBO::Gte)),
            TO::Question => Some(Self::ControlQuestion),
            TO::Assign => Some(Self::Assign),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct BinaryOperatorPrecedence(u8);
impl<'a> From<&'a BinaryOperatorInfo> for BinaryOperatorPrecedence {
    fn from(boi: &'a BinaryOperatorInfo) -> Self {
        use BinaryOperator as BO;
        match boi {
            BinaryOperatorInfo::Generic(bo) => match bo {
                BO::Mul | BO::Div | BO::Rem => Self(50),
                BO::Sub | BO::Add => Self(45),
                BO::Lt | BO::Lte | BO::Gt | BO::Gte => Self(35),
                BO::Eq | BO::Neq => Self(30),
                BO::And => Self(10),
                BO::Or => Self(5),
            },
            BinaryOperatorInfo::ControlQuestion => Self(3),
            BinaryOperatorInfo::Assign => Self(1),
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
        inner().context("tokens -> c_ast <program>")
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

            let mut body = vec![];
            loop {
                match self.tokens.peek() {
                    Some(Ok(Token::Demarcator(Demarcator::BraceClose))) => {
                        self.tokens.next();
                        break;
                    }
                    _ => {
                        let block_item = self.parse_block_item()?;
                        body.push(block_item);
                    }
                }
            }

            Ok(Function { ident, body })
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
    fn parse_block_item(&mut self) -> Result<BlockItem> {
        let mut inner = || -> Result<BlockItem> {
            match self.tokens.peek() {
                Some(Ok(Token::Keyword(Keyword::Int))) => {
                    self.parse_declaration().map(BlockItem::Declaration)
                }
                _ => self.parse_stmt().map(BlockItem::Statement),
            }
        };
        inner().context("<block-item>")
    }
    fn parse_declaration(&mut self) -> Result<Declaration> {
        let mut inner = || -> Result<Declaration> {
            self.expect_exact([Keyword::Int.into()])?;

            let ident = self.parse_ident()?;

            let init = match self.tokens.peek() {
                Some(Ok(Token::Operator(Operator::Assign))) => {
                    self.tokens.next();

                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                    Some(exp)
                }
                _ => None,
            };

            self.expect_exact([Demarcator::Semicolon.into()])?;

            Ok(Declaration { ident, init })
        };
        inner().context("<declaration>")
    }
    fn parse_stmt(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<Statement> {
            match self.tokens.peek() {
                Some(Ok(Token::Demarcator(Demarcator::Semicolon))) => {
                    self.tokens.next();

                    Ok(Statement::Null)
                }
                Some(Ok(Token::Keyword(Keyword::Return))) => {
                    self.tokens.next();

                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact([Demarcator::Semicolon.into()])?;

                    Ok(Statement::Return(exp))
                }
                Some(Ok(Token::Control(Control::If))) => {
                    self.tokens.next();

                    self.expect_exact([Demarcator::ParenOpen.into()])?;

                    let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact([Demarcator::ParenClose.into()])?;

                    let then = self.parse_stmt()?;

                    let elze = match self.tokens.peek() {
                        Some(Ok(Token::Control(Control::Else))) => {
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
                }
                _ => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact([Demarcator::Semicolon.into()])?;

                    Ok(Statement::Expression(exp))
                }
            }
        };
        inner().context("<statement>")
    }
    fn parse_exp(&mut self, min_prec: BinaryOperatorPrecedence) -> Result<Expression> {
        let mut inner = || -> Result<Expression> {
            let mut lhs = self.parse_factor()?;

            loop {
                if let Some(boi) = self.peek_binary_op() {
                    let nxt_prec = BinaryOperatorPrecedence::from(&boi);
                    if nxt_prec >= min_prec {
                        self.tokens.next();

                        match boi {
                            BinaryOperatorInfo::Generic(c_op) => {
                                let beyond_prec = nxt_prec.inc1(); // Left associative
                                let rhs = self.parse_exp(beyond_prec)?;

                                lhs = Expression::Binary(Binary {
                                    op: c_op,
                                    lhs: Box::new(lhs),
                                    rhs: Box::new(rhs),
                                });
                            }
                            BinaryOperatorInfo::ControlQuestion => {
                                let then = self.parse_exp(BinaryOperatorPrecedence::min())?;

                                self.expect_exact([Operator::Colon.into()])?;

                                let beyond_prec = nxt_prec; // Right associative
                                let elze = self.parse_exp(beyond_prec)?;

                                lhs = Expression::Conditional(Conditional {
                                    condition: Box::new(lhs),
                                    then: Box::new(then),
                                    elze: Box::new(elze),
                                });
                            }
                            BinaryOperatorInfo::Assign => {
                                let beyond_prec = nxt_prec; // Right associative
                                let rhs = self.parse_exp(beyond_prec)?;

                                lhs = Expression::Assignment(Assignment {
                                    lhs: Box::new(lhs),
                                    rhs: Box::new(rhs),
                                });
                            }
                        }

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
                Some(Ok(Token::Identifier(ident))) => return Ok(Expression::Var(ident)),
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
    fn peek_binary_op(&mut self) -> Option<BinaryOperatorInfo> {
        match self.tokens.peek() {
            Some(Ok(Token::Operator(t_op))) => BinaryOperatorInfo::from(t_op),
            _ => None,
        }
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
