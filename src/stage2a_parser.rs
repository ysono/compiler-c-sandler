//! ```ebnf
//! <program> ::= <function>
//! <function> ::= "int" <identifier> "(" "void" ")" <block>
//! <block> ::= "{" { <block-item> } "}"
//! <block-item> ::= <statement> | <declaration>
//! <declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
//! <statement> ::= "return" <exp> ";"
//!               | <exp> ";"
//!               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
//!               | <block>
//!               | "break" ";"
//!               | "continue" ";"
//!               | "while" "(" <exp> ")" <statement>
//!               | "do" <statement> "while" "(" <exp> ")" ";"
//!               | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
//!               | ";"
//! <for-init> ::= <variable-declaration> | [ <exp> ] ";"
//! <exp> ::= <factor> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
//! <factor> ::= <int> | <identifier> | <unop> <factor> | "(" <exp> ")"
//! <unop> ::= "-" | "~" | "!"
//! <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
//! <identifier> ::= ? An identifier token ?
//! <int> ::= ? A constant token ?
//! ```

pub mod c_ast {
    pub use self::expression::*;
    pub use self::statement::*;
    pub use crate::stage1_lexer::tokens::{Const, Identifier};

    #[derive(Debug)]
    pub struct Program {
        pub func: Function,
    }

    #[derive(Debug)]
    pub struct Function {
        pub ident: Identifier,
        pub body: Block,
    }

    #[derive(Debug)]
    pub struct Block {
        pub items: Vec<BlockItem>,
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
        Compound(Block),
        Break,
        Continue,
        While(CondBody),
        DoWhile(CondBody),
        For(For),
        Null,
    }
    mod statement {
        use super::*;

        #[derive(Debug)]
        pub struct If {
            pub condition: Expression,
            pub then: Box<Statement>,
            pub elze: Option<Box<Statement>>,
        }

        #[derive(Debug)]
        pub struct CondBody {
            pub condition: Expression,
            pub body: Box<Statement>,
        }

        #[derive(Debug)]
        pub struct For {
            pub init: ForInit,
            pub condition: Option<Expression>,
            pub post: Option<Expression>,
            pub body: Box<Statement>,
        }

        #[derive(Debug)]
        pub enum ForInit {
            Decl(Declaration),
            Exp(Expression),
            None,
        }
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
}

use self::c_ast::*;
use crate::stage1_lexer::tokens as t;
use anyhow::{anyhow, Context, Result};
use std::iter::Peekable;

enum BinaryOperatorInfo {
    Generic(BinaryOperator),
    ControlQuestion,
    Assign,
}
impl BinaryOperatorInfo {
    fn from(t_op: &t::Operator) -> Option<Self> {
        use c_ast::BinaryOperator as CBO;
        use t::Operator as TO;
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

pub struct Parser<T: Iterator<Item = Result<t::Token>>> {
    tokens: Peekable<T>,
}
impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
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
            self.expect_exact([t::Keyword::Int.into()])?;

            let ident = self.parse_ident()?;

            self.expect_exact([
                t::Demarcator::ParenOpen.into(),
                t::Keyword::Void.into(),
                t::Demarcator::ParenClose.into(),
            ])?;

            let body = self.parse_block()?;

            Ok(Function { ident, body })
        };
        inner().context("<function>")
    }
    fn parse_ident(&mut self) -> Result<Identifier> {
        let mut inner = || -> Result<Identifier> {
            match self.tokens.next() {
                Some(Ok(t::Token::Identifier(ident))) => Ok(ident),
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<identifier>")
    }
    fn parse_block(&mut self) -> Result<Block> {
        self.expect_exact([t::Demarcator::BraceOpen.into()])?;

        let mut items = vec![];
        loop {
            match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::BraceClose))) => {
                    self.tokens.next();
                    break;
                }
                _ => {
                    let item = self.parse_block_item()?;
                    items.push(item);
                }
            }
        }

        Ok(Block { items })
    }
    fn parse_block_item(&mut self) -> Result<BlockItem> {
        let mut inner = || -> Result<BlockItem> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Keyword(t::Keyword::Int))) => {
                    self.parse_declaration().map(BlockItem::Declaration)
                }
                _ => self.parse_stmt().map(BlockItem::Statement),
            }
        };
        inner().context("<block-item>")
    }
    fn parse_declaration(&mut self) -> Result<Declaration> {
        let mut inner = || -> Result<Declaration> {
            self.expect_exact([t::Keyword::Int.into()])?;

            let ident = self.parse_ident()?;

            let init = match self.tokens.peek() {
                Some(Ok(t::Token::Operator(t::Operator::Assign))) => {
                    self.tokens.next();

                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                    Some(exp)
                }
                _ => None,
            };

            self.expect_exact([t::Demarcator::Semicolon.into()])?;

            Ok(Declaration { ident, init })
        };
        inner().context("<declaration>")
    }

    /* Statement */

    fn parse_stmt(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<Statement> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    self.tokens.next();

                    Ok(Statement::Null)
                }
                Some(Ok(t::Token::Keyword(t::Keyword::Return))) => {
                    self.tokens.next();

                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact([t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Return(exp))
                }
                Some(Ok(t::Token::Control(t::Control::If))) => self.parse_stmt_if(),
                Some(Ok(t::Token::Demarcator(t::Demarcator::BraceOpen))) => {
                    let block = self.parse_block()?;
                    Ok(Statement::Compound(block))
                }
                Some(Ok(t::Token::Loop(t::Loop::Break))) => {
                    self.tokens.next();

                    self.expect_exact([t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Break)
                }
                Some(Ok(t::Token::Loop(t::Loop::Continue))) => {
                    self.tokens.next();

                    self.expect_exact([t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Continue)
                }
                Some(Ok(t::Token::Loop(t::Loop::While))) => self.parse_stmt_while(),
                Some(Ok(t::Token::Loop(t::Loop::Do))) => self.parse_stmt_dowhile(),
                Some(Ok(t::Token::Loop(t::Loop::For))) => self.parse_stmt_for(),
                _ => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact([t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Expression(exp))
                }
            }
        };
        inner().context("<statement>")
    }
    fn parse_stmt_if(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<Statement> {
            self.expect_exact([t::Control::If.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

            self.expect_exact([t::Demarcator::ParenClose.into()])?;

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
    fn parse_stmt_while(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<Statement> {
            self.expect_exact([t::Loop::While.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

            self.expect_exact([t::Demarcator::ParenClose.into()])?;

            let body = self.parse_stmt()?;

            Ok(Statement::While(CondBody {
                condition,
                body: Box::new(body),
            }))
        };
        inner().context("<statement> while")
    }
    fn parse_stmt_dowhile(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<Statement> {
            self.expect_exact([t::Loop::Do.into()])?;

            let body = self.parse_stmt()?;

            self.expect_exact([t::Loop::While.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

            self.expect_exact([
                t::Demarcator::ParenClose.into(),
                t::Demarcator::Semicolon.into(),
            ])?;

            Ok(Statement::DoWhile(CondBody {
                body: Box::new(body),
                condition,
            }))
        };
        inner().context("<statement> dowhile")
    }
    fn parse_stmt_for(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<Statement> {
            self.expect_exact([t::Loop::For.into(), t::Demarcator::ParenOpen.into()])?;

            let init = match self.tokens.peek() {
                Some(Ok(t::Token::Keyword(t::Keyword::Int))) => {
                    let decl = self.parse_declaration()?;
                    ForInit::Decl(decl)
                }
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    self.tokens.next();

                    ForInit::None
                }
                _ => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact([t::Demarcator::Semicolon.into()])?;

                    ForInit::Exp(exp)
                }
            };

            let condition = match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => None,
                _ => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                    Some(exp)
                }
            };

            self.expect_exact([t::Demarcator::Semicolon.into()])?;

            let post = match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenClose))) => None,
                _ => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                    Some(exp)
                }
            };

            self.expect_exact([t::Demarcator::ParenClose.into()])?;

            let body = self.parse_stmt()?;

            Ok(Statement::For(For {
                init,
                condition,
                post,
                body: Box::new(body),
            }))
        };
        inner().context("<statement> for")
    }

    /* Expression */

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

                                self.expect_exact([t::Operator::Colon.into()])?;

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
                Some(Ok(t::Token::Const(konst))) => return Ok(Expression::Const(konst)),
                Some(Ok(t::Token::Identifier(ident))) => return Ok(Expression::Var(ident)),
                Some(Ok(t::Token::Operator(t_op))) => match Self::convert_unary_op(&t_op) {
                    Some(c_op) => {
                        let exp = self.parse_factor()?;
                        return Ok(Expression::Unary(Unary {
                            op: c_op,
                            sub_exp: Box::new(exp),
                        }));
                    }
                    None => {
                        let actual: Option<Result<t::Token>> = Some(Ok(t::Token::Operator(t_op)));
                        return Err(anyhow!("{actual:?}"));
                    }
                },
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                    self.expect_exact([t::Demarcator::ParenClose.into()])?;
                    return Ok(exp);
                }
                actual => return Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<factor>")
    }

    /* Operator */

    fn peek_binary_op(&mut self) -> Option<BinaryOperatorInfo> {
        match self.tokens.peek() {
            Some(Ok(t::Token::Operator(t_op))) => BinaryOperatorInfo::from(t_op),
            _ => None,
        }
    }
    fn convert_unary_op(t_op: &t::Operator) -> Option<UnaryOperator> {
        match t_op {
            t::Operator::Tilde => Some(UnaryOperator::Complement),
            t::Operator::Minus => Some(UnaryOperator::Negate),
            t::Operator::Not => Some(UnaryOperator::Not),
            _ => None,
        }
    }

    fn expect_exact<const LEN: usize>(&mut self, next_tokens: [t::Token; LEN]) -> Result<()> {
        for expected in next_tokens {
            match self.tokens.next() {
                Some(Ok(actual)) if expected == actual => {}
                actual => return Err(anyhow!("Expected {:?} but found {:?}", expected, actual)),
            }
        }
        Ok(())
    }
}
