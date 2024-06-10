//! ```ebnf
//! <program> ::= { <function-declaration> }
//! <declaration> ::= <variable-declaration> | <function-declaration>
//! <variable-declaration> ::= "int" <identifier> [ "=" <exp> ] ";"
//! <function-declaration> ::= "int" <identifier> "(" <param-list> ")" ( <block> | ";")
//! <param-list> ::= "void" | "int" <identifier> { "," "int" <identifier> }
//! <block> ::= "{" { <block-item> } "}"
//! <block-item> ::= <statement> | <declaration>
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
//!            | <identifier> "(" [ <argument-list> ] ")"
//! <argument-list> ::= <exp> { "," <exp> }
//! <unop> ::= "-" | "~" | "!"
//! <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "="
//! <identifier> ::= ? An identifier token ?
//! <int> ::= ? A constant token ?
//! ```

use crate::{stage1_lex::tokens as t, stage2_parse::c_ast::*};
use anyhow::{anyhow, Context, Result};
use std::iter::Peekable;

enum BinaryOperatorInfo {
    Generic(BinaryOperator),
    ControlQuestion,
    Assign,
}
impl BinaryOperatorInfo {
    fn from(t_op: &t::Operator) -> Option<Self> {
        use t::Operator as TO;
        use BinaryOperator as CBO;
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
        let mut inner = || -> Result<_> {
            let mut fun_decls = vec![];
            loop {
                match self.tokens.peek() {
                    None => break,
                    _ => match self.parse_decl()? {
                        Declaration::FunDecl(fun_decl) => fun_decls.push(fun_decl),
                        Declaration::VarDecl(var_decl) => {
                            return Err(anyhow!(
                                "Global variable declarations are not supported yet. {var_decl:?}"
                            ))
                        }
                    },
                }
            }
            Ok(Program { fun_decls })
        };
        inner().context("tokens -> c_ast <program>")
    }

    /* Declaration */

    fn parse_decl(&mut self) -> Result<Declaration> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Keyword::Int.into()])?;

            let ident = match self.tokens.next() {
                Some(Ok(t::Token::Identifier(ident))) => ident,
                actual => return Err(anyhow!("{actual:?}")),
            };

            match self.tokens.next() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    Ok(Declaration::VarDecl(VariableDeclaration {
                        ident,
                        init: None,
                    }))
                }
                Some(Ok(t::Token::Operator(t::Operator::Assign))) => {
                    let init = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Declaration::VarDecl(VariableDeclaration {
                        ident,
                        init: Some(init),
                    }))
                }
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    let params = self.parse_param_list()?;

                    self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                    let body = match self.tokens.peek() {
                        Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                            self.tokens.next();
                            None
                        }
                        _ => {
                            let body = self.parse_block()?;
                            Some(body)
                        }
                    };

                    Ok(Declaration::FunDecl(FunctionDeclaration {
                        ident,
                        params,
                        body,
                    }))
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<declaration>")
    }
    fn parse_param_list(&mut self) -> Result<Vec<Identifier>> {
        let mut inner = || -> Result<Vec<_>> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Keyword(t::Keyword::Void))) => {
                    self.tokens.next();

                    Ok(Vec::with_capacity(0))
                }
                Some(Ok(t::Token::Keyword(t::Keyword::Int))) => {
                    let mut idents = vec![];

                    loop {
                        self.expect_exact(&[t::Keyword::Int.into()])?;

                        match self.tokens.next() {
                            Some(Ok(t::Token::Identifier(ident))) => idents.push(ident),
                            actual => return Err(anyhow!("{actual:?}")),
                        }

                        match self.tokens.peek() {
                            Some(Ok(t::Token::Demarcator(t::Demarcator::Comma))) => {
                                self.tokens.next();
                                continue;
                            }
                            _ => break,
                        }
                    }

                    Ok(idents)
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<param-list>")
    }

    /* Block */

    fn parse_block(&mut self) -> Result<Block> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Demarcator::BraceOpen.into()])?;

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
        };
        inner().context("<block>")
    }
    fn parse_block_item(&mut self) -> Result<BlockItem> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Keyword(t::Keyword::Int))) => {
                    self.parse_decl().map(BlockItem::Declaration)
                }
                _ => self.parse_stmt().map(BlockItem::Statement),
            }
        };
        inner().context("<block-item>")
    }

    /* Statement */

    fn parse_stmt(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    self.tokens.next();

                    Ok(Statement::Null)
                }
                Some(Ok(t::Token::Keyword(t::Keyword::Return))) => {
                    self.tokens.next();

                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

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

                    Ok(Statement::Break)
                }
                Some(Ok(t::Token::Loop(t::Loop::Continue))) => {
                    self.tokens.next();

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Continue)
                }
                Some(Ok(t::Token::Loop(t::Loop::While))) => self.parse_stmt_while(),
                Some(Ok(t::Token::Loop(t::Loop::Do))) => self.parse_stmt_dowhile(),
                Some(Ok(t::Token::Loop(t::Loop::For))) => self.parse_stmt_for(),
                _ => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Expression(exp))
                }
            }
        };
        inner().context("<statement>")
    }
    fn parse_stmt_if(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Control::If.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

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
    fn parse_stmt_while(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::While.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

            let body = self.parse_stmt()?;

            Ok(Statement::While(CondBody {
                condition,
                body: Box::new(body),
            }))
        };
        inner().context("<statement> while")
    }
    fn parse_stmt_dowhile(&mut self) -> Result<Statement> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::Do.into()])?;

            let body = self.parse_stmt()?;

            self.expect_exact(&[t::Loop::While.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

            self.expect_exact(&[
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
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::For.into(), t::Demarcator::ParenOpen.into()])?;

            let init = match self.tokens.peek() {
                Some(Ok(t::Token::Keyword(t::Keyword::Int))) => match self.parse_decl()? {
                    Declaration::VarDecl(var_decl) => ForInit::Decl(var_decl),
                    Declaration::FunDecl(fun_decl) => return Err(anyhow!("{fun_decl:?}")),
                },
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    self.tokens.next();

                    ForInit::None
                }
                _ => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

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

            self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

            let post = match self.tokens.peek() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenClose))) => None,
                _ => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                    Some(exp)
                }
            };

            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

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
        let mut inner = || -> Result<_> {
            let mut lhs = self.parse_factor()?;

            loop {
                let boi = match self.tokens.peek() {
                    Some(Ok(t::Token::Operator(t_op))) => BinaryOperatorInfo::from(t_op),
                    _ => None,
                };
                if let Some(boi) = boi {
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

                                self.expect_exact(&[t::Operator::Colon.into()])?;

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
        let mut inner = || -> Result<_> {
            match self.tokens.next() {
                Some(Ok(t::Token::Const(konst))) => return Ok(Expression::Const(konst)),
                Some(Ok(t::Token::Identifier(ident))) => match self.tokens.peek() {
                    Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                        let args = self.parse_arg_list()?;
                        return Ok(Expression::FunctionCall(FunctionCall { ident, args }));
                    }
                    _ => return Ok(Expression::Var(ident)),
                },
                Some(Ok(t::Token::Operator(t_op))) => {
                    let c_op_unary = match t_op {
                        t::Operator::Tilde => Some(UnaryOperator::Complement),
                        t::Operator::Minus => Some(UnaryOperator::Negate),
                        t::Operator::Not => Some(UnaryOperator::Not),
                        _ => None,
                    };
                    match c_op_unary {
                        Some(c_op_unary) => {
                            let exp = self.parse_factor()?;
                            return Ok(Expression::Unary(Unary {
                                op: c_op_unary,
                                sub_exp: Box::new(exp),
                            }));
                        }
                        None => {
                            let actual: Option<Result<t::Token>> =
                                Some(Ok(t::Token::Operator(t_op)));
                            return Err(anyhow!("{actual:?}"));
                        }
                    }
                }
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                    self.expect_exact(&[t::Demarcator::ParenClose.into()])?;
                    return Ok(exp);
                }
                actual => return Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<factor>")
    }
    fn parse_arg_list(&mut self) -> Result<Vec<Expression>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Demarcator::ParenOpen.into()])?;

            let mut args = vec![];
            loop {
                match self.tokens.peek() {
                    Some(Ok(t::Token::Demarcator(t::Demarcator::ParenClose))) => {
                        self.tokens.next();
                        break;
                    }
                    _ => {
                        if args.len() > 0 {
                            self.expect_exact(&[t::Demarcator::Comma.into()])?;
                        }

                        let arg = self.parse_exp(BinaryOperatorPrecedence::min())?;
                        args.push(arg);
                    }
                }
            }
            Ok(args)
        };
        inner().context("<argument-list>")
    }

    /* Helpers */

    fn expect_exact(&mut self, next_tokens: &[t::Token]) -> Result<()> {
        for expected in next_tokens {
            match self.tokens.next() {
                Some(Ok(actual)) if expected == &actual => {}
                actual => return Err(anyhow!("Expected {:?} but found {:?}", expected, actual)),
            }
        }
        Ok(())
    }
}
