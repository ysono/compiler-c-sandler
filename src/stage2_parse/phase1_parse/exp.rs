use super::{ParsedCAst, Parser};
use crate::{stage1_lex::tokens as t, stage2_parse::c_ast::*};
use anyhow::{anyhow, Context, Result};
use std::borrow::Borrow;

impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    pub(super) fn parse_exp(&mut self) -> Result<Expression<ParsedCAst>> {
        self.do_parse_exp(BinaryOperatorPrecedence::min())
    }
    fn do_parse_exp(
        &mut self,
        min_prec: BinaryOperatorPrecedence,
    ) -> Result<Expression<ParsedCAst>> {
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
                                let rhs = self.do_parse_exp(beyond_prec)?;

                                lhs = Expression::Binary(Binary {
                                    op: c_op,
                                    lhs: Box::new(lhs),
                                    rhs: Box::new(rhs),
                                });
                            }
                            BinaryOperatorInfo::ControlQuestion => {
                                let then = self.do_parse_exp(BinaryOperatorPrecedence::min())?;

                                self.expect_exact(&[t::Operator::Colon.into()])?;

                                let beyond_prec = nxt_prec; // Right associative
                                let elze = self.do_parse_exp(beyond_prec)?;

                                lhs = Expression::Conditional(Conditional {
                                    condition: Box::new(lhs),
                                    then: Box::new(then),
                                    elze: Box::new(elze),
                                });
                            }
                            BinaryOperatorInfo::Assign => {
                                let beyond_prec = nxt_prec; // Right associative
                                let rhs = self.do_parse_exp(beyond_prec)?;

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
    fn parse_factor(&mut self) -> Result<Expression<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            match self.tokens.next() {
                Some(Ok(t::Token::Const(konst))) => Ok(Expression::Const(konst)),
                Some(Ok(t::Token::Identifier(ident))) => match self.tokens.peek() {
                    Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                        let args = self.parse_arg_list()?;
                        Ok(Expression::FunctionCall(FunctionCall { ident, args }))
                    }
                    _ => Ok(Expression::Var(ident)),
                },
                Some(Ok(t::Token::Operator(t_op))) => self.parse_unary(t_op),
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    match self.parse_abstract_declaration()? {
                        Some(typ) => {
                            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                            let sub_exp = Box::new(self.parse_factor()?);

                            Ok(Expression::Cast(Cast { typ, sub_exp }))
                        }
                        None => {
                            let exp = self.do_parse_exp(BinaryOperatorPrecedence::min())?;

                            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                            Ok(exp)
                        }
                    }
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<factor>")
    }
    fn parse_unary(&mut self, t_op: t::Operator) -> Result<Expression<ParsedCAst>> {
        let inner = || -> Result<_> {
            let mut new_unary = |op: UnaryOperator| {
                let exp = self.parse_factor()?;
                Ok(Expression::Unary(Unary { op, sub_exp: Box::new(exp) }))
            };
            match t_op {
                t::Operator::Tilde => new_unary(UnaryOperator::Complement),
                t::Operator::Minus => new_unary(UnaryOperator::Negate),
                t::Operator::Bang => new_unary(UnaryOperator::Not),
                t::Operator::Star => {
                    let exp = Box::new(self.parse_factor()?);
                    Ok(Expression::Dereference(Dereference(exp)))
                }
                t::Operator::Ampersand => {
                    let exp = Box::new(self.parse_factor()?);
                    Ok(Expression::AddrOf(AddrOf(exp)))
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<factor> unary")
    }
    fn parse_arg_list(&mut self) -> Result<Vec<Expression<ParsedCAst>>> {
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

                        let arg = self.do_parse_exp(BinaryOperatorPrecedence::min())?;
                        args.push(arg);
                    }
                }
            }
            Ok(args)
        };
        inner().context("<argument-list>")
    }
}

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
impl<B: Borrow<BinaryOperatorInfo>> From<B> for BinaryOperatorPrecedence {
    fn from(boi: B) -> Self {
        use BinaryOperator as BO;
        match boi.borrow() {
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
