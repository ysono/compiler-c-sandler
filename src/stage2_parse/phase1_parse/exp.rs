use super::{ParsedCAst, Parser};
use crate::{stage1_lex::tokens as t, stage2_parse::c_ast::*};
use anyhow::{anyhow, Context, Result};
use derive_more::Add;

impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    pub(super) fn parse_exp(
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
    fn parse_factor(&mut self) -> Result<Expression<ParsedCAst>> {
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
                        t::Operator::Tilde => UnaryOperator::Complement,
                        t::Operator::Minus => UnaryOperator::Negate,
                        t::Operator::Not => UnaryOperator::Not,
                        actual => return Err(anyhow!("{actual:?}")),
                    };
                    let exp = self.parse_factor()?;
                    return Ok(Expression::Unary(Unary {
                        op: c_op_unary,
                        sub_exp: Box::new(exp),
                    }));
                }
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    match self.maybe_parse_specifiers()? {
                        Some((typ, None)) => {
                            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;
                            let factor = self.parse_factor()?;
                            let sub_exp = Box::new(factor);
                            return Ok(Expression::Cast(Cast { typ, sub_exp }));
                        }
                        Some(actual) => return Err(anyhow!("{actual:?}")),
                        None => {
                            let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;
                            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;
                            return Ok(exp);
                        }
                    }
                }
                actual => return Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<factor>")
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

                        let arg = self.parse_exp(BinaryOperatorPrecedence::min())?;
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Add)]
pub struct BinaryOperatorPrecedence(u8);
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
    pub fn min() -> Self {
        Self(0)
    }
    fn inc1(&self) -> Self {
        Self(self.0 + 1)
    }
}
