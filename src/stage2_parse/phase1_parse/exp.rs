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
                    Some(Ok(t::Token::Operator(t_op))) => BinaryOperatorInfo::parse(t_op),
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
                    match self.parse_cast_type()? {
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
impl<Bo: Into<BinaryOperator>> From<Bo> for BinaryOperatorInfo {
    fn from(bo: Bo) -> Self {
        Self::Generic(bo.into())
    }
}
impl BinaryOperatorInfo {
    fn parse(t_op: &t::Operator) -> Option<Self> {
        use t::Operator as TO;

        use ArithmeticBinaryOperator as COA;
        use ComparisonBinaryOperator as COC;
        use LogicBinaryOperator as COL;

        match t_op {
            TO::Minus => Some(Self::from(COA::Sub)),
            TO::Plus => Some(Self::from(COA::Add)),
            TO::Star => Some(Self::from(COA::Mul)),
            TO::Slash => Some(Self::from(COA::Div)),
            TO::Percent => Some(Self::from(COA::Rem)),
            TO::And => Some(Self::from(COL::And)),
            TO::Or => Some(Self::from(COL::Or)),
            TO::Eq => Some(Self::from(COC::Eq)),
            TO::Neq => Some(Self::from(COC::Neq)),
            TO::Lt => Some(Self::from(COC::Lt)),
            TO::Lte => Some(Self::from(COC::Lte)),
            TO::Gt => Some(Self::from(COC::Gt)),
            TO::Gte => Some(Self::from(COC::Gte)),
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
        use BinaryOperator as CO;

        use ArithmeticBinaryOperator as COA;
        use ComparisonBinaryOperator as COC;
        use LogicBinaryOperator as COL;

        match boi.borrow() {
            BinaryOperatorInfo::Generic(bo) => match bo {
                CO::Arith(COA::Mul | COA::Div | COA::Rem) => Self(50),
                CO::Arith(COA::Sub | COA::Add) => Self(45),
                CO::Cmp(COC::Lt | COC::Lte | COC::Gt | COC::Gte) => Self(35),
                CO::Cmp(COC::Eq | COC::Neq) => Self(30),
                CO::Logic(COL::And) => Self(10),
                CO::Logic(COL::Or) => Self(5),
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
