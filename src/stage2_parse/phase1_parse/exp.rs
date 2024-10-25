use super::{ParsedCAst, Parser};
use crate::{stage1_lex::tokens as t, stage2_parse::c_ast::*};
use anyhow::{anyhow, Context, Result};
use derive_more::From;
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
            let mut lhs = self.parse_unary_exp()?;

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

                                lhs = RExp::Binary(Binary {
                                    op: c_op,
                                    lhs: Box::new(lhs),
                                    rhs: Box::new(rhs),
                                })
                                .into();
                            }
                            BinaryOperatorInfo::Question => {
                                let then = self.do_parse_exp(BinaryOperatorPrecedence::min())?;

                                self.expect_exact(&[t::Operator::Colon.into()])?;

                                let beyond_prec = nxt_prec; // Right associative
                                let elze = self.do_parse_exp(beyond_prec)?;

                                lhs = RExp::Conditional(Conditional {
                                    condition: Box::new(lhs),
                                    then: Box::new(then),
                                    elze: Box::new(elze),
                                })
                                .into();
                            }
                            BinaryOperatorInfo::Assign => {
                                let beyond_prec = nxt_prec; // Right associative
                                let rhs = self.do_parse_exp(beyond_prec)?;

                                lhs = RExp::Assignment(Assignment {
                                    lhs: Box::new(lhs),
                                    rhs: Box::new(rhs),
                                })
                                .into();
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
    fn parse_unary_exp(&mut self) -> Result<Expression<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            match self.tokens.next() {
                Some(Ok(t::Token::Const(konst))) => {
                    let primary_exp = RExp::Const(konst).into();

                    self.parse_postfix_exp(primary_exp)
                }
                Some(Ok(t::Token::Identifier(ident))) => {
                    let primary_exp = match self.tokens.peek() {
                        Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                            let args = self.parse_arg_list()?;
                            RExp::FunctionCall(FunctionCall { ident, args }).into()
                        }
                        _ => LExp::Var(ident).into(),
                    };

                    self.parse_postfix_exp(primary_exp)
                }
                Some(Ok(t::Token::Operator(t_op))) => self.parse_unary_op_exp(t_op),
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    match self.parse_cast_type()? {
                        Some(typ) => {
                            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                            let sub_exp = Box::new(self.parse_unary_exp()?);
                            Ok(RExp::Cast(Cast { typ, sub_exp }).into())
                        }
                        None => {
                            let primary_exp = self.parse_exp()?;

                            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                            self.parse_postfix_exp(primary_exp)
                        }
                    }
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<unary-exp>")
    }
    fn parse_unary_op_exp(&mut self, t_op: t::Operator) -> Result<Expression<ParsedCAst>> {
        let inner = || -> Result<_> {
            #[derive(From)]
            enum Op {
                Unary(UnaryOperator),
                Deref,
                AddrOf,
            }
            let op = match t_op {
                t::Operator::Tilde => UnaryOperator::Complement.into(),
                t::Operator::Minus => UnaryOperator::Negate.into(),
                t::Operator::Bang => UnaryOperator::Not.into(),
                t::Operator::Star => Op::Deref,
                t::Operator::Ampersand => Op::AddrOf,
                actual => return Err(anyhow!("{actual:?}")),
            };

            let sub_exp = Box::new(self.parse_unary_exp()?);
            let exp = match op {
                Op::Unary(op) => RExp::Unary(Unary { op, sub_exp }).into(),
                Op::Deref => LExp::Dereference(Dereference(sub_exp)).into(),
                Op::AddrOf => RExp::AddrOf(AddrOf(sub_exp)).into(),
            };
            Ok(exp)
        };
        inner().context("<unary-exp> unary")
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

                        let arg = self.parse_exp()?;
                        args.push(arg);
                    }
                }
            }
            Ok(args)
        };
        inner().context("<argument-list>")
    }
    fn parse_postfix_exp(
        &mut self,
        mut lhs_exp: Expression<ParsedCAst>,
    ) -> Result<Expression<ParsedCAst>> {
        let inner = || -> Result<_> {
            while let Some(Ok(t::Token::Demarcator(t::Demarcator::SquareOpen))) = self.tokens.peek()
            {
                self.tokens.next();

                let rhs_exp = self.parse_exp()?;

                self.expect_exact(&[t::Demarcator::SquareClose.into()])?;

                lhs_exp = LExp::Subscript(Subscript {
                    exp1: Box::new(lhs_exp),
                    exp2: Box::new(rhs_exp),
                })
                .into();
            }
            Ok(lhs_exp)
        };
        inner().context("<postfix-exp> suffix")
    }
}

enum BinaryOperatorInfo {
    Generic(BinaryOperator),
    Question,
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
            TO::Question => Some(Self::Question),
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
            BinaryOperatorInfo::Question => Self(3),
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
