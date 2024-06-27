#![doc = include_str!("./phase1_parse/ebnf.md")]

mod binary_operator;

use self::binary_operator::{BinaryOperatorInfo, BinaryOperatorPrecedence};
use crate::{
    stage1_lex::tokens as t,
    stage2_parse::c_ast::*,
    symbol_table::{FunType, VarType},
};
use anyhow::{anyhow, Context, Result};
use std::iter::Peekable;
use std::rc::Rc;

#[derive(Debug)]
pub struct ParsedCAst(());
impl CAstVariant for ParsedCAst {
    type Identifier = t::Identifier;
    type BlockScopeDeclaration = Declaration<ParsedCAst>;
    type LoopId = ();
    type Expression = Expression<ParsedCAst>;
    type Lvalue = Box<Expression<ParsedCAst>>;
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

    pub fn parse_program(&mut self) -> Result<Program<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            let mut decls = vec![];
            loop {
                match self.tokens.peek() {
                    None => break,
                    _ => {
                        let decl = self.maybe_parse_decl()?.ok_or_else(|| {
                            let actual = self.tokens.peek();
                            anyhow!("Expected <declaration> but found {actual:?}")
                        })?;
                        decls.push(decl);
                    }
                }
            }
            Ok(Program { decls })
        };
        inner().context("tokens -> c_ast <program>")
    }

    /* Declaration */

    fn maybe_parse_decl(&mut self) -> Result<Option<Declaration<ParsedCAst>>> {
        let mut inner = || -> Result<_> {
            let (typ, storage_class) = match self.maybe_parse_specifiers()? {
                None => return Ok(None),
                Some((t, sc)) => (t, sc),
            };

            let ident = match self.tokens.next() {
                Some(Ok(t::Token::Identifier(ident))) => ident,
                actual => return Err(anyhow!("{actual:?}")),
            };

            match self.tokens.next() {
                Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                    Ok(Some(Declaration::VarDecl(VariableDeclaration {
                        ident,
                        init: None,
                        typ,
                        storage_class,
                    })))
                }
                Some(Ok(t::Token::Operator(t::Operator::Assign))) => {
                    let init = self.parse_exp(BinaryOperatorPrecedence::min())?;

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Some(Declaration::VarDecl(VariableDeclaration {
                        ident,
                        init: Some(init),
                        typ,
                        storage_class,
                    })))
                }
                Some(Ok(t::Token::Demarcator(t::Demarcator::ParenOpen))) => {
                    let (param_typs, param_idents) = self.parse_param_list()?;

                    self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

                    let decl = FunctionDeclaration {
                        ident,
                        param_idents,
                        typ: Rc::new(FunType {
                            params: param_typs,
                            ret: typ,
                        }),
                        storage_class,
                    };

                    let decl = match self.tokens.peek() {
                        Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                            self.tokens.next();

                            Declaration::FunDecl(decl)
                        }
                        _ => {
                            let body = self.parse_block()?;

                            Declaration::FunDefn(FunctionDefinition { decl, body })
                        }
                    };

                    Ok(Some(decl))
                }
                actual => Err(anyhow!("{actual:?}")),
            }
        };
        inner().context("<declaration>")
    }
    fn maybe_parse_specifiers(
        &mut self,
    ) -> Result<Option<(VarType, Option<StorageClassSpecifier>)>> {
        let mut inner = || -> Result<_> {
            let mut t_typs = vec![];
            let mut scss = vec![];

            while let Some(Ok(t::Token::Type(_) | t::Token::StorageClassSpecifier(_))) =
                self.tokens.peek()
            {
                match self.tokens.next().unwrap().unwrap() {
                    t::Token::Type(t_typ) => t_typs.push(t_typ),
                    t::Token::StorageClassSpecifier(scs) => scss.push(scs),
                    _ => { /* Impossible. */ }
                }
            }

            match (&t_typs[..], &scss[..]) {
                ([], []) => Ok(None),
                (t_typs, scss) => {
                    let typ = match t_typs {
                        [t::Type::Int] => VarType::Int,
                        [t::Type::Long]
                        | [t::Type::Int, t::Type::Long]
                        | [t::Type::Long, t::Type::Int] => VarType::Long,
                        actual => return Err(anyhow!("Invalid types. {actual:?}")),
                        /* Void is not supported yet. */
                    };

                    let scs = match scss {
                        [] => None,
                        [one] => Some(*one),
                        actual => {
                            return Err(anyhow!("Invalid storage class specifiers. {actual:?}"))
                        }
                    };

                    Ok(Some((typ, scs)))
                }
            }
        };
        inner().context("<declaration> specifiers")
    }
    fn parse_param_list(&mut self) -> Result<(Vec<VarType>, Vec<t::Identifier>)> {
        let mut inner = || -> Result<_> {
            match self.tokens.peek() {
                Some(Ok(t::Token::Type(t::Type::Void))) => {
                    self.tokens.next();

                    Ok((Vec::with_capacity(0), Vec::with_capacity(0)))
                }
                _ => {
                    let mut typs = vec![];
                    let mut idents = vec![];

                    loop {
                        match self.maybe_parse_specifiers()? {
                            Some((typ, None)) => typs.push(typ),
                            actual => return Err(anyhow!("{actual:?}")),
                        };

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

                    Ok((typs, idents))
                }
            }
        };
        inner().context("<param-list>")
    }

    /* Block */

    fn parse_block(&mut self) -> Result<Block<ParsedCAst>> {
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
    fn parse_block_item(&mut self) -> Result<BlockItem<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            match self.maybe_parse_decl()? {
                Some(decl) => Ok(BlockItem::Declaration(decl)),
                None => self.parse_stmt().map(BlockItem::Statement),
            }
        };
        inner().context("<block-item>")
    }

    /* Statement */

    fn parse_stmt(&mut self) -> Result<Statement<ParsedCAst>> {
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

                    Ok(Statement::Break(()))
                }
                Some(Ok(t::Token::Loop(t::Loop::Continue))) => {
                    self.tokens.next();

                    self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                    Ok(Statement::Continue(()))
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
    fn parse_stmt_if(&mut self) -> Result<Statement<ParsedCAst>> {
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
    fn parse_stmt_while(&mut self) -> Result<Statement<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::While.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

            self.expect_exact(&[t::Demarcator::ParenClose.into()])?;

            let body = self.parse_stmt()?;

            Ok(Statement::While(
                (),
                CondBody {
                    condition,
                    body: Box::new(body),
                },
            ))
        };
        inner().context("<statement> while")
    }
    fn parse_stmt_dowhile(&mut self) -> Result<Statement<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::Do.into()])?;

            let body = self.parse_stmt()?;

            self.expect_exact(&[t::Loop::While.into(), t::Demarcator::ParenOpen.into()])?;

            let condition = self.parse_exp(BinaryOperatorPrecedence::min())?;

            self.expect_exact(&[
                t::Demarcator::ParenClose.into(),
                t::Demarcator::Semicolon.into(),
            ])?;

            Ok(Statement::DoWhile(
                (),
                CondBody {
                    body: Box::new(body),
                    condition,
                },
            ))
        };
        inner().context("<statement> dowhile")
    }
    fn parse_stmt_for(&mut self) -> Result<Statement<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Loop::For.into(), t::Demarcator::ParenOpen.into()])?;

            let init = match self.maybe_parse_decl()? {
                Some(Declaration::VarDecl(vd)) => ForInit::Decl(vd),
                Some(Declaration::FunDecl(fd)) => return Err(anyhow!("{fd:?}")),
                Some(Declaration::FunDefn(fd)) => return Err(anyhow!("{fd:?}")),
                None => match self.tokens.peek() {
                    Some(Ok(t::Token::Demarcator(t::Demarcator::Semicolon))) => {
                        self.tokens.next();

                        ForInit::None
                    }
                    _ => {
                        let exp = self.parse_exp(BinaryOperatorPrecedence::min())?;

                        self.expect_exact(&[t::Demarcator::Semicolon.into()])?;

                        ForInit::Exp(exp)
                    }
                },
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

            Ok(Statement::For(
                (),
                For {
                    init,
                    condition,
                    post,
                    body: Box::new(body),
                },
            ))
        };
        inner().context("<statement> for")
    }

    /* Expression */

    fn parse_exp(&mut self, min_prec: BinaryOperatorPrecedence) -> Result<Expression<ParsedCAst>> {
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
                        None => return Err(anyhow!("{t_op:?}")),
                    }
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
