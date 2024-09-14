#![doc = include_str!("./phase1_parse/ebnf.md")]

mod decl;
mod exp;
mod stmt;

use self::exp::BinaryOperatorPrecedence;
use crate::{stage1_lex::tokens as t, stage2_parse::c_ast::*};
use anyhow::{anyhow, Context, Result};
use std::iter::Peekable;

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
        Self { tokens: tokens.peekable() }
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
