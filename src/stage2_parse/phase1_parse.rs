#![doc = include_str!("./phase1_parse/ebnf.md")]

mod decl;
mod decl_init;
mod exp;
mod stmt;

use crate::{
    common::{
        identifier::RawIdentifier,
        types_frontend::{ObjType, ParsedFunType, ParsedObjType},
    },
    ds_n_a::singleton::SingletonRepository,
    stage1_lex::tokens as t,
    stage2_parse::c_ast::*,
    utils::noop,
};
use anyhow::{Context, Result, anyhow};
use derive_more::Into;
use std::{fmt::Debug, iter::Peekable};

#[derive(Debug)]
pub struct ParsedCAst(());
impl CAstVariant for ParsedCAst {
    /* Declarations */

    type FileScopeDeclaration = Declaration<Self>;
    type BlockScopeDeclaration = Declaration<Self>;
    type ForInitDeclaration = VariableDeclaration<Self>;

    /* IDs */

    type SymbolId = RawIdentifier;
    type LoopId = ();

    /* Categories of Expressions */

    type Expression_AnyType = Expression<Self>;
    type Expression_ScalarType = Expression<Self>;
    type Expression_Lvalue_AnyType = Expression<Self>;
    type Expression_Lvalue_ScalarType = Expression<Self>;

    /* Specific Expressions ; Operands */

    type BinaryOperator = BinaryOperator;
    type StringExpression = Vec<u8>;
    type TypeOperand<Typ: Debug> = ParsedObjType;
}

#[derive(Into)]
pub struct Parser<T: Iterator<Item = Result<t::Token>>> {
    tokens: Peekable<T>,

    obj_type_repo: SingletonRepository<ObjType>,
    fun_type_repo: SingletonRepository<ParsedFunType>,
}

impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Self {
            tokens: tokens.peekable(),
            obj_type_repo: Default::default(),
            fun_type_repo: Default::default(),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            let mut decls = vec![];
            loop {
                match self.tokens.peek() {
                    None => break,
                    Some(_) => match self.parse_declaration()? {
                        Some(decl) => {
                            decls.push(decl);
                        }
                        None => {
                            let actual = self.tokens.peek();
                            return Err(anyhow!("Expected <declaration> but found {actual:#?}"));
                        }
                    },
                }
            }
            Ok(Program { decls })
        };
        inner().context("tokens -> c_ast <program>")
    }
}

/// Block
impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    fn parse_block(&mut self) -> Result<Block<ParsedCAst>> {
        let mut inner = || -> Result<_> {
            self.expect_exact(&[t::Demarcator::CurlyOpen.into()])?;

            let mut items = vec![];
            loop {
                match self.peek_token()? {
                    t::Token::Demarcator(t::Demarcator::CurlyClose) => {
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
            match self.parse_declaration()? {
                Some(decl) => Ok(BlockItem::Declaration(decl)),
                None => self.parse_stmt().map(BlockItem::Statement),
            }
        };
        inner().context("<block-item>")
    }
}

/// Helpers
impl<T: Iterator<Item = Result<t::Token>>> Parser<T> {
    fn next_token(&mut self) -> Result<t::Token> {
        match self.tokens.next() {
            Some(res) => res,
            None => Err(anyhow!("EOF")),
        }
    }

    fn peek_token(&mut self) -> Result<&t::Token> {
        match self.tokens.peek() {
            Some(res) => match res {
                Ok(token) => Ok(token),
                Err(e) => Err(anyhow!(e.to_string())),
            },
            None => Err(anyhow!("EOF")),
        }
    }

    fn expect_exact(&mut self, next_tokens: &[t::Token]) -> Result<()> {
        for expected in next_tokens {
            match self.tokens.next() {
                Some(Ok(actual)) if expected == &actual => noop!(),
                actual => return Err(anyhow!("Expected {:#?} but found {:#?}", expected, actual)),
            }
        }
        Ok(())
    }
}
