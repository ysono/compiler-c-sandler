//! + Resolve declarations, in the symbol table.
//! + Retain the run-time portion of the C AST:
//!     function definitions (which are not nested) and
//!     non-static variable definitions.
//! + At each Expression node, validate and annotate the output type.
//! + Make implicit casts explicit.

mod decl_fun;
mod decl_var;
mod exp;
mod exp_binary;
mod exp_cast;

use self::{decl_fun::FunDeclScope, decl_var::VarDeclScope};
use crate::{
    common::{
        identifier::SymbolIdentifier,
        symbol_table_frontend::SymbolTable,
        types_frontend::{FunType, VarType},
    },
    ds_n_a::singleton::{Singleton, SingletonRepository},
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::Result;
use std::rc::Rc;

#[derive(Debug)]
pub struct TypeCheckedCAst(());
impl CAstVariant for TypeCheckedCAst {
    type FileScopeDeclaration = FunctionDefinition<Self>;
    type BlockScopeDeclaration = VariableDefinition<Self>;
    type ForInitDeclaration = VariableDefinition<Self>;
    type Identifier = Rc<SymbolIdentifier>;
    type LoopId = Rc<LoopId>;
    type Expression = TypedExp;
    type LvalueExpression = TypedLExp;
}

pub struct TypeChecker {
    var_type_repo: SingletonRepository<VarType>,

    symbol_table: SymbolTable,

    curr_fun_type: Option<Singleton<FunType>>,
}
impl TypeChecker {
    pub fn new(var_type_repo: SingletonRepository<VarType>) -> Self {
        Self {
            var_type_repo,
            symbol_table: Default::default(),
            curr_fun_type: Default::default(),
        }
    }

    pub fn typecheck_prog(
        mut self,
        Program { decls }: Program<ResolvedCAst>,
    ) -> Result<(Program<TypeCheckedCAst>, SymbolTable)> {
        let mut fun_defns = Vec::with_capacity(decls.len());
        for decl in decls {
            match decl {
                Declaration::Var(var_decl) => {
                    // Does transform.
                    let var_defn = self.typecheck_decl_var(var_decl, VarDeclScope::File)?;
                    debug_assert!(var_defn.is_none());
                }
                Declaration::Fun(fun_decl) => {
                    // Does transform.
                    let fun_defn = self.typecheck_decl_fun(fun_decl, FunDeclScope::File)?;
                    if let Some(fun_defn) = fun_defn {
                        fun_defns.push(fun_defn)
                    }
                }
            }
        }

        let program = Program { decls: fun_defns };

        Ok((program, self.symbol_table))
    }

    /* Block */

    fn typecheck_block(
        &mut self,
        Block { items }: Block<ResolvedCAst>,
    ) -> Result<Block<TypeCheckedCAst>> {
        let mut out_items = Vec::with_capacity(items.len());
        for item in items {
            let item = self.typecheck_block_item(item)?;
            if let Some(item) = item {
                out_items.push(item);
            }
        }
        Ok(Block { items: out_items })
    }
    fn typecheck_block_item(
        &mut self,
        item: BlockItem<ResolvedCAst>,
    ) -> Result<Option<BlockItem<TypeCheckedCAst>>> {
        match item {
            BlockItem::Declaration(decl) => match decl {
                Declaration::Var(var_decl) => {
                    // Does transform.
                    let var_defn = self.typecheck_decl_var(var_decl, VarDeclScope::Block)?;
                    Ok(var_defn.map(BlockItem::Declaration))
                }
                Declaration::Fun(fun_decl) => {
                    // Does transform.
                    let fun_defn = self.typecheck_decl_fun(fun_decl, FunDeclScope::Block)?;
                    debug_assert!(fun_defn.is_none());
                    Ok(None)
                }
            },
            BlockItem::Statement(stmt) => {
                let stmt = self.typecheck_stmt(stmt)?;
                Ok(Some(BlockItem::Statement(stmt)))
            }
        }
    }

    /* Statement */

    fn typecheck_stmt(
        &mut self,
        stmt: Statement<ResolvedCAst>,
    ) -> Result<Statement<TypeCheckedCAst>> {
        match stmt {
            Statement::Return(exp) => {
                // Does transform.
                let fun_typ = self.curr_fun_type.as_ref().unwrap();
                let ret_typ = fun_typ.ret.clone();
                let exp = self.cast_by_assignment(ret_typ, exp)?;

                Ok(Statement::Return(exp))
            }
            Statement::Expression(exp) => self.typecheck_exp(exp).map(Statement::Expression),
            Statement::If(If { condition, then, elze }) => {
                let condition = self.typecheck_exp(condition)?;
                let then = Box::new(self.typecheck_stmt(*then)?);
                let elze = elze
                    .map(|elze| self.typecheck_stmt(*elze))
                    .transpose()?
                    .map(Box::new);
                Ok(Statement::If(If { condition, then, elze }))
            }
            Statement::Compound(block) => self.typecheck_block(block).map(Statement::Compound),
            Statement::Break(loop_id) => Ok(Statement::Break(loop_id)),
            Statement::Continue(loop_id) => Ok(Statement::Continue(loop_id)),
            Statement::While(loop_id, CondBody { condition, body }) => {
                let condition = self.typecheck_exp(condition)?;
                let body = Box::new(self.typecheck_stmt(*body)?);
                Ok(Statement::While(loop_id, CondBody { condition, body }))
            }
            Statement::DoWhile(loop_id, CondBody { body, condition }) => {
                let body = Box::new(self.typecheck_stmt(*body)?);
                let condition = self.typecheck_exp(condition)?;
                Ok(Statement::DoWhile(loop_id, CondBody { body, condition }))
            }
            Statement::For(loop_id, For { init, condition, post, body }) => {
                let init = match init {
                    ForInit::Decl(var_decl) => {
                        // Does transform.
                        let var_defn = self.typecheck_decl_var(var_decl, VarDeclScope::Paren)?;
                        match var_defn {
                            Some(var_defn) => ForInit::Decl(var_defn),
                            None => ForInit::None,
                        }
                    }
                    ForInit::Exp(exp) => self.typecheck_exp(exp).map(ForInit::Exp)?,
                    ForInit::None => ForInit::None,
                };
                let condition = condition.map(|cond| self.typecheck_exp(cond)).transpose()?;
                let post = post.map(|post| self.typecheck_exp(post)).transpose()?;
                let body = Box::new(self.typecheck_stmt(*body)?);
                Ok(Statement::For(loop_id, For { init, condition, post, body }))
            }
            Statement::Null => Ok(Statement::Null),
        }
    }
}
