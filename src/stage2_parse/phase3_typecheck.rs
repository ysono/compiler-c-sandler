mod casting;
mod decl_fun;
mod decl_var;
mod exp;

use self::{decl_fun::FunDeclScope, decl_var::VarDeclScope};
use crate::{
    common::{
        identifier::UniqueIdentifier, symbol_table_frontend::SymbolTable, types_frontend::FunType,
    },
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::Result;
use std::rc::Rc;

#[derive(Debug)]
pub struct TypeCheckedCAst(());
impl CAstVariant for TypeCheckedCAst {
    type Identifier = Rc<UniqueIdentifier>;
    type BlockScopeDeclaration = BlockScopeDeclaration<TypeCheckedCAst>;
    type LoopId = Rc<LoopId>;
    type Expression = TypedExpression<TypeCheckedCAst>;
    type Lvalue = Rc<UniqueIdentifier>;
}

#[derive(Default)]
pub struct TypeChecker {
    symbol_table: SymbolTable,

    curr_fun_type: Option<Rc<FunType>>,
}
impl TypeChecker {
    pub fn typecheck_prog(
        mut self,
        Program { decls }: Program<ResolvedCAst>,
    ) -> Result<(Program<TypeCheckedCAst>, SymbolTable)> {
        let decls = decls
            .into_iter()
            .map(|decl| match decl {
                Declaration::VarDecl(vd) => self
                    .typecheck_decl_var(vd, VarDeclScope::File)
                    .map(Declaration::VarDecl),
                Declaration::FunDecl(fd) => self
                    .typecheck_decl_fundecl(fd, FunDeclScope::File)
                    .map(Declaration::FunDecl),
                Declaration::FunDefn(fd) => self
                    .typecheck_decl_fundefn(fd, FunDeclScope::File)
                    .map(Declaration::FunDefn),
            })
            .collect::<Result<Vec<_>>>()?;
        let program = Program { decls };
        Ok((program, self.symbol_table))
    }

    /* Block */

    fn typecheck_block(
        &mut self,
        Block { items }: Block<ResolvedCAst>,
    ) -> Result<Block<TypeCheckedCAst>> {
        let items = items
            .into_iter()
            .map(|item| self.typecheck_block_item(item))
            .collect::<Result<Vec<_>>>()?;
        Ok(Block { items })
    }
    fn typecheck_block_item(
        &mut self,
        item: BlockItem<ResolvedCAst>,
    ) -> Result<BlockItem<TypeCheckedCAst>> {
        match item {
            BlockItem::Declaration(decl) => {
                let decl = match decl {
                    BlockScopeDeclaration::VarDecl(vd) => self
                        .typecheck_decl_var(vd, VarDeclScope::Block)
                        .map(BlockScopeDeclaration::VarDecl),
                    BlockScopeDeclaration::FunDecl(fd) => self
                        .typecheck_decl_fundecl(fd, FunDeclScope::Block)
                        .map(BlockScopeDeclaration::FunDecl),
                };
                decl.map(BlockItem::Declaration)
            }
            BlockItem::Statement(stmt) => self.typecheck_stmt(stmt).map(BlockItem::Statement),
        }
    }

    /* Statement */

    fn typecheck_stmt(
        &mut self,
        stmt: Statement<ResolvedCAst>,
    ) -> Result<Statement<TypeCheckedCAst>> {
        match stmt {
            Statement::Return(exp) => {
                let exp = self.typecheck_exp(exp)?;

                let ret_type = self.curr_fun_type.as_ref().unwrap().ret;
                let exp = Self::maybe_cast_exp(exp, ret_type);

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
                    ForInit::Decl(var_decl) => self
                        .typecheck_decl_var(var_decl, VarDeclScope::Paren)
                        .map(ForInit::Decl)?,
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
