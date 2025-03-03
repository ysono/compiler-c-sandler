//! + Resolve declarations, in the frontend symbol table.
//! + Retain the run-time portion of the C AST:
//!     function definitions (which are not nested) and
//!     non-static variable definitions.
//! + At each Expression node, validate and annotate the output type.
//! + Make implicit casts explicit.

mod decl_fun;
mod decl_var;
mod decl_var_init;
mod exp;
mod exp_binary;
mod exp_cast;
mod exp_misc;
mod exp_obj;
mod obj;

use self::{decl_fun::FunDeclScope, decl_var::VarDeclScope};
use crate::{
    common::{
        identifier::{LoopId, SymbolIdentifier},
        symbol_table_frontend::FrontendSymbolTableWithDeduper,
        types_frontend::{
            NonAggrType, NonVoidType, ObjType, ScalarType, SubObjType, TypecheckedFunType,
        },
    },
    ds_n_a::{
        singleton::{Singleton, SingletonRepository},
        witness::Witness,
    },
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
};
use anyhow::{Result, anyhow};
use std::{borrow::Cow, fmt::Debug, rc::Rc};

#[derive(Debug)]
pub struct TypeCheckedCAst(());
impl CAstVariant for TypeCheckedCAst {
    /* Declarations */

    type FileScopeDeclaration = FunctionDefinition<Self>;
    type BlockScopeDeclaration = VariableDefinition<Self>;
    type ForInitDeclaration = VariableDefinition<Self>;

    /* IDs */

    type SymbolId = Rc<SymbolIdentifier>;
    type LoopId = LoopId;

    /* Categories of Expressions */

    type Expression_AnyType = AnyExp;
    type Expression_NonAggrType = NonAggrExp;
    type Expression_ScalarType = ScalarExp;
    type Expression_Lvalue_AnyType = TypedLExp<NonVoidType>;
    type Expression_Lvalue_ScalarType = TypedLExp<SubObjType<ScalarType>>;

    /* Specific Expressions and their parameters */

    type BinaryOperator = TypeCheckedBinaryOperator;
    type StringExpression = Rc<SymbolIdentifier>;
    type TypeOperand<Typ: Debug> = Typ;
    type SizeOfExpExpression = ();

    /* Expressions' outputs */

    type ConcreteType<Typ: Debug> = Witness<Typ>;
}

pub struct TypeChecker {
    obj_type_repo: SingletonRepository<ObjType>,
    fun_type_repo: SingletonRepository<TypecheckedFunType>,

    frontend_symtab: FrontendSymbolTableWithDeduper,

    curr_fun_type: Option<Singleton<TypecheckedFunType>>,
}
impl TypeChecker {
    pub fn new(obj_type_repo: SingletonRepository<ObjType>) -> Self {
        Self {
            obj_type_repo,
            fun_type_repo: Default::default(), // New repo, separate from the one that Parser uses.
            frontend_symtab: Default::default(),
            curr_fun_type: Default::default(),
        }
    }

    pub fn typecheck_prog(
        mut self,
        Program { decls }: Program<ResolvedCAst>,
    ) -> Result<(Program<TypeCheckedCAst>, FrontendSymbolTableWithDeduper)> {
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

        Ok((program, self.frontend_symtab))
    }
}

/// Block
impl TypeChecker {
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
}

/// Statement
impl TypeChecker {
    fn typecheck_stmt(
        &mut self,
        stmt: Statement<ResolvedCAst>,
    ) -> Result<Statement<TypeCheckedCAst>> {
        match stmt {
            Statement::Return(exp) => {
                // Does transform.

                let fun_typ = self.curr_fun_type.as_ref().unwrap();
                let ret_typ = fun_typ.ret.clone();

                match (ret_typ, exp) {
                    (NonAggrType::Void(_), None) => Ok(Statement::Return(None)),
                    (NonAggrType::Scalar(ret_sca_typ), Some(exp)) => {
                        let exp = self.cast_by_assignment(Cow::Owned(ret_sca_typ), exp)?;
                        Ok(Statement::Return(Some(exp)))
                    }
                    (ret_typ, exp) => {
                        /* Given a function that returns `void`, it's illegal to have a `return <exp>;` stmt, even if the <exp> is typed void.
                        gcc and clang allow it in non-pedantic mode. */
                        Err(anyhow!(
                            "Invalid return stmt. ret_typ={ret_typ:#?} vs has_exp={}",
                            exp.is_some()
                        ))
                    }
                }
            }
            Statement::Expression(exp) => self.typecheck_exp(exp).map(Statement::Expression),
            Statement::If(If { condition, then, elze }) => {
                let condition =
                    self.typecheck_exp_then_convert_array_then_assert_scalar(condition)?;
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
            Statement::While(CondBody { loop_id, condition, body }) => {
                let condition =
                    self.typecheck_exp_then_convert_array_then_assert_scalar(condition)?;
                let body = Box::new(self.typecheck_stmt(*body)?);
                Ok(Statement::While(CondBody { loop_id, condition, body }))
            }
            Statement::DoWhile(CondBody { loop_id, body, condition }) => {
                let body = Box::new(self.typecheck_stmt(*body)?);
                let condition =
                    self.typecheck_exp_then_convert_array_then_assert_scalar(condition)?;
                Ok(Statement::DoWhile(CondBody { loop_id, body, condition }))
            }
            Statement::For(For {
                loop_id,
                init,
                condition,
                post,
                body,
            }) => {
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
                let condition = condition
                    .map(|cond| self.typecheck_exp_then_convert_array_then_assert_scalar(cond))
                    .transpose()?;
                let post = post.map(|post| self.typecheck_exp(post)).transpose()?;
                let body = Box::new(self.typecheck_stmt(*body)?);
                Ok(Statement::For(For {
                    loop_id,
                    init,
                    condition,
                    post,
                    body,
                }))
            }
            Statement::Null => Ok(Statement::Null),
        }
    }
}
