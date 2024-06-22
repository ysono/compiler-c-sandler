//! - Identifier resolution
//! - Loop labelling

use crate::{
    stage2_parse::{
        c_ast as p, // "p" for "previous c_ast".
        c_ast_resolved::*,
    },
    symbol_table::ResolvedIdentifier,
};
use anyhow::{anyhow, Context, Result};
use derive_more::{Deref, DerefMut};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Default)]
pub struct CAstValidator {
    ident_resolver: IdentResolver,

    loop_ids_stack: LoopIdsStack,
}
impl CAstValidator {
    pub fn resolve_program(&mut self, p::Program { decls }: p::Program) -> Result<Program> {
        let inner = || -> Result<_> {
            let decls = decls
                .into_iter()
                .map(|p_decl| self.resolve_decl(p_decl))
                .collect::<Result<Vec<_>>>()?;
            Ok(Program { decls })
        };
        inner().context("c_ast validator on <program>")
    }

    /* Declaration */

    fn resolve_decl(&mut self, p_decl: p::Declaration) -> Result<Declaration> {
        let inner = || -> Result<_> {
            match p_decl {
                p::Declaration::VarDecl(p_var_decl) => {
                    self.resolve_decl_var(p_var_decl).map(Declaration::VarDecl)
                }
                p::Declaration::FunDecl(p_fun_decl) => self.resolve_decl_fun(p_fun_decl),
            }
        };
        inner().context("<declaration>")
    }
    fn resolve_decl_block_scope(
        &mut self,
        p_decl: p::Declaration,
    ) -> Result<BlockScopeDeclaration> {
        let inner = || -> Result<_> {
            let decl = match self.resolve_decl(p_decl)? {
                Declaration::VarDecl(vd) => BlockScopeDeclaration::VarDecl(vd),
                Declaration::FunDecl(fd) => BlockScopeDeclaration::FunDecl(fd),
                Declaration::FunDefn(_fd) => {
                    return Err(anyhow!("Cannot define function in any block scope."))
                }
            };
            Ok(decl)
        };
        inner().context("<declaration> at block scope")
    }
    fn resolve_decl_var(
        &mut self,
        p::VariableDeclaration {
            ident,
            init,
            storage_class,
        }: p::VariableDeclaration,
    ) -> Result<VariableDeclaration> {
        let inner = || -> Result<_> {
            let ident = self.ident_resolver.declare_new_var(ident, &storage_class)?;

            let init = init.map(|p_exp| self.resolve_exp(p_exp)).transpose()?;

            Ok(VariableDeclaration {
                ident,
                init,
                storage_class,
            })
        };
        inner().context("<variable-declaration>")
    }
    fn resolve_decl_fun(
        &mut self,
        p::FunctionDeclaration {
            ident,
            params,
            body,
            storage_class,
        }: p::FunctionDeclaration,
    ) -> Result<Declaration> {
        let inner = || -> Result<_> {
            let ident = self.ident_resolver.declare_new_fun(ident)?;

            self.ident_resolver.push_new_scope();

            let params = params
                .into_iter()
                .map(|ident| {
                    let storage_class = None;
                    self.ident_resolver.declare_new_var(ident, &storage_class)
                })
                .collect::<Result<Vec<_>>>()?;

            let fun_decl = FunctionDeclaration {
                ident,
                params,
                storage_class,
            };

            let fun_decl = match body {
                None => Declaration::FunDecl(fun_decl),
                Some(p_body) => {
                    let body = self.resolve_block(p_body, false)?;
                    Declaration::FunDefn(FunctionDefinition {
                        decl: fun_decl,
                        body,
                    })
                }
            };

            self.ident_resolver.pop_scope();

            Ok(fun_decl)
        };
        inner().context("<function-declaration>")
    }

    /* Block */

    fn resolve_block(
        &mut self,
        p::Block { items }: p::Block,
        need_new_ident_scope: bool,
    ) -> Result<Block> {
        let inner = || -> Result<_> {
            if need_new_ident_scope {
                self.ident_resolver.push_new_scope();
            }

            let items = items
                .into_iter()
                .map(|p_item| self.resolve_block_item(p_item))
                .collect::<Result<Vec<_>>>()?;

            if need_new_ident_scope {
                self.ident_resolver.pop_scope();
            }

            Ok(Block { items })
        };
        inner().context("<block>")
    }
    fn resolve_block_item(&mut self, p_item: p::BlockItem) -> Result<BlockItem> {
        let inner = || -> Result<_> {
            match p_item {
                p::BlockItem::Declaration(p_decl) => {
                    let decl = self.resolve_decl_block_scope(p_decl)?;
                    Ok(BlockItem::Declaration(decl))
                }
                p::BlockItem::Statement(p_stmt) => {
                    let stmt = self.resolve_stmt(p_stmt)?;
                    Ok(BlockItem::Statement(stmt))
                }
            }
        };
        inner().context("<block-item>")
    }

    /* Statement */

    fn resolve_stmt(&mut self, p_stmt: p::Statement) -> Result<Statement> {
        let inner = || -> Result<_> {
            match p_stmt {
                p::Statement::Return(p_exp) => {
                    let exp = self.resolve_exp(p_exp)?;
                    Ok(Statement::Return(exp))
                }
                p::Statement::Expression(p_exp) => {
                    let exp = self.resolve_exp(p_exp)?;
                    Ok(Statement::Expression(exp))
                }
                p::Statement::If(p::If {
                    condition,
                    then,
                    elze,
                }) => {
                    let condition = self.resolve_exp(condition)?;
                    let then = self.resolve_stmt(*then)?;
                    let elze = elze.map(|elze| self.resolve_stmt(*elze)).transpose()?;
                    return Ok(Statement::If(If {
                        condition,
                        then: Box::new(then),
                        elze: elze.map(Box::new),
                    }));
                }
                p::Statement::Compound(p_block) => {
                    let block = self.resolve_block(p_block, true)?;
                    Ok(Statement::Compound(block))
                }
                p::Statement::Break => {
                    let loop_id = self
                        .loop_ids_stack
                        .last()
                        .ok_or(anyhow!("Cannot break outside loop scope"))?;
                    Ok(Statement::Break(Rc::clone(loop_id)))
                }
                p::Statement::Continue => {
                    let loop_id = self
                        .loop_ids_stack
                        .last()
                        .ok_or(anyhow!("Cannot continue outside loop scope"))?;
                    Ok(Statement::Continue(Rc::clone(loop_id)))
                }
                p::Statement::While(p_condbody) => self.resolve_stmt_while(p_condbody),
                p::Statement::DoWhile(p_condbody) => self.resolve_stmt_dowhile(p_condbody),
                p::Statement::For(p_for) => self.resolve_stmt_for(p_for),
                p::Statement::Null => Ok(Statement::Null),
            }
        };
        inner().context("<statement>")
    }
    fn resolve_stmt_while(&mut self, p_condbody: p::CondBody) -> Result<Statement> {
        let inner = || -> Result<_> {
            let (loop_id, condbody) = self.resolve_stmt_condbody("while", p_condbody)?;
            Ok(Statement::While(loop_id, condbody))
        };
        inner().context("<statement> while")
    }
    fn resolve_stmt_dowhile(&mut self, p_condbody: p::CondBody) -> Result<Statement> {
        let inner = || -> Result<_> {
            let (loop_id, condbody) = self.resolve_stmt_condbody("dowhile", p_condbody)?;
            Ok(Statement::DoWhile(loop_id, condbody))
        };
        inner().context("<statement> dowhile")
    }
    fn resolve_stmt_condbody(
        &mut self,
        descr: &'static str,
        p::CondBody { condition, body }: p::CondBody,
    ) -> Result<(Rc<LoopId>, CondBody)> {
        let inner = || -> Result<_> {
            let condition = self.resolve_exp(condition)?;

            let loop_id = Rc::new(LoopId::new(descr));
            self.loop_ids_stack.push(loop_id);

            let body = Box::new(self.resolve_stmt(*body)?);

            let loop_id = self.loop_ids_stack.pop().unwrap();

            Ok((loop_id, CondBody { condition, body }))
        };
        inner().context("condbody")
    }
    fn resolve_stmt_for(
        &mut self,
        p::For {
            init,
            condition,
            post,
            body,
        }: p::For,
    ) -> Result<Statement> {
        let inner = || -> Result<_> {
            self.ident_resolver.push_new_scope();

            let init = match init {
                p::ForInit::Decl(p_var_decl) => ForInit::Decl(self.resolve_decl_var(p_var_decl)?),
                p::ForInit::Exp(p_exp) => ForInit::Exp(self.resolve_exp(p_exp)?),
                p::ForInit::None => ForInit::None,
            };

            let condition = condition.map(|p_exp| self.resolve_exp(p_exp)).transpose()?;

            let post = post.map(|p_exp| self.resolve_exp(p_exp)).transpose()?;

            let loop_id = Rc::new(LoopId::new("for"));
            self.loop_ids_stack.push(loop_id);

            let body = Box::new(self.resolve_stmt(*body)?);

            let loop_id = self.loop_ids_stack.pop().unwrap();

            self.ident_resolver.pop_scope();

            let foor = For {
                init,
                condition,
                post,
                body,
            };
            Ok(Statement::For(loop_id, foor))
        };
        inner().context("<statement> for")
    }

    /* Expression */

    fn resolve_exp(&mut self, p_exp: p::Expression) -> Result<Expression> {
        let inner = || -> Result<_> {
            match p_exp {
                p::Expression::Const(konst) => return Ok(Expression::Const(konst)),
                p::Expression::Var(ident) => {
                    let ident = self.ident_resolver.get(&ident)?;
                    return Ok(Expression::Var(ident));
                }
                p::Expression::Unary(p::Unary { op, sub_exp }) => {
                    let sub_exp = self.resolve_exp(*sub_exp)?;
                    return Ok(Expression::Unary(Unary {
                        op,
                        sub_exp: Box::new(sub_exp),
                    }));
                }
                p::Expression::Binary(p::Binary { op, lhs, rhs }) => {
                    let lhs = self.resolve_exp(*lhs)?;
                    let rhs = self.resolve_exp(*rhs)?;
                    return Ok(Expression::Binary(Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                p::Expression::Assignment(p::Assignment { lhs, rhs }) => match *lhs {
                    p::Expression::Var(ident) => {
                        let ident = self.ident_resolver.get(&ident)?;
                        let rhs = self.resolve_exp(*rhs)?;
                        return Ok(Expression::Assignment(Assignment {
                            ident,
                            rhs: Box::new(rhs),
                        }));
                    }
                    _ => {
                        return Err(anyhow!(
                            "Assignment LHS must be an lvalue, but found {lhs:?}"
                        ))
                    }
                },
                p::Expression::Conditional(p::Conditional {
                    condition,
                    then,
                    elze,
                }) => {
                    let condition = self.resolve_exp(*condition)?;
                    let then = self.resolve_exp(*then)?;
                    let elze = self.resolve_exp(*elze)?;
                    return Ok(Expression::Conditional(Conditional {
                        condition: Box::new(condition),
                        then: Box::new(then),
                        elze: Box::new(elze),
                    }));
                }
                p::Expression::FunctionCall(p::FunctionCall { ident, args }) => {
                    let ident = self.ident_resolver.get(&ident)?;
                    let args = args
                        .into_iter()
                        .map(|exp| self.resolve_exp(exp))
                        .collect::<Result<Vec<_>>>()?;
                    Ok(Expression::FunctionCall(FunctionCall { ident, args }))
                }
            }
        };
        inner().context("<exp>")
    }
}

struct IdentResolver {
    /// This abstracts a copy-on-write dict.
    ident_to_resolved_idents: HashMap<Rc<p::Identifier>, Vec<Rc<ResolvedIdentifier>>>,

    /// This tracks each copy-on-write layer's keys.
    scope_to_idents: Vec<HashSet<Rc<p::Identifier>>>,
}
impl Default for IdentResolver {
    fn default() -> Self {
        Self {
            ident_to_resolved_idents: HashMap::new(),
            scope_to_idents: vec![HashSet::new()], // The one file scope.
        }
    }
}
impl IdentResolver {
    fn is_file_scope(&self) -> bool {
        self.scope_to_idents.len() == 1
    }

    fn push_new_scope(&mut self) {
        self.scope_to_idents.push(HashSet::new());
    }
    fn pop_scope(&mut self) {
        let idents = self.scope_to_idents.pop().unwrap();
        for ident in idents {
            let resolved_idents = self.ident_to_resolved_idents.get_mut(&ident).unwrap();
            if resolved_idents.len() > 1 {
                resolved_idents.pop();
            } else {
                self.ident_to_resolved_idents.remove(&ident);
            }
        }
    }

    fn declare_new_var(
        &mut self,
        ident: p::Identifier,
        storage_class: &Option<StorageClassSpecifier>,
    ) -> Result<Rc<ResolvedIdentifier>> {
        let has_linkage = self.is_file_scope()
            || match storage_class {
                Some(StorageClassSpecifier::Extern) => true,
                Some(StorageClassSpecifier::Static) | None => false,
            };
        self.declare_new(ident, has_linkage)
    }
    fn declare_new_fun(&mut self, ident: p::Identifier) -> Result<Rc<ResolvedIdentifier>> {
        let has_linkage = true;
        self.declare_new(ident, has_linkage)
    }
    fn declare_new(
        &mut self,
        ident: p::Identifier,
        new_has_linkage: bool,
    ) -> Result<Rc<ResolvedIdentifier>> {
        let local_scope = self.scope_to_idents.last_mut().unwrap();
        match local_scope.contains(&ident) {
            false => {
                let ident = Rc::new(ident);

                local_scope.insert(Rc::clone(&ident));

                let resolved_ident = if new_has_linkage {
                    ResolvedIdentifier::SomeLinkage(Rc::clone(&ident))
                } else {
                    ResolvedIdentifier::new_no_linkage(Some(Rc::clone(&ident)))
                };
                let resolved_ident = Rc::new(resolved_ident);

                self.ident_to_resolved_idents
                    .entry(ident)
                    .or_default()
                    .push(Rc::clone(&resolved_ident));

                Ok(resolved_ident)
            }
            true => {
                let resolved_idents = self.ident_to_resolved_idents.get_mut(&ident).unwrap();
                let resolved_ident = resolved_idents.last_mut().unwrap();

                let prev_has_linkage = matches!(
                    resolved_ident.as_ref(),
                    ResolvedIdentifier::SomeLinkage { .. }
                );
                if (prev_has_linkage && new_has_linkage) == false {
                    Err(anyhow!("In one scope, 2+ declarations of a same identifier must all refer to the same object or function, hence must all have some linkage. {resolved_ident:?} vs {new_has_linkage}"))
                } else {
                    Ok(Rc::clone(resolved_ident))
                }
            }
        }
    }

    fn get(&self, ident: &p::Identifier) -> Result<Rc<ResolvedIdentifier>> {
        let resolved_idents = self
            .ident_to_resolved_idents
            .get(ident)
            .ok_or_else(|| anyhow!("Identifier wasn't declared in scope. {ident:?}"))?;
        let resolved_ident = resolved_idents.last().unwrap();
        Ok(Rc::clone(resolved_ident))
    }
}

#[derive(Default, Deref, DerefMut)]
struct LoopIdsStack {
    loop_ids_stack: Vec<Rc<LoopId>>,
}
