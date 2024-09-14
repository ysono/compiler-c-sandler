//! + Resolve raw identifiers into unique identifiers.
//! + Attach loop IDs to relevant nodes.

mod ident_resolver;

use self::ident_resolver::IdentResolver;
use crate::{
    common::identifier::SymbolIdentifier,
    stage2_parse::{c_ast::*, phase1_parse::ParsedCAst},
};
use anyhow::{anyhow, Context, Result};
use std::rc::Rc;

#[derive(Debug)]
pub struct ResolvedCAst(());
impl CAstVariant for ResolvedCAst {
    type FileScopeDeclaration = Declaration<Self>;
    type BlockScopeDeclaration = Declaration<Self>;
    type ForInitDeclaration = VariableDeclaration<Self>;
    type Identifier = Rc<SymbolIdentifier>;
    type LoopId = Rc<LoopId>;
    type Expression = Expression<Self>;
    type Lvalue = Rc<SymbolIdentifier>;
}

#[derive(Default)]
pub struct CAstValidator {
    ident_resolver: IdentResolver,

    loop_ids_stack: Vec<Rc<LoopId>>,
}
impl CAstValidator {
    pub fn resolve_program(
        &mut self,
        Program { decls }: Program<ParsedCAst>,
    ) -> Result<Program<ResolvedCAst>> {
        let inner = || -> Result<_> {
            let decls = decls
                .into_iter()
                .map(|decl| self.resolve_decl(decl))
                .collect::<Result<Vec<_>>>()?;
            Ok(Program { decls })
        };
        inner().context("c_ast resolver on <program>")
    }

    /* Declaration */

    fn resolve_decl(&mut self, decl: Declaration<ParsedCAst>) -> Result<Declaration<ResolvedCAst>> {
        let inner = || -> Result<_> {
            match decl {
                Declaration::Var(vd) => self.resolve_decl_var(vd).map(Declaration::Var),
                Declaration::Fun(fd) => self.resolve_decl_fun(fd).map(Declaration::Fun),
            }
        };
        inner().context("<declaration>")
    }
    fn resolve_decl_var(
        &mut self,
        VariableDeclaration { ident, typ, storage_class, init }: VariableDeclaration<ParsedCAst>,
    ) -> Result<VariableDeclaration<ResolvedCAst>> {
        let inner = || -> Result<_> {
            // Does transform.
            let ident = self
                .ident_resolver
                .declare_var(ident, storage_class.as_ref())?;

            let init = init.map(|exp| self.resolve_exp(exp)).transpose()?;

            Ok(VariableDeclaration { ident, typ, storage_class, init })
        };
        inner().context("<variable-declaration>")
    }
    fn resolve_decl_fun(
        &mut self,
        FunctionDeclaration {
            ident,
            typ,
            storage_class,
            param_idents,
            body,
        }: FunctionDeclaration<ParsedCAst>,
    ) -> Result<FunctionDeclaration<ResolvedCAst>> {
        // Does transform.
        let inner = || -> Result<_> {
            let ident = self.ident_resolver.declare_fun(ident)?;

            self.ident_resolver.push_new_scope();

            let param_idents = param_idents
                .into_iter()
                .map(|ident| {
                    let storage_class = None;
                    self.ident_resolver.declare_var(ident, storage_class)
                })
                .collect::<Result<Vec<_>>>()?;

            let body = body
                .map(|body| self.resolve_block(body, false))
                .transpose()?;

            self.ident_resolver.pop_scope();

            Ok(FunctionDeclaration {
                ident,
                typ,
                storage_class,
                param_idents,
                body,
            })
        };
        inner().context("<function-declaration>")
    }

    /* Block */

    fn resolve_block(
        &mut self,
        Block { items }: Block<ParsedCAst>,
        use_new_ident_scope: bool,
    ) -> Result<Block<ResolvedCAst>> {
        let inner = || -> Result<_> {
            if use_new_ident_scope {
                self.ident_resolver.push_new_scope();
            }

            let items = items
                .into_iter()
                .map(|item| self.resolve_block_item(item))
                .collect::<Result<Vec<_>>>()?;

            if use_new_ident_scope {
                self.ident_resolver.pop_scope();
            }

            Ok(Block { items })
        };
        inner().context("<block>")
    }
    fn resolve_block_item(
        &mut self,
        item: BlockItem<ParsedCAst>,
    ) -> Result<BlockItem<ResolvedCAst>> {
        let inner = || -> Result<_> {
            match item {
                BlockItem::Declaration(decl) => self.resolve_decl(decl).map(BlockItem::Declaration),
                BlockItem::Statement(stmt) => self.resolve_stmt(stmt).map(BlockItem::Statement),
            }
        };
        inner().context("<block-item>")
    }

    /* Statement */

    fn resolve_stmt(&mut self, stmt: Statement<ParsedCAst>) -> Result<Statement<ResolvedCAst>> {
        let inner = || -> Result<_> {
            match stmt {
                Statement::Return(exp) => self.resolve_exp(exp).map(Statement::Return),
                Statement::Expression(exp) => self.resolve_exp(exp).map(Statement::Expression),
                Statement::If(If { condition, then, elze }) => {
                    let condition = self.resolve_exp(condition)?;
                    let then = Box::new(self.resolve_stmt(*then)?);
                    let elze = elze
                        .map(|elze| self.resolve_stmt(*elze))
                        .transpose()?
                        .map(Box::new);
                    return Ok(Statement::If(If { condition, then, elze }));
                }
                Statement::Compound(block) => {
                    // Does transform.
                    self.resolve_block(block, true).map(Statement::Compound)
                }
                Statement::Break(()) => {
                    // Does transform.
                    let loop_id = self
                        .loop_ids_stack
                        .last()
                        .ok_or(anyhow!("Cannot break outside loop scope"))?;
                    Ok(Statement::Break(Rc::clone(loop_id)))
                }
                Statement::Continue(()) => {
                    // Does transform.
                    let loop_id = self
                        .loop_ids_stack
                        .last()
                        .ok_or(anyhow!("Cannot continue outside loop scope"))?;
                    Ok(Statement::Continue(Rc::clone(loop_id)))
                }
                Statement::While((), condbody) => self.resolve_stmt_while(condbody), // Does transform.
                Statement::DoWhile((), condbody) => self.resolve_stmt_dowhile(condbody), // Does transform.
                Statement::For((), foor) => self.resolve_stmt_for(foor), // Does transform.
                Statement::Null => Ok(Statement::Null),
            }
        };
        inner().context("<statement>")
    }
    fn resolve_stmt_while(
        &mut self,
        condbody: CondBody<ParsedCAst>,
    ) -> Result<Statement<ResolvedCAst>> {
        let inner = || -> Result<_> {
            let (loop_id, condbody) = self.resolve_stmt_condbody("while", condbody)?;
            Ok(Statement::While(loop_id, condbody))
        };
        inner().context("<statement> while")
    }
    fn resolve_stmt_dowhile(
        &mut self,
        condbody: CondBody<ParsedCAst>,
    ) -> Result<Statement<ResolvedCAst>> {
        let inner = || -> Result<_> {
            let (loop_id, condbody) = self.resolve_stmt_condbody("dowhile", condbody)?;
            Ok(Statement::DoWhile(loop_id, condbody))
        };
        inner().context("<statement> dowhile")
    }
    fn resolve_stmt_condbody(
        &mut self,
        descr: &'static str,
        CondBody { condition, body }: CondBody<ParsedCAst>,
    ) -> Result<(Rc<LoopId>, CondBody<ResolvedCAst>)> {
        let condition = self.resolve_exp(condition)?;

        let loop_id = Rc::new(LoopId::new(descr));
        self.loop_ids_stack.push(loop_id);

        let body = Box::new(self.resolve_stmt(*body)?);

        let loop_id = self.loop_ids_stack.pop().unwrap();

        Ok((loop_id, CondBody { condition, body }))
    }
    fn resolve_stmt_for(
        &mut self,
        For { init, condition, post, body }: For<ParsedCAst>,
    ) -> Result<Statement<ResolvedCAst>> {
        let inner = || -> Result<_> {
            self.ident_resolver.push_new_scope();

            let init = match init {
                ForInit::Decl(vd) => ForInit::Decl(self.resolve_decl_var(vd)?),
                ForInit::Exp(exp) => ForInit::Exp(self.resolve_exp(exp)?),
                ForInit::None => ForInit::None,
            };

            let condition = condition.map(|exp| self.resolve_exp(exp)).transpose()?;

            let post = post.map(|exp| self.resolve_exp(exp)).transpose()?;

            let loop_id = Rc::new(LoopId::new("for"));
            self.loop_ids_stack.push(loop_id);

            let body = Box::new(self.resolve_stmt(*body)?);

            let loop_id = self.loop_ids_stack.pop().unwrap();

            self.ident_resolver.pop_scope();

            let foor = For { init, condition, post, body };
            Ok(Statement::For(loop_id, foor))
        };
        inner().context("<statement> for")
    }

    /* Expression */

    fn resolve_exp(&mut self, exp: Expression<ParsedCAst>) -> Result<Expression<ResolvedCAst>> {
        let inner = || -> Result<_> {
            let exp = match exp {
                Expression::Const(konst) => Expression::Const(konst),
                Expression::Var(ident) => {
                    // Does transform.
                    let ident = self.ident_resolver.get(&ident)?;
                    Expression::Var(ident)
                }
                Expression::Cast(Cast { typ, sub_exp }) => {
                    let sub_exp = Box::new(self.resolve_exp(*sub_exp)?);
                    Expression::Cast(Cast { typ, sub_exp })
                }
                Expression::Unary(Unary { op, sub_exp }) => {
                    let sub_exp = Box::new(self.resolve_exp(*sub_exp)?);
                    Expression::Unary(Unary { op, sub_exp })
                }
                Expression::Binary(Binary { op, lhs, rhs }) => {
                    let lhs = Box::new(self.resolve_exp(*lhs)?);
                    let rhs = Box::new(self.resolve_exp(*rhs)?);
                    Expression::Binary(Binary { op, lhs, rhs })
                }
                Expression::Assignment(Assignment { lhs, rhs }) => match *lhs {
                    // Does transform.
                    Expression::Var(ident) => {
                        let lhs = self.ident_resolver.get(&ident)?;
                        let rhs = Box::new(self.resolve_exp(*rhs)?);
                        Expression::Assignment(Assignment { lhs, rhs })
                    }
                    _ => {
                        return Err(anyhow!(
                            "Assignment LHS must be an lvalue, but found {lhs:?}"
                        ))
                    }
                },
                Expression::Conditional(Conditional { condition, then, elze }) => {
                    let condition = Box::new(self.resolve_exp(*condition)?);
                    let then = Box::new(self.resolve_exp(*then)?);
                    let elze = Box::new(self.resolve_exp(*elze)?);
                    Expression::Conditional(Conditional { condition, then, elze })
                }
                Expression::FunctionCall(FunctionCall { ident, args }) => {
                    // Does transform.
                    let ident = self.ident_resolver.get(&ident)?;
                    let args = args
                        .into_iter()
                        .map(|exp| self.resolve_exp(exp))
                        .collect::<Result<Vec<_>>>()?;
                    Expression::FunctionCall(FunctionCall { ident, args })
                }
            };
            Ok(exp)
        };
        inner().context("<exp>")
    }
}
