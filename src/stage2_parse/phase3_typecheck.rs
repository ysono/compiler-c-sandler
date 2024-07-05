use crate::{
    stage2_parse::{c_ast::*, phase2_resolve::ResolvedCAst},
    symbol_table_frontend::{FunDeclScope, ResolvedIdentifier, SymbolTable, VarDeclScope},
    types_frontend::{FunType, VarType},
};
use anyhow::{anyhow, Result};
use std::rc::Rc;

#[derive(Debug)]
pub struct TypeCheckedCAst(());
impl CAstVariant for TypeCheckedCAst {
    type Identifier = Rc<ResolvedIdentifier>;
    type BlockScopeDeclaration = BlockScopeDeclaration<TypeCheckedCAst>;
    type LoopId = Rc<LoopId>;
    type Expression = TypedExpression<TypeCheckedCAst>;
    type Lvalue = Rc<ResolvedIdentifier>;
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

    /* Declaration */

    fn typecheck_decl_var(
        &mut self,
        decl: VariableDeclaration<ResolvedCAst>,
        scope: VarDeclScope,
    ) -> Result<VariableDeclaration<TypeCheckedCAst>> {
        self.symbol_table.declare_var(scope, &decl)?;

        let VariableDeclaration { ident, init, typ, storage_class } = decl;

        let init = init
            .map(|exp| -> Result<_> {
                let exp = self.typecheck_exp(exp)?;
                let exp = Self::maybe_cast_exp(exp, typ);
                Ok(exp)
            })
            .transpose()?;

        Ok(VariableDeclaration { ident, init, typ, storage_class })
    }
    fn typecheck_decl_fundecl(
        &mut self,
        decl: FunctionDeclaration<ResolvedCAst>,
        scope: FunDeclScope,
    ) -> Result<FunctionDeclaration<TypeCheckedCAst>> {
        self.symbol_table.declare_fun(scope, &decl, None)?;

        let FunctionDeclaration {
            ident,
            param_idents,
            typ,
            storage_class,
        } = decl;
        let decl = FunctionDeclaration {
            ident,
            param_idents,
            typ,
            storage_class,
        };

        Ok(decl)
    }
    fn typecheck_decl_fundefn(
        &mut self,
        FunctionDefinition { decl, body }: FunctionDefinition<ResolvedCAst>,
        scope: FunDeclScope,
    ) -> Result<FunctionDefinition<TypeCheckedCAst>> {
        self.symbol_table.declare_fun(scope, &decl, Some(&body))?;

        let FunctionDeclaration {
            ident,
            param_idents,
            typ,
            storage_class,
        } = decl;

        for (ident, typ) in param_idents.iter().zip(typ.params.iter()) {
            let mock_var_decl = VariableDeclaration {
                ident: Rc::clone(ident),
                init: None,
                typ: *typ,
                storage_class: None,
            };
            self.symbol_table
                .declare_var(VarDeclScope::Paren, &mock_var_decl)?;
        }

        self.curr_fun_type = Some(Rc::clone(&typ));
        let body = self.typecheck_block(body)?;
        self.curr_fun_type = None;

        let decl = FunctionDeclaration {
            ident,
            param_idents,
            typ,
            storage_class,
        };
        Ok(FunctionDefinition { decl, body })
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

    /* Expression */

    fn typecheck_exp(
        &mut self,
        exp: Expression<ResolvedCAst>,
    ) -> Result<TypedExpression<TypeCheckedCAst>> {
        let exp = match exp {
            Expression::Const(konst) => {
                let typ = konst.var_type();
                let exp = Expression::Const(konst);
                TypedExpression { typ, exp }
            }
            Expression::Var(ident) => {
                let typ = self.symbol_table.use_var(&ident)?;
                let exp = Expression::Var(ident);
                TypedExpression { typ, exp }
            }
            Expression::Cast(Cast { typ, sub_exp }) => {
                let sub_exp = Box::new(self.typecheck_exp(*sub_exp)?);
                let exp = Expression::Cast(Cast { typ, sub_exp });
                TypedExpression { typ, exp }
            }
            Expression::Unary(Unary { op, sub_exp }) => {
                let sub_exp = Box::new(self.typecheck_exp(*sub_exp)?);

                if matches!(
                    (&op, sub_exp.typ),
                    (UnaryOperator::Complement, VarType::Double)
                ) {
                    return Err(anyhow!("Cannot apply {op:?} on {sub_exp:?}"));
                }

                let typ = match &op {
                    UnaryOperator::Not => VarType::Int,
                    UnaryOperator::Complement | UnaryOperator::Negate => sub_exp.typ,
                };
                let exp = Expression::Unary(Unary { op, sub_exp });
                TypedExpression { typ, exp }
            }
            Expression::Binary(Binary { op, lhs, rhs }) => {
                use BinaryOperator as BO;

                let lhs = self.typecheck_exp(*lhs)?;
                let rhs = self.typecheck_exp(*rhs)?;

                let common_typ = VarType::derive_common_type(lhs.typ, rhs.typ);
                let cast_exp =
                    |exp: TypedExpression<TypeCheckedCAst>| Self::maybe_cast_exp(exp, common_typ);

                if matches!((&op, common_typ), (BO::Rem, VarType::Double)) {
                    return Err(anyhow!("Cannot apply {op:?} on {lhs:?} and {rhs:?}"));
                }

                let (out_typ, lhs, rhs) = match &op {
                    BO::And | BO::Or => (VarType::Int, lhs, rhs),
                    BO::Eq | BO::Neq | BO::Lt | BO::Lte | BO::Gt | BO::Gte => {
                        (VarType::Int, cast_exp(lhs), cast_exp(rhs))
                    }
                    BO::Sub | BO::Add | BO::Mul | BO::Div | BO::Rem => {
                        (common_typ, cast_exp(lhs), cast_exp(rhs))
                    }
                };
                let out_exp = Expression::Binary(Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                });
                TypedExpression { typ: out_typ, exp: out_exp }
            }
            Expression::Assignment(Assignment { lhs, rhs }) => {
                let typ = self.symbol_table.use_var(&lhs)?;
                let rhs = self.typecheck_exp(*rhs)?;
                let rhs = Self::maybe_cast_exp(rhs, typ);
                let exp = Expression::Assignment(Assignment { lhs, rhs: Box::new(rhs) });
                TypedExpression { typ, exp }
            }
            Expression::Conditional(Conditional { condition, then, elze }) => {
                let condition = self.typecheck_exp(*condition)?;
                let then = self.typecheck_exp(*then)?;
                let elze = self.typecheck_exp(*elze)?;

                let typ = VarType::derive_common_type(then.typ, elze.typ);
                let then = Self::maybe_cast_exp(then, typ);
                let elze = Self::maybe_cast_exp(elze, typ);

                let exp = Expression::Conditional(Conditional {
                    condition: Box::new(condition),
                    then: Box::new(then),
                    elze: Box::new(elze),
                });
                TypedExpression { typ, exp }
            }
            Expression::FunctionCall(fun_call) => {
                let fun_typ = self.symbol_table.call_fun(&fun_call)?;
                let fun_typ = Rc::clone(fun_typ);

                let FunctionCall { ident, args } = fun_call;

                let mut out_args = Vec::with_capacity(args.len());
                for (param_typ, arg_exp) in fun_typ.params.iter().cloned().zip(args.into_iter()) {
                    let arg_exp = self.typecheck_exp(arg_exp)?;
                    let arg_exp = Self::maybe_cast_exp(arg_exp, param_typ);
                    out_args.push(arg_exp);
                }
                let exp = Expression::FunctionCall(FunctionCall { ident, args: out_args });
                TypedExpression { typ: fun_typ.ret, exp }
            }
        };
        Ok(exp)
    }
    fn maybe_cast_exp(
        exp: TypedExpression<TypeCheckedCAst>,
        typ: VarType,
    ) -> TypedExpression<TypeCheckedCAst> {
        if exp.typ == typ {
            exp
        } else {
            let exp = Expression::Cast(Cast { typ, sub_exp: Box::new(exp) });
            TypedExpression { typ, exp }
        }
    }
}
