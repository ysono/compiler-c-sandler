pub mod c_ast {
    pub use self::expression::*;
    pub use crate::stage2a_parser::c_ast::{BinaryOperator, Const, Identifier, UnaryOperator};
    use std::hash::{Hash, Hasher};
    use std::rc::Rc;
    use std::sync::atomic::{AtomicU64, Ordering};

    #[derive(Debug)]
    pub struct Program {
        pub func: Function,
    }

    #[derive(Debug)]
    pub struct Function {
        pub ident: Identifier,
        pub body: Vec<BlockItem>,
    }

    #[derive(Debug)]
    pub enum BlockItem {
        Declaration(Declaration),
        Statement(Statement),
    }

    #[derive(Debug)]
    pub struct Declaration {
        pub var: Rc<Variable>,
        pub init: Option<Expression>,
    }

    #[derive(Debug)]
    pub enum Statement {
        Return(Expression),
        Expression(Expression),
        Null,
    }

    #[derive(Debug)]
    pub enum Expression {
        Const(Const),
        Var(Rc<Variable>),
        Unary(Unary),
        Binary(Binary),
        Assignment(Assignment),
    }
    mod expression {
        use super::*;

        #[derive(Debug)]
        pub struct Unary {
            pub op: UnaryOperator,
            pub sub_exp: Box<Expression>,
        }

        #[derive(Debug)]
        pub struct Binary {
            pub op: BinaryOperator,
            pub lhs: Box<Expression>,
            pub rhs: Box<Expression>,
        }

        #[derive(Debug)]
        pub struct Assignment {
            pub var: Rc<Variable>,
            pub rhs: Box<Expression>,
        }
    }

    #[derive(Debug)]
    pub struct Variable {
        id: u64,
        orig_ident: Option<Rc<Identifier>>,
    }
    impl Variable {
        pub fn new_anon() -> Self {
            Self::new(None)
        }
        pub(super) fn new(orig_ident: Option<Rc<Identifier>>) -> Self {
            static NEXT_ID: AtomicU64 = AtomicU64::new(0);
            let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
            Self {
                id: curr_id,
                orig_ident,
            }
        }
        pub fn orig_ident(&self) -> &Option<Rc<Identifier>> {
            &self.orig_ident
        }
    }
    impl Hash for Variable {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.id.hash(state);
        }
    }
    impl PartialEq for Variable {
        fn eq(&self, other: &Variable) -> bool {
            self.id == other.id
        }
    }
    impl Eq for Variable {}
}

use self::c_ast::*;
use crate::stage2a_parser::c_ast as prev_c_ast;
use anyhow::{anyhow, Context, Result};
use log;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Default)]
pub struct CAstValidator {
    ident_to_var: IdentToVar,
}
impl CAstValidator {
    pub fn resolve_program(&mut self, prog: prev_c_ast::Program) -> Result<Program> {
        let inner = || -> Result<Program> {
            let prev_c_ast::Program { func } = prog;
            let func = self.resolve_func(func)?;
            Ok(Program { func })
        };
        inner().context("c_ast validator on <program>")
    }
    fn resolve_func(&mut self, func: prev_c_ast::Function) -> Result<Function> {
        let inner = || -> Result<Function> {
            let prev_c_ast::Function { ident, body } = func;
            let body = body
                .into_iter()
                .map(|item| self.resolve_block_item(item))
                .collect::<Result<Vec<_>>>()?;
            Ok(Function { ident, body })
        };
        inner().context("<function>")
    }
    fn resolve_block_item(&mut self, item: prev_c_ast::BlockItem) -> Result<BlockItem> {
        let inner = || -> Result<BlockItem> {
            match item {
                prev_c_ast::BlockItem::Declaration(decl) => {
                    let decl = self.resolve_decl(decl)?;
                    Ok(BlockItem::Declaration(decl))
                }
                prev_c_ast::BlockItem::Statement(stmt) => {
                    let stmt = self.resolve_stmt(stmt)?;
                    Ok(BlockItem::Statement(stmt))
                }
            }
        };
        inner().context("<block-item>")
    }
    fn resolve_decl(&mut self, decl: prev_c_ast::Declaration) -> Result<Declaration> {
        let inner = || -> Result<Declaration> {
            let prev_c_ast::Declaration { ident, init } = decl;

            let var = self.ident_to_var.declare_new_var(ident)?;

            let init = init
                .map(|init_exp| self.resolve_exp(init_exp))
                .transpose()?;

            if init.is_some() {
                self.ident_to_var.mark_var_as_initialized(&var)?;
            }

            Ok(Declaration { var, init })
        };
        inner().context("<declaration>")
    }
    fn resolve_stmt(&mut self, stmt: prev_c_ast::Statement) -> Result<Statement> {
        let inner = || -> Result<Statement> {
            match stmt {
                prev_c_ast::Statement::Return(exp) => {
                    let exp = self.resolve_exp(exp)?;
                    Ok(Statement::Return(exp))
                }
                prev_c_ast::Statement::Expression(exp) => {
                    let exp = self.resolve_exp(exp)?;
                    Ok(Statement::Expression(exp))
                }
                prev_c_ast::Statement::Null => Ok(Statement::Null),
            }
        };
        inner().context("<statement>")
    }
    fn resolve_exp(&mut self, exp: prev_c_ast::Expression) -> Result<Expression> {
        let inner = || -> Result<Expression> {
            match exp {
                prev_c_ast::Expression::Const(konst) => return Ok(Expression::Const(konst)),
                prev_c_ast::Expression::Var(ident) => {
                    let var = self.resolve_var(ident, true)?;
                    return Ok(Expression::Var(var));
                }
                prev_c_ast::Expression::Unary(prev_c_ast::Unary { op, sub_exp }) => {
                    let sub_exp = self.resolve_exp(*sub_exp)?;
                    return Ok(Expression::Unary(Unary {
                        op,
                        sub_exp: Box::new(sub_exp),
                    }));
                }
                prev_c_ast::Expression::Binary(prev_c_ast::Binary { op, lhs, rhs }) => {
                    let lhs = self.resolve_exp(*lhs)?;
                    let rhs = self.resolve_exp(*rhs)?;
                    return Ok(Expression::Binary(Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }));
                }
                prev_c_ast::Expression::Assignment(prev_c_ast::Assignment { lhs, rhs }) => {
                    match *lhs {
                        prev_c_ast::Expression::Var(ident) => {
                            let lhs_var = self.resolve_var(ident, false)?;

                            let rhs = self.resolve_exp(*rhs)?;

                            self.ident_to_var.mark_var_as_initialized(&lhs_var)?;

                            return Ok(Expression::Assignment(Assignment {
                                var: lhs_var,
                                rhs: Box::new(rhs),
                            }));
                        }
                        _ => {
                            return Err(anyhow!(
                                "Assignment LHS must be an lvalue, but found {lhs:?}"
                            ))
                        }
                    }
                }
            }
        };
        inner().context("<exp>")
    }
    fn resolve_var(&self, ident: Identifier, check_initialized: bool) -> Result<Rc<Variable>> {
        let VariableState {
            var,
            is_initialized,
        } = self.ident_to_var.get(&ident)?;
        if check_initialized == true {
            if is_initialized == &false {
                log::warn!("Non-initialized variable {ident:?}");
            }
        }
        Ok(Rc::clone(var))
    }
}

#[derive(Default)]
struct IdentToVar {
    ident_to_var: HashMap<Rc<Identifier>, VariableState>,
}
impl IdentToVar {
    fn get(&self, ident: &Identifier) -> Result<&VariableState> {
        self.ident_to_var
            .get(ident)
            .ok_or_else(|| anyhow!("Non-declared identity {ident:?}"))
    }
    fn declare_new_var(&mut self, ident: Identifier) -> Result<Rc<Variable>> {
        match self.ident_to_var.get(&ident) {
            Some(_) => Err(anyhow!("Duplicate declaration of identifier {ident:?}")),
            None => {
                let ident = Rc::new(ident);
                let var = Variable::new(Some(Rc::clone(&ident)));
                let var = Rc::new(var);
                let var_st = VariableState {
                    var: Rc::clone(&var),
                    is_initialized: false,
                };
                self.ident_to_var.insert(ident, var_st);
                Ok(var)
            }
        }
    }
    fn mark_var_as_initialized(&mut self, var: &Variable) -> Result<()> {
        let ident = var
            .orig_ident()
            .as_ref()
            .ok_or_else(|| anyhow!("Validator impl is wrong. Non-named variable {var:?}"))?;
        self.mark_ident_as_initialized(ident)
    }
    fn mark_ident_as_initialized(&mut self, ident: &Identifier) -> Result<()> {
        let var_st = self.ident_to_var.get_mut(ident).ok_or(anyhow!(
            "Validator impl is wrong. Tried to mark a non-declared variable as initialized."
        ))?;
        var_st.is_initialized = true;
        Ok(())
    }
}

struct VariableState {
    var: Rc<Variable>,
    is_initialized: bool,
}
