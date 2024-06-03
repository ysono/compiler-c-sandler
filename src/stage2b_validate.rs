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
        pub body: Block,
    }

    #[derive(Debug)]
    pub struct Block {
        pub items: Vec<BlockItem>,
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
        If(If),
        Compound(Block),
        Null,
    }

    #[derive(Debug)]
    pub enum Expression {
        Const(Const),
        Var(Rc<Variable>),
        Unary(Unary),
        Binary(Binary),
        Assignment(Assignment),
        Conditional(Conditional),
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

        #[derive(Debug)]
        pub struct Conditional {
            pub condition: Box<Expression>,
            pub then: Box<Expression>,
            pub elze: Box<Expression>,
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
        pub fn id(&self) -> u64 {
            self.id
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

    #[derive(Debug)]
    pub struct If {
        pub condition: Expression,
        pub then: Box<Statement>,
        pub elze: Option<Box<Statement>>,
    }
}

use self::c_ast::*;
use crate::stage2a_parser::c_ast as p; // "p" for "previous c_ast".
use anyhow::{anyhow, Context, Result};
use log;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct CAstValidator {
    ident_to_var: IdentToVar,
}
impl CAstValidator {
    pub fn new() -> Self {
        let ident_to_var = IdentToVar::new();
        CAstValidator { ident_to_var }
    }

    pub fn resolve_program(&mut self, prog: p::Program) -> Result<Program> {
        let inner = || -> Result<Program> {
            let p::Program { func } = prog;
            let func = self.resolve_func(func)?;
            Ok(Program { func })
        };
        inner().context("c_ast validator on <program>")
    }
    fn resolve_func(&mut self, func: p::Function) -> Result<Function> {
        let inner = || -> Result<Function> {
            let p::Function { ident, body } = func;
            let body = self.resolve_block(body)?;
            Ok(Function { ident, body })
        };
        inner().context("<function>")
    }
    fn resolve_block(&mut self, block: p::Block) -> Result<Block> {
        self.ident_to_var.push_new_scope();

        let items = block
            .items
            .into_iter()
            .map(|item| self.resolve_block_item(item))
            .collect::<Result<Vec<_>>>()?;

        self.ident_to_var.pop_scope();

        Ok(Block { items })
    }
    fn resolve_block_item(&mut self, item: p::BlockItem) -> Result<BlockItem> {
        let inner = || -> Result<BlockItem> {
            match item {
                p::BlockItem::Declaration(decl) => {
                    let decl = self.resolve_decl(decl)?;
                    Ok(BlockItem::Declaration(decl))
                }
                p::BlockItem::Statement(stmt) => {
                    let stmt = self.resolve_stmt(stmt)?;
                    Ok(BlockItem::Statement(stmt))
                }
            }
        };
        inner().context("<block-item>")
    }
    fn resolve_decl(&mut self, decl: p::Declaration) -> Result<Declaration> {
        let inner = || -> Result<Declaration> {
            let p::Declaration { ident, init } = decl;

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
    fn resolve_stmt(&mut self, stmt: p::Statement) -> Result<Statement> {
        let inner = || -> Result<Statement> {
            match stmt {
                p::Statement::Return(exp) => {
                    let exp = self.resolve_exp(exp)?;
                    Ok(Statement::Return(exp))
                }
                p::Statement::Expression(exp) => {
                    let exp = self.resolve_exp(exp)?;
                    Ok(Statement::Expression(exp))
                }
                p::Statement::If(yf) => {
                    let condition = self.resolve_exp(yf.condition)?;
                    let then = self.resolve_stmt(*yf.then)?;
                    let elze = yf.elze.map(|elze| self.resolve_stmt(*elze)).transpose()?;
                    return Ok(Statement::If(If {
                        condition,
                        then: Box::new(then),
                        elze: elze.map(Box::new),
                    }));
                }
                p::Statement::Compound(block) => {
                    let block = self.resolve_block(block)?;
                    Ok(Statement::Compound(block))
                }
                p::Statement::Break => todo!(),
                p::Statement::Continue => todo!(),
                p::Statement::While(_) => todo!(),
                p::Statement::DoWhile(_) => todo!(),
                p::Statement::For(_) => todo!(),
                p::Statement::Null => Ok(Statement::Null),
            }
        };
        inner().context("<statement>")
    }
    fn resolve_exp(&mut self, exp: p::Expression) -> Result<Expression> {
        let inner = || -> Result<Expression> {
            match exp {
                p::Expression::Const(konst) => return Ok(Expression::Const(konst)),
                p::Expression::Var(ident) => {
                    let var = self.resolve_var(ident, true)?;
                    return Ok(Expression::Var(var));
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
                },
                p::Expression::Conditional(cond) => {
                    let condition = self.resolve_exp(*cond.condition)?;
                    let then = self.resolve_exp(*cond.then)?;
                    let elze = self.resolve_exp(*cond.elze)?;
                    return Ok(Expression::Conditional(Conditional {
                        condition: Box::new(condition),
                        then: Box::new(then),
                        elze: Box::new(elze),
                    }));
                }
            }
        };
        inner().context("<exp>")
    }
    fn resolve_var(&self, ident: Identifier, check_initialized: bool) -> Result<Rc<Variable>> {
        let var_st = self.ident_to_var.get(&ident)?;
        if check_initialized == true {
            if var_st.is_initialized == false {
                log::warn!("Non-initialized variable {ident:?}");
            }
        }
        Ok(Rc::clone(&var_st.var))
    }
}

struct IdentToVar {
    /// This abstracts a copy-on-write dict.
    ident_to_scoped_vars: HashMap<Rc<Identifier>, Vec<VariableState>>,

    /// This tracks each copy-on-write layer's keys.
    scope_to_idents: Vec<HashSet<Rc<Identifier>>>,
}
impl IdentToVar {
    fn new() -> Self {
        let ident_to_scoped_vars = HashMap::new();

        let global_scope = HashSet::new();
        let scope_to_idents = vec![global_scope];

        Self {
            ident_to_scoped_vars,
            scope_to_idents,
        }
    }

    fn push_new_scope(&mut self) {
        let idents = HashSet::new();
        self.scope_to_idents.push(idents);
    }
    fn declare_new_var(&mut self, ident: Identifier) -> Result<Rc<Variable>> {
        let last_scope = self.scope_to_idents.last_mut().unwrap();
        if last_scope.contains(&ident) {
            return Err(anyhow!(
                "Same-scope duplicate declaration of identifier {ident:?}"
            ));
        }
        let ident = Rc::new(ident);
        last_scope.insert(Rc::clone(&ident));

        let var = Rc::new(Variable::new(Some(Rc::clone(&ident))));
        let var_st = VariableState {
            var: Rc::clone(&var),
            is_initialized: false,
        };
        let scoped_vars = self
            .ident_to_scoped_vars
            .entry(ident)
            .or_insert_with(|| vec![]);
        scoped_vars.push(var_st);

        Ok(var)
    }
    fn pop_scope(&mut self) {
        let idents = self.scope_to_idents.pop().unwrap();
        for ident in idents {
            let scoped_vars = self.ident_to_scoped_vars.get_mut(&ident).unwrap();
            if scoped_vars.len() > 1 {
                scoped_vars.pop();
            } else {
                self.ident_to_scoped_vars.remove(&ident);
            }
        }
    }

    fn get(&self, ident: &Identifier) -> Result<&VariableState> {
        let scoped_vars = self
            .ident_to_scoped_vars
            .get(ident)
            .ok_or_else(|| anyhow!("Non-declared identity {ident:?}"))?;
        let var_st = scoped_vars.last().unwrap();
        Ok(var_st)
    }
    fn get_mut(&mut self, ident: &Identifier) -> Result<&mut VariableState> {
        let scoped_vars = self
            .ident_to_scoped_vars
            .get_mut(ident)
            .ok_or_else(|| anyhow!("Non-declared identity {ident:?}"))?;
        let var_st = scoped_vars.last_mut().unwrap();
        Ok(var_st)
    }

    fn mark_var_as_initialized(&mut self, var: &Variable) -> Result<()> {
        let ident = var.orig_ident().as_ref().unwrap();
        self.mark_ident_as_initialized(ident)
    }
    fn mark_ident_as_initialized(&mut self, ident: &Identifier) -> Result<()> {
        let var_st = self.get_mut(ident).unwrap();
        var_st.is_initialized = true;
        Ok(())
    }
}

struct VariableState {
    var: Rc<Variable>,
    is_initialized: bool,
}
