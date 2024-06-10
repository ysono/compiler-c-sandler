pub mod c_ast {
    pub use self::expression::*;
    pub use self::statement::*;
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
        Break(Rc<LoopId>),
        Continue(Rc<LoopId>),
        While(Rc<LoopId>, CondBody),
        DoWhile(Rc<LoopId>, CondBody),
        For(Rc<LoopId>, For),
        Null,
    }
    mod statement {
        use super::*;

        #[derive(Debug)]
        pub struct If {
            pub condition: Expression,
            pub then: Box<Statement>,
            pub elze: Option<Box<Statement>>,
        }

        #[derive(Debug)]
        pub struct CondBody {
            pub condition: Expression,
            pub body: Box<Statement>,
        }

        #[derive(Debug)]
        pub struct For {
            pub init: ForInit,
            pub condition: Option<Expression>,
            pub post: Option<Expression>,
            pub body: Box<Statement>,
        }

        #[derive(Debug)]
        pub enum ForInit {
            Decl(Declaration),
            Exp(Expression),
            None,
        }
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
        fn eq(&self, other: &Self) -> bool {
            self.id == other.id
        }
    }
    impl Eq for Variable {}

    #[derive(Debug)]
    pub struct LoopId {
        id: u64,
        descr: &'static str,
    }
    impl LoopId {
        pub fn new(descr: &'static str) -> Self {
            static NEXT_ID: AtomicU64 = AtomicU64::new(0);
            let curr_id = NEXT_ID.fetch_add(1, Ordering::SeqCst);
            Self { id: curr_id, descr }
        }
        pub fn id(&self) -> u64 {
            self.id
        }
        pub fn descr(&self) -> &'static str {
            self.descr
        }
    }
    impl Hash for LoopId {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.id.hash(state);
        }
    }
    impl PartialEq for LoopId {
        fn eq(&self, other: &Self) -> bool {
            self.id == other.id
        }
    }
    impl Eq for LoopId {}
}

use self::c_ast::*;
use crate::stage2a_parser::c_ast as p; // "p" for "previous c_ast".
use anyhow::{anyhow, Context, Result};
use derive_more::{Deref, DerefMut};
use log;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Default)]
pub struct CAstValidator {
    ident_to_var: IdentToVar,

    loop_ids_stack: LoopIdsStack,
}
impl CAstValidator {
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

    /* Statement */

    fn resolve_stmt(&mut self, stmt: p::Statement) -> Result<Statement> {
        let inner = || -> Result<Statement> {
            match stmt {
                p::Statement::Return(p_exp) => {
                    let exp = self.resolve_exp(p_exp)?;
                    Ok(Statement::Return(exp))
                }
                p::Statement::Expression(p_exp) => {
                    let exp = self.resolve_exp(p_exp)?;
                    Ok(Statement::Expression(exp))
                }
                p::Statement::If(p_yf) => {
                    let condition = self.resolve_exp(p_yf.condition)?;
                    let then = self.resolve_stmt(*p_yf.then)?;
                    let elze = p_yf.elze.map(|elze| self.resolve_stmt(*elze)).transpose()?;
                    return Ok(Statement::If(If {
                        condition,
                        then: Box::new(then),
                        elze: elze.map(Box::new),
                    }));
                }
                p::Statement::Compound(p_block) => {
                    let block = self.resolve_block(p_block)?;
                    Ok(Statement::Compound(block))
                }
                p::Statement::Break => {
                    let loop_id = self
                        .loop_ids_stack
                        .last()
                        .ok_or(anyhow!("Break outside loop scope"))?;
                    Ok(Statement::Break(Rc::clone(loop_id)))
                }
                p::Statement::Continue => {
                    let loop_id = self
                        .loop_ids_stack
                        .last()
                        .ok_or(anyhow!("Continue outside loop scope"))?;
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
        let inner = || -> Result<Statement> {
            let (loop_id, condbody) = self.resolve_stmt_condbody("while", p_condbody)?;
            Ok(Statement::While(loop_id, condbody))
        };
        inner().context("<statement> while")
    }
    fn resolve_stmt_dowhile(&mut self, p_condbody: p::CondBody) -> Result<Statement> {
        let inner = || -> Result<Statement> {
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
        let inner = || -> Result<_, anyhow::Error> {
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
        self.ident_to_var.push_new_scope();

        let init = match init {
            p::ForInit::Decl(p_decl) => ForInit::Decl(self.resolve_decl(p_decl)?),
            p::ForInit::Exp(p_exp) => ForInit::Exp(self.resolve_exp(p_exp)?),
            p::ForInit::None => ForInit::None,
        };

        let condition = condition.map(|p_exp| self.resolve_exp(p_exp)).transpose()?;

        let post = post.map(|p_exp| self.resolve_exp(p_exp)).transpose()?;

        let loop_id = Rc::new(LoopId::new("for"));
        self.loop_ids_stack.push(loop_id);

        let body = Box::new(self.resolve_stmt(*body)?);

        let loop_id = self.loop_ids_stack.pop().unwrap();

        self.ident_to_var.pop_scope();

        let foor = For {
            init,
            condition,
            post,
            body,
        };
        Ok(Statement::For(loop_id, foor))
    }

    /* Expression */

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

    /* Variable */

    fn resolve_var(&self, ident: Identifier, check_initialized: bool) -> Result<Rc<Variable>> {
        let var_st = self.ident_to_var.get(&ident)?;
        if (check_initialized == true) && (var_st.is_initialized == false) {
            log::warn!("Non-initialized variable {ident:?}");
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
impl Default for IdentToVar {
    fn default() -> Self {
        let ident_to_scoped_vars = HashMap::new();

        let global_scope = HashSet::new();
        let scope_to_idents = vec![global_scope];

        Self {
            ident_to_scoped_vars,
            scope_to_idents,
        }
    }
}
impl IdentToVar {
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
        let scoped_vars = self.ident_to_scoped_vars.entry(ident).or_default();
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

#[derive(Default, Deref, DerefMut)]
struct LoopIdsStack {
    loop_ids_stack: Vec<Rc<LoopId>>,
}
