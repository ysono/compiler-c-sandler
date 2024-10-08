//! + Translate each function from a tree to a linear sequence of abstract assembly-like instructions and jumps,
//!     where each instruction operates on values that exist at abstract locations.
//! + Give each new intermediary value its own abstract location,
//!     by registering it as a new symbol, alongside the symbols that are declared in the C src code.

mod ary;
mod conditional;
mod looping;
mod misc;
mod obj;

use self::looping::LoopIdToLabels;
use crate::{
    common::{
        primitive::Const,
        symbol_table_frontend::{StaticInitialValue, Symbol, SymbolTable, VarAttrs},
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
    utils::noop,
};
use std::rc::Rc;

pub struct Tackifier {}
impl Tackifier {
    pub fn tackify_program(
        c::Program { decls }: c::Program<TypeCheckedCAst>,
        symbol_table: &mut SymbolTable,
    ) -> Program {
        let funs = Self::tackify_funs(decls, symbol_table);

        /* Book ch10 Tacky page 235:
        ```
        Right now, it doesn’t matter whether we process the AST or the symbol table first. Starting in Chapter 16, it will be important that we process the AST first and the symbol table second.
        ``` */
        let static_vars = Self::tackify_static_vars(symbol_table);

        Program { static_vars, funs }
    }
    fn tackify_funs(
        c_funs: Vec<c::FunctionDefinition<TypeCheckedCAst>>,
        symbol_table: &mut SymbolTable,
    ) -> Vec<Function> {
        c_funs
            .into_iter()
            .map(|c_fun| {
                let gen = FunInstrsGenerator::new(symbol_table);
                gen.tackify_fun_defn(c_fun)
            })
            .collect()
    }
    fn tackify_static_vars(symbol_table: &SymbolTable) -> Vec<StaticVariable> {
        let mut static_vars = vec![];
        for (ident, symbol) in symbol_table.iter() {
            if let Symbol::Var {
                typ,
                attrs: VarAttrs::StaticStorageDuration { visibility, initial_value },
            } = symbol
            {
                let konst = match initial_value {
                    StaticInitialValue::Initial(konst) => *konst,
                    StaticInitialValue::Tentative => Const::new_zero_bits(typ),
                    StaticInitialValue::NoInitializer => continue,
                };
                let static_var = StaticVariable {
                    ident: Rc::clone(ident),
                    visibility: *visibility,
                    typ: typ.clone(),
                    init: konst,
                };
                static_vars.push(static_var);
            }
        }
        static_vars
    }
}

// `pub` for rustdoc.
pub(crate) struct FunInstrsGenerator<'a> {
    symbol_table: &'a mut SymbolTable,

    loop_id_to_labels: LoopIdToLabels,

    instrs: Vec<Instruction>,
}
impl<'a> FunInstrsGenerator<'a> {
    fn new(symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            symbol_table,
            loop_id_to_labels: Default::default(),
            instrs: Default::default(),
        }
    }

    /* Definition */

    fn tackify_fun_defn(
        mut self,
        c::FunctionDefinition {
            ident,
            visibility,
            param_idents,
            body,
        }: c::FunctionDefinition<TypeCheckedCAst>,
    ) -> Function {
        self.gen_block(body);

        /* This below fill-in implicit return statement is required for each function,
            so that even if the src C code omits a `return` statement at the end, the emitted asm does return.
        The return type of the `main` function must be `int`.
        The return type of any non-`main` function that lacks a `return` statement is undefined.
        Therefore, it's correct to always return `int`. */
        let val = Value::Constant(Const::Int(0));
        self.instrs.push(Instruction::Return(val));

        Function {
            ident,
            visibility,
            param_idents,
            instrs: self.instrs,
        }
    }
    fn gen_var_defn(
        &mut self,
        c::VariableDefinition { ident, init }: c::VariableDefinition<TypeCheckedCAst>,
    ) {
        let obj = Object::Direct(ident);
        let val = self.gen_exp_and_get_value(init);
        self.gen_assignment(obj, val);
    }

    /* Block */

    fn gen_block(&mut self, c_block: c::Block<TypeCheckedCAst>) {
        for c_item in c_block.items {
            match c_item {
                c::BlockItem::Declaration(c_var_defn) => self.gen_var_defn(c_var_defn),
                c::BlockItem::Statement(c_stmt) => self.gen_stmt(c_stmt),
            }
        }
    }

    /* Statement */

    fn gen_stmt(&mut self, c_stmt: c::Statement<TypeCheckedCAst>) {
        match c_stmt {
            c::Statement::Return(c_exp) => {
                let val = self.gen_exp_and_get_value(c_exp);
                self.instrs.push(Instruction::Return(val));
            }
            c::Statement::Expression(c_exp) => {
                self.gen_exp(c_exp);
            }
            c::Statement::If(c_if) => self.gen_stmt_conditional(c_if),
            c::Statement::Compound(c_block) => self.gen_block(c_block),
            c::Statement::Break(loop_id) => self.gen_stmt_break(loop_id),
            c::Statement::Continue(loop_id) => self.gen_stmt_continue(loop_id),
            c::Statement::While(loop_id, c_condbody) => self.gen_stmt_while(loop_id, c_condbody),
            c::Statement::DoWhile(loop_id, c_condbody) => {
                self.gen_stmt_dowhile(loop_id, c_condbody)
            }
            c::Statement::For(loop_id, foor) => self.gen_stmt_for(loop_id, foor),
            c::Statement::Null => noop!(),
        }
    }

    /* Expression */

    fn gen_exp_and_get_value(
        &mut self,
        exp: c::TypedExpression<c::Expression<TypeCheckedCAst>>,
    ) -> Value {
        match self.gen_exp(exp) {
            ExpResult::Value(val) => val,
            ExpResult::Object(obj) => self.convert_object_to_value(obj),
        }
    }
    fn gen_exp(
        &mut self,
        c::TypedExpression { exp, typ }: c::TypedExpression<c::Expression<TypeCheckedCAst>>,
    ) -> ExpResult {
        match exp {
            c::Expression::Const(konst) => Value::Constant(konst).into(),
            c::Expression::Var(ident) => Object::Direct(ident).into(),
            c::Expression::Cast(c_cast) => self.gen_exp_cast(c_cast).into(),
            c::Expression::Unary(c_unary) => self.gen_exp_unary(c_unary, typ).into(),
            c::Expression::Binary(c_binary) => self.gen_exp_binary(c_binary, typ).into(),
            c::Expression::Assignment(c_assign) => self.gen_exp_assignment(c_assign).into(),
            c::Expression::Conditional(c_cond) => self.gen_exp_conditional(c_cond, typ).into(),
            c::Expression::FunctionCall(c_fun_call) => {
                self.gen_exp_fun_call(c_fun_call, typ).into()
            }
            c::Expression::Dereference(c_deref) => self.gen_exp_deref(c_deref, typ).into(),
            c::Expression::AddrOf(c_addrof) => self.gen_exp_addrof(c_addrof, typ).into(),
        }
    }
    fn gen_exp_lvalue(
        &mut self,
        c::TypedExpression { exp, typ }: c::TypedExpression<c::LvalueExpression<TypeCheckedCAst>>,
    ) -> Object {
        match exp {
            c::LvalueExpression::Var(ident) => Object::Direct(ident),
            c::LvalueExpression::Dereference(c_deref) => self.gen_exp_deref(c_deref, typ),
        }
    }
    /// Aka lvalue-converting an lvalue expression.
    fn convert_object_to_value(&mut self, object: Object) -> Value {
        match object {
            Object::Direct(ident) => Value::Variable(ident),
            Object::Pointee { addr, typ } => {
                let dst = self.symbol_table.declare_var_anon(typ);
                self.instrs.push(Instruction::Load(Load {
                    src_addr: addr,
                    dst: Rc::clone(&dst),
                }));
                Value::Variable(dst)
            }
        }
    }
}
