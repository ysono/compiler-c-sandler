//! + Translate each function from a tree to a linear sequence of abstract assembly-like instructions and jumps,
//!     where each instruction operates on values that exist at abstract locations.
//! + Give each new intermediary value its own abstract location,
//!     by registering it as a new symbol, alongside the symbols that are declared in the C src code.

mod ary;
mod conditional;
mod defn;
mod exp;
mod looping;
mod misc;
mod obj;
mod ptr;

use self::looping::LoopIdToLabels;
use crate::{
    common::{primitive::Const, symbol_table_frontend::SymbolTable},
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
    utils::noop,
};
use derive_more::{Constructor, Into};

#[derive(Constructor, Into)]
pub struct Tackifier {
    symbol_table: SymbolTable,
}
impl Tackifier {
    pub fn tackify_program(
        mut self,
        c::Program { decls }: c::Program<TypeCheckedCAst>,
    ) -> (Program, SymbolTable) {
        let funs = self.tackify_funs(decls);

        let prog = Program { funs };

        (prog, self.symbol_table)
    }
    fn tackify_funs(
        &mut self,
        c_funs: Vec<c::FunctionDefinition<TypeCheckedCAst>>,
    ) -> Vec<Function> {
        c_funs
            .into_iter()
            .map(|c_fun| {
                let gen = FunInstrsGenerator::new(&mut self.symbol_table);
                gen.tackify_fun_defn(c_fun)
            })
            .collect()
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
            typ,
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
            typ,
            visibility,
            param_idents,
            instrs: self.instrs,
        }
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
}
