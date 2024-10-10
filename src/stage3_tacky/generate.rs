//! + Translate each function from a tree to a linear sequence of abstract assembly-like instructions and jumps,
//!     where each instruction operates on values that exist at abstract locations.
//! + Give each new intermediary value its own abstract location,
//!     by registering it as a new symbol, alongside the symbols that are declared in the C src code.

mod ary;
mod conditional;
mod exp;
mod looping;
mod misc;
mod obj;
mod ptr;

use self::looping::LoopIdToLabels;
use crate::{
    common::{
        primitive::Const,
        symbol_table_frontend::{
            InitializerItem, StaticInitialValue, Symbol, SymbolTable, VarAttrs,
        },
        types_backend::ByteLen,
    },
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
    utils::noop,
};
use derive_more::{Constructor, Into};
use std::mem;
use std::rc::Rc;

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

        /* Book ch10 Tacky page 235:
        ```
        Right now, it doesn’t matter whether we process the AST or the symbol table first. Starting in Chapter 16, it will be important that we process the AST first and the symbol table second.
        ``` */
        let static_vars = self.tackify_static_vars();

        let prog = Program { static_vars, funs };

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
    fn tackify_static_vars(&mut self) -> Vec<StaticVariable> {
        let mut static_vars = vec![];
        for (ident, symbol) in self.symbol_table.as_mut().iter_mut() {
            if let Symbol::Var {
                typ,
                attrs: VarAttrs::StaticStorageDuration { visibility, initial_value },
            } = symbol
            {
                /* Here, we're moving the StaticInitialValue vector
                    out of the frontend symtab
                    into the tacky AST.
                In ch16, we might decide to keep everything in the symtab. */
                let inits = match initial_value {
                    StaticInitialValue::Initial(inits) => {
                        mem::replace(inits, Vec::with_capacity(0))
                    }
                    StaticInitialValue::Tentative => {
                        let bytelen = typ.bytelen();
                        let item = InitializerItem::Zero(bytelen);
                        vec![item]
                    }
                    StaticInitialValue::NoInitializer => continue,
                };
                let static_var = StaticVariable {
                    ident: Rc::clone(ident),
                    visibility: *visibility,
                    typ: typ.clone(),
                    inits,
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
    fn gen_var_defn(
        &mut self,
        c::VariableDefinition { ident, typ, init }: c::VariableDefinition<TypeCheckedCAst>,
    ) {
        let single_type = typ.single_type();
        let single_bytelen = ByteLen::from(single_type);
        let single_zero = Const::new_zero_bits(single_type);
        let new_cto = |src: Value, offset: ByteLen| {
            Instruction::CopyToOffset(CopyToOffset {
                src,
                dst_obj: Rc::clone(&ident),
                offset,
            })
        };

        /* Begin instructions */

        let mut offset = ByteLen::new(0);
        for item in init {
            match item {
                InitializerItem::Single(exp) => {
                    let val = self.gen_exp_and_get_value(exp);

                    self.instrs.push(new_cto(val, offset));
                    offset += single_bytelen;
                }
                InitializerItem::Zero(bytelen) => {
                    let final_offset = offset + bytelen;
                    while offset < final_offset {
                        self.instrs
                            .push(new_cto(Value::Constant(single_zero), offset));
                        offset += single_bytelen;
                    }
                    debug_assert_eq!(offset, final_offset);
                }
            }
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
