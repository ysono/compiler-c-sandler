mod helpers;

use self::helpers::{BinaryOperatorType, LoopIdToLabels, ShortCircuitBOT};
use crate::{
    stage2_parse::{c_ast as c, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast::*,
    symbol_table_frontend::{
        FunAttrs, ResolvedIdentifier, StaticInitialValue, Symbol, SymbolTable, VarAttrs,
    },
    types_backend::OperandByteLen,
    types_frontend::{Const, VarType},
};
use std::cmp::Ordering;
use std::rc::Rc;

pub struct Tackifier {}
impl Tackifier {
    pub fn tackify_program(
        c::Program { decls: c_decls }: c::Program<TypeCheckedCAst>,
        symbol_table: &mut SymbolTable,
    ) -> Program {
        let funs = Self::tackify_decls(c_decls, symbol_table);

        /* Book ch10 Tacky page 235:
        ```
        Right now, it doesn’t matter whether we process the AST or the symbol table first. Starting in Chapter 16, it will be important that we process the AST first and the symbol table second.
        ``` */
        let static_vars = Self::tackify_static_vars(symbol_table);

        Program { static_vars, funs }
    }
    fn tackify_decls(
        c_decls: Vec<c::Declaration<TypeCheckedCAst>>,
        symbol_table: &mut SymbolTable,
    ) -> Vec<Function> {
        let mut funs = vec![];
        for c_decl in c_decls {
            match c_decl {
                c::Declaration::VarDecl(_) => { /* No-op. */ }
                c::Declaration::FunDecl(_) => { /* No-op. */ }
                c::Declaration::FunDefn(fd) => {
                    let gen = FunInstrsGenerator::new(symbol_table);
                    let fun = gen.tackify_fun_defn(fd);
                    funs.push(fun);
                }
            }
        }
        funs
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
                    StaticInitialValue::Tentative => Const::new_zero_bits(*typ),
                    StaticInitialValue::NoInitializer => continue,
                };
                let static_var = StaticVariable {
                    ident: Rc::clone(ident),
                    visibility: *visibility,
                    typ: *typ,
                    init: konst,
                };
                static_vars.push(static_var);
            }
        }
        static_vars
    }
}

struct FunInstrsGenerator<'a> {
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

    /* Declaration, Definition */

    fn tackify_fun_defn(
        mut self,
        c::FunctionDefinition {
            decl:
                c::FunctionDeclaration {
                    ident,
                    param_idents,
                    typ: _,
                    storage_class: _,
                },
            body,
        }: c::FunctionDefinition<TypeCheckedCAst>,
    ) -> Function {
        let symbol = self.symbol_table.get(&ident).unwrap();
        let visibility = match symbol {
            Symbol::Var { .. } => panic!("Impossible."),
            Symbol::Fun {
                attrs: FunAttrs { visibility, .. },
                ..
            } => *visibility,
        };

        self.gen_block(body);

        /* This below fill-in implicit return statement is required for each function,
            so that even if the src C code omits a `return` statement at the end, the emitted asm does return.
        The return type of the `main` function must be `int`.
        The return type of any non-`main` function that lacks a `return` statement is undefined.
        Therefore, it's correct to always return `int`. */
        let konst = Const::Int(0);
        let ret_stmt = c::Statement::Return(c::TypedExpression {
            exp: c::Expression::Const(konst),
            typ: konst.var_type(),
        });
        self.gen_stmt(ret_stmt);

        Function {
            ident,
            visibility,
            param_idents,
            instrs: self.instrs,
        }
    }
    fn gen_decl_block_scope(&mut self, c_decl: c::BlockScopeDeclaration<TypeCheckedCAst>) {
        match c_decl {
            c::BlockScopeDeclaration::VarDecl(c_var_decl) => {
                self.gen_decl_var_block_scope(c_var_decl)
            }
            c::BlockScopeDeclaration::FunDecl(_c_fun_decl) => { /* No-op. */ }
        }
    }
    fn gen_decl_var_block_scope(
        &mut self,
        c::VariableDeclaration {
            ident,
            init,
            typ: _,
            storage_class,
        }: c::VariableDeclaration<TypeCheckedCAst>,
    ) {
        /* Iff this var has storage_duration=automatic and initializer=some, then initialize at runtime.
        As a performance improvement, we infer storage duration from storage class specifier, rather than from the symbol table. */
        match (storage_class, init) {
            (None, Some(init_exp)) => {
                self.gen_exp_assignment(ident, init_exp);
            }
            _ => { /* No-op. */ }
        }
    }

    /* Block, Statement, Expression */

    fn gen_block(&mut self, c_block: c::Block<TypeCheckedCAst>) {
        for c_item in c_block.items {
            match c_item {
                c::BlockItem::Declaration(c_decl) => self.gen_decl_block_scope(c_decl),
                c::BlockItem::Statement(c_stmt) => self.gen_stmt(c_stmt),
            }
        }
    }
    fn gen_stmt(&mut self, c_stmt: c::Statement<TypeCheckedCAst>) {
        match c_stmt {
            c::Statement::Return(c_exp) => {
                let t_val = self.gen_exp(c_exp);
                self.instrs.push(Instruction::Return(t_val));
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
            c::Statement::Null => { /* No-op. */ }
        }
    }
    fn gen_exp(
        &mut self,
        c::TypedExpression { exp, typ }: c::TypedExpression<TypeCheckedCAst>,
    ) -> ReadableValue {
        match exp {
            c::Expression::Const(konst) => ReadableValue::Constant(konst),
            c::Expression::Var(ident) => ReadableValue::Variable(ident),
            c::Expression::Cast(c_cast) => self.gen_exp_cast(c_cast),
            c::Expression::Unary(c_unary) => self.gen_exp_unary(c_unary, typ),
            c::Expression::Binary(c_binary) => self.gen_exp_binary(c_binary, typ),
            c::Expression::Assignment(c::Assignment { lhs, rhs }) => {
                self.gen_exp_assignment(lhs, *rhs)
            }
            c::Expression::Conditional(c_cond) => self.gen_exp_conditional(c_cond, typ),
            c::Expression::FunctionCall(c_fun_call) => self.gen_exp_fun_call(c_fun_call, typ),
        }
    }

    /* C Cast and Unary */

    fn gen_exp_cast(
        &mut self,
        c::Cast { typ, sub_exp }: c::Cast<TypeCheckedCAst>,
    ) -> ReadableValue {
        let sub_typ = sub_exp.typ;
        let sub_val = self.gen_exp(*sub_exp);
        if sub_typ == typ {
            sub_val
        } else {
            let dst = self.symbol_table.declare_var_anon(typ);
            let srcdst = SrcDst {
                src: sub_val,
                dst: Rc::clone(&dst),
            };

            let instr = if sub_typ == VarType::Double {
                if typ.is_signed() {
                    Instruction::DoubleToInt(srcdst)
                } else {
                    Instruction::DoubleToUInt(srcdst)
                }
            } else if typ == VarType::Double {
                if sub_typ.is_signed() {
                    Instruction::IntToDouble(srcdst)
                } else {
                    Instruction::UIntToDouble(srcdst)
                }
            } else {
                /* Then, casting between different integer types. */
                let out_bytelen = OperandByteLen::from(typ);
                let sub_bytelen = OperandByteLen::from(sub_typ);
                match out_bytelen.cmp(&sub_bytelen) {
                    Ordering::Equal => Instruction::Copy(srcdst),
                    Ordering::Less => Instruction::Truncate(srcdst),
                    Ordering::Greater => {
                        if sub_typ.is_signed() {
                            Instruction::SignExtend(srcdst)
                        } else {
                            Instruction::ZeroExtend(srcdst)
                        }
                    }
                }
            };

            self.instrs.push(instr);
            ReadableValue::Variable(dst)
        }
    }

    fn gen_exp_unary(
        &mut self,
        c::Unary { op, sub_exp }: c::Unary<TypeCheckedCAst>,
        out_typ: VarType,
    ) -> ReadableValue {
        let op = helpers::convert_op_unary(op);
        let src = self.gen_exp(*sub_exp);
        let dst = self.symbol_table.declare_var_anon(out_typ);
        self.instrs
            .push(Instruction::Unary(Unary { op, src, dst: Rc::clone(&dst) }));
        ReadableValue::Variable(dst)
    }

    /* C Binary */

    fn gen_exp_binary(
        &mut self,
        c_binary: c::Binary<TypeCheckedCAst>,
        out_typ: VarType,
    ) -> ReadableValue {
        match helpers::convert_op_binary(&c_binary.op) {
            BinaryOperatorType::EvaluateBothHands(t_op) => {
                self.gen_exp_binary_evalboth(t_op, c_binary, out_typ)
            }
            BinaryOperatorType::ShortCircuit(t_op) => {
                self.gen_exp_binary_shortcirc(t_op, c_binary, out_typ)
            }
        }
    }
    fn gen_exp_binary_evalboth(
        &mut self,
        op: BinaryOperator,
        c::Binary { op: _, lhs, rhs }: c::Binary<TypeCheckedCAst>,
        out_typ: VarType,
    ) -> ReadableValue {
        let src1 = self.gen_exp(*lhs);
        let src2 = self.gen_exp(*rhs);
        let dst = self.symbol_table.declare_var_anon(out_typ);
        self.instrs.push(Instruction::Binary(Binary {
            op,
            src1,
            src2,
            dst: Rc::clone(&dst),
        }));
        ReadableValue::Variable(dst)
    }
    fn gen_exp_binary_shortcirc(
        &mut self,
        op_type: ShortCircuitBOT,
        c::Binary { op: _, lhs, rhs }: c::Binary<TypeCheckedCAst>,
        out_typ: VarType,
    ) -> ReadableValue {
        let result = self.symbol_table.declare_var_anon(out_typ);

        let name = result.id_int().unwrap();
        let label_shortcirc = Rc::new(LabelIdentifier::new(format!(
            "{op_type}.{name:x}.shortcircuit",
        )));
        let label_end = Rc::new(LabelIdentifier::new(format!("{op_type}.{name:x}.end",)));

        let new_shortcirc_jump_instr = |condition: ReadableValue| {
            let tgt = Rc::clone(&label_shortcirc);
            let jumpif = JumpIf { condition, tgt };
            match op_type {
                ShortCircuitBOT::And => Instruction::JumpIfZero(jumpif),
                ShortCircuitBOT::Or => Instruction::JumpIfNotZero(jumpif),
            }
        };

        let new_out_const = |i: i32| Const::Int(i).cast_to(out_typ);
        let (shortcirc_val, fully_evald_val) = match op_type {
            ShortCircuitBOT::And => (new_out_const(0), new_out_const(1)),
            ShortCircuitBOT::Or => (new_out_const(1), new_out_const(0)),
        };

        /* Begin instructions */

        let lhs_val = self.gen_exp(*lhs);

        self.instrs.push(new_shortcirc_jump_instr(lhs_val));

        let rhs_val = self.gen_exp(*rhs);

        self.instrs.push(new_shortcirc_jump_instr(rhs_val));

        self.instrs.push(Instruction::Copy(SrcDst {
            src: ReadableValue::Constant(fully_evald_val),
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_shortcirc));

        self.instrs.push(Instruction::Copy(SrcDst {
            src: ReadableValue::Constant(shortcirc_val),
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Label(label_end));

        ReadableValue::Variable(result)
    }

    /* C Assignment */

    fn gen_exp_assignment(
        &mut self,
        ident: Rc<ResolvedIdentifier>,
        rhs: c::TypedExpression<TypeCheckedCAst>,
    ) -> ReadableValue {
        let rhs = self.gen_exp(rhs);

        self.instrs.push(Instruction::Copy(SrcDst {
            src: rhs,
            dst: Rc::clone(&ident),
        }));

        ReadableValue::Variable(ident)
    }

    /* Conditional */

    fn gen_stmt_conditional(&mut self, c::If { condition, then, elze }: c::If<TypeCheckedCAst>) {
        match elze {
            None => {
                let label_end = Rc::new(LabelIdentifier::new("stmt_cond_end".to_string()));

                /* Begin instructions */

                let condition = self.gen_exp(condition);

                self.instrs.push(Instruction::JumpIfZero(JumpIf {
                    condition,
                    tgt: Rc::clone(&label_end),
                }));

                self.gen_stmt(*then);

                self.instrs.push(Instruction::Label(label_end));
            }
            Some(elze) => {
                let name = &*then as *const c::Statement<TypeCheckedCAst> as usize;
                let label_else = Rc::new(LabelIdentifier::new(format!("stmt_cond.{name:x}.else")));
                let label_end = Rc::new(LabelIdentifier::new(format!("stmt_cond.{name:x}.end")));

                /* Begin instructions */

                let condition = self.gen_exp(condition);

                self.instrs.push(Instruction::JumpIfZero(JumpIf {
                    condition,
                    tgt: Rc::clone(&label_else),
                }));

                self.gen_stmt(*then);

                self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

                self.instrs.push(Instruction::Label(label_else));

                self.gen_stmt(*elze);

                self.instrs.push(Instruction::Label(label_end));
            }
        }
    }
    fn gen_exp_conditional(
        &mut self,
        c::Conditional { condition, then, elze }: c::Conditional<TypeCheckedCAst>,
        out_typ: VarType,
    ) -> ReadableValue {
        let result = self.symbol_table.declare_var_anon(out_typ);

        let name = result.id_int().unwrap();
        let label_else = Rc::new(LabelIdentifier::new(format!("exp_cond.{name:x}.else")));
        let label_end = Rc::new(LabelIdentifier::new(format!("exp_cond.{name:x}.end",)));

        /* Begin instructions */

        let condition = self.gen_exp(*condition);

        self.instrs.push(Instruction::JumpIfZero(JumpIf {
            condition,
            tgt: Rc::clone(&label_else),
        }));

        let then = self.gen_exp(*then);

        self.instrs.push(Instruction::Copy(SrcDst {
            src: then,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_else));

        let elze = self.gen_exp(*elze);

        self.instrs.push(Instruction::Copy(SrcDst {
            src: elze,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Label(label_end));

        ReadableValue::Variable(result)
    }

    /* C Loop */

    fn gen_stmt_break(&mut self, loop_id: Rc<c::LoopId>) {
        let lbls = self.loop_id_to_labels.get_or_insert(loop_id);
        let lbl_break = Rc::clone(&lbls.lbl_break);
        self.instrs.push(Instruction::Jump(lbl_break))
    }
    fn gen_stmt_continue(&mut self, loop_id: Rc<c::LoopId>) {
        let lbls = self.loop_id_to_labels.get_or_insert(loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        self.instrs.push(Instruction::Jump(lbl_cont))
    }
    fn gen_stmt_while(
        &mut self,
        loop_id: Rc<c::LoopId>,
        c::CondBody { condition, body }: c::CondBody<TypeCheckedCAst>,
    ) {
        let lbls = self.loop_id_to_labels.get_or_insert(loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        let lbl_break = Rc::clone(&lbls.lbl_break);

        /* Begin instructions */

        self.instrs.push(Instruction::Label(Rc::clone(&lbl_cont)));

        let condition = self.gen_exp(condition);

        self.instrs.push(Instruction::JumpIfZero(JumpIf {
            condition,
            tgt: Rc::clone(&lbl_break),
        }));

        self.gen_stmt(*body);

        self.instrs.push(Instruction::Jump(lbl_cont));

        self.instrs.push(Instruction::Label(lbl_break));
    }
    fn gen_stmt_dowhile(
        &mut self,
        loop_id: Rc<c::LoopId>,
        c::CondBody { body, condition }: c::CondBody<TypeCheckedCAst>,
    ) {
        let lbl_start = Rc::new(LoopIdToLabels::get_lbl_start(&loop_id));
        let lbls = self.loop_id_to_labels.get_or_insert(loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        let lbl_break = Rc::clone(&lbls.lbl_break);

        /* Begin instructions */

        self.instrs.push(Instruction::Label(Rc::clone(&lbl_start)));

        self.gen_stmt(*body);

        self.instrs.push(Instruction::Label(lbl_cont));

        let condition = self.gen_exp(condition);

        self.instrs.push(Instruction::JumpIfNotZero(JumpIf {
            condition,
            tgt: lbl_start,
        }));

        self.instrs.push(Instruction::Label(lbl_break));
    }
    fn gen_stmt_for(
        &mut self,
        loop_id: Rc<c::LoopId>,
        c::For { init, condition, post, body }: c::For<TypeCheckedCAst>,
    ) {
        let lbl_start = Rc::new(LoopIdToLabels::get_lbl_start(&loop_id));
        let lbls = self.loop_id_to_labels.get_or_insert(loop_id);
        let lbl_cont = Rc::clone(&lbls.lbl_cont);
        let lbl_break = Rc::clone(&lbls.lbl_break);

        /* Begin instructions */

        match init {
            c::ForInit::Decl(c_var_decl) => self.gen_decl_var_block_scope(c_var_decl),
            c::ForInit::Exp(c_exp) => {
                self.gen_exp(c_exp);
            }
            c::ForInit::None => {}
        }

        self.instrs.push(Instruction::Label(Rc::clone(&lbl_start)));

        if let Some(c_cond) = condition {
            let condition = self.gen_exp(c_cond);

            self.instrs.push(Instruction::JumpIfZero(JumpIf {
                condition,
                tgt: Rc::clone(&lbl_break),
            }));
        }

        self.gen_stmt(*body);

        self.instrs.push(Instruction::Label(lbl_cont));

        if let Some(c_post) = post {
            self.gen_exp(c_post);
        }

        self.instrs.push(Instruction::Jump(lbl_start));

        self.instrs.push(Instruction::Label(lbl_break));
    }

    /* Function call */

    fn gen_exp_fun_call(
        &mut self,
        c::FunctionCall { ident, args }: c::FunctionCall<TypeCheckedCAst>,
        out_typ: VarType,
    ) -> ReadableValue {
        let result = self.symbol_table.declare_var_anon(out_typ);

        /* Begin instructions */

        let args = args
            .into_iter()
            .map(|arg| self.gen_exp(arg))
            .collect::<Vec<_>>();

        self.instrs.push(Instruction::FunCall(FunCall {
            ident,
            args,
            dst: Rc::clone(&result),
        }));

        ReadableValue::Variable(result)
    }
}
