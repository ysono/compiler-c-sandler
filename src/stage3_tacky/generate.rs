use crate::{
    stage2_parse::c_ast_resolved as c,
    stage3_tacky::tacky_ast::*,
    symbol_table::{
        FunAttrs, ResolvedIdentifier, StaticInitialValue, Symbol, SymbolTable, VarAttrs,
    },
};
use derive_more::Display;
use std::collections::HashMap;
use std::rc::Rc;

enum BinaryOperatorType {
    EvaluateBothHands(BinaryOperator),
    ShortCircuit(ShortCircuitBOT),
}
#[derive(Display)]
enum ShortCircuitBOT {
    And,
    Or,
}

pub struct Tackifier {}
impl Tackifier {
    pub fn tackify_program(
        c::Program { decls: c_decls }: c::Program,
        symbol_table: &SymbolTable,
    ) -> Program {
        use StaticInitialValue as SIV;

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

        let mut vars = vec![];
        for (ident, symbol) in symbol_table.iter() {
            if let Symbol::Var {
                typ: _, // TODO
                attrs:
                    VarAttrs::StaticStorageDuration {
                        visibility,
                        initial_value,
                    },
            } = symbol
            {
                let konst = match initial_value {
                    SIV::Initial(konst) => *konst,
                    SIV::Tentative => Const::Int(0),
                    SIV::NoInitializer => continue,
                };
                let var = StaticVariable {
                    ident: Rc::clone(ident),
                    visibility: *visibility,
                    init: konst,
                };
                vars.push(var);
            }
        }

        Program { funs, vars }
    }
}

struct FunInstrsGenerator<'a> {
    loop_id_to_labels: LoopIdToLabels,

    symbol_table: &'a SymbolTable,

    instrs: Vec<Instruction>,
}
impl<'a> FunInstrsGenerator<'a> {
    fn new(symbol_table: &'a SymbolTable) -> Self {
        Self {
            loop_id_to_labels: Default::default(),
            symbol_table,
            instrs: Default::default(),
        }
    }

    /* Declaration, Definition */

    fn tackify_fun_defn(
        mut self,
        c::FunctionDefinition {
            decl: c::FunctionDeclaration { ident, params, .. },
            body,
        }: c::FunctionDefinition,
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

        let ret_kon = c::Const::Int(0);
        let ret_exp = c::Expression::Const(ret_kon);
        let ret_stmt = c::Statement::Return(ret_exp);
        self.gen_stmt(ret_stmt);

        Function {
            ident,
            visibility,
            params,
            instrs: self.instrs,
        }
    }
    fn gen_decl_block_scope(&mut self, c_decl: c::BlockScopeDeclaration) {
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
            storage_class,
        }: c::VariableDeclaration,
    ) {
        /* Iff this var has storage_duration=automatic and initializer=some, then initialize.
        As a performance improvement, we infer storage duration from storage class tokens, rather than from the symbol table. */
        match (storage_class, init) {
            (None, Some(init_exp)) => {
                self.gen_exp_assignment(ident, init_exp);
            }
            _ => { /* No-op. */ }
        }
    }

    /* Block, Statement, Expression */

    fn gen_block(&mut self, c_block: c::Block) {
        for c_item in c_block.items {
            match c_item {
                c::BlockItem::Declaration(c_decl) => self.gen_decl_block_scope(c_decl),
                c::BlockItem::Statement(c_stmt) => self.gen_stmt(c_stmt),
            }
        }
    }
    fn gen_stmt(&mut self, c_stmt: c::Statement) {
        match c_stmt {
            c::Statement::Return(c_root_exp) => {
                let t_root_val = self.gen_exp(c_root_exp);
                self.instrs.push(Instruction::Return(t_root_val));
            }
            c::Statement::Expression(c_root_exp) => {
                self.gen_exp(c_root_exp);
            }
            c::Statement::If(c_if) => self.gen_stmt_conditional(c_if),
            c::Statement::Compound(c_block) => self.gen_block(c_block),
            c::Statement::Break(loop_id) => self.gen_stmt_break(loop_id),
            c::Statement::Continue(loop_id) => self.gen_stmt_continue(loop_id),
            c::Statement::While(loop_id, condbody) => self.gen_stmt_while(loop_id, condbody),
            c::Statement::DoWhile(loop_id, condbody) => self.gen_stmt_dowhile(loop_id, condbody),
            c::Statement::For(loop_id, foor) => self.gen_stmt_for(loop_id, foor),
            c::Statement::Null => { /* No-op. */ }
        }
    }
    fn gen_exp(&mut self, c_exp: c::Expression) -> ReadableValue {
        match c_exp {
            c::Expression::Const(c::Const::Int(i)) => ReadableValue::Constant(i),
            c::Expression::Const(c::Const::Long(_)) => todo!(),
            c::Expression::Var(ident) => ReadableValue::Variable(ident),
            c::Expression::Unary(unary) => self.gen_exp_unary(unary),
            c::Expression::Binary(binary) => self.gen_exp_binary(binary),
            c::Expression::Assignment(c::Assignment { ident, rhs }) => {
                self.gen_exp_assignment(ident, *rhs)
            }
            c::Expression::Conditional(c_cond) => self.gen_exp_conditional(c_cond),
            c::Expression::FunctionCall(c_fun_call) => self.gen_exp_fun_call(c_fun_call),
        }
    }

    /* C Unary */

    fn gen_exp_unary(&mut self, c::Unary { op, sub_exp }: c::Unary) -> ReadableValue {
        let op = Self::convert_op_unary(op);
        let src = self.gen_exp(*sub_exp);
        let dst = Rc::new(ResolvedIdentifier::new_no_linkage(None));
        self.instrs.push(Instruction::Unary(Unary {
            op,
            src,
            dst: Rc::clone(&dst),
        }));
        ReadableValue::Variable(dst)
    }

    /* C Binary */

    fn gen_exp_binary(&mut self, c_binary: c::Binary) -> ReadableValue {
        match Self::convert_op_binary(&c_binary.op) {
            BinaryOperatorType::EvaluateBothHands(t_op) => {
                self.gen_exp_binary_evalboth(t_op, c_binary)
            }
            BinaryOperatorType::ShortCircuit(t_op) => self.gen_exp_binary_shortcirc(t_op, c_binary),
        }
    }
    fn gen_exp_binary_evalboth(
        &mut self,
        op: BinaryOperator,
        c::Binary { op: _, lhs, rhs }: c::Binary,
    ) -> ReadableValue {
        let src1 = self.gen_exp(*lhs);
        let src2 = self.gen_exp(*rhs);
        let dst = Rc::new(ResolvedIdentifier::new_no_linkage(None));
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
        c::Binary { op: _, lhs, rhs }: c::Binary,
    ) -> ReadableValue {
        let result = Rc::new(ResolvedIdentifier::new_no_linkage(None));

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

        let (shortcirc_val, fully_evald_val) = match op_type {
            ShortCircuitBOT::And => (0, 1),
            ShortCircuitBOT::Or => (1, 0),
        };

        /* Begin instructions */

        let lhs_val = self.gen_exp(*lhs);

        self.instrs.push(new_shortcirc_jump_instr(lhs_val));

        let rhs_val = self.gen_exp(*rhs);

        self.instrs.push(new_shortcirc_jump_instr(rhs_val));

        self.instrs.push(Instruction::Copy(Copy {
            src: ReadableValue::Constant(fully_evald_val),
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_shortcirc));

        self.instrs.push(Instruction::Copy(Copy {
            src: ReadableValue::Constant(shortcirc_val),
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Label(label_end));

        ReadableValue::Variable(result)
    }

    /* C Operator */

    fn convert_op_unary(c_unary_op: c::UnaryOperator) -> UnaryOperator {
        use c::UnaryOperator as CUO;
        match c_unary_op {
            CUO::Complement => UnaryOperator::Complement,
            CUO::Negate => UnaryOperator::Negate,
            CUO::Not => UnaryOperator::Not,
        }
    }
    fn convert_op_binary(c_binary_op: &c::BinaryOperator) -> BinaryOperatorType {
        use c::BinaryOperator as CBO;
        use BinaryOperator as TBO;
        use BinaryOperatorType as BOT;
        use ShortCircuitBOT as SBOT;
        match c_binary_op {
            CBO::And => BOT::ShortCircuit(SBOT::And),
            CBO::Or => BOT::ShortCircuit(SBOT::Or),
            CBO::Sub => BOT::EvaluateBothHands(TBO::Sub),
            CBO::Add => BOT::EvaluateBothHands(TBO::Add),
            CBO::Mul => BOT::EvaluateBothHands(TBO::Mul),
            CBO::Div => BOT::EvaluateBothHands(TBO::Div),
            CBO::Rem => BOT::EvaluateBothHands(TBO::Rem),
            CBO::Eq => BOT::EvaluateBothHands(TBO::Eq),
            CBO::Neq => BOT::EvaluateBothHands(TBO::Neq),
            CBO::Lt => BOT::EvaluateBothHands(TBO::Lt),
            CBO::Lte => BOT::EvaluateBothHands(TBO::Lte),
            CBO::Gt => BOT::EvaluateBothHands(TBO::Gt),
            CBO::Gte => BOT::EvaluateBothHands(TBO::Gte),
        }
    }

    /* C Assignment */

    fn gen_exp_assignment(
        &mut self,
        ident: Rc<ResolvedIdentifier>,
        rhs: c::Expression,
    ) -> ReadableValue {
        let rhs = self.gen_exp(rhs);

        self.instrs.push(Instruction::Copy(Copy {
            src: rhs,
            dst: Rc::clone(&ident),
        }));

        ReadableValue::Variable(ident)
    }

    /* Conditional */

    fn gen_stmt_conditional(
        &mut self,
        c::If {
            condition,
            then,
            elze,
        }: c::If,
    ) {
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
                let name = &*then as *const c::Statement as usize;
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
        c::Conditional {
            condition,
            then,
            elze,
        }: c::Conditional,
    ) -> ReadableValue {
        let result = Rc::new(ResolvedIdentifier::new_no_linkage(None));

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

        self.instrs.push(Instruction::Copy(Copy {
            src: then,
            dst: Rc::clone(&result),
        }));

        self.instrs.push(Instruction::Jump(Rc::clone(&label_end)));

        self.instrs.push(Instruction::Label(label_else));

        let elze = self.gen_exp(*elze);

        self.instrs.push(Instruction::Copy(Copy {
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
        c::CondBody { condition, body }: c::CondBody,
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
        c::CondBody { body, condition }: c::CondBody,
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
        c::For {
            init,
            condition,
            post,
            body,
        }: c::For,
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
        c::FunctionCall { ident, args }: c::FunctionCall,
    ) -> ReadableValue {
        let result = Rc::new(ResolvedIdentifier::new_no_linkage(None));

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

#[derive(Default)]
struct LoopIdToLabels {
    loop_id_to_labels: HashMap<Rc<c::LoopId>, Labels>,
}
impl LoopIdToLabels {
    fn get_lbl_start(loop_id: &c::LoopId) -> LabelIdentifier {
        let name_start = format!("{}.{}.start", loop_id.descr(), loop_id.id());
        LabelIdentifier::new(name_start)
    }
    fn get_or_insert(&mut self, loop_id: Rc<c::LoopId>) -> &Labels {
        self.loop_id_to_labels
            .entry(loop_id)
            .or_insert_with_key(|loop_id| {
                let name_break = format!("{}.{}.break", loop_id.descr(), loop_id.id());
                let name_cont = format!("{}.{}.cont", loop_id.descr(), loop_id.id());
                let lbl_break = Rc::new(LabelIdentifier::new(name_break));
                let lbl_cont = Rc::new(LabelIdentifier::new(name_cont));
                Labels {
                    lbl_break,
                    lbl_cont,
                }
            })
    }
}

struct Labels {
    lbl_break: Rc<LabelIdentifier>,
    lbl_cont: Rc<LabelIdentifier>,
}
