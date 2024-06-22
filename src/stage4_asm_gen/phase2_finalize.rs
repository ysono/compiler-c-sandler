use crate::{
    stage4_asm_gen::asm_ast::*,
    symbol_table::{ResolvedIdentifier, Symbol, SymbolTable, VarAttrs},
};
use std::collections::{HashMap, VecDeque};
use std::mem;
use std::rc::Rc;

pub struct InstrsFinalizer {
    symbol_table: Rc<SymbolTable>,

    last_used_stack_pos: StackPosition,
    var_to_stack_pos: HashMap<Rc<ResolvedIdentifier>, StackPosition>,
}
impl InstrsFinalizer {
    pub fn new(symbol_table: Rc<SymbolTable>) -> Self {
        Self {
            symbol_table,
            last_used_stack_pos: StackPosition(0),
            var_to_stack_pos: HashMap::new(),
        }
    }

    /// See documentation at [`crate::stage4_asm_gen`].
    pub fn finalize_instrs(
        &mut self,
        in_instrs: impl Iterator<Item = Instruction<PreFinalOperand>>,
    ) -> VecDeque<Instruction<Operand>> {
        let instrs = self.convert_operands(in_instrs);
        let instrs = OperandFixer::fix_invalid_operands(instrs);

        let dummy_alloc_stack_instr = Instruction::AllocateStack(StackPosition(0));
        let mut out_instrs = VecDeque::from([dummy_alloc_stack_instr]);

        out_instrs.extend(instrs);

        /* We must read `self.last_used_stack_pos` strictly after the iterator of `Instruction`s has been completely traversed. */
        let mut stack_frame_bytelen = self.last_used_stack_pos.0 * -1;
        let rem = stack_frame_bytelen % 16;
        if rem != 0 {
            stack_frame_bytelen += 16 - rem;
        }

        if stack_frame_bytelen == 0 {
            out_instrs.pop_front();
        } else {
            out_instrs[0] = Instruction::AllocateStack(StackPosition(stack_frame_bytelen));
        }

        out_instrs
    }

    fn convert_operands<'a>(
        &'a mut self,
        in_instrs: impl 'a + Iterator<Item = Instruction<PreFinalOperand>>,
    ) -> impl 'a + Iterator<Item = Instruction<Operand>> {
        in_instrs.map(move |in_instr| match in_instr {
            Instruction::Mov { src, dst } => {
                let src = self.convert_operand(src);
                let dst = self.convert_operand(dst);
                Instruction::Mov { src, dst }
            }
            Instruction::Unary(op, operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Unary(op, operand)
            }
            Instruction::Binary { op, arg, tgt } => {
                let arg = self.convert_operand(arg);
                let tgt = self.convert_operand(tgt);
                Instruction::Binary { op, arg, tgt }
            }
            Instruction::Cmp { arg, tgt } => {
                let arg = self.convert_operand(arg);
                let tgt = self.convert_operand(tgt);
                Instruction::Cmp { arg, tgt }
            }
            Instruction::Idiv(operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Idiv(operand)
            }
            Instruction::Cdq => Instruction::Cdq,
            Instruction::Jmp(l) => Instruction::Jmp(l),
            Instruction::JmpCC(c, l) => Instruction::JmpCC(c, l),
            Instruction::SetCC(c, o) => {
                let operand = self.convert_operand(o);
                Instruction::SetCC(c, operand)
            }
            Instruction::Label(l) => Instruction::Label(l),
            Instruction::AllocateStack(s) => Instruction::AllocateStack(s),
            Instruction::DeallocateStack(s) => Instruction::DeallocateStack(s),
            Instruction::Push(operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Push(operand)
            }
            Instruction::Call(ident) => Instruction::Call(ident),
            Instruction::Ret => Instruction::Ret,
        })
    }
    fn convert_operand(&mut self, pfo: PreFinalOperand) -> Operand {
        use PreFinalOperand as PFO;
        match pfo {
            PFO::ImmediateValue(i) => Operand::ImmediateValue(i),
            PFO::Register(r) => Operand::Register(r),
            PFO::StackPosition(s) => Operand::StackPosition(s),
            PFO::Pseudo(ident) => match self.symbol_table.get(&ident) {
                Some(Symbol::Var {
                    typ: _, // TODO
                    attrs: VarAttrs::AutomaticStorageDuration,
                })
                | None => self.var_to_stack_pos(ident).into(),
                Some(Symbol::Var {
                    typ: _, // TODO
                    attrs: VarAttrs::StaticStorageDuration { .. },
                }) => Operand::Data(ident),
                Some(Symbol::Fun { .. }) => panic!("Cannot use fun as var."),
            },
        }
    }
    fn var_to_stack_pos(&mut self, ident: Rc<ResolvedIdentifier>) -> StackPosition {
        /* For now, all Tacky Values represent 32-bit values. */
        const VAL_BYTELEN: isize = mem::size_of::<i32>() as isize;
        let pos = self.var_to_stack_pos.entry(ident).or_insert_with(|| {
            self.last_used_stack_pos.0 -= VAL_BYTELEN;
            self.last_used_stack_pos
        });
        *pos
    }
}

struct OperandFixer {}
impl OperandFixer {
    fn fix_invalid_operands<'a>(
        in_instrs: impl 'a + Iterator<Item = Instruction<Operand>>,
    ) -> impl 'a + Iterator<Item = Instruction<Operand>> {
        in_instrs.flat_map(|in_instr| match in_instr {
            Instruction::Mov { src, dst } => {
                let src_to_reg1 = src.is_on_mem() && dst.is_on_mem();
                let new_instr = |src: Operand, dst: Operand| Instruction::Mov { src, dst };
                Self::maybe_use_2_regs(src, dst, src_to_reg1, (false, false), new_instr)
            }
            Instruction::Binary { op, arg, tgt } => {
                let src_to_reg1 = matches!(&op, BinaryOperator::Add | BinaryOperator::Sub)
                    && arg.is_on_mem()
                    && tgt.is_on_mem();
                let (dst_to_reg2, reg2_to_dst) =
                    if matches!(&op, BinaryOperator::Mul) && tgt.is_on_mem() {
                        (true, true)
                    } else {
                        (false, false)
                    };
                let new_instr = |arg: Operand, tgt: Operand| Instruction::Binary { op, arg, tgt };
                Self::maybe_use_2_regs(arg, tgt, src_to_reg1, (dst_to_reg2, reg2_to_dst), new_instr)
            }
            Instruction::Cmp { arg, tgt } => {
                let src_to_reg1 = arg.is_on_mem() && tgt.is_on_mem();
                let dst_to_reg2 = matches!(&tgt, Operand::ImmediateValue(_));
                let reg2_to_dst = false;
                let new_instr = |arg: Operand, tgt: Operand| Instruction::Cmp { arg, tgt };
                Self::maybe_use_2_regs(arg, tgt, src_to_reg1, (dst_to_reg2, reg2_to_dst), new_instr)
            }
            Instruction::Idiv(imm @ Operand::ImmediateValue(_)) => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Idiv(reg.into());
                Self::to_reg(imm, reg, instr_at_reg)
            }
            _ => vec![in_instr],
        })
    }
    fn to_reg(
        operand_to_reg: Operand,
        reg: Register,
        instr_at_reg: Instruction<Operand>,
    ) -> Vec<Instruction<Operand>> {
        let instr_to_reg = Instruction::Mov {
            src: operand_to_reg,
            dst: reg.into(),
        };
        vec![instr_to_reg, instr_at_reg]
    }
    fn maybe_use_2_regs(
        mut src: Operand,
        mut dst: Operand,
        src_to_reg1: bool,
        (dst_to_reg2, reg2_to_dst): (bool, bool),
        new_instr: impl FnOnce(Operand, Operand) -> Instruction<Operand>,
    ) -> Vec<Instruction<Operand>> {
        let reg1 = Register::R10;
        let reg2 = Register::R11;

        let new_mov = |src: Operand, dst: Operand| Instruction::Mov { src, dst };

        let mut instr_to_reg1 = None;
        if src_to_reg1 {
            instr_to_reg1 = Some(new_mov(src, reg1.into()));
            src = reg1.into();
        }

        let mut instr_to_reg2 = None;
        let mut instr_from_reg2 = None;
        match (dst_to_reg2, reg2_to_dst) {
            (false, false) => { /* No-op. */ }
            (false, true) => {
                instr_from_reg2 = Some(new_mov(reg2.into(), dst));
                dst = reg2.into();
            }
            (true, false) => {
                instr_to_reg2 = Some(new_mov(dst, reg2.into()));
                dst = reg2.into();
            }
            (true, true) => {
                instr_to_reg2 = Some(new_mov(dst.clone(), reg2.into()));
                instr_from_reg2 = Some(new_mov(reg2.into(), dst));
                dst = reg2.into();
            }
        }

        let instr = new_instr(src, dst);

        [instr_to_reg1, instr_to_reg2, Some(instr), instr_from_reg2]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
    }
}
