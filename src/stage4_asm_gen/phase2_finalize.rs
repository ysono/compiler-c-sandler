use crate::stage4_asm_gen::asm_ast::*;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

pub struct InstrsFinalizer {
    last_used_stack_pos: StackPosition,
    var_to_stack_pos: HashMap<Rc<ResolvedIdentifier>, StackPosition>,
}
impl Default for InstrsFinalizer {
    fn default() -> Self {
        Self {
            last_used_stack_pos: StackPosition(0),
            var_to_stack_pos: HashMap::new(),
        }
    }
}
impl InstrsFinalizer {
    pub fn finalize_instrs(
        &mut self,
        in_instrs: impl Iterator<Item = Instruction<PreFinalOperand>>,
    ) -> Vec<Instruction<Operand>> {
        let instrs = self.convert_operands(in_instrs);
        let instrs = OperandFixer::fix_invalid_operands(instrs);

        let dummy_alloc_stack_instr = Instruction::AllocateStack(StackPosition(0));
        let mut out_instrs = vec![dummy_alloc_stack_instr];

        out_instrs.extend(instrs);

        /*
        RBP == RSP@1 -> | The previous stack frame's RBP
                        | \
                        |  + Local variables; and maybe padding
                RSP@2-> | /
                        | \
                        |  + For one function call at a time: maybe padding; and args
                        | /

        When the next instruction is executed,
            + RBP equals RSP. This is ensured by the function prologue (emitted in the subsequent asm emission stage).
            + RBP and RSP are are 16-byte aligned. This is ensured by the cooperation of:
                + how we allocate each stack frame, by the finalizer here and by the function-call instructions
                + the fact that the `call` instruction pushes one 8-byte item (the RIP at which to resume)

        Here in the finalizer, we pre-allocate the stack, for local variables only.
        The current function's stack frame may be longer than the amount we allocate here. That extra portion is managed atomically per function call.

        We must allocate s.t. RSP becomes 16-byte aligned (at "RSP@2"). For the reason, see documentation for function calls.

        The compiler can't know the total byte length of all local variables until the iterator of `Instruction`s has been completely traversed.
        */
        let mut stack_frame_bytelen = self.last_used_stack_pos.0 * -1;
        let rem = stack_frame_bytelen % 16;
        if rem != 0 {
            stack_frame_bytelen += 16 - rem;
        }
        let alloc_stack_instr = Instruction::AllocateStack(StackPosition(stack_frame_bytelen));
        out_instrs[0] = alloc_stack_instr;

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
            PFO::PseudoRegister(ident) => self.var_to_stack_pos(ident).into(),
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
            Instruction::Mov {
                src: src @ Operand::StackPosition(_),
                dst: dst @ Operand::StackPosition(_),
            } => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Mov {
                    src: reg.into(),
                    dst,
                };
                Self::to_reg(src, reg, instr_at_reg)
            }
            Instruction::Binary {
                op: op @ (BinaryOperator::Add | BinaryOperator::Sub),
                arg: arg @ Operand::StackPosition(_),
                tgt: tgt @ Operand::StackPosition(_),
            } => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Binary {
                    op,
                    arg: reg.into(),
                    tgt,
                };
                Self::to_reg(arg, reg, instr_at_reg)
            }
            Instruction::Binary {
                op: op @ BinaryOperator::Mul,
                arg,
                tgt: tgt @ Operand::StackPosition(_),
            } => {
                let reg = Register::R11;
                let instr_at_reg = Instruction::Binary {
                    op,
                    arg,
                    tgt: reg.into(),
                };
                Self::to_and_from_reg(tgt, reg, instr_at_reg)
            }
            Instruction::Cmp {
                arg: arg @ Operand::StackPosition(_),
                tgt: tgt @ Operand::StackPosition(_),
            } => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Cmp {
                    arg,
                    tgt: reg.into(),
                };
                Self::to_reg(tgt, reg, instr_at_reg)
            }
            Instruction::Cmp {
                arg,
                tgt: tgt @ Operand::ImmediateValue(_),
            } => {
                let reg = Register::R11;
                let instr_at_reg = Instruction::Cmp {
                    arg,
                    tgt: reg.into(),
                };
                Self::to_reg(tgt, reg, instr_at_reg)
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
    fn to_and_from_reg(
        operand_to_and_from_reg: Operand,
        reg: Register,
        instr_at_reg: Instruction<Operand>,
    ) -> Vec<Instruction<Operand>> {
        let instr_to_reg = Instruction::Mov {
            src: operand_to_and_from_reg.clone(),
            dst: reg.into(),
        };
        let instr_from_reg = Instruction::Mov {
            src: reg.into(),
            dst: operand_to_and_from_reg,
        };
        vec![instr_to_reg, instr_at_reg, instr_from_reg]
    }
}
