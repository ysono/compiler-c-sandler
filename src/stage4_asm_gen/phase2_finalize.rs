use crate::{
    stage4_asm_gen::asm_ast::*,
    symbol_table::{Symbol, SymbolTable, VarAttrs},
};
use std::collections::HashMap;
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
    ) -> Vec<Instruction<Operand>> {
        let instrs = self.convert_operands(in_instrs);
        let instrs = OperandFixer::fix_invalid_operands(instrs);

        let dummy_alloc_stack_instr = Instruction::AllocateStack(StackPosition(0));
        let mut out_instrs = vec![dummy_alloc_stack_instr];

        out_instrs.extend(instrs);

        /* We must read `self.last_used_stack_pos` strictly after the iterator of `Instruction`s has been completely traversed. */
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
            PFO::Pseudo(ident) => match self.symbol_table.get(&ident) {
                Some(Symbol::Var {
                    attrs: VarAttrs::AutomaticStorageDuration,
                })
                | None => self.var_to_stack_pos(ident).into(),
                Some(Symbol::Var {
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
            Instruction::Mov {
                src: src @ (Operand::StackPosition(_) | Operand::Data(_)),
                dst: dst @ (Operand::StackPosition(_) | Operand::Data(_)),
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
                arg: arg @ (Operand::StackPosition(_) | Operand::Data(_)),
                tgt: tgt @ (Operand::StackPosition(_) | Operand::Data(_)),
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
                tgt: tgt @ (Operand::StackPosition(_) | Operand::Data(_)),
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
                arg: arg @ (Operand::StackPosition(_) | Operand::Data(_)),
                tgt: tgt @ (Operand::StackPosition(_) | Operand::Data(_)),
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
