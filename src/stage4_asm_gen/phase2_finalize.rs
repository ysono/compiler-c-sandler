use crate::{
    stage4_asm_gen::asm_ast::*,
    symbol_table::ResolvedIdentifier,
    symbol_table_backend::{
        Alignment, AsmEntry, AssemblyType, BackendSymbolTable, OperandByteLen, StorageDuration,
    },
};
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

pub struct InstrsFinalizer {
    backend_symbol_table: Rc<BackendSymbolTable>,

    last_used_stack_pos: StackPosition,
    var_to_stack_pos: HashMap<Rc<ResolvedIdentifier>, StackPosition>,
}
impl InstrsFinalizer {
    pub fn new(backend_symbol_table: Rc<BackendSymbolTable>) -> Self {
        Self {
            backend_symbol_table,
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

        let dummy_alloc_stack_instr = Instruction::Binary {
            op: BinaryOperator::Sub,
            asm_type: AssemblyType::Quadword,
            tgt: Register::SP.into(),
            arg: Operand::ImmediateValue(0),
        };
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
        } else if let Instruction::Binary { arg, .. } = &mut out_instrs[0] {
            *arg = Operand::ImmediateValue(stack_frame_bytelen);
        }

        out_instrs
    }

    fn convert_operands<'a>(
        &'a mut self,
        in_instrs: impl 'a + Iterator<Item = Instruction<PreFinalOperand>>,
    ) -> impl 'a + Iterator<Item = Instruction<Operand>> {
        in_instrs.map(move |in_instr| match in_instr {
            Instruction::Mov { asm_type, src, dst } => {
                let src = self.convert_operand(src);
                let dst = self.convert_operand(dst);
                Instruction::Mov { asm_type, src, dst }
            }
            Instruction::Movsx { src, dst } => {
                let src = self.convert_operand(src);
                let dst = self.convert_operand(dst);
                Instruction::Movsx { src, dst }
            }
            Instruction::Unary(op, asm_type, operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Unary(op, asm_type, operand)
            }
            Instruction::Binary {
                op,
                asm_type,
                arg,
                tgt,
            } => {
                let arg = self.convert_operand(arg);
                let tgt = self.convert_operand(tgt);
                Instruction::Binary {
                    op,
                    asm_type,
                    arg,
                    tgt,
                }
            }
            Instruction::Cmp { asm_type, arg, tgt } => {
                let arg = self.convert_operand(arg);
                let tgt = self.convert_operand(tgt);
                Instruction::Cmp { asm_type, arg, tgt }
            }
            Instruction::Idiv(asm_type, operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Idiv(asm_type, operand)
            }
            Instruction::Cdq(asm_type) => Instruction::Cdq(asm_type),
            Instruction::Jmp(l) => Instruction::Jmp(l),
            Instruction::JmpCC(c, l) => Instruction::JmpCC(c, l),
            Instruction::SetCC(c, o) => {
                let operand = self.convert_operand(o);
                Instruction::SetCC(c, operand)
            }
            Instruction::Label(l) => Instruction::Label(l),
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
            PFO::Pseudo(ident) => match self.backend_symbol_table.get(&ident) {
                None => panic!("All pseudos are expected to have been added to the symbol table."),
                Some(AsmEntry::Obj {
                    asm_type,
                    storage_duration,
                }) => match storage_duration {
                    StorageDuration::Automatic => self.var_to_stack_pos(ident, *asm_type).into(),
                    StorageDuration::Static => Operand::Data(ident),
                },
                Some(AsmEntry::Fun { .. }) => {
                    panic!("All pseudos are expected to correspond to objs.")
                }
            },
        }
    }
    fn var_to_stack_pos(
        &mut self,
        ident: Rc<ResolvedIdentifier>,
        asm_type: AssemblyType,
    ) -> StackPosition {
        let pos = self.var_to_stack_pos.entry(ident).or_insert_with(|| {
            let alloc = OperandByteLen::from(asm_type) as i64;
            self.last_used_stack_pos.0 -= alloc;

            let alignment = Alignment::from(asm_type) as i64;
            let rem = self.last_used_stack_pos.0 % alignment;
            if rem != 0 {
                self.last_used_stack_pos.0 -= alignment + rem;
            }

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
            Instruction::Mov { asm_type, mut src, dst } => {
                if let (AssemblyType::Longword, Operand::ImmediateValue(i)) = (asm_type, &src) {
                    src = Operand::ImmediateValue(*i as u32 as i64); // Zero-out most significant bytes.
                }

                let src_to_reg1 =
                    (src.is_on_mem() && dst.is_on_mem())
                    || (matches!(asm_type, AssemblyType::Quadword)
                        && matches!(&src, Operand::ImmediateValue(i) if i32::try_from(*i).is_err())
                        && dst.is_on_mem());

                let new_instr = |src: Operand, dst: Operand| Instruction::Mov { asm_type, src, dst };

                Self::maybe_use_2_regs(
                    (asm_type, src),
                    (asm_type, dst),
                    src_to_reg1,
                    (false, false),
                    new_instr,
                )
            }
            Instruction::Movsx { src, dst } => {
                let src_to_reg1 = matches!(&src, Operand::ImmediateValue(_));
                let dst_to_reg2 = false;
                let reg2_to_dst = dst.is_on_mem();
                let new_instr = |src: Operand, dst: Operand| Instruction::Movsx { src, dst };
                Self::maybe_use_2_regs(
                    (AssemblyType::Longword, src),
                    (AssemblyType::Quadword, dst),
                    src_to_reg1,
                    (dst_to_reg2, reg2_to_dst),
                    new_instr,
                )
            }
            Instruction::Binary { op, asm_type, arg, tgt } => {
                let src_to_reg1 =
                    (matches!(&op, BinaryOperator::Add | BinaryOperator::Sub)
                        && arg.is_on_mem()
                        && tgt.is_on_mem())
                    || (matches!(&asm_type, AssemblyType::Quadword)
                        && matches!(&arg, Operand::ImmediateValue(i) if i32::try_from(*i).is_err()));
                let (dst_to_reg2, reg2_to_dst) =
                    if matches!(&op, BinaryOperator::Mul) && tgt.is_on_mem() {
                        (true, true)
                    } else {
                        (false, false)
                    };
                let new_instr = |arg: Operand, tgt: Operand| Instruction::Binary { op, asm_type, arg, tgt };
                Self::maybe_use_2_regs(
                    (asm_type, arg),
                    (asm_type,tgt),
                    src_to_reg1,
                    (dst_to_reg2, reg2_to_dst),
                    new_instr,
                )
            }
            Instruction::Cmp { asm_type, arg, tgt } => {
                let src_to_reg1 =
                    (arg.is_on_mem() && tgt.is_on_mem())
                    || (matches!(&arg, Operand::ImmediateValue(i) if i32::try_from(*i).is_err()));
                let dst_to_reg2 = matches!(&tgt, Operand::ImmediateValue(_));
                let reg2_to_dst = false;
                let new_instr = |arg: Operand, tgt: Operand| Instruction::Cmp { asm_type, arg, tgt };
                Self::maybe_use_2_regs(
                    (asm_type, arg),
                    (asm_type,tgt),
                    src_to_reg1,
                    (dst_to_reg2, reg2_to_dst),
                    new_instr,
                )
            }
            Instruction::Idiv(asm_type, imm @ Operand::ImmediateValue(_)) => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Idiv(asm_type, reg.into());
                Self::to_reg(asm_type, imm, reg, instr_at_reg)
            }
            Instruction::Push(operand @ Operand::ImmediateValue(i)) if i32::try_from(i).is_err() => {
                let reg = Register::R10;
                let instr_at_reg = Instruction::Push(reg.into());
                Self::to_reg(AssemblyType::Quadword, operand, reg, instr_at_reg)
            }
            _ => vec![in_instr],
        })
    }
    fn to_reg(
        asm_type: AssemblyType,
        operand_to_reg: Operand,
        reg: Register,
        instr_at_reg: Instruction<Operand>,
    ) -> Vec<Instruction<Operand>> {
        let instr_to_reg = Instruction::Mov {
            asm_type,
            src: operand_to_reg,
            dst: reg.into(),
        };
        vec![instr_to_reg, instr_at_reg]
    }
    fn maybe_use_2_regs(
        (src_asm_type, mut src): (AssemblyType, Operand),
        (dst_asm_type, mut dst): (AssemblyType, Operand),
        src_to_reg1: bool,
        (dst_to_reg2, reg2_to_dst): (bool, bool),
        new_instr: impl FnOnce(Operand, Operand) -> Instruction<Operand>,
    ) -> Vec<Instruction<Operand>> {
        let reg1 = Register::R10;
        let reg2 = Register::R11;

        let new_mov = |asm_type: AssemblyType, src: Operand, dst: Operand| Instruction::Mov {
            asm_type,
            src,
            dst,
        };

        let mut instr_to_reg1 = None;
        if src_to_reg1 {
            instr_to_reg1 = Some(new_mov(src_asm_type, src, reg1.into()));
            src = reg1.into();
        }

        let mut instr_to_reg2 = None;
        let mut instr_from_reg2 = None;
        match (dst_to_reg2, reg2_to_dst) {
            (false, false) => { /* No-op. */ }
            (false, true) => {
                instr_from_reg2 = Some(new_mov(dst_asm_type, reg2.into(), dst));
                dst = reg2.into();
            }
            (true, false) => {
                instr_to_reg2 = Some(new_mov(dst_asm_type, dst, reg2.into()));
                dst = reg2.into();
            }
            (true, true) => {
                instr_to_reg2 = Some(new_mov(dst_asm_type, dst.clone(), reg2.into()));
                instr_from_reg2 = Some(new_mov(dst_asm_type, reg2.into(), dst));
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
