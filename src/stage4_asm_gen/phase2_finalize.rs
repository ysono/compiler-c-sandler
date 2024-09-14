//! + Translate each remaining abstract operand location into a concrete location.
//! + Derive the stack frame length; and allocate the frame.

pub(crate) mod var_to_stack_pos; // `pub` for rustdoc.

use self::var_to_stack_pos::VarToStackPos;
use crate::{
    common::{
        symbol_table_backend::{AsmObj, BackendSymbolTable, ObjLocation},
        types_backend::AssemblyType,
    },
    stage4_asm_gen::{asm_ast::*, phase1_generate::GeneratedAsmAst, phase3_fix::OperandFixer},
};
use derive_more::Into;
use std::collections::VecDeque;
use std::rc::Rc;

#[derive(Debug)]
pub struct FinalizedAsmAst(());
impl AsmAstVariant for FinalizedAsmAst {
    type Instructions = VecDeque<Instruction<FinalizedAsmAst>>;
    type Operand = Operand;
}

#[derive(Into)]
pub struct InstrsFinalizer {
    backend_symtab: Rc<BackendSymbolTable>,

    var_to_stack_pos: VarToStackPos,
}
impl InstrsFinalizer {
    pub fn new(backend_symtab: Rc<BackendSymbolTable>) -> Self {
        Self {
            backend_symtab,
            var_to_stack_pos: VarToStackPos::default(),
        }
    }

    pub fn finalize_fun(
        mut self,
        Function { ident, visibility, instrs }: Function<GeneratedAsmAst>,
    ) -> Function<FinalizedAsmAst> {
        let instrs = self.finalize_instrs(instrs.into_iter());
        Function { ident, visibility, instrs }
    }

    /// See documentation at [`crate::stage4_asm_gen`].
    fn finalize_instrs(
        &mut self,
        in_instrs: impl Iterator<Item = Instruction<GeneratedAsmAst>>,
    ) -> VecDeque<Instruction<FinalizedAsmAst>> {
        let instrs = self.convert_operands(in_instrs);
        let instrs = OperandFixer::fix_invalid_operands(instrs);
        let mut out_instrs = instrs.collect::<VecDeque<_>>();

        /* We must read `self.last_used_stack_pos` strictly after the iterator of `Instruction`s has been completely traversed. */
        let mut stack_frame_bytelen = self.var_to_stack_pos.last_used_stack_pos().0 * -1;
        let rem = stack_frame_bytelen % 16;
        if rem != 0 {
            stack_frame_bytelen += 16 - rem;
        }

        if stack_frame_bytelen > 0 {
            out_instrs.push_front(Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: AssemblyType::Quadword,
                tgt: Register::SP.into(),
                arg: Operand::ImmediateValue(stack_frame_bytelen as u64),
            });
        }

        out_instrs
    }

    fn convert_operands<'a>(
        &'a mut self,
        in_instrs: impl 'a + Iterator<Item = Instruction<GeneratedAsmAst>>,
    ) -> impl 'a + Iterator<Item = Instruction<FinalizedAsmAst>> {
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
            Instruction::MovZeroExtend { src, dst } => {
                let src = self.convert_operand(src);
                let dst = self.convert_operand(dst);
                Instruction::MovZeroExtend { src, dst }
            }
            Instruction::Cvttsd2si { dst_asm_type, src, dst } => {
                let src = self.convert_operand(src);
                let dst = self.convert_operand(dst);
                Instruction::Cvttsd2si { dst_asm_type, src, dst }
            }
            Instruction::Cvtsi2sd { src_asm_type, src, dst } => {
                let src = self.convert_operand(src);
                let dst = self.convert_operand(dst);
                Instruction::Cvtsi2sd { src_asm_type, src, dst }
            }
            Instruction::Unary(op, asm_type, operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Unary(op, asm_type, operand)
            }
            Instruction::Binary { op, asm_type, arg, tgt } => {
                let arg = self.convert_operand(arg);
                let tgt = self.convert_operand(tgt);
                Instruction::Binary { op, asm_type, arg, tgt }
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
            Instruction::Div(asm_type, operand) => {
                let operand = self.convert_operand(operand);
                Instruction::Div(asm_type, operand)
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
            PFO::Data(ident) => Operand::Data(ident),
            PreFinalOperand::Pseudo(ident) => {
                let AsmObj { asm_type, loc } = self.backend_symtab.objs().get(&ident).unwrap();
                match loc {
                    ObjLocation::Stack => self
                        .var_to_stack_pos
                        .var_to_stack_pos(ident, *asm_type)
                        .into(),
                    ObjLocation::StaticReadWrite => Operand::Data(ident),
                    ObjLocation::StaticReadonly => Operand::Data(ident),
                }
            }
        }
    }
}
