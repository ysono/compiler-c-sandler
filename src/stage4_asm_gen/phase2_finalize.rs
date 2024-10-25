//! + Translate each remaining abstract operand location into a concrete location.
//! + Derive the stack frame length; and allocate the frame.

pub(crate) mod var_to_stack_pos; // `pub` for rustdoc.

use self::var_to_stack_pos::VarToStackPos;
use crate::{
    common::{
        identifier::SymbolIdentifier,
        symbol_table_backend::{AsmObj, BackendSymbolTable, ObjLocation},
        types_backend::{ByteLen, ScalarAssemblyType},
    },
    ds_n_a::immutable_owned::ImmutableOwned,
    stage4_asm_gen::{asm_ast::*, phase1_generate::GeneratedAsmAst, phase3_fix::OperandFixer},
};
use std::collections::VecDeque;
use std::rc::Rc;

#[derive(Debug)]
pub struct FinalizedAsmAst(());
impl AsmAstVariant for FinalizedAsmAst {
    type Instructions = VecDeque<Instruction<Self>>;
    type Operand = Operand;
}

pub struct InstrsFinalizer {
    backend_symtab: ImmutableOwned<BackendSymbolTable>,

    var_to_stack_pos: VarToStackPos,
}
impl InstrsFinalizer {
    pub fn new(backend_symtab: ImmutableOwned<BackendSymbolTable>) -> Self {
        Self {
            backend_symtab,
            var_to_stack_pos: VarToStackPos::default(),
        }
    }

    pub fn finalize_fun(
        mut self,
        Function { ident, visibility, instrs }: Function<GeneratedAsmAst>,
    ) -> (
        Function<FinalizedAsmAst>,
        ImmutableOwned<BackendSymbolTable>,
    ) {
        let instrs = self.convert_instrs(instrs.into_iter());
        let instrs = OperandFixer::fix_invalid_operands(instrs);
        let instrs = instrs.collect::<VecDeque<_>>();

        let instrs = Self::finalize_instrs(instrs, self.var_to_stack_pos);

        let fun = Function { ident, visibility, instrs };

        (fun, self.backend_symtab)
    }

    fn convert_instrs<'a>(
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
            Instruction::Lea { src, dst } => {
                let src = self.convert_operand(src);
                let dst = self.convert_operand(dst);
                Instruction::Lea { src, dst }
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
        match pfo {
            PreFinalOperand::O(o) => o,
            PreFinalOperand::PseudoRegOrMem(ident) => {
                // Unconditionally on mem, for now.
                self.derive_mem_operand(ident, ByteLen::new(0))
            }
            PreFinalOperand::PseudoMem { obj, offset } => self.derive_mem_operand(obj, offset),
        }
    }
    fn derive_mem_operand(&mut self, ident: Rc<SymbolIdentifier>, offset: ByteLen) -> Operand {
        let AsmObj { asm_type, loc } = self.backend_symtab.ident_to_obj().get(&ident).unwrap();
        match loc {
            ObjLocation::Stack => {
                let mut stack_pos = self.var_to_stack_pos.resolve_stack_pos(ident, asm_type);
                *stack_pos.as_mut() += offset.as_int() as i64;
                Operand::Memory(Register::BP, stack_pos)
            }
            ObjLocation::StaticReadWrite => {
                if cfg!(debug_assertions) && offset.as_int() != 0 {
                    unimplemented!("`foo+4(%rip)` will be impl'd in ch18.")
                }
                Operand::ReadWriteData(ident)
            }
            ObjLocation::StaticReadonly => {
                if cfg!(debug_assertions) && offset.as_int() != 0 {
                    unimplemented!("`foo+4(%rip)` will be impl'd in ch18.")
                }
                Operand::ReadonlyData(ident)
            }
        }
    }

    /// See documentation at [`crate::stage4_asm_gen`].
    fn finalize_instrs(
        mut instrs: VecDeque<Instruction<FinalizedAsmAst>>,
        var_to_stack_pos: VarToStackPos,
    ) -> VecDeque<Instruction<FinalizedAsmAst>> {
        let last_used_stack_pos = var_to_stack_pos.last_used_stack_pos().as_int();
        if last_used_stack_pos < 0 {
            let stack_frame_bytelen = (((last_used_stack_pos * -1) + 15) / 16) * 16;

            instrs.push_front(Instruction::Binary {
                op: BinaryOperator::Sub,
                asm_type: ScalarAssemblyType::Quadword,
                tgt: Operand::Register(Register::SP),
                arg: Operand::ImmediateValue(stack_frame_bytelen),
            });
        }

        instrs
    }
}
