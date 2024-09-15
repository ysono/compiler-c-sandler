use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::types_backend::AssemblyType, stage3_tacky::tacky_ast as t, stage4_asm_gen::asm_ast::*,
};

impl InstrsGenerator {
    /* Tacky Copy */

    pub(super) fn gen_copy_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, asm_type) = self.value_to_operand_and_type(src);
        let dst = self.value_to_operand(dst);
        vec![Instruction::Mov { asm_type, src, dst }]
    }

    /* Tacky instructions using a memory address. */

    pub(super) fn gen_getaddr_instrs(
        &mut self,
        t::GetAddress { src, dst_addr }: t::GetAddress,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src = self.value_to_operand(src);
        let dst = self.value_to_operand(dst_addr);
        vec![Instruction::Lea { src, dst }]
    }
    pub(super) fn gen_load_instrs(
        &mut self,
        t::Load { src_addr, dst }: t::Load,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let src_addr = self.value_to_operand(src_addr);
        let (dst, _, dst_asm_type) = self.value_to_operand_and_type(dst);

        let reg = Register::AX;

        vec![
            Instruction::Mov {
                asm_type: AssemblyType::Quadword,
                src: src_addr,
                dst: Operand::Register(reg).into(),
            },
            Instruction::Mov {
                asm_type: dst_asm_type,
                src: Operand::MemoryAddress(reg, MemoryAddressOffset(0)).into(),
                dst,
            },
        ]
    }
    pub(super) fn gen_store_instrs(
        &mut self,
        t::Store { src, dst_addr }: t::Store,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, src_asm_type) = self.value_to_operand_and_type(src);
        let dst_addr = self.value_to_operand(dst_addr);

        let reg = Register::AX;

        vec![
            Instruction::Mov {
                asm_type: AssemblyType::Quadword,
                src: dst_addr,
                dst: Operand::Register(reg).into(),
            },
            Instruction::Mov {
                asm_type: src_asm_type,
                src,
                dst: Operand::MemoryAddress(reg, MemoryAddressOffset(0)).into(),
            },
        ]
    }
}
