use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::{GeneratedAsmAst, InstrsGenerator},
    },
    types_backend::AssemblyType,
};

impl<'slf> InstrsGenerator<'slf> {
    pub(super) fn gen_signextend_instrs(
        &self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Movsx { src, dst }]
    }
    pub(super) fn gen_truncate_instrs(
        &self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Mov {
            asm_type: AssemblyType::Longword,
            src,
            dst,
        }]
    }
    pub(super) fn gen_zeroextend_instrs(
        &self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, _) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::MovZeroExtend { src, dst }]
    }
}
