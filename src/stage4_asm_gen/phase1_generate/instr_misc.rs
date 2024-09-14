use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{stage3_tacky::tacky_ast as t, stage4_asm_gen::asm_ast::*};

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
}
