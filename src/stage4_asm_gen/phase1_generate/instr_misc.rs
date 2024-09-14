use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{stage3_tacky::tacky_ast as t, stage4_asm_gen::asm_ast::*};

impl InstrsGenerator {
    /* Tacky Copy */

    pub(super) fn gen_copy_instrs(
        &mut self,
        t::SrcDst { src, dst }: t::SrcDst,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let (src, _, asm_type) = self.convert_value(src);
        let (dst, _, _) = self.convert_value(dst);
        vec![Instruction::Mov { asm_type, src, dst }]
    }
}
