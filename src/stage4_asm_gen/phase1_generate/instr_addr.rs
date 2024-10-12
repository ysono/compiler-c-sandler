use super::{GeneratedAsmAst, InstrsGenerator};
use crate::{
    common::types_backend::ScalarAssemblyType, stage3_tacky::tacky_ast as t,
    stage4_asm_gen::asm_ast::*,
};

impl InstrsGenerator {
    pub(super) fn gen_addptr_instrs(
        &mut self,
        t::AddPtr { ptr, idx, scale, dst }: t::AddPtr,
    ) -> Vec<Instruction<GeneratedAsmAst>> {
        let idx_konst = match &idx {
            t::Value::Constant(konst) => Some(*konst),
            t::Value::Variable(..) => None,
        };

        let ptr = self.value_to_operand(ptr);
        let idx = self.value_to_operand(idx);
        let dst = self.value_to_operand(dst);

        let asm_type = ScalarAssemblyType::Quadword;

        let ptr_reg = Register::AX;
        let idx_reg = Register::DX;

        let mov_ptr_to_reg = || Instruction::Mov {
            asm_type,
            src: ptr,
            dst: Operand::Register(ptr_reg).into(),
        };
        let mov_idx_to_reg = || Instruction::Mov {
            asm_type,
            src: idx,
            dst: Operand::Register(idx_reg).into(),
        };

        /* Begin instructions */

        match idx_konst {
            Some(konst) => {
                let idx = konst.as_bits();
                let scale = scale.as_int() as i64;
                let offset = MemoryOffset::new(idx * scale);

                let instr1 = mov_ptr_to_reg();
                let instr2 = Instruction::Lea {
                    src: Operand::Memory(ptr_reg, offset).into(),
                    dst,
                };
                vec![instr1, instr2]
            }
            None => match MemoryIndexScale::try_from(scale) {
                Ok(scale) => {
                    let instr1 = mov_ptr_to_reg();
                    let instr2 = mov_idx_to_reg();
                    let instr3 = Instruction::Lea {
                        src: Operand::IndexedMemory {
                            base: ptr_reg,
                            idx: idx_reg,
                            scale,
                        }
                        .into(),
                        dst,
                    };
                    vec![instr1, instr2, instr3]
                }
                Err(bytelen) => {
                    let instr1 = mov_ptr_to_reg();
                    let instr2 = mov_idx_to_reg();
                    let instr3 = Instruction::Binary {
                        op: BinaryOperator::Mul,
                        asm_type,
                        tgt: Operand::Register(idx_reg).into(),
                        arg: Operand::ImmediateValue(bytelen.as_int() as i64).into(),
                    };
                    let instr4 = Instruction::Lea {
                        src: Operand::IndexedMemory {
                            base: ptr_reg,
                            idx: idx_reg,
                            scale: MemoryIndexScale::B1,
                        }
                        .into(),
                        dst,
                    };
                    vec![instr1, instr2, instr3, instr4]
                }
            },
        }
    }
}
