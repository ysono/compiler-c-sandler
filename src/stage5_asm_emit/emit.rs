use crate::{
    identifier::UniqueIdentifier,
    stage4_asm_gen::{
        asm_ast::{
            BinaryOperator, ConditionCode, Function, Instruction, Operand, Program, Register,
            StaticVariable, UnaryOperator,
        },
        FinalizedAsmAst,
    },
    symbol_table_backend::{AsmEntry, BackendSymbolTable},
    symbol_table_frontend::StaticVisibility,
    types_backend::{AssemblyType, OperandByteLen},
    types_frontend::Const,
};
use regex::Regex;
use std::io::{self, Write};

const TAB: &str = "\t";

pub struct AsmCodeEmitter<W> {
    label_bad_char: Regex,

    backend_symtab: BackendSymbolTable,

    w: W,
}
impl<W: Write> AsmCodeEmitter<W> {
    pub fn new(backend_symtab: BackendSymbolTable, w: W) -> Result<Self, io::Error> {
        Ok(Self {
            label_bad_char: Regex::new(r"[^a-zA-Z0-9._]").unwrap(),
            backend_symtab,
            w,
        })
    }

    pub fn emit_program(
        mut self,
        Program { static_consts, static_vars, funs }: Program<FinalizedAsmAst>,
    ) -> Result<(), io::Error> {
        let _ = static_consts; // TODO

        for fun in funs {
            self.write_fun(fun)?;
        }

        for static_var in static_vars {
            self.write_static_var(static_var)?;
        }

        if cfg!(target_os = "linux") {
            writeln!(&mut self.w, "{TAB}.section	.note.GNU-stack,\"\",@progbits")?;
        }

        self.w.flush()?;
        Ok(())
    }

    /* Function */

    fn write_fun(
        &mut self,
        Function { ident, visibility, instrs }: Function<FinalizedAsmAst>,
    ) -> Result<(), io::Error> {
        self.write_label_visibility(&ident, visibility, LabelLocality::of_fun())?;
        writeln!(&mut self.w, "{TAB}.text")?;
        self.write_label_instance(&ident, LabelLocality::of_fun())?;
        writeln!(&mut self.w, "{TAB}pushq{TAB}%rbp")?;
        writeln!(&mut self.w, "{TAB}movq{TAB}%rsp, %rbp")?;
        for instr in instrs {
            self.write_instr(instr)?;
        }
        Ok(())
    }
    fn write_instr(&mut self, instr: Instruction<FinalizedAsmAst>) -> Result<(), io::Error> {
        match instr {
            Instruction::Mov { asm_type, src, dst } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}mov{instr_sfx}{TAB}")?;
                self.write_operand(src, bytelen)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(dst, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Movsx { src, dst } => {
                let instr_sfx = "lq";

                write!(&mut self.w, "{TAB}movs{instr_sfx}{TAB}")?;
                self.write_operand(src, OperandByteLen::B4)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(dst, OperandByteLen::B8)?;
                writeln!(&mut self.w)?;
            }
            Instruction::MovZeroExtend { .. } => { /* No-op, b/c this instruction type is strictly pre-final. */
            }
            Instruction::Cvttsd2si { .. } => todo!(),
            Instruction::Cvtsi2sd { .. } => todo!(),
            Instruction::Unary(op, asm_type, operand) => {
                let instr = match op {
                    UnaryOperator::BitwiseComplement => "not",
                    UnaryOperator::TwosComplement => "neg",
                    UnaryOperator::Shr => todo!(),
                };
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}{instr}{instr_sfx}{TAB}")?;
                self.write_operand(operand, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Binary { op, asm_type, arg, tgt } => {
                let instr = match op {
                    BinaryOperator::Add => "add",
                    BinaryOperator::Sub => "sub",
                    BinaryOperator::Mul => "imul",
                    _ => todo!(),
                };
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}{instr}{instr_sfx}{TAB}")?;
                self.write_operand(arg, bytelen)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(tgt, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Cmp { asm_type, arg, tgt } => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}cmp{instr_sfx}{TAB}")?;
                self.write_operand(arg, bytelen)?;
                write!(&mut self.w, ", ")?;
                self.write_operand(tgt, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Idiv(asm_type, operand) => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}idiv{instr_sfx}{TAB}")?;
                self.write_operand(operand, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Div(asm_type, operand) => {
                let instr_sfx = Self::get_instr_sfx_wordlen(asm_type);
                let bytelen = OperandByteLen::from(asm_type);

                write!(&mut self.w, "{TAB}div{instr_sfx}{TAB}")?;
                self.write_operand(operand, bytelen)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Cdq(asm_type) => {
                let instr = match asm_type {
                    AssemblyType::Longword => "cdq",
                    AssemblyType::Quadword => "cqo",
                    AssemblyType::Double => todo!(),
                };

                writeln!(&mut self.w, "{TAB}{instr}")?;
            }
            Instruction::Jmp(lbl) => {
                write!(&mut self.w, "{TAB}jmp{TAB}")?;
                self.write_label_name(&lbl, LabelLocality::of_jump())?;
                writeln!(&mut self.w)?;
            }
            Instruction::JmpCC(cc, lbl) => {
                let instr_sfx = Self::get_instr_sfx_condition(cc);
                write!(&mut self.w, "{TAB}j{instr_sfx}{TAB}")?;
                self.write_label_name(&lbl, LabelLocality::of_jump())?;
                writeln!(&mut self.w)?;
            }
            Instruction::SetCC(cc, operand) => {
                let instr_sfx = Self::get_instr_sfx_condition(cc);
                write!(&mut self.w, "{TAB}set{instr_sfx}{TAB}")?;
                self.write_operand(operand, OperandByteLen::B1)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Label(lbl) => self.write_label_instance(&lbl, LabelLocality::of_jump())?,
            Instruction::Push(operand) => {
                write!(&mut self.w, "{TAB}pushq{TAB}")?;
                self.write_operand(operand, OperandByteLen::B8)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Call(ident) => {
                write!(&mut self.w, "{TAB}call{TAB}")?;
                self.write_label_name(&ident, LabelLocality::of_fun())?;
                self.write_fun_call_sfx(&ident)?;
                writeln!(&mut self.w)?;
            }
            Instruction::Ret => {
                writeln!(&mut self.w, "{TAB}movq{TAB}%rbp, %rsp")?;
                writeln!(&mut self.w, "{TAB}popq{TAB}%rbp")?;
                writeln!(&mut self.w, "{TAB}ret")?;
            }
        }
        Ok(())
    }
    fn get_instr_sfx_wordlen(asm_type: AssemblyType) -> char {
        match asm_type {
            AssemblyType::Longword => 'l',
            AssemblyType::Quadword => 'q',
            AssemblyType::Double => todo!(),
        }
    }
    fn get_instr_sfx_condition(cc: ConditionCode) -> &'static str {
        match cc {
            ConditionCode::E => "e",
            ConditionCode::Ne => "ne",
            ConditionCode::L => "l",
            ConditionCode::Le => "le",
            ConditionCode::G => "g",
            ConditionCode::Ge => "ge",
            ConditionCode::A => "a",
            ConditionCode::Ae => "ae",
            ConditionCode::B => "b",
            ConditionCode::Be => "be",
        }
    }
    fn write_operand(&mut self, operand: Operand, obl: OperandByteLen) -> Result<(), io::Error> {
        use OperandByteLen as OBL;
        match operand {
            Operand::ImmediateValue(val) => {
                write!(&mut self.w, "${val}")?;
            }
            Operand::Register(reg) => {
                let reg_str = match (reg, obl) {
                    (Register::AX, OBL::B8) => "%rax",
                    (Register::AX, OBL::B4) => "%eax",
                    (Register::AX, OBL::B1) => "%al",
                    (Register::CX, OBL::B8) => "%rcx",
                    (Register::CX, OBL::B4) => "%ecx",
                    (Register::CX, OBL::B1) => "%cl",
                    (Register::DX, OBL::B8) => "%rdx",
                    (Register::DX, OBL::B4) => "%edx",
                    (Register::DX, OBL::B1) => "%dl",
                    (Register::DI, OBL::B8) => "%rdi",
                    (Register::DI, OBL::B4) => "%edi",
                    (Register::DI, OBL::B1) => "%dil",
                    (Register::SI, OBL::B8) => "%rsi",
                    (Register::SI, OBL::B4) => "%esi",
                    (Register::SI, OBL::B1) => "%sil",
                    (Register::R8, OBL::B8) => "%r8",
                    (Register::R8, OBL::B4) => "%r8d",
                    (Register::R8, OBL::B1) => "%r8b",
                    (Register::R9, OBL::B8) => "%r9",
                    (Register::R9, OBL::B4) => "%r9d",
                    (Register::R9, OBL::B1) => "%r9b",
                    (Register::R10, OBL::B8) => "%r10",
                    (Register::R10, OBL::B4) => "%r10d",
                    (Register::R10, OBL::B1) => "%r10b",
                    (Register::R11, OBL::B8) => "%r11",
                    (Register::R11, OBL::B4) => "%r11d",
                    (Register::R11, OBL::B1) => "%r11b",
                    (Register::SP, _) => "%rsp",
                    _ => todo!(),
                };
                write!(&mut self.w, "{reg_str}")?;
            }
            Operand::StackPosition(stkpos) => {
                write!(&mut self.w, "{}(%rbp)", *stkpos)?;
            }
            Operand::Data(ident) => {
                self.write_label_name(&ident, LabelLocality::of_static_var())?;
                write!(&mut self.w, "(%rip)")?;
            }
        }
        Ok(())
    }
    fn write_fun_call_sfx(&mut self, ident: &UniqueIdentifier) -> Result<(), io::Error> {
        if cfg!(target_os = "linux") {
            match self.backend_symtab.get(ident).unwrap() {
                AsmEntry::Obj { .. } => { /* No-op. */ }
                AsmEntry::Fun { is_defined } => {
                    if *is_defined == false {
                        /* This is required iff the identifier will be lazily bound by a dynamic linker.
                        It doesn't hurt to specify even if the identifier's address offset will become statically known at link-time. */
                        write!(&mut self.w, "@PLT")?;
                    }
                }
            }
        }
        Ok(())
    }

    /* Static variable */

    fn write_static_var(
        &mut self,
        StaticVariable {
            ident,
            visibility,
            alignment,
            init,
        }: StaticVariable,
    ) -> Result<(), io::Error> {
        let section = match init {
            Const::Int(0) | Const::Long(0) | Const::UInt(0) | Const::ULong(0) => ".bss",
            Const::Int(_) | Const::Long(_) | Const::UInt(_) | Const::ULong(_) => ".data",
            Const::Double(_) => todo!(),
        };
        let alignment = alignment as u8;
        let bytelen = OperandByteLen::from(init.var_type()) as u8;

        /* Begin emission. */

        self.write_label_visibility(&ident, visibility, LabelLocality::of_static_var())?;
        writeln!(&mut self.w, "{TAB}{section}")?;
        writeln!(&mut self.w, "{TAB}.balign {alignment}")?;
        self.write_label_instance(&ident, LabelLocality::of_static_var())?;
        match init {
            Const::Int(0) | Const::UInt(0) | Const::Long(0) | Const::ULong(0) => {
                writeln!(&mut self.w, "{TAB}.zero {bytelen}")?;
            }
            Const::Int(i) => writeln!(&mut self.w, "{TAB}.long {i}")?,
            Const::UInt(i) => writeln!(&mut self.w, "{TAB}.long {i}")?,
            Const::Long(i) => writeln!(&mut self.w, "{TAB}.quad {i}")?,
            Const::ULong(i) => writeln!(&mut self.w, "{TAB}.quad {i}")?,
            Const::Double(_) => todo!(),
        };
        Ok(())
    }

    /* Asm label */

    fn write_label_visibility(
        &mut self,
        ident: &UniqueIdentifier,
        visibility: StaticVisibility,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        match visibility {
            StaticVisibility::Global => {
                write!(&mut self.w, "{TAB}.globl{TAB}")?;
                self.write_label_name(ident, locality)?;
                writeln!(&mut self.w)?;
            }
            StaticVisibility::NonGlobal => { /* No-op. */ }
        }
        Ok(())
    }
    fn write_label_instance(
        &mut self,
        ident: &UniqueIdentifier,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        self.write_label_name(ident, locality)?;
        writeln!(&mut self.w, ":")?;
        Ok(())
    }
    fn write_label_name(
        &mut self,
        ident: &UniqueIdentifier,
        locality: LabelLocality,
    ) -> Result<(), io::Error> {
        const IDENT_PFX_NONLOCAL: &str = if cfg!(target_os = "macos") { "_" } else { "" };
        const IDENT_PFX_LOCAL: &str = if cfg!(target_os = "macos") {
            "L."
        } else {
            ".L."
        };
        let ident_pfx = match locality {
            LabelLocality::InObjSymTab => IDENT_PFX_NONLOCAL,
            LabelLocality::Local => IDENT_PFX_LOCAL,
        };
        write!(&mut self.w, "{ident_pfx}")?;
        self.write_ident(ident)?;
        Ok(())
    }
    fn write_ident(&mut self, ident: &UniqueIdentifier) -> Result<(), io::Error> {
        match ident {
            UniqueIdentifier::Exact(ident) => {
                let name = ident as &str;
                write!(&mut self.w, "{name}")
            }
            UniqueIdentifier::Generated { id, descr } => {
                /* DELIM must be, and NAME_DEFAULT ought to be,
                a non-empty str that cannot be a substring within any original identifier string in the C src code. */
                const DELIM: char = '.';
                const NAME_DEFAULT: &str = "anon.";
                let name = descr
                    .as_ref()
                    .map(|descr| descr as &str)
                    .unwrap_or(NAME_DEFAULT);
                let name = self.label_bad_char.replace_all(name, "_");
                let id = id.as_int();
                write!(&mut self.w, "{name}{DELIM}{id:x}")
            }
        }
    }
}

#[derive(Clone, Copy)]
enum LabelLocality {
    InObjSymTab, // Non-local, ie assembler will include this label in the object file's symbol table.
    Local,       // Ditto not include.
}
impl LabelLocality {
    fn of_fun() -> Self {
        Self::InObjSymTab
    }
    fn of_static_var() -> Self {
        Self::InObjSymTab
    }
    fn of_jump() -> Self {
        Self::Local
    }
}
