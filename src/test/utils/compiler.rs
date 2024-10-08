use crate::{
    common::types_frontend::Const,
    driver::{
        config::{Args, CompilerUntil, DriverUntil},
        files::SrcFilepath,
        CompilationResult, Driver,
    },
    stage2_parse::{c_ast as c, phase1_parse::ParsedCAst, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast as t,
};
use anyhow::{anyhow, Result};
use std::{
    io::{BufReader, Cursor},
    path::PathBuf,
};

fn compile(pre_processed_src_code: &str, until: CompilerUntil) -> Result<CompilationResult> {
    let args = Args {
        src_filepaths: Vec::with_capacity(0),
        until: DriverUntil::Compiler(until),
        lib_names: Vec::with_capacity(0),
    };
    let driver = Driver::from(args);

    let mock_src_filepath = SrcFilepath::try_from(PathBuf::from("/tmp/mock.c")).unwrap();
    let pp_reader = BufReader::new(Cursor::new(pre_processed_src_code));
    driver.compile(&mock_src_filepath, pp_reader)
}

pub fn compile_parsed_c_prog(pp: &str) -> Result<c::Program<ParsedCAst>> {
    let compil_res = compile(pp, CompilerUntil::Parser)?;
    match compil_res {
        CompilationResult::Parsed(prog) => Ok(prog),
        actual => Err(anyhow!("{actual:#?}")),
    }
}

pub fn compile_typechecked_c_prog(pp: &str) -> Result<c::Program<TypeCheckedCAst>> {
    let compil_res = compile(pp, CompilerUntil::ParserValidate)?;
    match compil_res {
        CompilationResult::Validated(prog, _fe_symtab) => Ok(prog),
        actual => Err(anyhow!("{actual:#?}")),
    }
}
pub fn compile_tacky_prog(pp: &str) -> Result<t::Program> {
    let compil_res = compile(pp, CompilerUntil::Tacky)?;
    match compil_res {
        CompilationResult::Tacky(prog, _be_symtab) => Ok(prog),
        actual => Err(anyhow!("{actual:#?}")),
    }
}

pub fn expect_tacky_implicit_return_instr(mut t_prog: t::Program) -> t::Program {
    for fun in t_prog.funs.iter_mut() {
        let last_instr = fun.instrs.pop();
        assert!(matches!(
            last_instr,
            Some(t::Instruction::Return(t::Value::Constant(Const::Int(0))))
        ));
    }
    t_prog
}
