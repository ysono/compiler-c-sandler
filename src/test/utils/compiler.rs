use crate::{
    common::{primitive::Const, symbol_table_frontend::SymbolTable},
    driver::{
        config::{Args, CompilerUntil, DriverUntil},
        files::SrcFilepath,
        CompilationResult, Driver,
    },
    stage2_parse::{c_ast, phase1_parse::ParsedCAst, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast,
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

pub fn compile_until_parser(pp: &str) -> Result<c_ast::Program<ParsedCAst>> {
    let compil_res = compile(pp, CompilerUntil::Parser)?;
    match compil_res {
        CompilationResult::Parsed(prog) => Ok(prog),
        actual => Err(anyhow!("{actual:#?}")),
    }
}

pub fn compile_until_typechecker(
    pp: &str,
) -> Result<(c_ast::Program<TypeCheckedCAst>, SymbolTable)> {
    let compil_res = compile(pp, CompilerUntil::ParserValidate)?;
    match compil_res {
        CompilationResult::Validated(prog, fe_symtab) => Ok((prog, fe_symtab)),
        actual => Err(anyhow!("{actual:#?}")),
    }
}
pub fn compile_until_tacky(pp: &str) -> Result<(tacky_ast::Program, SymbolTable)> {
    let compil_res = compile(pp, CompilerUntil::Tacky)?;
    match compil_res {
        CompilationResult::Tacky(prog, fe_symtab) => Ok((prog, fe_symtab)),
        actual => Err(anyhow!("{actual:#?}")),
    }
}

pub fn expect_tacky_implicit_return_instr(mut t_prog: tacky_ast::Program) -> tacky_ast::Program {
    for fun in t_prog.funs.iter_mut() {
        let last_instr = fun.instrs.pop();
        assert!(matches!(
            last_instr,
            Some(tacky_ast::Instruction::Return(tacky_ast::Value::Constant(
                Const::Int(0)
            )))
        ));
    }
    t_prog
}
