use crate::{
    common::{
        symbol_table_backend::BackendSymbolTable,
        symbol_table_frontend::{FrontendSymbolTable, FrontendSymbolTableWithDeduper},
    },
    driver::{
        CompilationResult, Driver,
        config::{Args, CompilerUntil, DriverUntil},
        files::SrcFilepath,
    },
    stage2_parse::{c_ast, phase1_parse::ParsedCAst, phase3_typecheck::TypeCheckedCAst},
    stage3_tacky::tacky_ast,
    stage4_asm_gen::{FinalizedAsmAst, asm_ast},
};
use anyhow::{Result, anyhow};
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
) -> Result<(
    c_ast::Program<TypeCheckedCAst>,
    FrontendSymbolTableWithDeduper,
)> {
    let compil_res = compile(pp, CompilerUntil::ParserValidate)?;
    match compil_res {
        CompilationResult::Validated(prog, fe_symtab) => Ok((prog, fe_symtab)),
        actual => Err(anyhow!("{actual:#?}")),
    }
}

pub fn compile_until_tacky(pp: &str) -> Result<(tacky_ast::Program, FrontendSymbolTable)> {
    let compil_res = compile(pp, CompilerUntil::Tacky)?;
    match compil_res {
        CompilationResult::Tacky(prog, fe_symtab) => Ok((prog, fe_symtab)),
        actual => Err(anyhow!("{actual:#?}")),
    }
}

pub fn compile_until_asm_gen(
    pp: &str,
) -> Result<(asm_ast::Program<FinalizedAsmAst>, BackendSymbolTable)> {
    let compil_res = compile(pp, CompilerUntil::AsmGen)?;
    match compil_res {
        CompilationResult::AsmCode(prog, be_symtab) => Ok((prog, be_symtab)),
        actual => Err(anyhow!("{actual:#?}")),
    }
}
