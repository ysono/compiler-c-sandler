use crate::{
    files::{AsmFilepath, PreprocessedFilepath, ProgramFilepath, SrcFilepath},
    stage1_lexer::Lexer,
    stage2a_parser::Parser,
    stage2b_validate::CAstValidator,
    stage3_tacky::Tackifier,
    stage4_asm_gen::AsmCodeGenerator,
    stage5_asm_emit::AsmCodeEmitter,
};
use anyhow::{Context, Result};
use clap::Parser as ClapParser;
use log;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

#[derive(ClapParser, Debug)]
struct CliArgs {
    src_filepath: String,

    #[clap(long = "lex")]
    until_lexer: bool,

    #[clap(long = "parse")]
    until_parser: bool,

    #[clap(long = "validate")]
    until_parser_validate: bool,

    #[clap(long = "tacky")]
    until_tacky: bool,

    #[clap(long = "codegen")]
    until_asm_codegen: bool,

    #[clap(short = 'S')]
    until_asm_emission: bool,
}

fn preprocess(src_filepath: &SrcFilepath) -> Result<PreprocessedFilepath> {
    let pp_filepath = PreprocessedFilepath::from(src_filepath);

    let mut cmd = Command::new("gcc");
    cmd.args([
        "-E",
        "-P",
        src_filepath.to_str().unwrap(),
        "-o",
        pp_filepath.to_str().unwrap(),
    ]);
    log::info!("Preprocessor: {cmd:?}");
    let mut child = cmd
        .spawn()
        .context("Failed to launch the preprocessor process.")?;
    let child_code = child
        .wait()
        .context("The preprocessor process was not running.")?;
    assert!(
        child_code.success(),
        "The preprocessor exit code = {child_code}",
    );

    Ok(pp_filepath)
}

fn compile(pp_filepath: PreprocessedFilepath, args: &CliArgs) -> Result<Option<AsmFilepath>> {
    let lexer = Lexer::try_from(&pp_filepath)?;
    if args.until_lexer {
        let tokens = lexer.collect::<Result<Vec<_>>>()?;
        println!("tokens: {tokens:?}");
        return Ok(None);
    }

    let mut parser = Parser::new(lexer);
    let c_prog = parser.parse_program()?;
    if args.until_parser {
        println!("c_prog: {c_prog:?}");
        return Ok(None);
    }

    let mut vadlidator = CAstValidator::default();
    let c_prog = vadlidator.resolve_program(c_prog)?;
    if args.until_parser_validate {
        println!("validated c_prog: {c_prog:?}");
        return Ok(None);
    }

    let tacky_prog = Tackifier::tackify_program(c_prog);
    if args.until_tacky {
        println!("tacky_prog: {tacky_prog:?}");
        return Ok(None);
    }

    let asm_prog = AsmCodeGenerator::gen_program(tacky_prog);
    if args.until_asm_codegen {
        println!("asm_prog: {asm_prog:?}");
        return Ok(None);
    }

    let asm_filepath = AsmFilepath::from(&pp_filepath);
    let asm_emitter = AsmCodeEmitter::try_from(&asm_filepath)?;
    asm_emitter.emit_program(asm_prog)?;
    if args.until_asm_emission {
        return Ok(None);
    }

    Ok(Some(asm_filepath))
}

fn assemble_and_link(asm_filepath: AsmFilepath) -> Result<ProgramFilepath> {
    let prog_filepath = ProgramFilepath::from(&asm_filepath);

    let mut cmd = Command::new("gcc");
    cmd.args([
        asm_filepath.to_str().unwrap(),
        "-o",
        prog_filepath.to_str().unwrap(),
    ]);
    log::info!("Assembler and linker: {cmd:?}");
    let mut child = cmd
        .spawn()
        .context("Failed to launch the assembler and linker process.")?;
    let child_code = child
        .wait()
        .context("The assembler and linker process was not running.")?;
    assert!(
        child_code.success(),
        "The assembler and linker exit code = {child_code}",
    );

    fs::remove_file(&asm_filepath as &PathBuf)?;

    Ok(prog_filepath)
}

pub fn driver_main() -> Result<()> {
    env_logger::init();

    let args = CliArgs::parse();
    log::info!("{args:?}");

    let src_filepath = SrcFilepath::try_from(&args.src_filepath[..])?;
    let pp_filepath = preprocess(&src_filepath)?;
    log::info!("Preprocessor done -> {pp_filepath:?}");

    let asm_filepath = compile(pp_filepath, &args)?;
    log::info!("Assembly generator done -> {asm_filepath:?}");

    if let Some(asm_filepath) = asm_filepath {
        let prog_filepath = assemble_and_link(asm_filepath)?;
        log::info!("Linker done -> {prog_filepath:?}");
    }

    Ok(())
}
