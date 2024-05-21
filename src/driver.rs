use crate::{
    asm_codeemission::AsmCodeEmitter,
    asm_codegen::AsmCodeGenerator,
    files::{AsmFilepath, PreprocessedFilepath, ProgramFilepath, SrcFilepath},
    lexer::Lexer,
    parser::Parser,
};
use anyhow::{Context, Result};
use clap::Parser as ClapParser;
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

    #[clap(long = "codegen")]
    until_asm_codegen: bool,

    #[clap(short = 'S')]
    until_asm_emission: bool,
}

#[derive(Debug)]
struct AppParams {
    src_filepath: SrcFilepath,
    compiler_driver_until: CompilerDriverUntil,
}
impl TryFrom<CliArgs> for AppParams {
    type Error = anyhow::Error;
    fn try_from(args: CliArgs) -> Result<Self> {
        Ok(Self {
            src_filepath: SrcFilepath::try_from(&args.src_filepath[..])?,
            compiler_driver_until: CompilerDriverUntil::from(&args),
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CompilerUntil {
    Lexer,
    Parser,
    AsmCodegen,
    AsmEmission,
}
#[derive(Clone, Copy, Debug)]
enum CompilerDriverUntil {
    Compiler(CompilerUntil),
    Linker,
}
impl<'a> From<&'a CliArgs> for CompilerDriverUntil {
    fn from(args: &'a CliArgs) -> Self {
        if args.until_asm_emission {
            Self::Compiler(CompilerUntil::AsmEmission)
        } else if args.until_asm_codegen {
            Self::Compiler(CompilerUntil::AsmCodegen)
        } else if args.until_parser {
            Self::Compiler(CompilerUntil::Parser)
        } else if args.until_lexer {
            Self::Compiler(CompilerUntil::Lexer)
        } else {
            Self::Linker
        }
    }
}
impl CompilerDriverUntil {
    fn compiler_until(&self) -> CompilerUntil {
        match self {
            Self::Compiler(u) => u.clone(),
            Self::Linker => CompilerUntil::AsmEmission,
        }
    }
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
    eprintln!("Preprocessor: {cmd:?}");
    let mut child = cmd
        .spawn()
        .context("Failed to launch the preprocessor process.")?;
    child
        .wait()
        .context("The preprocessor process did not succeed.")?;

    Ok(pp_filepath)
}

fn compile(pp_filepath: PreprocessedFilepath, until: CompilerUntil) -> Result<Option<AsmFilepath>> {
    let lexer = Lexer::try_from(&pp_filepath)?;
    if until == CompilerUntil::Lexer {
        return Ok(None);
    }

    let mut parser = Parser::new(lexer);
    let c_prog = parser.parse_program()?;
    if until == CompilerUntil::Parser {
        return Ok(None);
    }

    let asm_prog = AsmCodeGenerator::gen_program(c_prog);
    if until == CompilerUntil::AsmCodegen {
        return Ok(None);
    }

    let asm_filepath = AsmFilepath::from(&pp_filepath);
    let asm_emitter = AsmCodeEmitter::try_from(&asm_filepath)?;
    asm_emitter.emit_program(asm_prog)?;
    return Ok(Some(asm_filepath));
}

fn assemble_and_link(asm_filepath: AsmFilepath) -> Result<ProgramFilepath> {
    let prog_filepath = ProgramFilepath::from(&asm_filepath);

    let mut cmd = Command::new("gcc");
    cmd.args([
        asm_filepath.to_str().unwrap(),
        "-o",
        prog_filepath.to_str().unwrap(),
    ]);
    eprintln!("Assembler and linker: {cmd:?}");
    let mut child = cmd
        .spawn()
        .context("Failed to launch the preprocessor process.")?;
    child
        .wait()
        .context("The preprocessor process did not succeed.")?;

    fs::remove_file(&asm_filepath as &PathBuf)?;

    Ok(prog_filepath)
}

pub fn driver_main() -> Result<()> {
    let args = CliArgs::parse();

    let params = AppParams::try_from(args)?;
    eprintln!("{params:?}");

    let pp_filepath = preprocess(&params.src_filepath)?;
    eprintln!("Preprocessor done -> {pp_filepath:?}");

    let asm_filepath = compile(pp_filepath, params.compiler_driver_until.compiler_until())?;
    eprintln!("Assembly generator done -> {asm_filepath:?}");

    match (params.compiler_driver_until, asm_filepath) {
        (CompilerDriverUntil::Linker, Some(asm_filepath)) => {
            let prog_filepath = assemble_and_link(asm_filepath)?;
            eprintln!("Linker done -> {prog_filepath:?}");
        }
        _ => {}
    }

    Ok(())
}
