use anyhow::{anyhow, Context, Result};
use clap::Parser;
use derive_more::Deref;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

#[derive(Parser, Debug)]
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

#[derive(Deref, Debug)]
struct SrcFilepath(PathBuf);
impl<'a> TryFrom<&'a str> for SrcFilepath {
    type Error = anyhow::Error;
    fn try_from(s: &'a str) -> Result<Self> {
        if s.ends_with(".c") {
            Ok(Self(PathBuf::from(s)))
        } else {
            Err(anyhow!("The c source code file must have extension `.c`."))
        }
    }
}

#[derive(Deref, Debug)]
struct PreprocessedFilepath(PathBuf);
impl<'a> From<&'a SrcFilepath> for PreprocessedFilepath {
    fn from(src_filepath: &'a SrcFilepath) -> Self {
        let mut pp_filepath = PathBuf::from(src_filepath as &PathBuf);
        pp_filepath.set_extension("i");
        Self(pp_filepath)
    }
}

#[derive(Deref, Debug)]
struct AsmFilepath(PathBuf);
impl<'a> From<&'a PreprocessedFilepath> for AsmFilepath {
    fn from(pp_filepath: &'a PreprocessedFilepath) -> Self {
        let mut asm_filepath = PathBuf::from(pp_filepath as &PathBuf);
        asm_filepath.set_extension("s");
        Self(asm_filepath)
    }
}

#[derive(Deref, Debug)]
struct ProgramFilepath(PathBuf);
impl<'a> From<&'a AsmFilepath> for ProgramFilepath {
    fn from(asm_filepath: &'a AsmFilepath) -> Self {
        let mut prog_filepath = PathBuf::from(asm_filepath as &PathBuf);
        prog_filepath.set_extension("");
        Self(prog_filepath)
    }
}

#[derive(Clone, Copy, Debug)]
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
    println!("Preprocessor: {cmd:?}");
    let mut child = cmd
        .spawn()
        .context("Failed to launch the preprocessor process.")?;
    child
        .wait()
        .context("The preprocessor process did not succeed.")?;

    Ok(pp_filepath)
}

fn compile(pp_filepath: PreprocessedFilepath, until: CompilerUntil) -> Result<Option<AsmFilepath>> {
    // TODO compile

    // begin mock
    match until {
        CompilerUntil::Lexer | CompilerUntil::Parser | CompilerUntil::AsmCodegen => {}
        CompilerUntil::AsmEmission => {
            let asm_filepath = AsmFilepath::from(&pp_filepath);

            let mut asm_file = fs::OpenOptions::new()
                .create(true)
                .write(true)
                .open(&asm_filepath as &PathBuf)?;
            writeln!(asm_file, ".global main")?;
            writeln!(asm_file, "main:")?;
            writeln!(asm_file, "movl $123, %eax")?;
            writeln!(asm_file, "ret")?;
            asm_file.sync_all()?;

            fs::remove_file(&pp_filepath as &PathBuf)?;

            return Ok(Some(asm_filepath));
        }
    }
    // end mock

    Ok(None)
}

fn assemble_and_link(asm_filepath: AsmFilepath) -> Result<ProgramFilepath> {
    let prog_filepath = ProgramFilepath::from(&asm_filepath);

    let mut cmd = Command::new("gcc");
    cmd.args([
        asm_filepath.to_str().unwrap(),
        "-o",
        prog_filepath.to_str().unwrap(),
    ]);
    println!("Assembler and linker: {cmd:?}");
    let mut child = cmd
        .spawn()
        .context("Failed to launch the preprocessor process.")?;
    child
        .wait()
        .context("The preprocessor process did not succeed.")?;

    fs::remove_file(&asm_filepath as &PathBuf)?;

    Ok(prog_filepath)
}

fn main() -> Result<()> {
    let args = CliArgs::parse();

    let params = AppParams::try_from(args)?;
    println!("{params:?}");

    let pp_filepath = preprocess(&params.src_filepath)?;
    println!("Preprocessor done -> {pp_filepath:?}");

    let asm_filepath = compile(pp_filepath, params.compiler_driver_until.compiler_until())?;
    println!("Assembly generator done -> {asm_filepath:?}");

    match (params.compiler_driver_until, asm_filepath) {
        (CompilerDriverUntil::Linker, Some(asm_filepath)) => {
            let prog_filepath = assemble_and_link(asm_filepath)?;
            println!("Linker done -> {prog_filepath:?}");
        }
        _ => {}
    }

    Ok(())
}
