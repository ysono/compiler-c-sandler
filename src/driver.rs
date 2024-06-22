use crate::{
    files::{AsmFilepath, ObjectFilepath, PreprocessedFilepath, ProgramFilepath, SrcFilepath},
    stage1_lex::lexer::Lexer,
    stage2_parse::{
        phase1_parse::Parser, phase2_resolve::CAstValidator, phase3_typecheck::TypeChecker,
    },
    stage3_tacky::generate::Tackifier,
    stage4_asm_gen::phase1_generate::AsmCodeGenerator,
    stage5_asm_emit::emit::AsmCodeEmitter,
};
use anyhow::{Context, Result};
use clap::Parser as ClapParser;
use log;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;

#[derive(ClapParser, Debug)]
struct CliArgs {
    src_filepaths: Vec<String>,

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

    #[clap(short = 'c')]
    output_object_file: bool,
}

pub fn driver_main() -> Result<()> {
    env_logger::init();

    let args = CliArgs::parse();
    log::info!("{args:?}");

    let mut asm_filepaths = vec![];
    for src_filepath in args.src_filepaths.iter() {
        let src_filepath = SrcFilepath::try_from(&src_filepath[..])?;
        let pp_filepath = preprocess(&src_filepath)?;
        log::info!("Preprocessor done -> {pp_filepath:?}");

        let asm_filepath = compile(pp_filepath, &args)?;
        log::info!("Compiler done -> {asm_filepath:?}");
        if let Some(asm_filepath) = asm_filepath {
            asm_filepaths.push(asm_filepath);
        }
    }

    match args.output_object_file {
        true => {
            for asm_filepath in asm_filepaths {
                assemble(asm_filepath)?;
            }
        }
        false => {
            assemble_and_link_program(asm_filepaths)?;
        }
    }

    Ok(())
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
        println!("tokens: {tokens:#?}");
        return Ok(None);
    }

    let mut parser = Parser::new(lexer);
    let c_prog = parser.parse_program()?;
    if args.until_parser {
        println!("c_prog: {c_prog:#?}");
        return Ok(None);
    }

    let mut vadlidator = CAstValidator::default();
    let c_prog = vadlidator.resolve_program(c_prog)?;

    let type_checker = TypeChecker::default();
    let (c_prog, symbol_table) = type_checker.typecheck_prog(c_prog)?;

    if args.until_parser_validate {
        println!("validated c_prog: {c_prog:#?}");
        println!("symbol table: {symbol_table:#?}");
        return Ok(None);
    }

    let tacky_prog = Tackifier::tackify_program(c_prog, &symbol_table);
    if args.until_tacky {
        println!("tacky_prog: {tacky_prog:#?}");
        return Ok(None);
    }

    let symbol_table = Rc::new(symbol_table);
    let asm_gen = AsmCodeGenerator::new(Rc::clone(&symbol_table));
    let asm_prog = asm_gen.gen_program(tacky_prog);
    if args.until_asm_codegen {
        println!("asm_prog: {asm_prog:#?}");
        return Ok(None);
    }

    let asm_filepath = AsmFilepath::from(&pp_filepath);
    let asm_emitter = AsmCodeEmitter::new(&asm_filepath, &symbol_table)?;
    asm_emitter.emit_program(asm_prog)?;
    if args.until_asm_emission {
        println!("asm file: {asm_filepath:?}");
        return Ok(None);
    }

    Ok(Some(asm_filepath))
}

fn assemble(asm_filepath: AsmFilepath) -> Result<ObjectFilepath> {
    /* Run separate gcc command per foo.s file, so that we can specify each `-o foo.o` filepath. */
    let obj_filepath = ObjectFilepath::from(&asm_filepath);
    use_gcc_on_asms(&["-c"], vec![asm_filepath], &obj_filepath, "assembler")?;
    Ok(obj_filepath)
}
fn assemble_and_link_program(asm_filepaths: Vec<AsmFilepath>) -> Result<Option<ProgramFilepath>> {
    /* In general, earlier inputs may depend on later inputs. We assume `main()` is inside the first asm input. */
    match asm_filepaths.first() {
        None => Ok(None),
        Some(asm0) => {
            let prog_filepath = ProgramFilepath::from(asm0);
            use_gcc_on_asms(&[], asm_filepaths, &prog_filepath, "assembler and linker")?;
            Ok(Some(prog_filepath))
        }
    }
}
fn use_gcc_on_asms(
    gcc_flags: &[&str],
    asm_paths: Vec<AsmFilepath>,
    out_path: &PathBuf,
    descr: &str,
) -> Result<()> {
    let mut cmd = Command::new("gcc");
    cmd.args(gcc_flags);
    cmd.args(asm_paths.iter().map(|p| p.as_os_str()));
    cmd.args(["-o", out_path.to_str().unwrap()]);
    log::info!("{descr} command: {cmd:?}");

    let mut child = cmd
        .spawn()
        .with_context(|| format!("Failed to launch the {descr} process."))?;
    let child_exit_status = child
        .wait()
        .with_context(|| format!("The {descr} process was not running."))?;
    assert!(
        child_exit_status.success(),
        "The {descr} exit status = {child_exit_status}",
    );
    log::info!("{descr} done -> {out_path:?}");

    for asm_path in asm_paths {
        fs::remove_file(&asm_path as &PathBuf)?;
    }

    Ok(())
}
