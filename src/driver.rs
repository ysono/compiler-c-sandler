mod files;

use self::files::{AsmFilepath, ObjectFilepath, ProgramFilepath, SrcFilepath};
use crate::{
    stage1_lex::lexer::Lexer,
    stage2_parse::{
        phase1_parse::Parser, phase2_resolve::CAstValidator, phase3_typecheck::TypeChecker,
    },
    stage3_tacky::generate::Tackifier,
    stage4_asm_gen::AsmCodeGenerator,
    stage5_asm_emit::emit::AsmCodeEmitter,
};
use anyhow::Result;
use clap::Parser as ClapParser;
use derive_more::From;
use duct::{cmd, Handle, ReaderHandle};
use log;
use nonempty::NonEmpty;
use std::fs::{self, OpenOptions};
use std::io::{self, BufRead, BufReader, BufWriter, Read};
use std::mem;
use std::path::PathBuf;

#[derive(ClapParser, Debug)]
pub struct CliArgs {
    src_filepaths: Vec<PathBuf>,

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
    until_assembler: bool,

    #[clap(short = 'l')]
    lib_names: Vec<String>,
}

#[derive(From)]
pub struct Driver {
    args: CliArgs,
}
impl Driver {
    pub fn run(mut self) -> Result<()> {
        let src_filepaths = mem::replace(&mut self.args.src_filepaths, Vec::with_capacity(0));

        let mut asm_filepaths = Vec::with_capacity(self.args.src_filepaths.len());
        for src_filepath in src_filepaths {
            let src_filepath = SrcFilepath::try_from(src_filepath)?;

            let pp_reader = Self::preprocess(&src_filepath)?;
            let pp_reader = BufReader::new(pp_reader);

            let asm_filepath = self.compile(&src_filepath, pp_reader)?;
            if let Some(asm_filepath) = asm_filepath {
                asm_filepaths.push(asm_filepath);
            }
        }

        let asm_filepaths = NonEmpty::from_vec(asm_filepaths);
        if let Some(asm_filepaths) = asm_filepaths {
            let downstream_handles = self.assemble_or_link(&asm_filepaths);

            let is_ok = Self::wait_for_downstream(downstream_handles);

            if is_ok == true {
                for asm_filepath in asm_filepaths {
                    fs::remove_file(&asm_filepath as &PathBuf)?;
                }
            }
        }

        Ok(())
    }

    fn preprocess(src_filepath: &SrcFilepath) -> Result<ReaderHandle, io::Error> {
        let gcc_cmd = cmd!(
            "gcc",
            src_filepath.as_os_str(),
            "-E", // Stop after the preprocessing stage.
            "-P", // Inhibit generation of linemarkers in the output from the preprocessor.
            "-o", // Output.
            "/dev/stdout",
        );
        log::info!("Preprocessor: {gcc_cmd:?}");
        gcc_cmd.reader()
    }

    fn compile<R: Read + BufRead>(
        &self,
        src_filepath: &SrcFilepath,
        pp_reader: R,
    ) -> Result<Option<AsmFilepath>> {
        let lexer = Lexer::new(pp_reader)?;
        if self.args.until_lexer {
            let tokens = lexer.collect::<Result<Vec<_>>>()?;
            println!("tokens: {tokens:#?}");
            return Ok(None);
        }

        let mut parser = Parser::new(lexer);
        let c_prog = parser.parse_program()?;
        if self.args.until_parser {
            println!("c_prog: {c_prog:#?}");
            return Ok(None);
        }

        let mut vadlidator = CAstValidator::default();
        let c_prog = vadlidator.resolve_program(c_prog)?;

        let type_checker = TypeChecker::default();
        let (c_prog, mut frontend_symtab) = type_checker.typecheck_prog(c_prog)?;

        if self.args.until_parser_validate {
            println!("validated c_prog: {c_prog:#?}");
            println!("symbol table: {frontend_symtab:#?}");
            return Ok(None);
        }

        let tacky_prog = Tackifier::tackify_program(c_prog, &mut frontend_symtab);
        if self.args.until_tacky {
            println!("tacky_prog: {tacky_prog:#?}");
            return Ok(None);
        }

        let asm_gen = AsmCodeGenerator::new(frontend_symtab);
        let (asm_prog, backend_symtab) = asm_gen.gen_program(tacky_prog);
        if self.args.until_asm_codegen {
            println!("asm_prog: {asm_prog:#?}");
            return Ok(None);
        }

        let asm_filepath = AsmFilepath::from(src_filepath);
        let asm_file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&asm_filepath as &PathBuf)?;
        let asm_bw = BufWriter::new(asm_file);
        let asm_emitter = AsmCodeEmitter::new(backend_symtab, asm_bw)?;
        asm_emitter.emit_program(asm_prog)?;
        log::info!("Compiler done -> {asm_filepath:?}");
        if self.args.until_asm_emission {
            return Ok(None);
        }

        Ok(Some(asm_filepath))
    }

    fn assemble_or_link(
        &mut self,
        asm_filepaths: &NonEmpty<AsmFilepath>,
    ) -> NonEmpty<Result<Handle, io::Error>> {
        if self.args.until_assembler {
            Self::launch_assembler(asm_filepaths)
        } else {
            let handle = self.launch_linker(asm_filepaths);
            NonEmpty::new(handle)
        }
    }
    fn launch_assembler(
        asm_filepaths: &NonEmpty<AsmFilepath>,
    ) -> NonEmpty<Result<Handle, io::Error>> {
        /* In order to specify each output `*.o` filepath, execute a separate gcc command per assembly file. */
        let gcc_procs = asm_filepaths
            .iter()
            .map(|asm_filepath| {
                let obj_filepath = ObjectFilepath::from(asm_filepath);
                let gcc_cmd = cmd!(
                    "gcc",
                    asm_filepath.as_os_str(),
                    "-c", // Compile or assemble the source files, but do not link.
                    "-o", // Output
                    obj_filepath.as_os_str()
                );
                log::info!("Assembler: {gcc_cmd:?}");
                gcc_cmd.start()
            })
            .collect::<Vec<_>>();
        NonEmpty::from_vec(gcc_procs).unwrap()
    }
    fn launch_linker(
        &mut self,
        asm_filepaths: &NonEmpty<AsmFilepath>,
    ) -> Result<Handle, io::Error> {
        let asm_args = asm_filepaths
            .iter()
            .map(|asm_filepath| asm_filepath.to_str().unwrap());

        let lib_names = mem::replace(&mut self.args.lib_names, Vec::with_capacity(0));
        let lib_args = lib_names
            .into_iter()
            .map(|lib_name| format!("-l{lib_name}"))
            .collect::<Vec<_>>();
        let lib_args = lib_args.iter().map(|s| &s[..]);

        /* Among input file arguments to gcc, in general, earlier inputs may depend on later inputs.
        We assume `main()` is inside the first asm input, and name our output program file after the first input file. */
        let name0 = &asm_filepaths[0];
        let prog_filepath = ProgramFilepath::from(name0);
        let prog_args = ["-o", prog_filepath.to_str().unwrap()];

        let args = asm_args.chain(lib_args).chain(prog_args);
        let gcc_cmd = cmd("gcc", args);
        log::info!("Linker: {gcc_cmd:?}");
        gcc_cmd.start()
    }

    fn wait_for_downstream(downstream_handles: NonEmpty<Result<Handle, io::Error>>) -> bool {
        let mut is_ok = true;
        for handle in downstream_handles {
            match handle {
                Err(_) => {
                    log::error!("Downstream failed to launch. {handle:?}");
                    is_ok = false;
                }
                Ok(handle) => {
                    let output = handle.wait();
                    match output {
                        Err(_) => {
                            log::error!("Downstream failed. {handle:?} {output:?}");
                            is_ok = false;
                        }
                        Ok(output) => {
                            log::info!("Downstream done. {handle:?} {output:?}");
                        }
                    }
                }
            }
        }
        is_ok
    }
}
