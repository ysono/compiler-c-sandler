pub mod config;
pub mod files;

use self::{
    config::{Args, CompilerUntil, Downstream, DriverUntil},
    files::{AsmFilepath, ObjectFilepath, ProgramFilepath, SrcFilepath},
};
use crate::{
    common::{
        symbol_table_backend::BackendSymbolTable, symbol_table_frontend::FrontendSymbolTable,
    },
    ds_n_a::nonempty::NonEmpty,
    stage1_lex::{lexer::Lexer, tokens::Token},
    stage2_parse::{
        c_ast,
        phase1_parse::{ParsedCAst, Parser},
        phase2_resolve::CAstValidator,
        phase3_typecheck::{TypeCheckedCAst, TypeChecker},
    },
    stage3_tacky::{generate::Tackifier, tacky_ast},
    stage4_asm_gen::{AsmCodeGenerator, FinalizedAsmAst, asm_ast},
    stage5_asm_emit::emit::AsmCodeEmitter,
};
use anyhow::Result;
use duct::{Handle, ReaderHandle, cmd};
use std::{
    fs::{self, OpenOptions},
    io::{self, BufRead, BufReader, BufWriter, Read},
    mem,
    path::PathBuf,
};

pub struct Driver {
    args: Args,
}
impl<A: Into<Args>> From<A> for Driver {
    fn from(args: A) -> Self {
        Self { args: args.into() }
    }
}
impl Driver {
    pub fn run(mut self) -> Result<()> {
        let src_filepaths = mem::replace(&mut self.args.src_filepaths, Vec::with_capacity(0));

        let mut asm_filepaths = Vec::with_capacity(self.args.src_filepaths.len());
        for src_filepath in src_filepaths {
            let src_filepath = SrcFilepath::try_from(src_filepath)?;

            let pp_reader = Self::preprocess(&src_filepath)?;
            let pp_reader = BufReader::new(pp_reader);

            let compil_res = self.compile(&src_filepath, pp_reader)?;
            log::info!("Compiler done"); // To stderr.
            if log::log_enabled!(log::Level::Info) {
                println!("{compil_res:#?}"); // To stdout.
            }

            if let CompilationResult::AsmFile(asm_filepath) = compil_res {
                asm_filepaths.push(asm_filepath);
            }
        }

        if let DriverUntil::Downstream(downstream) = self.args.until {
            let asm_filepaths = NonEmpty::from_vec(asm_filepaths);
            if let Some(asm_filepaths) = asm_filepaths {
                let downstream_handles = self.assemble_or_link(&asm_filepaths, downstream);

                let is_ok = Self::wait_for_downstream(downstream_handles);

                if is_ok == true {
                    for asm_filepath in asm_filepaths {
                        fs::remove_file(&asm_filepath as &PathBuf)?;
                    }
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
        log::info!("Preprocessor: {gcc_cmd:#?}");
        gcc_cmd.reader()
    }

    pub(crate) fn compile<R: Read + BufRead>(
        &self,
        src_filepath: &SrcFilepath,
        pp_reader: R,
    ) -> Result<CompilationResult> {
        let lexer = Lexer::new(pp_reader)?;
        if self.args.until == DriverUntil::Compiler(CompilerUntil::Lexer) {
            let tokens = lexer.collect::<Result<Vec<_>>>()?;
            return Ok(CompilationResult::Lexed(tokens));
        }

        let mut parser = Parser::new(lexer);
        let c_prog = parser.parse_program()?;
        if self.args.until == DriverUntil::Compiler(CompilerUntil::Parser) {
            return Ok(CompilationResult::Parsed(c_prog));
        }

        let (_, obj_type_repo, _) = parser.into();

        let mut vadlidator = CAstValidator::default();
        let c_prog = vadlidator.resolve_program(c_prog)?;

        let type_checker = TypeChecker::new(obj_type_repo);
        let (c_prog, frontend_symtab) = type_checker.typecheck_prog(c_prog)?;

        if self.args.until == DriverUntil::Compiler(CompilerUntil::ParserValidate) {
            return Ok(CompilationResult::Validated(c_prog, frontend_symtab));
        }

        let tackifier = Tackifier::new(frontend_symtab);
        let (tacky_prog, frontend_symtab) = tackifier.tackify_program(c_prog);
        if self.args.until == DriverUntil::Compiler(CompilerUntil::Tacky) {
            return Ok(CompilationResult::Tacky(tacky_prog, frontend_symtab));
        }

        let asm_gen = AsmCodeGenerator::new(frontend_symtab);
        let (asm_prog, backend_symtab) = asm_gen.gen_program(tacky_prog);
        if self.args.until == DriverUntil::Compiler(CompilerUntil::AsmGen) {
            return Ok(CompilationResult::AsmCode(
                asm_prog,
                backend_symtab.into_inner(),
            ));
        }

        let asm_filepath = AsmFilepath::from(src_filepath);
        let asm_file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&asm_filepath as &PathBuf)?;
        let asm_bw = BufWriter::new(asm_file);
        let asm_emitter = AsmCodeEmitter::new(backend_symtab, asm_bw);
        asm_emitter.emit_program(asm_prog)?;
        return Ok(CompilationResult::AsmFile(asm_filepath));
    }

    fn assemble_or_link(
        &mut self,
        asm_filepaths: &NonEmpty<AsmFilepath>,
        downstream: Downstream,
    ) -> NonEmpty<Result<Handle, io::Error>> {
        match downstream {
            Downstream::Assembler => Self::launch_assembler(asm_filepaths),
            Downstream::Linker => {
                let handle = self.launch_linker(asm_filepaths);
                NonEmpty::new(handle)
            }
        }
    }
    fn launch_assembler(
        asm_filepaths: &NonEmpty<AsmFilepath>,
    ) -> NonEmpty<Result<Handle, io::Error>> {
        /* In order to specify each output `*.o` filepath, execute a separate gcc command per assembly file. */
        asm_filepaths.ref_map(|asm_filepath| {
            let obj_filepath = ObjectFilepath::from(asm_filepath);
            let gcc_cmd = cmd!(
                "gcc",
                asm_filepath.as_os_str(),
                "-c", // Compile or assemble the source files, but do not link.
                "-o", // Output
                obj_filepath.as_os_str()
            );
            log::info!("Assembler: {gcc_cmd:#?}");
            gcc_cmd.start()
        })
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
        let name0 = asm_filepaths.first();
        let prog_filepath = ProgramFilepath::from(name0);
        let prog_args = ["-o", prog_filepath.to_str().unwrap()];

        let args = asm_args.chain(lib_args).chain(prog_args);
        let gcc_cmd = cmd("gcc", args);
        log::info!("Linker: {gcc_cmd:#?}");
        gcc_cmd.start()
    }

    fn wait_for_downstream(downstream_handles: NonEmpty<Result<Handle, io::Error>>) -> bool {
        let mut is_ok = true;
        for handle in downstream_handles {
            match handle {
                Err(_) => {
                    log::error!("Downstream failed to launch. {handle:#?}");
                    is_ok = false;
                }
                Ok(handle) => {
                    let output = handle.wait();
                    match output {
                        Err(_) => {
                            log::error!("Downstream failed. {handle:#?} {output:#?}");
                            is_ok = false;
                        }
                        Ok(output) => {
                            log::info!("Downstream done. {handle:#?} {output:#?}");
                        }
                    }
                }
            }
        }
        is_ok
    }
}

#[allow(unused)] // This is indeed used, by print!().
#[derive(Debug)]
pub(crate) enum CompilationResult {
    Lexed(Vec<Token>),
    Parsed(c_ast::Program<ParsedCAst>),
    Validated(c_ast::Program<TypeCheckedCAst>, FrontendSymbolTable),
    Tacky(tacky_ast::Program, FrontendSymbolTable),
    AsmCode(asm_ast::Program<FinalizedAsmAst>, BackendSymbolTable),
    AsmFile(AsmFilepath),
}
