use clap::Parser as ClapParser;
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

pub struct Args {
    pub src_filepaths: Vec<PathBuf>,

    pub until: DriverUntil,

    pub lib_names: Vec<String>,
}
impl From<CliArgs> for Args {
    fn from(cli_args: CliArgs) -> Self {
        let until = if cli_args.until_lexer {
            DriverUntil::Compiler(CompilerUntil::Lexer)
        } else if cli_args.until_parser {
            DriverUntil::Compiler(CompilerUntil::Parser)
        } else if cli_args.until_parser_validate {
            DriverUntil::Compiler(CompilerUntil::ParserValidate)
        } else if cli_args.until_tacky {
            DriverUntil::Compiler(CompilerUntil::Tacky)
        } else if cli_args.until_asm_codegen {
            DriverUntil::Compiler(CompilerUntil::AsmGen)
        } else if cli_args.until_asm_emission {
            DriverUntil::Compiler(CompilerUntil::AsmEmit)
        } else if cli_args.until_assembler {
            DriverUntil::Downstream(Downstream::Assembler)
        } else {
            DriverUntil::Downstream(Downstream::Linker)
        };

        Self {
            src_filepaths: cli_args.src_filepaths,
            until,
            lib_names: cli_args.lib_names,
        }
    }
}

#[derive(PartialEq, Eq)]
pub enum DriverUntil {
    Compiler(CompilerUntil),
    Downstream(Downstream),
}
#[derive(PartialEq, Eq)]
pub enum CompilerUntil {
    Lexer,
    Parser,
    ParserValidate,
    Tacky,
    AsmGen,
    AsmEmit,
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Downstream {
    Assembler,
    Linker,
}
