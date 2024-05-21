use anyhow::{anyhow, Result};
use derive_more::Deref;
use std::path::PathBuf;

#[derive(Deref, Debug)]
pub struct SrcFilepath(PathBuf);
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
pub struct PreprocessedFilepath(PathBuf);
impl<'a> From<&'a SrcFilepath> for PreprocessedFilepath {
    fn from(src_filepath: &'a SrcFilepath) -> Self {
        let mut pp_filepath = PathBuf::from(src_filepath as &PathBuf);
        pp_filepath.set_extension("i");
        Self(pp_filepath)
    }
}

#[derive(Deref, Debug)]
pub struct AsmFilepath(PathBuf);
impl<'a> From<&'a PreprocessedFilepath> for AsmFilepath {
    fn from(pp_filepath: &'a PreprocessedFilepath) -> Self {
        let mut asm_filepath = PathBuf::from(pp_filepath as &PathBuf);
        asm_filepath.set_extension("s");
        Self(asm_filepath)
    }
}

#[derive(Deref, Debug)]
pub struct ProgramFilepath(PathBuf);
impl<'a> From<&'a AsmFilepath> for ProgramFilepath {
    fn from(asm_filepath: &'a AsmFilepath) -> Self {
        let mut prog_filepath = PathBuf::from(asm_filepath as &PathBuf);
        prog_filepath.set_extension("");
        Self(prog_filepath)
    }
}
