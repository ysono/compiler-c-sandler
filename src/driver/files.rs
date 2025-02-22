use anyhow::{Result, anyhow};
use derive_more::Deref;
use std::{ffi::OsStr, path::PathBuf};

#[derive(Deref, Debug)]
pub struct SrcFilepath(PathBuf);
impl TryFrom<PathBuf> for SrcFilepath {
    type Error = anyhow::Error;
    fn try_from(p: PathBuf) -> Result<Self> {
        if p.extension() == Some(OsStr::new("c")) {
            Ok(Self(p))
        } else {
            Err(anyhow!("The c source code file must have extension `.c`."))
        }
    }
}

#[derive(Deref, Debug)]
pub struct AsmFilepath(PathBuf);
impl From<&SrcFilepath> for AsmFilepath {
    fn from(src_filepath: &SrcFilepath) -> Self {
        let mut asm_filepath = PathBuf::from(&src_filepath.0);
        asm_filepath.set_extension("s");
        Self(asm_filepath)
    }
}

#[derive(Deref, Debug)]
pub struct ObjectFilepath(PathBuf);
impl From<&AsmFilepath> for ObjectFilepath {
    fn from(asm_filepath: &AsmFilepath) -> Self {
        let mut obj_filepath = PathBuf::from(&asm_filepath.0);
        obj_filepath.set_extension("o");
        Self(obj_filepath)
    }
}

#[derive(Deref, Debug)]
pub struct ProgramFilepath(PathBuf);
impl From<&AsmFilepath> for ProgramFilepath {
    fn from(asm_filepath: &AsmFilepath) -> Self {
        let mut prog_filepath = PathBuf::from(&asm_filepath.0);
        prog_filepath.set_extension("");
        Self(prog_filepath)
    }
}
