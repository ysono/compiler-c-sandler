use crate::{
    common::{symbol_table_backend::BackendSymbolTable, symbol_table_frontend::SymbolTable},
    ds_n_a::immutable_owned::ImmutableOwned,
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::InstrsGenerator,
        phase2_finalize::{FinalizedAsmAst, InstrsFinalizer},
    },
};

pub struct AsmCodeGenerator {
    frontend_symtab: ImmutableOwned<SymbolTable>,
}
impl AsmCodeGenerator {
    pub fn new(frontend_symtab: SymbolTable) -> Self {
        let frontend_symtab = ImmutableOwned::from(frontend_symtab);
        Self { frontend_symtab }
    }

    pub fn gen_program(
        self,
        t::Program { funs }: t::Program,
    ) -> (Program<FinalizedAsmAst>, ImmutableOwned<BackendSymbolTable>) {
        let (funs, backend_symtab) = self.convert_funs(funs);

        let prog = Program { funs };

        (prog, backend_symtab)
    }

    /* Tacky Function */

    fn convert_funs(
        self,
        funs: Vec<t::Function>,
    ) -> (
        Vec<Function<FinalizedAsmAst>>,
        ImmutableOwned<BackendSymbolTable>,
    ) {
        /* Instrs phase 1 */

        let mut gener = InstrsGenerator::new(self.frontend_symtab);
        let prefinal_funs = funs
            .into_iter()
            .map(|t_fun| gener.convert_fun(t_fun))
            .collect::<Vec<_>>();
        let (frontend_symtab, mut backend_symtab) = gener.into();

        /* Preparing for instrs phase 2 and 3 */

        backend_symtab.merge_symbols_from(frontend_symtab.into_inner());
        let mut backend_symtab = ImmutableOwned::from(backend_symtab);

        /* Instrs phase 2 and 3 */

        let mut final_funs = Vec::with_capacity(prefinal_funs.len());
        for in_fun in prefinal_funs {
            let finzer = InstrsFinalizer::new(backend_symtab);
            let out_fun;
            (out_fun, backend_symtab) = finzer.finalize_fun(in_fun);
            final_funs.push(out_fun);
        }

        (final_funs, backend_symtab)
    }
}
