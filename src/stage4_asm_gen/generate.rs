use crate::{
    common::{
        symbol_table_backend::BackendSymbolTable, symbol_table_frontend::SymbolTable,
        types_backend::Alignment,
    },
    ds_n_a::immutable_owned::ImmutableOwned,
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::InstrsGenerator,
        phase2_finalize::{FinalizedAsmAst, InstrsFinalizer},
    },
};
use std::rc::Rc;

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
        t::Program { static_vars, funs }: t::Program,
    ) -> (Program<FinalizedAsmAst>, ImmutableOwned<BackendSymbolTable>) {
        let static_vars = static_vars
            .into_iter()
            .map(Self::convert_static_var)
            .collect::<Vec<_>>();

        let (funs, backend_symtab) = self.convert_funs(funs);

        /* In ch16, we could consolidate Program::static_consts and intra-backend-symtab static const AsmObjs. */
        let static_consts =
            Self::clone_static_readonly_objs_from_backend_symtab_to_asm_ast(&backend_symtab);

        let prog = Program { static_vars, static_consts, funs };

        (prog, backend_symtab)
    }

    /* Tacky StaticVariable */

    fn convert_static_var(
        t::StaticVariable { ident, visibility, typ, inits }: t::StaticVariable,
    ) -> StaticVariable {
        let alignment = Alignment::default_of_obj_type(&typ);
        StaticVariable {
            ident,
            visibility,
            alignment,
            inits,
        }
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

        let mut gen = InstrsGenerator::new(self.frontend_symtab);
        let prefinal_funs = funs
            .into_iter()
            .map(|t_fun| gen.convert_fun(t_fun))
            .collect::<Vec<_>>();
        let (frontend_symtab, mut backend_symtab) = gen.into();

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

    fn clone_static_readonly_objs_from_backend_symtab_to_asm_ast(
        backend_symtab: &BackendSymbolTable,
    ) -> Vec<StaticConstant> {
        backend_symtab
            .static_readonly_to_ident()
            .iter()
            .map(|((align, konst), ident)| {
                let ident = Rc::clone(ident);
                let alignment = *align;
                let init = *konst;
                StaticConstant { ident, alignment, init }
            })
            .collect()
    }
}
