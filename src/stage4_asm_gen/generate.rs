use crate::{
    common::{
        symbol_table_backend::{AsmEntry, BackendSymbolTable, ObjLocation},
        symbol_table_frontend::SymbolTable,
        types_backend::{Alignment, AssemblyType},
    },
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::InstrsGenerator,
        phase2_finalize::{FinalizedAsmAst, InstrsFinalizer},
    },
};
use std::rc::Rc;

pub struct AsmCodeGenerator {
    frontend_symtab: SymbolTable,
}
impl AsmCodeGenerator {
    pub fn new(frontend_symtab: SymbolTable) -> Self {
        Self { frontend_symtab }
    }

    pub fn gen_program(
        self,
        t::Program { static_vars, funs }: t::Program,
    ) -> (Program<FinalizedAsmAst>, BackendSymbolTable) {
        let static_vars = static_vars
            .into_iter()
            .map(Self::convert_static_var)
            .collect::<Vec<_>>();

        let (funs, static_consts, backend_symtab) = self.convert_funs(funs);

        let prog = Program { static_vars, funs, static_consts };

        (prog, backend_symtab)
    }

    /* Tacky StaticVariable */

    fn convert_static_var(
        t::StaticVariable { ident, visibility, typ, init }: t::StaticVariable,
    ) -> StaticVariable {
        let alignment = Alignment::default_of(typ);
        StaticVariable {
            ident,
            visibility,
            alignment,
            init,
        }
    }

    /* Tacky Function */

    fn convert_funs(
        self,
        funs: Vec<t::Function>,
    ) -> (
        Vec<Function<FinalizedAsmAst>>,
        Vec<StaticConstant>,
        BackendSymbolTable,
    ) {
        /* Instrs phase 1 */

        let mut gen = InstrsGenerator::new(self.frontend_symtab);
        let funs = funs
            .into_iter()
            .map(|t_fun| gen.convert_fun(t_fun))
            .collect::<Vec<_>>();
        let (frontend_symtab, static_consts) = gen.into();

        /* Preparing for instrs phase 2 and 3 */

        let backend_symtab = BackendSymbolTable::from(frontend_symtab);
        let backend_symtab = Rc::new(backend_symtab);

        /* Instrs phase 2 and 3 */

        let funs = funs
            .into_iter()
            .map(|fun| {
                let fin = InstrsFinalizer::new(Rc::clone(&backend_symtab));
                fin.finalize_fun(fun)
            })
            .collect::<Vec<_>>();

        /* Static consts */

        let mut backend_symtab = Rc::try_unwrap(backend_symtab).ok().unwrap();

        /* It's not necessary to add static consts into the backend symbol table earlier, b/c
        they aren't read by the finalizer (phase 2) and the fixer (phase 3). */
        let static_consts = static_consts
            .into_iter()
            .map(|((alignment, init), ident)| {
                backend_symtab.insert(
                    Rc::clone(&ident),
                    AsmEntry::Obj {
                        asm_type: AssemblyType::from(init.var_type()),
                        // storage_duration: StorageDuration::Static,
                        // constness: Constness::Const,
                        loc: ObjLocation::StaticReadonly,
                    },
                );

                StaticConstant { ident, alignment, init }
            })
            .collect::<Vec<_>>();

        (funs, static_consts, backend_symtab)
    }
}
