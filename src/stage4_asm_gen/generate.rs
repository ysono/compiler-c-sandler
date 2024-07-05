use crate::{
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::InstrsGenerator,
        phase2_finalize::{FinalizedAsmAst, InstrsFinalizer},
    },
    symbol_table_backend::BackendSymbolTable,
    symbol_table_frontend::SymbolTable,
    types_backend::Alignment,
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

        let (funs, backend_symtab) = self.convert_funs(funs);

        let prog = Program { static_vars, funs };

        (prog, backend_symtab)
    }

    /* Tacky StaticVariable */

    fn convert_static_var(
        t::StaticVariable { ident, visibility, typ, init }: t::StaticVariable,
    ) -> StaticVariable {
        let alignment = Alignment::from(typ);
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
    ) -> (Vec<Function<FinalizedAsmAst>>, BackendSymbolTable) {
        /* Instrs phase 1 */

        let gen = InstrsGenerator::new(&self.frontend_symtab);
        let funs = funs
            .into_iter()
            .map(|t_fun| gen.convert_fun(t_fun))
            .collect::<Vec<_>>();

        /* Preparing for instrs phase 2 and 3 */

        let backend_symtab = BackendSymbolTable::from(self.frontend_symtab);
        let backend_symtab = Rc::new(backend_symtab);

        /* Instrs phase 2 and 3 */

        let funs = funs
            .into_iter()
            .map(|fun| {
                let mut fin = InstrsFinalizer::new(Rc::clone(&backend_symtab));
                fin.finalize_fun(fun)
            })
            .collect::<Vec<_>>();

        let backend_symtab = Rc::try_unwrap(backend_symtab).ok().unwrap();

        (funs, backend_symtab)
    }
}
