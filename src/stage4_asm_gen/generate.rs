use crate::{
    common::{
        identifier::SymbolIdentifier,
        primitive::Const,
        symbol_table_backend::{AsmObj, BackendSymbolTable, ObjLocation},
        symbol_table_frontend::SymbolTable,
        types_backend::{Alignment, AssemblyType},
    },
    ds_n_a::immutable_owned::ImmutableOwned,
    stage3_tacky::tacky_ast as t,
    stage4_asm_gen::{
        asm_ast::*,
        phase1_generate::InstrsGenerator,
        phase2_finalize::{FinalizedAsmAst, InstrsFinalizer},
    },
};
use std::{collections::HashMap, rc::Rc};

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

        let (funs, static_consts, backend_symtab) = self.convert_funs(funs);

        let prog = Program { static_vars, static_consts, funs };

        (prog, backend_symtab)
    }

    /* Tacky StaticVariable */

    fn convert_static_var(
        t::StaticVariable { ident, visibility, typ, init }: t::StaticVariable,
    ) -> StaticVariable {
        let alignment = Alignment::default_of(typ.effective_arithmetic_type());
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
        ImmutableOwned<BackendSymbolTable>,
    ) {
        /* Instrs phase 1 */

        let mut gen = InstrsGenerator::new(self.frontend_symtab);
        let prefinal_funs = funs
            .into_iter()
            .map(|t_fun| gen.convert_fun(t_fun))
            .collect::<Vec<_>>();
        let (frontend_symtab, static_consts) = gen.into();

        /* Preparing for instrs phase 2 and 3 */

        let frontend_symtab = frontend_symtab.into_inner();
        let mut backend_symtab = BackendSymbolTable::from(frontend_symtab);
        let static_consts = Self::reorganize_static_consts(static_consts, &mut backend_symtab);
        let mut backend_symtab = ImmutableOwned::from(backend_symtab);

        /* Instrs phase 2 and 3 */

        let mut final_funs = Vec::with_capacity(prefinal_funs.len());
        for in_fun in prefinal_funs {
            let finzer = InstrsFinalizer::new(backend_symtab);
            let out_fun;
            (out_fun, backend_symtab) = finzer.finalize_fun(in_fun);
            final_funs.push(out_fun);
        }

        (final_funs, static_consts, backend_symtab)
    }

    fn reorganize_static_consts(
        deduped_static_consts: HashMap<(Alignment, Const), Rc<SymbolIdentifier>>,
        backend_symtab: &mut BackendSymbolTable,
    ) -> Vec<StaticConstant> {
        deduped_static_consts
            .into_iter()
            .map(|((alignment, init), ident)| {
                /* Note, static consts in the backend symtab are never retrieved,
                    by asm_gen phase2 or phase3 or by asm_emit.
                In ch16, we could consolidate Program::static_consts and intra-backend-symtab static const AsmObjs.
                */
                backend_symtab.objs_mut().insert(
                    Rc::clone(&ident),
                    AsmObj {
                        asm_type: AssemblyType::from(init.arithmetic_type()),
                        loc: ObjLocation::StaticReadonly,
                    },
                );

                StaticConstant { ident, alignment, init }
            })
            .collect::<Vec<_>>()
    }
}
