use crate::{stage2_parse::c_ast_resolved::*, symbol_table::SymbolTable};
use anyhow::Result;

#[derive(Default)]
pub struct TypeChecker {
    symbol_table: SymbolTable,
}
impl TypeChecker {
    pub fn typecheck_prog(mut self, Program { funs }: &Program) -> Result<SymbolTable> {
        for fun in funs.iter() {
            self.typecheck_decl_fun(fun)?;
        }
        Ok(self.symbol_table)
    }

    fn typecheck_decl_fun(&mut self, fun: &FunctionDeclOrDefn) -> Result<()> {
        match fun {
            FunctionDeclOrDefn::FunDecl(fd) => self.typecheck_decl_fundecl(fd),
            FunctionDeclOrDefn::FunDefn(fd) => self.typecheck_decl_fundefn(fd),
        }
    }
    fn typecheck_decl_block_scope(&mut self, decl: &BlockScopeDeclaration) -> Result<()> {
        match decl {
            BlockScopeDeclaration::VarDecl(vd) => self.typecheck_decl_var(vd),
            BlockScopeDeclaration::FunDecl(fd) => self.typecheck_decl_fundecl(fd),
        }
    }
    fn typecheck_decl_var(
        &mut self,
        VariableDeclaration { ident, init }: &VariableDeclaration,
    ) -> Result<()> {
        self.symbol_table.declare_var(ident)?;

        if let Some(exp) = init {
            self.typecheck_exp(exp)?;
        }

        Ok(())
    }
    fn typecheck_decl_fundecl(
        &mut self,
        FunctionDeclaration { ident, params }: &FunctionDeclaration,
    ) -> Result<()> {
        self.symbol_table
            .declare_or_define_fun(ident, params, false)
    }
    fn typecheck_decl_fundefn(
        &mut self,
        FunctionDefinition {
            decl: FunctionDeclaration { ident, params },
            body,
        }: &FunctionDefinition,
    ) -> Result<()> {
        self.symbol_table
            .declare_or_define_fun(ident, params, true)?;

        for ident in params.iter() {
            self.symbol_table.declare_var(ident)?;
        }

        self.typecheck_block(body)?;

        Ok(())
    }

    fn typecheck_block(&mut self, Block { items }: &Block) -> Result<()> {
        for item in items.iter() {
            self.typecheck_block_item(item)?;
        }
        Ok(())
    }
    fn typecheck_block_item(&mut self, item: &BlockItem) -> Result<()> {
        match item {
            BlockItem::Declaration(decl) => self.typecheck_decl_block_scope(decl),
            BlockItem::Statement(stmt) => self.typecheck_stmt(stmt),
        }
    }

    fn typecheck_stmt(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Return(exp) => self.typecheck_exp(exp)?,
            Statement::Expression(exp) => self.typecheck_exp(exp)?,
            Statement::If(If {
                condition,
                then,
                elze,
            }) => {
                self.typecheck_exp(condition)?;
                self.typecheck_stmt(then)?;
                if let Some(elze) = elze {
                    self.typecheck_stmt(elze)?;
                }
            }
            Statement::Compound(block) => self.typecheck_block(block)?,
            Statement::Break(_loop_id) => { /* No-op. */ }
            Statement::Continue(_loop_id) => { /* No-op. */ }
            Statement::While(_loop_id, CondBody { condition, body }) => {
                self.typecheck_exp(condition)?;
                self.typecheck_stmt(body)?;
            }
            Statement::DoWhile(_loop_id, CondBody { condition, body }) => {
                self.typecheck_stmt(body)?;
                self.typecheck_exp(condition)?;
            }
            Statement::For(
                _loop_id,
                For {
                    init,
                    condition,
                    post,
                    body,
                },
            ) => {
                match init {
                    ForInit::Decl(vd) => self.typecheck_decl_var(vd)?,
                    ForInit::Exp(exp) => self.typecheck_exp(exp)?,
                    ForInit::None => {}
                }
                if let Some(condition) = condition {
                    self.typecheck_exp(condition)?;
                }
                if let Some(post) = post {
                    self.typecheck_exp(post)?;
                }
                self.typecheck_stmt(body)?;
            }
            Statement::Null => {}
        }
        Ok(())
    }

    fn typecheck_exp(&mut self, exp: &Expression) -> Result<()> {
        match exp {
            Expression::Const(_konst) => {}
            Expression::Var(ident) => {
                self.symbol_table.use_var(ident)?;
            }
            Expression::Unary(Unary { op: _, sub_exp }) => {
                self.typecheck_exp(sub_exp)?;
            }
            Expression::Binary(Binary { op: _, lhs, rhs }) => {
                self.typecheck_exp(lhs)?;
                self.typecheck_exp(rhs)?;
            }
            Expression::Assignment(Assignment { ident, rhs }) => {
                self.typecheck_exp(rhs)?;
                self.symbol_table.use_var(ident)?;
            }
            Expression::Conditional(Conditional {
                condition,
                then,
                elze,
            }) => {
                self.typecheck_exp(condition)?;
                self.typecheck_exp(then)?;
                self.typecheck_exp(elze)?;
            }
            Expression::FunctionCall(FunctionCall { ident, args }) => {
                self.symbol_table.call_fun(ident, args)?;
                for arg in args.iter() {
                    self.typecheck_exp(arg)?;
                }
            }
        }
        Ok(())
    }
}
