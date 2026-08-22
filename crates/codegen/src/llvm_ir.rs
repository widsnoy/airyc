use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::values::{FunctionValue, GlobalValue, PointerValue};
use inkwell::{builder::Builder, context::Context};
use syntax::ast::*;

use crate::error::{CodegenError, Result};

mod decl;
mod expr;
mod func;
mod stmt;

/// 变量和函数的符号表
#[derive(Default)]
pub struct SymbolTable<'a, 'ctx> {
    pub current_function: Option<FunctionValue<'ctx>>,
    pub scopes: Vec<HashMap<String, Symbol<'a, 'ctx>>>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    pub globals: HashMap<String, Symbol<'a, 'ctx>>,
    pub loop_stack: Vec<LoopContext<'ctx>>,
}

pub struct Program<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a inkwell::module::Module<'ctx>,
    pub analyzer: &'a analyzer::module::Module,
    pub symbols: SymbolTable<'a, 'ctx>,
    pub string_constants: HashMap<String, GlobalValue<'ctx>>,
}

#[derive(Clone, Copy)]
pub struct Symbol<'a, 'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub ty: &'a analyzer::r#type::Ty,
}

impl<'a, 'ctx> Symbol<'a, 'ctx> {
    pub fn new(ptr: PointerValue<'ctx>, ty: &'a analyzer::r#type::Ty) -> Self {
        Self { ptr, ty }
    }
}

pub struct LoopContext<'ctx> {
    pub cond_bb: BasicBlock<'ctx>,
    pub end_bb: BasicBlock<'ctx>,
}

impl<'a, 'ctx> Program<'a, 'ctx> {
    pub fn compile_comp_unit(&mut self, node: CompUnit) -> Result<()> {
        if let Some(ref metadata) = self.analyzer.metadata {
            for func_id in self.analyzer.function_map.values() {
                if func_id.module != self.analyzer.file_id
                    && let Some(thin_module) = metadata.get(&func_id.module)
                    && let Some(func_info) = thin_module.functions.get(func_id.index)
                {
                    self.declare_function(func_info)?;
                }
            }
        }

        // 先声明本模块的所有函数，保证函数定义顺序不影响调用。
        for global in node.global_decls() {
            if let GlobalDecl::FuncDef(func) = global {
                let sign = func
                    .sign()
                    .ok_or(CodegenError::Missing("function signature"))?;
                self.compile_func_signature(sign)?;
            }
        }

        for global in node.global_decls() {
            match global {
                GlobalDecl::VarDef(decl) => self.compile_var_def(decl)?,
                GlobalDecl::FuncDef(func) => {
                    self.compile_func_body(func.sign().and_then(|n| n.name()), func.block())?
                }
                GlobalDecl::StructDef(_) => {}
            }
        }
        Ok(())
    }
}
