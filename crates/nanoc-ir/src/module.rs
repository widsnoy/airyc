use std::{
    collections::{HashMap, HashSet},
    env::var,
    ops::Deref,
};

use text_size::{TextRange, TextSize};
use thunderdome::Arena;

use crate::ntype::{NType, Value};

#[derive(Default)]
pub struct Module {
    pub variables: Arena<Variable>, // 所有 scope 的都存在这里
    pub functions: Arena<Function>,
    pub scopes: Arena<Scope>,

    pub global_scope: ScopeID,
    // 检查是否是编译期可计算的常量节点
    pub constant_nodes: HashSet<TextRange>,

    // 记录编译时的在值，有的非 const 也可以求
    pub value_table: HashMap<TextRange, Value>,

    // 分析的时候上下文，使用后清除
    pub(super) analyzing: AnalyzeContext,
}

#[derive(Default)]
pub(super) struct AnalyzeContext {
    pub(super) current_scope: ScopeID,
    pub(super) errors: Vec<SemanticError>,
}

pub enum SemanticError {
    TypeMismatch {
        expected: NType,
        found: NType,
        range: TextRange,
    },
    ConstantExprExpected {
        range: TextRange,
    },
}

impl Module {
    pub fn mark_constant(&mut self, range: TextRange) {
        self.constant_nodes.insert(range);
    }

    pub fn is_constant(&self, range: TextRange) -> bool {
        self.constant_nodes.contains(&range)
    }

    pub fn new_scope(&mut self, parent: Option<ScopeID>) -> ScopeID {
        let scope = Scope {
            parent,
            variables: HashMap::new(),
        };
        let id = self.scopes.insert(scope);
        ScopeID(id)
    }

    pub fn new_function(
        &mut self,
        name: String,
        params: Vec<VariableID>,
        ret_type: NType,
    ) -> FunctionID {
        let function = Function {
            name,
            params,
            ret_type,
        };
        let id = self.functions.insert(function);
        FunctionID(id)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct VariableID(pub thunderdome::Index);
impl VariableID {
    pub fn none() -> Self {
        VariableID(thunderdome::Index::DANGLING)
    }
}
impl From<thunderdome::Index> for VariableID {
    fn from(index: thunderdome::Index) -> Self {
        VariableID(index)
    }
}
impl Deref for VariableID {
    type Target = thunderdome::Index;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Variable {
    pub name: String,
    pub ty: NType,
    pub range: TextRange,
    pub tag: VariableTag,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum VariableTag {
    Define,
    Write,
    Read,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct FunctionID(pub thunderdome::Index);
impl FunctionID {
    pub fn none() -> Self {
        FunctionID(thunderdome::Index::DANGLING)
    }
}
impl From<thunderdome::Index> for FunctionID {
    fn from(index: thunderdome::Index) -> Self {
        FunctionID(index)
    }
}
impl Deref for FunctionID {
    type Target = thunderdome::Index;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Function {
    pub name: String,
    pub params: Vec<VariableID>,
    pub ret_type: NType,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ScopeID(pub thunderdome::Index);

impl ScopeID {
    pub fn none() -> Self {
        ScopeID(thunderdome::Index::DANGLING)
    }
}

impl Default for ScopeID {
    fn default() -> Self {
        Self::none()
    }
}

impl From<thunderdome::Index> for ScopeID {
    fn from(index: thunderdome::Index) -> Self {
        ScopeID(index)
    }
}

impl Deref for ScopeID {
    type Target = thunderdome::Index;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Scope {
    pub parent: Option<ScopeID>,
    pub variables: HashMap<String, HashSet<VariableID>>,
}

impl Scope {
    pub fn new_variable(
        &mut self,
        variables: &mut Arena<Variable>,
        name: String,
        ty: NType,
        range: TextRange,
        tag: VariableTag,
    ) -> VariableID {
        let idx = variables.insert(Variable {
            name: name.clone(),
            ty,
            range,
            tag,
        });
        let var_id = VariableID(idx);
        let entry = self.variables.entry(name).or_default();
        entry.insert(var_id);
        var_id
    }

    /// 查找变量
    pub fn look_up(&self, m: &Module, var_name: &str, var_tag: VariableTag) -> Option<VariableID> {
        let mut u_opt = Some(self);
        while let Some(u) = u_opt {
            if let Some(entry) = u.variables.get(var_name)
                && let Some(idx) = entry.iter().find(|x| {
                    let var = m.variables.get(***x).unwrap();
                    var.tag == var_tag
                })
            {
                return Some(*idx);
            }
            u_opt = u.parent.map(|x| m.scopes.get(*x).unwrap());
        }
        None
    }
}
