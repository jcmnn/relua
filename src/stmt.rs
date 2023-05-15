use std::collections::HashMap;

use relua::{cfg::Block, Code, ConstIdx, Error, Instruction, RegIdx};

/// Variable stack
#[derive(Debug)]
struct VarStack {
    map: HashMap<RegIdx, VarId>,
    /// First register from a 'top' instruction
    top: Option<RegIdx> 
}

impl VarStack {
    pub fn new() -> VarStack {
        VarStack {
            map: HashMap::new(),
            top: None,
        }
    }

    /// Gets the variable at the stack position. If no variable exists, creates one
    pub fn get(&mut self, reg: RegIdx, variables: &mut Variables) -> VarId {
        *self.map.entry(reg).or_insert_with(|| variables.add())
    }

    /// Creates a new variable at the stack position and replaces any existing variable
    pub fn set(&mut self, reg: RegIdx, variables: &mut Variables) -> VarId {
        let var = variables.add();
        self.map.insert(reg, var);
        var
    }
}

/// Variable or constant
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarConst {
    Var(VarId),
    Const(ConstIdx),
    Bool(bool),
}

/// Statement builder for contiguous instructions
#[derive(Debug)]
pub struct StatementBuilder<'a> {
    statements: Vec<Statement>,
    variables: &'a mut Variables,
    stack: VarStack,
}

impl StatementBuilder<'_> {
    pub fn build<'a>(
        block: &'a Block,
        code: &Code,
        variables: &'a mut Variables,
    ) -> Result<StatementBuilder<'a>, Error> {
        let mut builder = StatementBuilder {
            statements: Vec::new(),
            variables,
            stack: VarStack::new(),
        };

        for (idx, instruction) in block.iter(code) {
            builder.process(instruction?);
        }

        Ok(builder)
    }

    /// Returns the variable id at a stack position. If no variable exists, creates a new one
    fn get_stack(&mut self, reg: RegIdx) -> VarId {
        self.stack.get(reg, self.variables)
    }

    /// Creates and returns a variable on the stack. Use when setting a register
    fn set_stack(&mut self, reg: RegIdx) -> VarId {
        self.stack.set(reg, self.variables)
    }

    /// Processes an instruction
    fn process(&mut self, i: Instruction) {
        match i {
            Instruction::Move { dst, src } => todo!(),
            Instruction::LoadK { dst, src } => todo!(),
            Instruction::LoadBool {
                dst,
                value,
                skip_next,
            } => todo!(),
            Instruction::LoadNil { dst_begin, dst_end } => todo!(),
            Instruction::GetUpVal { dst, src } => todo!(),
            Instruction::GetGlobal { dst, src } => todo!(),
            Instruction::GetTable { dst, table, key } => todo!(),
            Instruction::SetGlobal { dst, src } => todo!(),
            Instruction::SetUpVal { dst, src } => todo!(),
            Instruction::SetTable { table, key, src } => todo!(),
            Instruction::NewTable {
                dst,
                size_array,
                size_hash,
            } => todo!(),
            Instruction::OpSelf { dst, r_self, key } => todo!(),
            Instruction::Add { dst, left, right } => todo!(),
            Instruction::Sub { dst, left, right } => todo!(),
            Instruction::Mul { dst, left, right } => todo!(),
            Instruction::Div { dst, left, right } => todo!(),
            Instruction::Mod { dst, left, right } => todo!(),
            Instruction::Pow { dst, left, right } => todo!(),
            Instruction::Unm { dst, src } => todo!(),
            Instruction::Not { dst, src } => todo!(),
            Instruction::Len { dst, src } => todo!(),
            Instruction::Concat {
                dst,
                src_begin,
                src_end,
            } => todo!(),
            Instruction::Jmp { offset } => todo!(),
            Instruction::Eq {
                left,
                right,
                polarity,
            } => todo!(),
            Instruction::Lt {
                left,
                right,
                polarity,
            } => todo!(),
            Instruction::Le {
                left,
                right,
                polarity,
            } => todo!(),
            Instruction::Test { src, polarity } => todo!(),
            Instruction::TestSet { src, dst, polarity } => todo!(),
            Instruction::Call {
                call_args,
                return_count,
            } => todo!(),
            Instruction::TailCall { call_args } => todo!(),
            Instruction::Return { src, arg_count } => todo!(),
            Instruction::ForLoop { idx, offset } => todo!(),
            Instruction::ForPrep { idx, offset } => todo!(),
            Instruction::TForLoop { func, return_count } => todo!(),
            Instruction::SetList {
                table,
                offset,
                count,
            } => todo!(),
            Instruction::Close { base } => todo!(),
            Instruction::Closure { proto, dst } => todo!(),
            Instruction::VarArg { dst, count } => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    GetGlobal { dst: VarId, key: ConstIdx },
    Move {
        dst: VarId,
        src: VarConst,
    },

}

/// Conditional expression
#[derive(Debug)]
pub enum ConditionalExpr {
    Eq { left: VarId, right: VarId },
    Lt { left: VarId, right: VarId },
    Le { left: VarId, right: VarId },
    Test { var: VarId, polarity: bool },
}

/// Variable storage
#[derive(Debug)]
pub struct Variables {
    variables: Vec<Variable>,
}

impl Variables {
    /// Creates a new empty variables set
    pub fn new() -> Variables {
        Variables {
            variables: Vec::new(),
        }
    }

    /// Creates a new named variable. Returns the associated [VarId]
    pub fn add_named(&mut self, name: String) -> VarId {
        let id = self.variables.len();
        self.variables.push(Variable { label: Some(name) });
        VarId(id)
    }

    /// Creates a new unnamed variable. Returns the associated [VarId]
    pub fn add(&mut self) -> VarId {
        let id = self.variables.len();
        self.variables.push(Variable { label: None });
        VarId(id)
    }

    /// Returns a reference to the variable associated with `id`
    ///
    /// Returns [None] if `id` is not associated with any variable
    pub fn get(&self, id: VarId) -> Option<&Variable> {
        self.variables.get(id.0)
    }

    /// Returns a mutable reference to the variable associated with `id`
    ///
    /// Returns [None] if `id` is not associated with any variable
    pub fn get_mut(&mut self, id: VarId) -> Option<&mut Variable> {
        self.variables.get_mut(id.0)
    }
}

impl Default for Variables {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(usize);

#[derive(Debug)]
pub struct Variable {
    pub label: Option<String>,
}

impl Variable {
    /// Sets the variable name
    pub fn set_name(&mut self, name: String) {
        self.label = Some(name);
    }

    /// Clears the variable name
    pub fn clear_name(&mut self) {
        self.label = None;
    }

    /// Returns the variable name or [None] if unnamed
    pub fn name(&self) -> Option<&str> {
        self.label.as_deref()
    }
}
