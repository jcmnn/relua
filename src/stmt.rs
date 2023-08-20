use std::collections::HashMap;

use crate::{
    cfg::Block, CallArgs, Code, ConstIdx, Error, Instruction, ProtoIdx, RegConst, RegIdx, UpValIdx,
};

/// Variable stack
#[derive(Debug)]
pub struct VarStack {
    map: HashMap<RegIdx, VarId>,
    /// First register from a 'top' instruction
    top_set: Option<RegIdx>,
    top_get: Option<RegIdx>,
}

impl VarStack {
    fn new() -> VarStack {
        VarStack {
            map: HashMap::new(),
            top_set: None,
            top_get: None,
        }
    }

    /// Gets the variable at the stack position. If no variable exists, creates one
    pub fn get(&mut self, reg: RegIdx, variables: &mut Variables) -> VarId {
        if let Some(top) = self.top_set {
            if reg > top {
                panic!("Attempt to get single register returned from 'top' instruction");
            }
        }
        *self.map.entry(reg).or_insert_with(|| variables.add())
    }

    pub fn get_top(&mut self, base: RegIdx, variables: &mut Variables) -> VarArg {
        self.top_get = Some(base);
        VarArg(base)
    }

    pub fn set_top(&mut self, base: RegIdx, variables: &mut Variables) -> VarArg {
        self.top_set = Some(base);
        VarArg(base)
    }

    /// Creates a new variable at the stack position and replaces any existing variable
    pub fn set(&mut self, reg: RegIdx, variables: &mut Variables) -> VarId {
        if let Some(top) = self.top_set {
            if reg > top {
                panic!("Attempt to set single register returned from 'top' instruction");
            }
        }
        let var = variables.add();
        self.map.insert(reg, var);
        var
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VarArg(RegIdx);

/// Variable or constant
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarConst {
    Var(VarId),
    Const(ConstIdx),
    Bool(bool),
    Nil,
    UpValue(UpValIdx),
    Global(ConstIdx),
}

impl From<VarId> for VarConst {
    fn from(var: VarId) -> Self {
        VarConst::Var(var)
    }
}

impl From<ConstIdx> for VarConst {
    fn from(c: ConstIdx) -> Self {
        VarConst::Const(c)
    }
}

#[derive(Debug)]
pub struct BlockStatements {
    pub statements: Vec<Statement>,
    pub stack: VarStack,
}

impl BlockStatements {
    pub fn build(
        block: &Block,
        code: &Code,
        variables: &mut Variables,
    ) -> Result<BlockStatements, Error> {
        let mut builder = StatementBuilder {
            statements: Vec::new(),
            variables,
            stack: VarStack::new(),
        };

        for (idx, instruction) in block.iter(code) {
            builder.process(instruction?);
        }

        Ok(BlockStatements {
            statements: builder.statements,
            stack: builder.stack,
        })
    }
}

/// Statement builder for contiguous instructions
#[derive(Debug)]
struct StatementBuilder<'a> {
    pub statements: Vec<Statement>,
    variables: &'a mut Variables,
    stack: VarStack,
}

impl StatementBuilder<'_> {
    /// Returns the variable id at a stack position. If no variable exists, creates a new one
    fn get_stack(&mut self, reg: RegIdx) -> VarId {
        self.stack.get(reg, self.variables)
    }

    fn get_stack_or_const(&mut self, rc: RegConst) -> VarConst {
        match rc {
            RegConst::Reg(reg) => VarConst::Var(self.get_stack(reg)),
            RegConst::Const(c) => VarConst::Const(c),
        }
    }

    /// Creates and returns a variable on the stack. Use when setting a register
    fn set_stack(&mut self, reg: RegIdx) -> VarId {
        self.stack.set(reg, self.variables)
    }

    fn get_top(&mut self, base: RegIdx) -> VarArg {
        self.stack.get_top(base, self.variables)
    }

    fn set_top(&mut self, base: RegIdx) -> VarArg {
        self.stack.set_top(base, self.variables)
    }

    /// Creates an returns variables associated with a range of registers.
    fn set_contiguous_stack(&mut self, reg_begin: RegIdx, reg_end: RegIdx) -> Vec<VarId> {
        (reg_begin.0..reg_end.0)
            .map(|reg| self.set_stack(RegIdx(reg)))
            .collect()
        /*let first = self.set_stack(reg_first);
        let last = if reg_first == reg_last {
            first
        } else {
            let mut dst = reg_first + 1usize;
            while dst < reg_last {
                // TODO: Verify that this variable id increments by 1 each iteration
                self.set_stack(dst);
                dst = dst + 1usize;
            }
            self.set_stack(reg_last)
        };
        assert!(
            last.0 - first.0 == reg_last.0 - reg_first.0,
            "reg = ({}, {}), var = ({}, {})",
            first.0,
            last.0,
            reg_first.0,
            reg_last.0
        );

        (first, last)*/
    }

    /// Creates an returns variables associated with a range of registers.
    fn get_contiguous_stack(&mut self, reg_begin: RegIdx, reg_end: RegIdx) -> Vec<VarId> {
        //let first = self.set_stack(reg_first);
        (reg_begin.0..reg_end.0)
            .map(|reg| self.get_stack(RegIdx(reg)))
            .collect()
    }

    fn call(&mut self, args: &CallArgs) -> Call {
        let func = self.get_stack(args.dst);

        let args = if let Some(arg_count) = args.arg_count {
            VarOrArgs::Args(
                self.get_contiguous_stack(args.dst + 1usize, args.dst + arg_count + 1usize),
            )
        } else {
            VarOrArgs::Var(self.get_top(args.dst + 1usize))
        };

        Call { func, args }
    }

    /// Processes an instruction
    fn process(&mut self, i: Instruction) {
        let stmt = match i {
            Instruction::Move { dst, src } => Statement::Move {
                dst: self.set_stack(dst),
                src: self.get_stack(src).into(),
            },
            Instruction::LoadK { dst, src } => Statement::Move {
                dst: self.set_stack(dst),
                src: src.into(),
            },
            Instruction::LoadBool {
                dst,
                value,
                skip_next: _,
            } => Statement::Move {
                dst: self.set_stack(dst),
                src: VarConst::Bool(value),
            },
            Instruction::LoadNil { dst_begin, dst_end } => {
                let dsts = self.set_contiguous_stack(dst_begin, dst_end + 1usize);
                Statement::LoadNil { dsts }
            }

            Instruction::GetUpVal { dst, src } => Statement::Move {
                dst: self.set_stack(dst),
                src: VarConst::UpValue(src),
            },
            Instruction::GetGlobal { dst, key } => Statement::Move {
                dst: self.set_stack(dst),
                src: VarConst::Global(key),
            },
            Instruction::GetTable { dst, table, key } => Statement::GetTable {
                dst: self.set_stack(dst),
                table: self.get_stack(table),
                key: self.get_stack_or_const(key),
            },
            Instruction::SetGlobal { dst, src } => Statement::SetGlobal {
                key: dst,
                src: self.get_stack(src),
            },
            Instruction::SetUpVal { dst, src } => Statement::SetUpValue {
                dst,
                src: self.get_stack(src),
            },
            Instruction::SetTable { table, key, src } => Statement::SetTable {
                table: self.get_stack(table),
                key: self.get_stack_or_const(key),
                value: self.get_stack_or_const(src),
            },
            Instruction::NewTable {
                dst,
                size_array,
                size_hash,
            } => Statement::NewTable {
                dst: self.set_stack(dst),
                size_array,
                size_hash,
            },
            Instruction::OpSelf { dst, r_self, key } => Statement::OpSelf {
                dst: self.set_stack(dst),
                dst_self: self.set_stack(dst + 1usize),
                r_self: self.get_stack(r_self),
                key: self.get_stack_or_const(key),
            },
            Instruction::Add { dst, left, right } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Add {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
            },
            Instruction::Sub { dst, left, right } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Sub {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
            },
            Instruction::Mul { dst, left, right } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Mul {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
            },
            Instruction::Div { dst, left, right } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Div {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
            },
            Instruction::Mod { dst, left, right } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Mod {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
            },
            Instruction::Pow { dst, left, right } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Pow {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
            },
            Instruction::Unm { dst, src } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Unm {
                    src: self.get_stack(src),
                },
            },
            Instruction::Not { dst, src } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Not {
                    src: self.get_stack(src),
                },
            },
            Instruction::Len { dst, src } => Statement::Operation {
                dst: self.set_stack(dst),
                operation: Operation::Len {
                    src: self.get_stack(src),
                },
            },
            Instruction::Concat {
                dst,
                src_begin,
                src_end,
            } => {
                let args = self.get_contiguous_stack(src_begin, src_end + 1usize);
                Statement::Concat {
                    dst: self.set_stack(dst),
                    args,
                }
            }
            // JMP doesn't generate a statement
            Instruction::Jmp { offset } => return,
            Instruction::Eq {
                left,
                right,
                polarity,
            } => Statement::Conditional {
                expr: ConditionalExpr::Eq {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
                polarity,
            },
            Instruction::Lt {
                left,
                right,
                polarity,
            } => Statement::Conditional {
                expr: ConditionalExpr::Lt {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
                polarity,
            },
            Instruction::Le {
                left,
                right,
                polarity,
            } => Statement::Conditional {
                expr: ConditionalExpr::Le {
                    left: self.get_stack_or_const(left),
                    right: self.get_stack_or_const(right),
                },
                polarity,
            },
            Instruction::Test { src, polarity } => Statement::Conditional {
                expr: ConditionalExpr::Test {
                    var: self.get_stack(src),
                },
                polarity,
            },
            Instruction::TestSet { src, dst, polarity } => Statement::Conditional {
                expr: ConditionalExpr::TestSet {
                    dst: self.set_stack(dst),
                    var: self.get_stack(src),
                },
                polarity,
            },
            Instruction::Call {
                call_args,
                return_count,
            } => {
                let call = self.call(&call_args);
                let returns = if let Some(return_count) = return_count {
                    VarOrArgs::Args(
                        self.set_contiguous_stack(call_args.dst, call_args.dst + return_count),
                    )
                } else {
                    VarOrArgs::Var(self.get_top(call_args.dst))
                };

                Statement::Call { call, returns }
            }
            Instruction::TailCall { call_args } => {
                let call = self.call(&call_args);

                Statement::TailCall {
                    call,
                    returns: VarArg(call_args.dst),
                }
            }
            Instruction::Return { src, arg_count } => {
                let args = match arg_count {
                    Some(count) => VarOrArgs::Args(self.get_contiguous_stack(src, src + count)),
                    None => VarOrArgs::Var(self.get_top(src)),
                };

                Statement::Return { args }
            }
            Instruction::ForLoop { idx, offset: _ } => Statement::ForLoop {
                idx: self.get_stack(idx),
                idx_set: self.set_stack(idx),
                limit: self.get_stack(idx + 1usize),
                step: self.get_stack(idx + 2usize),
                idx_ext: self.set_stack(idx + 3usize),
            },
            Instruction::ForPrep { init, offset: _ } => Statement::ForPrep {
                init: self.get_stack(init),
                idx_set: self.set_stack(init),
                plimit: self.get_stack(init + 1usize),
                pstep: self.get_stack(init + 2usize),
            },
            Instruction::TForLoop { func, return_count } => {
                let func_var = self.get_stack(func);
                let state = self.get_stack(func + 1usize);
                let idx = self.get_stack(func + 2usize);
                let returns =
                    self.set_contiguous_stack(func + 3usize, func + 3usize + return_count);
                let idx_set = self.set_stack(func + 2usize);
                Statement::TForLoop {
                    func: func_var,
                    state,
                    idx,
                    returns,
                    idx_set,
                }
            }
            Instruction::SetList {
                table,
                offset,
                count,
            } => {
                let args = match count {
                    Some(count) => VarOrArgs::Args(
                        self.get_contiguous_stack(table + 1usize, table + 1usize + count),
                    ),
                    None => VarOrArgs::Var(self.get_top(table + 1usize)),
                };

                Statement::SetList {
                    table: self.get_stack(table),
                    offset,
                    args,
                }
            }
            Instruction::Close { base } => todo!(),
            Instruction::Closure { proto, dst } => Statement::Closure {
                dst: self.set_stack(dst),
                proto,
            },
            Instruction::VarArg { dst, count } => {
                let args = match count {
                    Some(count) => VarOrArgs::Args(self.set_contiguous_stack(dst, dst + count)),
                    None => VarOrArgs::Var(self.set_top(dst)),
                };

                Statement::VarArg { args }
            }
        };

        self.statements.push(stmt);
    }
}

#[derive(Debug)]
pub enum Statement {
    Move {
        dst: VarId,
        src: VarConst,
    },
    // Variable ids here must be contiguous
    LoadNil {
        dsts: Vec<VarId>,
    },
    GetTable {
        dst: VarId,
        table: VarId,
        key: VarConst,
    },
    SetGlobal {
        key: ConstIdx,
        src: VarId,
    },
    SetUpValue {
        dst: UpValIdx,
        src: VarId,
    },
    SetTable {
        table: VarId,
        key: VarConst,
        value: VarConst,
    },
    NewTable {
        dst: VarId,
        size_array: usize,
        size_hash: usize,
    },
    OpSelf {
        dst: VarId,
        dst_self: VarId,
        r_self: VarId,
        key: VarConst,
    },
    Operation {
        dst: VarId,
        operation: Operation,
    },
    Concat {
        dst: VarId,
        args: Vec<VarId>,
    },
    Conditional {
        expr: ConditionalExpr,
        polarity: bool,
    },
    Call {
        call: Call,
        returns: VarOrArgs,
    },
    TailCall {
        call: Call,
        returns: VarArg,
    },
    Return {
        args: VarOrArgs,
    },
    ForLoop {
        idx: VarId,
        idx_set: VarId,
        limit: VarId,
        step: VarId,
        idx_ext: VarId,
    },
    ForPrep {
        init: VarId,
        idx_set: VarId,
        plimit: VarId,
        pstep: VarId,
    },
    TForLoop {
        func: VarId,
        state: VarId,
        idx: VarId,
        idx_set: VarId,
        returns: Vec<VarId>,
    },
    Closure {
        dst: VarId,
        proto: ProtoIdx,
    },
    VarArg {
        args: VarOrArgs,
    },
    SetList {
        table: VarId,
        offset: Option<usize>,
        args: VarOrArgs,
    },
}

#[derive(Debug)]
pub enum VarOrArgs {
    Args(Vec<VarId>),
    Var(VarArg),
}

/// Call arguments
#[derive(Debug)]
pub struct Call {
    func: VarId,
    /// If `args` is [None], this call is vararg
    args: VarOrArgs,
}

impl Call {
    pub fn is_vararg(&self) -> bool {
        matches!(self.args, VarOrArgs::Var(_))
    }
}

/// Operation (e.g. add, sub, mul, div)
#[derive(Debug)]
pub enum Operation {
    Add { left: VarConst, right: VarConst },
    Sub { left: VarConst, right: VarConst },
    Mul { left: VarConst, right: VarConst },
    Div { left: VarConst, right: VarConst },
    Mod { left: VarConst, right: VarConst },
    Pow { left: VarConst, right: VarConst },
    Unm { src: VarId },
    Not { src: VarId },
    Len { src: VarId },
}

/// Conditional expression
#[derive(Debug)]
pub enum ConditionalExpr {
    Eq { left: VarConst, right: VarConst },
    Lt { left: VarConst, right: VarConst },
    Le { left: VarConst, right: VarConst },
    Test { var: VarId },
    TestSet { dst: VarId, var: VarId },
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
