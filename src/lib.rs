use std::{
    fmt::{Debug, Display},
    io::SeekFrom,
    ops::{self, Add, Index, IndexMut, Sub},
};

use binrw::{binread, binrw, BinReaderExt, BinResult, BinWriterExt};

use crate::{cfg::ControlFlowGraph, lua51::Lua51OpcodeMap};

pub mod cfg;
pub mod graph;
pub mod lua51;

#[derive(Debug)]
pub enum Error {
    InvalidOpcode(u8),
    /// An instruction branches to a non-existing instruction
    InvalidBranch(InstructionIdx),
}

pub fn parse<T: BinReaderExt>(mut reader: T) -> Function {
    let header: lua51::Lua51Header = reader.read_le().unwrap();
    let func: lua51::Lua51Function = reader.read_le().unwrap();

    let func: Function = func.into();

    func
}

#[derive(Debug, Clone, Copy)]
#[binrw]
pub struct RawInstruction(u32);

const SIZE_C: u32 = 9;
const SIZE_B: u32 = 9;
const SIZE_Bx: u32 = SIZE_C + SIZE_B;
const SIZE_A: u32 = 8;

const SIZE_OP: u32 = 6;

const POS_OP: u32 = 0;
const POS_A: u32 = POS_OP + SIZE_OP;
const POS_C: u32 = POS_A + SIZE_A;
const POS_B: u32 = POS_C + SIZE_C;
const POS_Bx: u32 = POS_C;

const MAXARG_Bx: u32 = (1 << SIZE_Bx) - 1;
const MAXARG_sBx: i32 = (MAXARG_Bx >> 1) as i32;

/// Creates a mask with `n` 1 bits at position `p`
///
/// # Examples
/// ```
/// assert_eq!(relua::mask1(8, 8), 0x0000FF00);
/// ```
pub const fn mask1(n: u32, p: u32) -> u32 {
    !((!0_u32) << n) << p
}

/// Creates a mask with `n` 0 bits at position `p`
///
/// # Examples
/// ```
/// assert_eq!(relua::mask0(8, 8), 0xFFFF00FF);
/// ```
pub const fn mask0(n: u32, p: u32) -> u32 {
    !mask1(n, p)
}

/// Maps from a raw opcode value to a [OpCode]
pub trait OpcodeMap {
    fn from_code(&self, code: u8) -> Result<OpCode, Error>;
}

impl OpcodeMap for Box<dyn OpcodeMap> {
    fn from_code(&self, code: u8) -> Result<OpCode, Error> {
        self.as_ref().from_code(code)
    }
}

impl RawInstruction {
    /// Returns the instruction opcode
    pub fn opcode<M: OpcodeMap>(&self, map: &M) -> Result<OpCode, Error> {
        map.from_code(((self.0 >> POS_OP) & mask1(SIZE_OP, 0)) as u8)
    }

    pub fn arg_a(&self) -> u32 {
        (self.0 >> POS_A) & mask1(SIZE_A, 0)
    }

    pub fn arg_b(&self) -> u32 {
        (self.0 >> POS_B) & mask1(SIZE_B, 0)
    }

    pub fn arg_c(&self) -> u32 {
        (self.0 >> POS_C) & mask1(SIZE_C, 0)
    }

    pub fn arg_bx(&self) -> u32 {
        (self.0 >> POS_Bx) & mask1(SIZE_Bx, 0)
    }

    pub fn arg_sbx(&self) -> i32 {
        self.arg_bx() as i32 - MAXARG_sBx
    }

    /// Gets argument A as a register index
    pub fn reg_a(&self) -> RegIdx {
        RegIdx(self.arg_a() as usize)
    }

    /// Gets argument B as a register index
    pub fn reg_b(&self) -> RegIdx {
        RegIdx(self.arg_b() as usize)
    }

    /// Gets argument C as a register index
    pub fn reg_c(&self) -> RegIdx {
        RegIdx(self.arg_c() as usize)
    }

    /// Gets argument Bx as a constant index
    pub fn const_bx(&self) -> ConstIdx {
        ConstIdx(self.arg_bx() as usize)
    }
}

#[derive(Debug)]
pub enum OpCode {
    /*----------------------------------------------------------------------
    name		args	description
    ------------------------------------------------------------------------*/
    Move,     /*	A B	R(A) := R(B)					*/
    LoadK,    /*	A Bx	R(A) := Kst(Bx)					*/
    LoadBool, /*	A B C	R(A) := (Bool)B; if (C) pc++			*/
    LoadNil,  /*	A B	R(A) := ... := R(B) := nil			*/
    GetUpVal, /*	A B	R(A) := UpValue[B]				*/

    GetGlobal, /*	A Bx	R(A) := Gbl[Kst(Bx)]				*/
    GetTable,  /*	A B C	R(A) := R(B)[RK(C)]				*/

    SetGlobal, /*	A Bx	Gbl[Kst(Bx)] := R(A)				*/
    SetUpVal,  /*	A B	UpValue[B] := R(A)				*/
    SetTable,  /*	A B C	R(A)[RK(B)] := RK(C)				*/

    NewTable, /*	A B C	R(A) := {} (size = B,C)				*/

    OpSelf, /*	A B C	R(A+1) := R(B); R(A) := R(B)[RK(C)]		*/

    Add, /*	A B C	R(A) := RK(B) + RK(C)				*/
    Sub, /*	A B C	R(A) := RK(B) - RK(C)				*/
    Mul, /*	A B C	R(A) := RK(B) * RK(C)				*/
    Div, /*	A B C	R(A) := RK(B) / RK(C)				*/
    Mod, /*	A B C	R(A) := RK(B) % RK(C)				*/
    Pow, /*	A B C	R(A) := RK(B) ^ RK(C)				*/
    Unm, /*	A B	R(A) := -R(B)					*/
    Not, /*	A B	R(A) := not R(B)				*/
    Len, /*	A B	R(A) := length of R(B)				*/

    Concat, /*	A B C	R(A) := R(B).. ... ..R(C)			*/

    Jmp, /*	sBx	pc+=sBx					*/

    Eq, /*	A B C	if ((RK(B) == RK(C)) ~= A) then pc++		*/
    Lt, /*	A B C	if ((RK(B) <  RK(C)) ~= A) then pc++  		*/
    Le, /*	A B C	if ((RK(B) <= RK(C)) ~= A) then pc++  		*/

    Test,    /*	A C	if not (R(A) <=> C) then pc++			*/
    TestSet, /*	A B C	if (R(B) <=> C) then R(A) := R(B) else pc++	*/

    Call,     /*	A B C	R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1)) */
    TailCall, /*	A B C	return R(A)(R(A+1), ... ,R(A+B-1))		*/
    Return,   /*	A B	return R(A), ... ,R(A+B-2)	(see note)	*/

    ForLoop, /*	A sBx	R(A)+=R(A+2);
             if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }*/
    ForPrep, /*	A sBx	R(A)-=R(A+2); pc+=sBx				*/

    TForLoop, /*	A C	R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));
              if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++	*/
    SetList, /*	A B C	R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B	*/

    Close,   /*	A 	close all variables in the stack up to (>=) R(A)*/
    Closure, /*	A Bx	R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))	*/

    VarArg, /*	A B	R(A), R(A+1), ..., R(A+B-1) = vararg		*/
}

/// Closure index from a function's proto table
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProtoIdx(pub usize);

impl Display for ProtoIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "P{}", self.0)
    }
}

/// Constant index from a function's constant table
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstIdx(pub usize);

impl Display for ConstIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "K{}", self.0)
    }
}

/// Register index
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegIdx(pub usize);

impl Display for RegIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R{}", self.0)
    }
}

impl ops::Add<usize> for RegIdx {
    type Output = RegIdx;

    fn add(self, rhs: usize) -> Self::Output {
        RegIdx(self.0 + rhs)
    }
}

impl ops::Add<isize> for RegIdx {
    type Output = RegIdx;

    fn add(self, rhs: isize) -> Self::Output {
        RegIdx((self.0 as isize + rhs) as usize)
    }
}

impl ops::Sub<usize> for RegIdx {
    type Output = RegIdx;

    fn sub(self, rhs: usize) -> Self::Output {
        RegIdx(self.0 - rhs)
    }
}

impl ops::Sub<isize> for RegIdx {
    type Output = RegIdx;

    fn sub(self, rhs: isize) -> Self::Output {
        RegIdx((self.0 as isize - rhs) as usize)
    }
}

/// UpValue index
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UpValIdx(pub usize);

impl Display for UpValIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "U{}", self.0)
    }
}

/// Register or constant
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegConst {
    Reg(RegIdx),
    Const(ConstIdx),
}

impl RegConst {
    pub fn decode(arg: u32) -> RegConst {
        const BITRK: u32 = 1 << (SIZE_B - 1);
        const KMASK: u32 = !BITRK;

        if arg & BITRK == 0 {
            RegConst::Reg(RegIdx(arg as usize))
        } else {
            RegConst::Const(ConstIdx((arg & KMASK) as usize))
        }
    }
}

impl Display for RegConst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegConst::Reg(r) => write!(f, "{}", r),
            RegConst::Const(k) => write!(f, "{}", k),
        }
    }
}

/// [Instruction::Call] and [Instruction::TailCall] arguments
#[derive(Debug)]
pub struct CallArgs {
    /// Register where the function is stored. Also used to store the first
    /// return value in the case of [Instruction::Call]. The first argument is at dst + 1
    dst: RegIdx,
    /// If `arg_count` is [None], all registers up to 'top' are passed
    arg_count: Option<usize>,
}

impl Display for CallArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(arg_count) = self.arg_count {
            write!(f, "{} {}", self.dst, arg_count)
        } else {
            write!(f, "{} top", self.dst)
        }
    }
}

impl CallArgs {
    fn decode(raw: RawInstruction) -> CallArgs {
        // Register of the function and first return value
        let dst = raw.reg_a();
        let arg_count = var_arg(raw.arg_b() as usize);
        CallArgs { dst, arg_count }
    }
}

/// Lua instruction
#[derive(Debug)]
pub enum Instruction {
    Move {
        dst: RegIdx,
        src: RegIdx,
    },
    LoadK {
        dst: RegIdx,
        src: ConstIdx,
    },
    LoadBool {
        dst: RegIdx,
        value: bool,
        /// If `skip_next` is true, the next instruction is skipped
        skip_next: bool,
    },
    LoadNil {
        dst_begin: RegIdx,
        /// Index of last register to set nil
        dst_end: RegIdx,
    },
    GetUpVal {
        dst: RegIdx,
        src: UpValIdx,
    },
    GetGlobal {
        dst: RegIdx,
        src: ConstIdx,
    },
    GetTable {
        dst: RegIdx,
        table: RegIdx,
        key: RegConst,
    },
    SetGlobal {
        dst: ConstIdx,
        src: RegIdx,
    },
    SetUpVal {
        dst: UpValIdx,
        src: RegIdx,
    },
    SetTable {
        table: RegIdx,
        key: RegConst,
        src: RegConst,
    },
    NewTable {
        dst: RegIdx,
        /// Size to allocate for the array part of the table
        size_array: usize,
        /// Size to allocate for the hashmap part of the table
        size_hash: usize,
    },
    OpSelf {
        dst: RegIdx,
        r_self: RegIdx,
        key: RegConst,
    },
    Add {
        dst: RegIdx,
        left: RegConst,
        right: RegConst,
    },
    Sub {
        dst: RegIdx,
        left: RegConst,
        right: RegConst,
    },
    Mul {
        dst: RegIdx,
        left: RegConst,
        right: RegConst,
    },
    Div {
        dst: RegIdx,
        left: RegConst,
        right: RegConst,
    },
    Mod {
        dst: RegIdx,
        left: RegConst,
        right: RegConst,
    },
    Pow {
        dst: RegIdx,
        left: RegConst,
        right: RegConst,
    },
    Unm {
        dst: RegIdx,
        src: RegIdx,
    },
    Not {
        dst: RegIdx,
        src: RegIdx,
    },
    Len {
        dst: RegIdx,
        src: RegIdx,
    },
    Concat {
        dst: RegIdx,
        src_begin: RegIdx,
        src_end: RegIdx,
    },
    Jmp {
        /// NOTE: This is the offset from the next instruction, i.e. the amount of instructions skipped
        offset: isize,
    },
    Eq {
        left: RegConst,
        right: RegConst,
        /// If `polarity` is true and the operands are unequal, the proceeding JMP instruction is skipped
        polarity: bool,
    },
    Lt {
        left: RegConst,
        right: RegConst,
        /// If `polarity` is true and the operands are unequal, the proceeding JMP instruction is skipped
        polarity: bool,
    },
    Le {
        left: RegConst,
        right: RegConst,
        /// If `polarity` is true and the operands are unequal, the proceeding JMP instruction is skipped
        polarity: bool,
    },
    Test {
        src: RegIdx,
        /// if not src <=> polarity, the proceeding JMP instruction is skipped
        polarity: bool,
    },
    TestSet {
        src: RegIdx,
        dst: RegIdx,
        /// if src <=> polarity, `dst` is set to `src`. Else, the proceeding JMP instruction is skipped
        polarity: bool,
    },
    Call {
        /// Function and arguments
        call_args: CallArgs,
        /// If `return_count` is None, OP_CALL sets 'top' to the last result + 1
        return_count: Option<usize>,
    },
    TailCall {
        /// Function and arguments
        call_args: CallArgs,
    },
    Return {
        /// Register of the first return value
        src: RegIdx,
        /// Number of return arguments. If [None], all registers from `dst` to 'top' are returned
        arg_count: Option<usize>,
    },
    ForLoop {
        /// The limit is idx + 1, step is idx + 2, and external index is idx + 3
        idx: RegIdx,
        /// Jump offset to start of loop if limit has not been reached
        offset: isize,
    },
    ForPrep {
        /// The limit is idx + 1, step is idx + 2
        idx: RegIdx,
        /// Jump offset to ForLoop instruction
        offset: isize,
    },
    TForLoop {
        /// Function index. State is func + 1, index is func + 2
        func: RegIdx,
        return_count: usize,
        // Next instruction should be JMP
    },
    SetList {
        table: RegIdx,
        /// Table offset / FPF. If [None], the real value is the next instruction
        offset: Option<usize>,
        count: Option<usize>,
    },
    Close {
        base: RegIdx,
    },
    Closure {
        proto: ProtoIdx,
        dst: RegIdx,
    },
    VarArg {
        dst: RegIdx,
        count: Option<usize>,
    },
}

/// Decodes a variable argument as used in Call, TailCall, and Return
fn var_arg(arg: usize) -> Option<usize> {
    match arg {
        x if x == 0 => None,
        x => Some(x - 1),
    }
}

pub enum Branches {
    /// Instruction exits program flow (e.g. [Instruction::Return])
    None,
    /// Instruction continues flow (NOTE: This may not be the next instruction (e.g. [Instruction::Jmp]))
    Flow(isize),
    /// Instruction branches (e.g. [Instruction::Eq])
    Branches(isize, isize),
}

/// List of registers
pub enum Registers {
    None,
    Range(RegIdx, RegIdx),
    ToTop(RegIdx),
    Pair(RegIdx, RegIdx),
    Triple(RegIdx, RegIdx, RegIdx),
}

impl Registers {
    /// Creates an empty list
    pub fn new() -> Registers {
        Registers::None
    }

    /// Creates a list of registers from a range
    pub fn from_range(first: RegIdx, last: RegIdx) -> Registers {
        Registers::Range(first, last)
    }

    /// Creates a list of registers to the top of the stack
    pub fn from_to_top(first: RegIdx) -> Registers {
        Registers::ToTop(first)
    }

    /// Creates a list from one register
    pub fn from(reg: RegIdx) -> Registers {
        Registers::Range(reg, reg)
    }

    /// Creates a list from a pair of registers
    pub fn from_pair(a: RegIdx, b: RegIdx) -> Registers {
        Registers::Pair(a, b)
    }

    /// Creates a list from a pair of [RegConst]. Only registers will be added.
    pub fn from_pair_rc(a: RegConst, b: RegConst) -> Registers {
        match (a, b) {
            (RegConst::Reg(a), RegConst::Reg(b)) => Registers::Pair(a, b),
            (RegConst::Reg(a), RegConst::Const(_)) | (RegConst::Const(_), RegConst::Reg(a)) => {
                Registers::Range(a, a)
            }
            _ => Registers::None,
        }
    }

    /// Creates a list from three registers
    pub fn from_triple(a: RegIdx, b: RegIdx, c: RegIdx) -> Registers {
        Registers::Triple(a, b, c)
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self::new()
    }
}

impl Instruction {
    /// Decodes a raw instruction value into an [Instruction]
    pub fn decode<M>(raw: RawInstruction, map: &M) -> Result<Instruction, Error>
    where
        M: OpcodeMap,
    {
        Ok(match raw.opcode(map)? {
            OpCode::Move => Instruction::Move {
                dst: raw.reg_a(),
                src: raw.reg_b(),
            },
            OpCode::LoadK => Instruction::LoadK {
                dst: raw.reg_a(),
                src: raw.const_bx(),
            },
            OpCode::LoadBool => Instruction::LoadBool {
                dst: raw.reg_a(),
                value: raw.arg_b() != 0,
                skip_next: raw.arg_c() != 0,
            },
            OpCode::LoadNil => Instruction::LoadNil {
                dst_begin: raw.reg_a(),
                dst_end: raw.reg_b(),
            },
            OpCode::GetUpVal => Instruction::GetUpVal {
                dst: raw.reg_a(),
                src: UpValIdx(raw.arg_b() as usize),
            },
            OpCode::GetGlobal => Instruction::GetGlobal {
                dst: raw.reg_a(),
                src: raw.const_bx(),
            },
            OpCode::GetTable => Instruction::GetTable {
                dst: raw.reg_a(),
                table: raw.reg_b(),
                key: RegConst::decode(raw.arg_c()),
            },
            OpCode::SetGlobal => Instruction::SetGlobal {
                dst: raw.const_bx(),
                src: raw.reg_a(),
            },
            OpCode::SetUpVal => Instruction::SetUpVal {
                dst: UpValIdx(raw.arg_b() as usize),
                src: raw.reg_a(),
            },
            OpCode::SetTable => Instruction::SetTable {
                table: raw.reg_a(),
                key: RegConst::decode(raw.arg_b()),
                src: RegConst::decode(raw.arg_c()),
            },
            OpCode::NewTable => Instruction::NewTable {
                dst: raw.reg_a(),
                size_array: raw.arg_b() as usize,
                size_hash: raw.arg_c() as usize,
            },
            OpCode::OpSelf => Instruction::OpSelf {
                dst: raw.reg_a(),
                r_self: raw.reg_b(),
                key: RegConst::decode(raw.arg_c()),
            },
            OpCode::Add => Instruction::Add {
                dst: raw.reg_a(),
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
            },
            OpCode::Sub => Instruction::Sub {
                dst: raw.reg_a(),
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
            },
            OpCode::Mul => Instruction::Mul {
                dst: raw.reg_a(),
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
            },
            OpCode::Div => Instruction::Div {
                dst: raw.reg_a(),
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
            },
            OpCode::Mod => Instruction::Mod {
                dst: raw.reg_a(),
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
            },
            OpCode::Pow => Instruction::Pow {
                dst: raw.reg_a(),
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
            },
            OpCode::Unm => Instruction::Unm {
                dst: raw.reg_a(),
                src: raw.reg_b(),
            },
            OpCode::Not => Instruction::Not {
                dst: raw.reg_a(),
                src: raw.reg_b(),
            },
            OpCode::Len => Instruction::Len {
                dst: raw.reg_a(),
                src: raw.reg_b(),
            },
            OpCode::Concat => Instruction::Concat {
                dst: raw.reg_a(),
                src_begin: raw.reg_b(),
                src_end: raw.reg_c(),
            },
            OpCode::Jmp => Instruction::Jmp {
                offset: raw.arg_sbx() as isize,
            },
            OpCode::Eq => Instruction::Eq {
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
                polarity: raw.arg_a() != 0,
            },
            OpCode::Lt => Instruction::Lt {
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
                polarity: raw.arg_a() != 0,
            },
            OpCode::Le => Instruction::Le {
                left: RegConst::decode(raw.arg_b()),
                right: RegConst::decode(raw.arg_c()),
                polarity: raw.arg_a() != 0,
            },
            OpCode::Test => {
                assert!(raw.arg_c() <= 1);
                Instruction::Test {
                    src: raw.reg_a(),
                    polarity: raw.arg_c() != 0,
                }
            }
            OpCode::TestSet => {
                assert!(raw.arg_c() <= 1);
                Instruction::TestSet {
                    src: raw.reg_b(),
                    dst: raw.reg_a(),
                    polarity: raw.arg_c() != 0,
                }
            }
            OpCode::Call => Instruction::Call {
                call_args: CallArgs::decode(raw),
                return_count: var_arg(raw.arg_c() as usize),
            },
            OpCode::TailCall => Instruction::TailCall {
                call_args: CallArgs::decode(raw),
            },
            OpCode::Return => {
                // Register of first return argument
                let dst = raw.reg_a();
                let arg_count = var_arg(raw.arg_b() as usize);
                Instruction::Return {
                    src: dst,
                    arg_count,
                }
            }
            OpCode::ForLoop => Instruction::ForLoop {
                idx: raw.reg_a(),
                offset: raw.arg_sbx() as isize,
            },
            OpCode::ForPrep => Instruction::ForPrep {
                idx: raw.reg_a(),
                offset: raw.arg_sbx() as isize,
            },
            OpCode::TForLoop => Instruction::TForLoop {
                func: raw.reg_a(),
                return_count: raw.arg_c() as usize,
            },
            OpCode::SetList => {
                let count = match raw.arg_b() {
                    0 => None,
                    c => Some(c as usize),
                };
                Instruction::SetList {
                    table: raw.reg_a(),
                    offset: var_arg(raw.arg_c() as usize),
                    count,
                }
            }
            OpCode::Close => Instruction::Close { base: raw.reg_a() },
            OpCode::Closure => Instruction::Closure {
                proto: ProtoIdx(raw.arg_bx() as usize),
                dst: raw.reg_a(),
            },
            OpCode::VarArg => Instruction::VarArg {
                dst: raw.reg_a(),
                count: var_arg(raw.arg_b() as usize),
            },
        })
    }

    /// Returns the relative position of the proceding instructions in the program flow
    ///
    /// Returns [Branches::None] for [Instruction::Return] and [Instruction::TailCall]
    pub fn branches(&self) -> Branches {
        match self {
            Instruction::Return { .. } | Instruction::TailCall { .. } => Branches::None,
            Instruction::Jmp { offset } => Branches::Flow(*offset + 1),
            Instruction::LoadBool {
                skip_next: true, ..
            } => Branches::Flow(2),
            Instruction::Eq { .. }
            | Instruction::Lt { .. }
            | Instruction::Le { .. }
            | Instruction::Test { .. }
            | Instruction::TestSet { .. }
            | Instruction::TForLoop { .. } => Branches::Branches(1, 2),
            Instruction::ForLoop { offset, .. } | Instruction::ForPrep { offset, .. } => {
                Branches::Branches(1, *offset)
            }
            // When offset is None, the next instruction stores the real value
            Instruction::SetList { offset: None, .. } => Branches::Flow(2),
            _ => Branches::Flow(1),
        }
    }

    /// Returns the registers referenced by the instruction
    pub fn inputs(&self) -> Registers {
        match self {
            Instruction::Move { dst, src } => Registers::from(*src),
            Instruction::LoadK { dst, src } => Registers::new(),
            Instruction::LoadBool {
                dst,
                value,
                skip_next,
            } => Registers::new(),
            Instruction::LoadNil { dst_begin, dst_end } => Registers::new(),
            Instruction::GetUpVal { dst, src } => Registers::new(),
            Instruction::GetGlobal { dst, src } => Registers::new(),
            Instruction::GetTable { dst, table, key } => match key {
                RegConst::Reg(key) => Registers::from_pair(*table, *key),
                RegConst::Const(_) => Registers::from(*table),
            },
            Instruction::SetGlobal { dst, src } => Registers::from(*src),
            Instruction::SetUpVal { dst, src } => Registers::from(*src),
            Instruction::SetTable { table, key, src } => match (key, src) {
                (RegConst::Reg(a), RegConst::Reg(b)) => Registers::from_triple(*table, *a, *b),
                (RegConst::Reg(a), RegConst::Const(_)) | (RegConst::Const(_), RegConst::Reg(a)) => {
                    Registers::from_pair(*table, *a)
                }
                (RegConst::Const(_), RegConst::Const(_)) => Registers::from(*table),
            },
            Instruction::NewTable {
                dst,
                size_array,
                size_hash,
            } => Registers::new(),
            Instruction::OpSelf { dst, r_self, key } => match key {
                RegConst::Reg(key) => Registers::from_pair(*r_self, *key),
                RegConst::Const(_) => Registers::from(*r_self),
            },
            Instruction::Add { dst, left, right } => Registers::from_pair_rc(*left, *right),
            Instruction::Sub { dst, left, right } => Registers::from_pair_rc(*left, *right),
            Instruction::Mul { dst, left, right } => Registers::from_pair_rc(*left, *right),
            Instruction::Div { dst, left, right } => Registers::from_pair_rc(*left, *right),
            Instruction::Mod { dst, left, right } => Registers::from_pair_rc(*left, *right),
            Instruction::Pow { dst, left, right } => Registers::from_pair_rc(*left, *right),
            Instruction::Unm { dst, src } => Registers::from(*src),
            Instruction::Not { dst, src } => Registers::from(*src),
            Instruction::Len { dst, src } => Registers::from(*src),
            Instruction::Concat {
                dst,
                src_begin,
                src_end,
            } => Registers::from_range(*src_begin, *src_end),
            Instruction::Jmp { offset } => Registers::new(),
            Instruction::Eq {
                left,
                right,
                polarity,
            } => Registers::from_pair_rc(*left, *right),
            Instruction::Lt {
                left,
                right,
                polarity,
            } => Registers::from_pair_rc(*left, *right),
            Instruction::Le {
                left,
                right,
                polarity,
            } => Registers::from_pair_rc(*left, *right),
            Instruction::Test { src, polarity } => Registers::from(*src),
            Instruction::TestSet { src, dst, polarity } => Registers::from(*src),
            Instruction::Call {
                call_args,
                return_count: _,
            }
            | Instruction::TailCall { call_args } => match call_args.arg_count {
                // Only the function (no arguments)
                Some(count) => Registers::from_range(call_args.dst, call_args.dst + count),
                None => Registers::from_to_top(call_args.dst),
            },
            Instruction::Return { src, arg_count } => match arg_count {
                Some(0) => Registers::new(),
                Some(count) => Registers::from_range(*src, *src + *count),
                None => Registers::from_to_top(*src),
            },
            Instruction::ForLoop { idx, offset } => Registers::from_range(*idx, *idx + 2usize),
            Instruction::ForPrep { idx, offset } => Registers::from_pair(*idx, *idx + 2usize),
            // TODO: This instruction references RA + 3 AFTER the call
            Instruction::TForLoop { func, return_count } => {
                Registers::from_range(*func, *func + 2usize)
            }
            Instruction::SetList {
                table,
                offset,
                count,
            } => match count {
                Some(count) => Registers::from_range(*table, *table + *count),
                None => Registers::from_to_top(*table),
            },
            Instruction::Close { base } => Registers::new(),
            // TODO: This one depends on the upvalues?
            Instruction::Closure { proto, dst } => Registers::from(*dst),
            Instruction::VarArg { dst, count } => Registers::new(),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Move { dst, src } => write!(f, "MOVE\t\t{} {}", dst, src),
            Instruction::LoadK { dst, src } => write!(f, "LOADK\t\t{} {}", dst, src),
            Instruction::LoadBool {
                dst,
                value,
                skip_next,
            } => write!(f, "LOADBOOL\t{} {} {}", dst, value, skip_next),
            Instruction::LoadNil { dst_begin, dst_end } => {
                write!(f, "LOADNIL\t{} {}", dst_begin, dst_end)
            }
            Instruction::GetUpVal { dst, src } => write!(f, "GETUPVAL\t{} {}", dst, src),
            Instruction::GetGlobal { dst, src } => write!(f, "GETGLOBAL\t{} {}", dst, src),
            Instruction::GetTable { dst, table, key } => {
                write!(f, "GETTABLE\t{} {} {}", dst, table, key)
            }
            Instruction::SetGlobal { dst, src } => write!(f, "SETGLOBAL\t{} {}", dst, src),
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
            Instruction::Jmp { offset } => write!(f, "JMP\t\t{}", offset),
            Instruction::Eq {
                left,
                right,
                polarity,
            } => write!(f, "EQ\t\t{} {} {}", left, right, polarity),
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
            } => write!(
                f,
                "CALL\t\t{} {}",
                call_args,
                return_count.map_or(-1, |rc| rc as isize)
            ),
            Instruction::TailCall { call_args } => todo!(),
            Instruction::Return {
                src: dst,
                arg_count,
            } => write!(
                f,
                "RETURN\t\t{} {}",
                dst,
                arg_count.map_or(-1, |ac| ac as isize)
            ),
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

/// Instruction index in a [Code] structure
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstructionIdx(usize);

impl Display for InstructionIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add<usize> for InstructionIdx {
    type Output = InstructionIdx;

    fn add(self, rhs: usize) -> Self::Output {
        InstructionIdx(self.0 + rhs)
    }
}

impl Add<isize> for InstructionIdx {
    type Output = InstructionIdx;

    fn add(self, rhs: isize) -> Self::Output {
        InstructionIdx((self.0 as isize + rhs) as usize)
    }
}

impl Sub<usize> for InstructionIdx {
    type Output = InstructionIdx;

    fn sub(self, rhs: usize) -> Self::Output {
        InstructionIdx(self.0 - rhs)
    }
}

impl Sub<isize> for InstructionIdx {
    type Output = InstructionIdx;

    fn sub(self, rhs: isize) -> Self::Output {
        InstructionIdx((self.0 as isize - rhs) as usize)
    }
}

/// Lua instructions
pub struct Code {
    pub(crate) instructions: Vec<RawInstruction>,
    // TODO: Replace dynamic dispatch here
    map: Box<dyn OpcodeMap>,
}

impl Debug for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(
                self.instructions
                    .iter()
                    .map(|i| Instruction::decode(*i, &self.map).ok()),
            )
            .finish()
    }
}

impl Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (idx, i) in self.decode_iter() {
            write!(f, "{}\t", idx.0)?;
            if let Ok(i) = i {
                writeln!(f, "{}", i)?;
            } else {
                writeln!(f, "INVALID")?;
            }
        }
        Ok(())
    }
}

impl Code {
    /// Creates a new code structure from a vector of instructions
    pub fn new<M: OpcodeMap + 'static>(instructions: Vec<RawInstruction>, map: M) -> Code {
        Code {
            instructions,
            map: Box::new(map),
        }
    }

    /// Returns the raw instruction at `idx`
    ///
    /// Returns [None] if idx >= amount of instructions
    pub fn raw(&self, idx: InstructionIdx) -> Option<RawInstruction> {
        self.instructions.get(idx.0).copied()
    }

    /// Decodes the instruction at `idx`
    ///
    /// Returns [None] if `idx` >= amount of instructions
    pub fn decode(&self, idx: InstructionIdx) -> Result<Option<Instruction>, Error> {
        self.raw(idx)
            .map_or(Ok(None), |r| Instruction::decode(r, &self.map).map(Some))
    }

    /// Returns the instruction [OpCode] at `idx`
    ///
    /// Returns [None] if `idx` >= amount of instructions
    pub fn opcode(&self, idx: InstructionIdx) -> Result<Option<OpCode>, Error> {
        self.raw(idx)
            .map_or(Ok(None), |i| i.opcode(&self.map).map(Some))
    }

    /// Returns the instructions length
    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

    /// Returns an iterator over raw instructions
    pub fn iter(&self) -> impl Iterator<Item = &RawInstruction> {
        self.instructions.iter()
    }

    /// Returns an iterator over decoded instructions
    pub fn decode_iter(
        &self,
    ) -> DecodeIter<Box<dyn OpcodeMap>, impl Iterator<Item = &RawInstruction>> {
        DecodeIter::new(self.iter(), &self.map)
    }
}

pub struct DecodeIter<'a, M: OpcodeMap, I: Iterator<Item = &'a RawInstruction> + 'a> {
    iter: I,
    map: &'a M,
    index: InstructionIdx,
}

impl<'a, M, I> DecodeIter<'a, M, I>
where
    M: OpcodeMap,
    I: Iterator<Item = &'a RawInstruction> + 'a,
{
    fn new(iter: I, map: &'a M) -> DecodeIter<'a, M, I> {
        DecodeIter {
            iter,
            map,
            index: InstructionIdx(0),
        }
    }
}

impl<'a, M, I> Iterator for DecodeIter<'a, M, I>
where
    M: OpcodeMap,
    I: Iterator<Item = &'a RawInstruction> + 'a,
{
    type Item = (InstructionIdx, Result<Instruction, Error>);

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: Handle SETLIST (next instruction may be used)
        let res = self
            .iter
            .next()
            .map(|raw| Instruction::decode(*raw, self.map))
            .map(|i| (self.index, i));
        self.index = self.index + 1usize;
        res
    }
}

impl Index<InstructionIdx> for Code {
    type Output = RawInstruction;

    fn index(&self, index: InstructionIdx) -> &Self::Output {
        &self.instructions[index.0]
    }
}

impl IndexMut<InstructionIdx> for Code {
    fn index_mut(&mut self, index: InstructionIdx) -> &mut Self::Output {
        &mut self.instructions[index.0]
    }
}

/// Lua function structure
#[derive(Debug)]
pub struct Function {
    pub source: String,
    pub line_defined: u32,
    pub last_line_defined: u32,
    pub nups: u8,
    pub num_params: u8,
    pub is_vararg: u8,
    pub max_stack_size: u8,

    pub code: Code,
    pub constants: Vec<Constant>,
    pub protos: Vec<Function>,
    pub debug: DebugInfo,
}

impl Function {
    /// Returns a reference to the [Constant] at `index`
    ///
    /// Returns [None] if out of bounds
    pub fn constant(&self, index: ConstIdx) -> Option<&Constant> {
        self.constants.get(index.0)
    }

    /// Returns a reference to the [Function] at `index`
    ///
    /// Returns [None] if out of bounds
    pub fn proto(&self, index: ConstIdx) -> Option<&Function> {
        self.protos.get(index.0)
    }
}

/// Constant value
#[derive(Debug)]
pub enum Constant {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Nil => write!(f, "nil"),
            Constant::Boolean(b) => write!(f, "{}", b),
            Constant::Number(n) => write!(f, "{}", n),
            Constant::String(s) => write!(f, "{:?}", s),
        }
    }
}

/// Local variable
#[derive(Debug)]
pub struct LocalVar {
    pub name: String,

    pub start_pc: u32,
    pub end_pc: u32,
}

/// UpValue
#[derive(Debug)]
pub struct UpValue(String);

/// Debug information
#[derive(Debug)]
pub struct DebugInfo {
    pub line_info: Vec<u32>,
    pub loc_vars: Vec<LocalVar>,
    pub upvalues: Vec<UpValue>,
}
