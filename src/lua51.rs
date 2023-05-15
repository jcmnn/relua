use binrw::{binread, binrw, BinReaderExt, BinResult, BinWriterExt};
use std::io::SeekFrom;

use crate::{
    Code, Constant, DebugInfo, Error, Function, LocalVar, OpCode, OpcodeMap, RawInstruction,
    UpValue,
};

#[derive(Debug)]
#[binrw]
#[brw(big, magic = b"\x1BLua")]
pub struct Lua51Header {
    pub version: u8,
    pub format: u8,
    pub endianness: u8,
    pub size_int: u8,
    pub size_size_t: u8,
    pub size_instruction: u8,
    pub size_number: u8,
    pub integral_number: u8,
}

#[binrw::parser(reader, endian)]
fn parse_string() -> BinResult<String> {
    // Parse string length
    let size: u64 = reader.read_type(endian)?;

    if size == 0 {
        return Ok(String::new());
    }

    // Read string content
    let mut content = vec![0u8; size as usize];
    let pos = reader.stream_position()?;
    reader.read_exact(&mut content)?;

    // Remove trailing whitespace
    if content.pop() != Some(0) {
        return Err(binrw::Error::AssertFail {
            pos,
            message: "String does not end in trailing null terminator".to_string(),
        });
    }

    String::from_utf8(content).map_err(|e| binrw::Error::Custom {
        pos,
        err: Box::new(e),
    })
}

#[binrw::writer(writer, endian)]
fn write_string(s: &String) -> BinResult<()> {
    // Write length. Add 1 for null terminator
    let len = s.len() as u64 + 1;
    writer.write_type(&len, endian)?;

    // Write string content
    writer.write_all(s.as_bytes())?;
    // Write null terminator
    writer.write_all(&[0])?;

    Ok(())
}

#[derive(Debug)]
#[binrw]
pub enum Lua51Constant {
    #[brw(magic = 0u8)]
    Nil,
    #[brw(magic = 1u8)]
    Boolean(
        #[br(map = |x: u8| x == 1)]
        #[bw(map = |x| if *x { 1u8 } else { 0u8 })]
        bool,
    ),
    #[brw(magic = 3u8)]
    Number(f64),
    #[brw(magic = 4u8)]
    String(
        #[br(parse_with = parse_string)]
        #[bw(write_with = write_string)]
        String,
    ),
}

#[derive(Debug)]
#[binrw]
pub struct Lua51LocalVar {
    #[br(parse_with = parse_string)]
    #[bw(write_with = write_string)]
    pub name: String,

    pub start_pc: u32,
    pub end_pc: u32,
}

#[derive(Debug)]
#[binrw]
pub struct Lua51UpValue(
    #[br(parse_with = parse_string)]
    #[bw(write_with = write_string)]
    String,
);

#[derive(Debug)]
#[binrw]
pub struct Lua51Debug {
    pub size_line_info: u32,
    #[br(count = size_line_info)]
    pub line_info: Vec<u32>,

    pub size_loc_vars: u32,
    #[br(count = size_loc_vars)]
    pub loc_vars: Vec<Lua51LocalVar>,

    pub size_upvalues: u32,
    #[br(count = size_upvalues)]
    pub upvalues: Vec<Lua51UpValue>,
}

#[derive(Debug)]
#[binrw(assert(code.len() == code_size as usize),
        assert(constants.len() == constants_size as usize))]
pub struct Lua51Function {
    #[br(parse_with = parse_string)]
    #[bw(write_with = write_string)]
    pub source: String,
    pub line_defined: u32,
    pub last_line_defined: u32,
    pub nups: u8,
    pub num_params: u8,
    pub is_vararg: u8,
    pub max_stack_size: u8,

    pub code_size: u32,
    #[br(count = code_size)]
    pub code: Vec<RawInstruction>,

    pub constants_size: u32,
    #[br(count = constants_size)]
    pub constants: Vec<Lua51Constant>,

    pub size_protos: u32,
    #[br(count = size_protos)]
    pub protos: Vec<Lua51Function>,

    pub debug: Lua51Debug,
}

pub struct Lua51OpcodeMap;

impl OpcodeMap for Lua51OpcodeMap {
    fn from_code(&self, code: u8) -> Result<OpCode, Error> {
        Ok(match code {
            0 => OpCode::Move,
            1 => OpCode::LoadK,
            2 => OpCode::LoadBool,
            3 => OpCode::LoadNil,
            4 => OpCode::GetUpVal,
            5 => OpCode::GetGlobal,
            6 => OpCode::GetTable,
            7 => OpCode::SetGlobal,
            8 => OpCode::SetUpVal,
            9 => OpCode::SetTable,
            10 => OpCode::NewTable,
            11 => OpCode::OpSelf,
            12 => OpCode::Add,
            13 => OpCode::Sub,
            14 => OpCode::Mul,
            15 => OpCode::Div,
            16 => OpCode::Mod,
            17 => OpCode::Pow,
            18 => OpCode::Unm,
            19 => OpCode::Not,
            20 => OpCode::Len,
            21 => OpCode::Concat,
            22 => OpCode::Jmp,
            23 => OpCode::Eq,
            24 => OpCode::Lt,
            25 => OpCode::Le,
            26 => OpCode::Test,
            27 => OpCode::TestSet,
            28 => OpCode::Call,
            29 => OpCode::TailCall,
            30 => OpCode::Return,
            31 => OpCode::ForLoop,
            32 => OpCode::ForPrep,
            33 => OpCode::TForLoop,
            34 => OpCode::SetList,
            35 => OpCode::Close,
            36 => OpCode::Closure,
            37 => OpCode::VarArg,
            _ => return Err(Error::InvalidOpcode(code)),
        })
    }
}

impl From<Lua51Constant> for Constant {
    fn from(val: Lua51Constant) -> Self {
        match val {
            Lua51Constant::Nil => Constant::Nil,
            Lua51Constant::Boolean(b) => Constant::Boolean(b),
            Lua51Constant::Number(n) => Constant::Number(n),
            Lua51Constant::String(s) => Constant::String(s),
        }
    }
}

impl From<Lua51LocalVar> for LocalVar {
    fn from(val: Lua51LocalVar) -> Self {
        LocalVar {
            name: val.name,
            start_pc: val.start_pc,
            end_pc: val.end_pc,
        }
    }
}

impl From<Lua51UpValue> for UpValue {
    fn from(val: Lua51UpValue) -> Self {
        UpValue(val.0)
    }
}

impl From<Lua51Debug> for DebugInfo {
    fn from(val: Lua51Debug) -> Self {
        DebugInfo {
            line_info: val.line_info,
            loc_vars: val.loc_vars.into_iter().map(|l| l.into()).collect(),
            upvalues: val.upvalues.into_iter().map(|u| u.into()).collect(),
        }
    }
}

impl From<Lua51Function> for Function {
    fn from(val: Lua51Function) -> Self {
        Function {
            source: val.source,
            line_defined: val.line_defined,
            last_line_defined: val.last_line_defined,
            nups: val.nups,
            num_params: val.num_params,
            is_vararg: val.is_vararg,
            max_stack_size: val.max_stack_size,
            code: Code::new(val.code, Lua51OpcodeMap),
            constants: val.constants.into_iter().map(|c| c.into()).collect(),
            protos: val.protos.into_iter().map(|p| p.into()).collect(),
            debug: val.debug.into(),
        }
    }
}
