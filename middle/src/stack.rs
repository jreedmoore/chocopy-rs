// A stack based IR
// The concept here is that the Stack IR can be easily emitted as WASM, and then compiled into a register IR to emit code for a register machine like x86 or AMD64

// As the virtual machine is growing to support new features these attributes are becoming clear
//   - We have stack frames for function calls, with locals stored at the bottom of the stack frame
//   - We have some kind of .data segment for globals
//   - We have a heap for dynamically sized data

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Program {
    pub start: BlockLocation,
    pub blocks: Vec<Block>,
    pub consts: Vec<MemVal>,
}
impl Program {
    pub fn new() -> Program {
        Program {
            start: BlockLocation::Named("entry".to_owned()),
            blocks: vec![],
            consts: vec![],
        }
    }

    pub fn push_instr(&mut self, instr: Instr<BlockLocation>) {
        self.blocks.last_mut().unwrap().instrs.push(instr);
    }

    pub fn start_block(&mut self) {
        self.blocks.push(Block::new())
    }

    pub fn start_named_block(&mut self, name: String) {
        self.blocks.push(Block::named(name))
    }

    pub fn insert_nop(&mut self) {
        if self.blocks.last().unwrap().instrs.is_empty() {
            self.push_instr(Instr::Nop)
        }
    }
}

const FLATTEN_DEBUG: bool = false;
#[derive(Debug, Clone)]
pub struct FlatProgram {
    pub start: usize,
    pub consts: Vec<MemVal>,
    pub instrs: Vec<Instr<InstrLocation>>,
}
impl FlatProgram {
    pub fn from_program(prog: Program) -> FlatProgram {
        let mut instruction_count: isize = 0;
        let mut block_offsets: Vec<isize> = vec![];
        let mut block_names: HashMap<String, usize> = HashMap::new();
        for block in &prog.blocks {
            if let Some(name) = &block.label {
                block_names.insert(name.to_owned(), block_offsets.len());
            }
            block_offsets.push(instruction_count);
            instruction_count += block.instrs.len() as isize;
        }
        if FLATTEN_DEBUG {
            println!("{:?}\n{:?}", prog.blocks, block_offsets);
        }

        let start = if let BlockLocation::Named(name) = prog.start {
            block_offsets[*block_names.get(&name).expect("start references undeclared block")] as usize
        } else {
            panic!("Unexpected BlockLocation type for start")
        };

        let mut flat = FlatProgram {
            start: start,
            instrs: vec![],
            consts: prog.consts,
        };
        let mut instruction_pointer: usize = 0;
        for (idx, block) in prog.blocks.into_iter().enumerate() {
            for instr in block.instrs {
                flat.instrs.push(instr.map(|block_off| match block_off {
                    BlockLocation::BlockOffset(off) => {
                        let dest_idx = idx.checked_add_signed(off).expect("block pointer overflow");
                        let block_begin = block_offsets[dest_idx];
                        // -1 because we want to exclude the jump instruction itself from the offset
                        let instr_offset = (block_begin as isize) - (instruction_pointer as isize) - 1;
                        if FLATTEN_DEBUG {
                            println!("BlockOffset {} from blocks {} to {}, from instr {} to {}, offset = {}", off, idx, dest_idx, instruction_pointer, block_begin, instr_offset);
                        }
                        InstrLocation::InstrOffset(instr_offset)
                    },
                    BlockLocation::Named(name) => InstrLocation::InstrAbsolute(block_offsets[*block_names.get(&name).expect("undeclared reference to block label")] as usize)
                }));
                instruction_pointer += 1;
            }
        }

        flat
    }

    pub fn from_program_clone(prog: &Program) -> FlatProgram {
        FlatProgram::from_program(prog.clone())
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub label: Option<String>,
    pub instrs: Vec<Instr<BlockLocation>>,
}
impl Block {
    fn new() -> Block {
        Block { label: None, instrs: vec![] }
    }

    fn named(name: String) -> Block {
        Block { label: Some(name), instrs: vec![] }
    }
}

#[derive(Debug, Clone)]
pub enum Instr<Loc> {
    NumConst(i32),
    BoolConst(bool),
    NoneConst,

    Add,
    Sub,
    Mul,
    Modulo,
    Div,

    LogicalAnd,
    LogicalOr,
    UnaryNot,

    // relational
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,

    Call(String),

    Drop,

    Jump(Loc),
    IfJump(Loc),
    Nop,

    StrConcat,

    LoadLocal(usize),
    StoreLocal(usize),
    LoadConstant(usize),

    // expects [StrRef Int] on stack
    StrIndex,
    Is,
}
impl<A> Instr<A> {
    fn map<B, F>(self, f: F) -> Instr<B>
    where
        F: Fn(A) -> B,
    {
        use Instr::*;
        match self {
            NumConst(n) => NumConst(n),
            BoolConst(b) => BoolConst(b),
            NoneConst => NoneConst,
            Add => Add,
            Sub => Sub,
            Mul => Mul,
            Modulo => Modulo,
            Div => Div,
            LogicalAnd => LogicalAnd,
            LogicalOr => LogicalOr,
            UnaryNot => UnaryNot,
            Eq => Eq,
            Ne => Ne,
            Lt => Lt,
            Lte => Lte,
            Gt => Gt,
            Gte => Gte,
            Is => Is,
            Call(f) => Call(f),
            Drop => Drop,
            Jump(a) => Jump(f(a)),
            IfJump(a) => IfJump(f(a)),
            Nop => Nop,
            LoadLocal(i) => LoadLocal(i),
            StoreLocal(i) => StoreLocal(i),
            LoadConstant(i) => LoadConstant(i),
            StrConcat => StrConcat,
            StrIndex => StrIndex,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BlockLocation {
    BlockOffset(isize),
    Named(String),
}

#[derive(Debug, Clone)]
pub enum InstrLocation {
    InstrOffset(isize),
    InstrAbsolute(usize),
}
impl InstrLocation {
    pub fn update(&self, instruction_pointer: &mut usize) {
        match self {
            InstrLocation::InstrOffset(off) => *instruction_pointer = instruction_pointer.checked_add_signed(*off).expect("ip overflow"),
            InstrLocation::InstrAbsolute(new_ip) => *instruction_pointer = *new_ip,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemVal {
    Str(String),
    Unused,
}
