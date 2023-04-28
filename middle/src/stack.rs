// A stack based IR
// The concept here is that the Stack IR can be easily emitted as WASM, and then compiled into a register IR to emit code for a register machine like x86 or AMD64

// As the virtual machine is growing to support new features these attributes are becoming clear
//   - We have stack frames for function calls, with locals stored at the bottom of the stack frame
//   - We have some kind of .data segment for globals
//   - We have a heap for dynamically sized data

use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Program {
    pub start: String,
    pub funcs: Vec<Function>,
    pub consts: Vec<MemVal>,
}
#[derive(Debug, Clone)]
pub struct Function {
    pub label: String,
    pub must_return: bool,
    pub blocks: Vec<Block>
}
impl Program {
    pub fn new() -> Program {
        Program {
            start: "entry".to_owned(),
            funcs: vec![],
            consts: vec![],
        }
    }

    fn current_fun(&mut self) -> &mut Function {
        self.funcs.last_mut().expect("empty funcs")
    }

    fn current_block(&mut self) -> &mut Block {
        self.current_fun().blocks.last_mut().expect("empty blocks")
    }

    pub fn push_instr(&mut self, instr: Instr<BlockLocation>) {
        self.current_block().instrs.push(instr);
    }

    pub fn start_block(&mut self) {
        self.current_fun().blocks.push(Block::new())
    }

    pub fn start_function(&mut self, name: String, must_return: bool) {
        self.funcs.push(Function { label: name, must_return, blocks: vec![Block::new()] })
    }

    pub fn insert_nop(&mut self) {
        if self.current_block().instrs.is_empty() {
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
        for func in &prog.funcs {
            block_names.insert(func.label.to_owned(), block_offsets.len());
            for block in &func.blocks {
                block_offsets.push(instruction_count);
                instruction_count += block.instrs.len() as isize;
            }
        }
        if FLATTEN_DEBUG {
            println!("{:?}\n{:?}", prog.funcs, block_offsets);
        }

        let start = block_offsets[*block_names
            .get(&prog.start)
            .expect("start references undeclared block")] as usize;

        let mut flat = FlatProgram {
            start: start,
            instrs: vec![],
            consts: prog.consts,
        };
        let mut instruction_pointer: usize = 0;
        let mut block_pointer: usize = 0;
        for func in prog.funcs {
            for block in func.blocks {
                for instr in block.instrs {
                    flat.instrs.push(instr.map(|block_off| match block_off {
                        BlockLocation::BlockOffset(off) => {
                            let dest_idx = block_pointer.checked_add_signed(off).expect("block pointer overflow");
                            let block_begin = block_offsets[dest_idx];
                            // -1 because IP is _next_ instruction
                            let instr_offset = (block_begin as isize) - (instruction_pointer as isize) - 1;
                            if FLATTEN_DEBUG {
                                println!("BlockOffset {} from blocks {} to {}, from instr {} to {}, offset = {}", off, block_pointer, dest_idx, instruction_pointer, block_begin, instr_offset);
                            }
                            InstrLocation::InstrOffset(instr_offset)
                        },
                        BlockLocation::Named(name) => InstrLocation::InstrAbsolute(block_offsets[*block_names.get(&name).expect("undeclared reference to block label")] as usize)
                    }));
                    instruction_pointer += 1;
                }
                block_pointer += 1;
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
        Block {
            label: None,
            instrs: vec![],
        }
    }

    fn named(name: String) -> Block {
        Block {
            label: Some(name),
            instrs: vec![],
        }
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

    CallNative(String),
    Call { loc: Loc, arity: usize },

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
    Return,
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
            CallNative(f) => CallNative(f),
            Call { loc, arity } => Call { loc: f(loc), arity },
            Return => Return,
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
            InstrLocation::InstrOffset(off) => {
                *instruction_pointer = instruction_pointer
                    .checked_add_signed(*off)
                    .expect("ip overflow")
            }
            InstrLocation::InstrAbsolute(new_ip) => *instruction_pointer = *new_ip,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemVal {
    Str(String),
    Unused,
}
