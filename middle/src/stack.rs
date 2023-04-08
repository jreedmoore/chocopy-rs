// A stack based IR
// The concept here is that the Stack IR can be easily emitted as WASM, and then compiled into a register IR to emit code for a register machine like x86 or AMD64

// As the virtual machine is growing to support new features these attributes are becoming clear
//   - We have stack frames for function calls, with locals stored at the bottom of the stack frame
//   - We have some kind of .data segment for globals
//   - We have a heap for dynamically sized data

#[derive(Debug, Clone)]
pub struct Program {
    pub blocks: Vec<Block>
}
impl Program {
    pub fn new() -> Program {
        Program {
            blocks: vec![],
        }
    }

    pub fn push_instr(&mut self, instr: Instr<BlockLocation>) {
        self.blocks.last_mut().unwrap().instrs.push(instr);
    }

    pub fn start_block(&mut self) {
        self.blocks.push(Block::new())
    }
}
#[derive(Debug, Clone)]
pub struct FlatProgram {
    pub instrs: Vec<Instr<InstrLocation>>
}
impl FlatProgram {
    pub fn from_program(prog: Program) -> FlatProgram {
        let mut instruction_count: isize = 0;
        let mut block_offsets: Vec<isize> = vec![];
        for block in &prog.blocks {
            block_offsets.push(instruction_count);
            instruction_count += block.instrs.len() as isize;
        }

        let mut flat = FlatProgram { instrs: vec![] };
        let mut instruction_pointer: usize = 0;
        for (idx, block) in prog.blocks.into_iter().enumerate() {
            for instr in block.instrs {
                flat.instrs.push(instr.map(|block_off| match block_off {
                    BlockLocation::BlockOffset(off) => {
                        let dest_idx = idx.checked_add_signed(off).expect("block pointer overflow");
                        let block_begin = block_offsets[dest_idx];
                        // -1 because we want to exclude the jump instruction itself from the offset
                        let instr_offset = (block_begin as isize) - (instruction_pointer as isize) - 1;
                        InstrLocation::InstrOffset(instr_offset)
                    }
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
    pub instrs: Vec<Instr<BlockLocation>>
}
impl Block {
    fn new() -> Block {
        Block { instrs: vec![] }
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

    // should these have types?
    // for now everything is an i32 in WASM
    // str might be represented as a pair of i32, but I could represent that as two different locals
    LoadLocal(usize),
    StoreLocal(usize),
}
impl<A> Instr<A> {
    fn map<B, F>(self, f: F) -> Instr<B>
    where 
        F: Fn(A) -> B
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
            Call(f) => Call(f),
            Drop => Drop,
            Jump(a) => Jump(f(a)),
            IfJump(a) => IfJump(f(a)),
            LoadLocal(i) => LoadLocal(i),
            StoreLocal(i) => StoreLocal(i),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BlockLocation {
    BlockOffset(isize),
}

#[derive(Debug, Clone)]
pub enum InstrLocation {
    InstrOffset(isize),
}