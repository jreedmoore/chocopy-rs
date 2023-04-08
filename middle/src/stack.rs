// A stack based IR
// The concept here is that the Stack IR can be easily emitted as WASM, and then compiled into a register IR to emit code for a register machine like x86 or AMD64

// As the virtual machine is growing to support new features these attributes are becoming clear
//   - We have stack frames for function calls, with locals stored at the bottom of the stack frame
//   - We have some kind of .data segment for globals
//   - We have a heap for dynamically sized data

#[derive(Debug, Clone)]
pub struct Program {
    pub instrs: Vec<Instr>,
    pub locals: usize,
}
impl Program {
    pub fn new() -> Program {
        Program {
            instrs: vec![],
            locals: 0,
        }
    }
}
pub type FlatProgram = Program;

#[derive(Debug, Clone)]
pub enum Instr {
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

    If(bool),
    Else,
    EndIf,

    Loop,
    EndLoop,
    BrIf,

    // should these have types?
    // for now everything is an i32 in WASM
    // str might be represented as a pair of i32, but I could represent that as two different locals
    LoadLocal(usize),
    StoreLocal(usize),
}
