// A stack based IR
// The concept here is that the Stack IR can be easily emitted as WASM, and then compiled into a register IR to emit code for a register machine like x86 or AMD64

pub struct Program {
    pub instrs: Vec<Instr>
}
impl Program {
    pub fn new() -> Program {
        Program { instrs: vec![] }
    }
}

pub enum Instr {
    NumConst(i64),

    Add,
    Sub,
    Mul,
    Modulo,
    Div,

    ArithShiftRight,
    ShiftLeft,

    BitAnd,
    BitXor,
    BitOr,

    // relational
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,

    Call(String),

    Drop,

    If,
    Else,
    EndIf
}