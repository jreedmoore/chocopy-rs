use itertools::Itertools;

pub enum WASMInstr {
    I32Const(i32),
    I64Const(i64),
    Call(String),
    I64Add,
    Drop,
    I64Or,
    I64Sub,
    I64And,
    I64Xor,
    I64Mul,
    I64RemSigned,
    I64DivSigned,
    I64ShrSigned,
    I64ShiftLeft,
    I64RotateRight,
    I64RotateLeft,

    I64ExtendI32,
    I32WrapI64,

    I64Eq,
    I64Ne,
    I64Lt,
    I64Lte,
    I64Gt,
    I64Gte,

    If(bool),
    Else,
    EndIf,

    LocalGet(usize),
    LocalSet(usize),
}
impl WATPrint for WASMInstr {
    fn wat_print(&self) -> String {
        match self {
            WASMInstr::I32Const(i) => format!("i32.const {}", i),
            WASMInstr::I64Const(i) => format!("i64.const {}", i),
            WASMInstr::I64Add => format!("i64.add"),
            WASMInstr::I64Sub => format!("i64.sub"),
            WASMInstr::I64Or => format!("i64.or"),
            WASMInstr::Call(name) => format!("call ${}", name),
            WASMInstr::Drop => format!("drop"),
            WASMInstr::I64And => format!("i64.and"),
            WASMInstr::I64Xor => format!("i64.xor"),
            WASMInstr::I64Mul => format!("i64.mul"),
            WASMInstr::I64RemSigned => format!("i64.rem_s"),
            WASMInstr::I64DivSigned => format!("i64.div_s"),
            WASMInstr::I64ShrSigned => format!("i64.shr_s"),
            WASMInstr::I64ShiftLeft => format!("i64.shl"),
            WASMInstr::I64RotateRight => format!("i64.rotr"),
            WASMInstr::I64RotateLeft => format!("i64.rotl"),
            WASMInstr::I64ExtendI32 => format!("i64.extend_i32_u"),
            WASMInstr::I32WrapI64 => format!("i32.wrap_i64"),
            WASMInstr::I64Eq => format!("i64.eq"),
            WASMInstr::I64Ne => format!("i64.ne"),
            WASMInstr::I64Lt => format!("i64.lt_s"),
            WASMInstr::I64Lte => format!("i64.le_s"),
            WASMInstr::I64Gt => format!("i64.gt_s"),
            WASMInstr::I64Gte => format!("i64.ge_s"),
            WASMInstr::If(has_return) => {
                let mut buf = "(if".to_string();
                if *has_return {
                    buf.push_str(" (result i64)")
                }
                buf.push_str("(then ");
                buf
            }
            WASMInstr::Else => format!(") (else "),
            WASMInstr::EndIf => format!("))"),
            WASMInstr::LocalGet(idx) => format!("local.get {}", idx),
            WASMInstr::LocalSet(idx) => format!("local.set {}", idx),
        }
    }
}

impl<T: WATPrint> WATPrint for Vec<T> {
    fn wat_print(&self) -> String {
        self.iter().map(|t| t.wat_print()).join(" ")
    }
}

pub enum WASMType {
    I32,
    I64,
    F32,
    F64,
}
impl WATPrint for WASMType {
    fn wat_print(&self) -> String {
        match self {
            WASMType::I32 => "i32".to_owned(),
            WASMType::I64 => "i64".to_owned(),
            WASMType::F32 => "f32".to_owned(),
            WASMType::F64 => "f64".to_owned(),
        }
    }
}

pub struct WASMFunImport {
    pub name: Vec<String>,
    pub params: Vec<WASMType>,
    pub return_type: Option<WASMType>,
}
impl WATPrint for WASMFunImport {
    fn wat_print(&self) -> String {
        let quoted_name = self.name.iter().map(|s| format!("\"{}\"", s)).join(" ");
        let snake_case_name = self.name.iter().join("_");
        let params = if !self.params.is_empty() {
            format!("(param {})", self.params.wat_print())
        } else {
            "".to_owned()
        };
        let ret = self
            .return_type
            .as_ref()
            .map(|r| format!("(return {})", r.wat_print()))
            .unwrap_or("".to_owned());
        format!(
            "(import {} (func ${} {}{}))",
            quoted_name, snake_case_name, params, ret
        )
    }
}

pub struct WASMFuncDef {
    name: String,
    params: Vec<WASMType>,
    return_type: Option<WASMType>,
    locals: usize,
    instrs: Vec<WASMInstr>,
}
impl WASMFuncDef {
    pub fn new(
        name: &str,
        params: Vec<WASMType>,
        return_type: Option<WASMType>,
        locals: usize,
        instrs: Vec<WASMInstr>,
    ) -> WASMFuncDef {
        WASMFuncDef {
            name: name.to_string(),
            params,
            return_type,
            locals,
            instrs,
        }
    }
}
impl WATPrint for WASMFuncDef {
    fn wat_print_mut(&self, buf: &mut String) {
        buf.push_str("(func (export \"");
        buf.push_str(&self.name);
        buf.push_str("\") ");
        if self.locals > 0 {
            buf.push_str("(local");
            for _ in 1..=self.locals {
                buf.push_str(" i64");
            }
            buf.push(')');
        }
        self.instrs.wat_print_mut(buf);
        buf.push(')');
    }
}

pub struct WASMModule {
    pub imports: Vec<WASMFunImport>,
    pub funcs: Vec<WASMFuncDef>,
}

pub trait WATPrint {
    fn wat_print(&self) -> String {
        let mut buf = String::new();
        self.wat_print_mut(&mut buf);
        buf
    }
    fn wat_print_mut(&self, buf: &mut String) {
        let s = self.wat_print();
        buf.push_str(&s);
    }
}
impl WATPrint for WASMModule {
    fn wat_print_mut(&self, buf: &mut String) {
        buf.push_str("(module ");
        self.imports.wat_print_mut(buf);
        buf.push(' ');
        self.funcs.wat_print_mut(buf);
        buf.push(')');
    }
}
