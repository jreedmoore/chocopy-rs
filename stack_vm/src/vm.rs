use middle::stack::{self, ConstVal};

const VM_DEBUG: bool = false;

pub struct CallFrame {
    return_ip: usize,
    return_stack_base: usize,
}

pub struct VM<S> {
    input: Box<dyn FnMut(&mut S) -> String>,
    print: Box<dyn FnMut(&mut S, String)>,
    instruction_pointer: usize,
    stack: Vec<VMVal>,
    stack_base: usize,
    call_stack: Vec<CallFrame>,
    consts: Vec<ConstVal>,
    globals: Vec<MemVal>,
    heap: Vec<MemVal>,
    pub s: S,
}
impl<S> VM<S> {
    fn run(&mut self, p: &stack::FlatProgram) {
        if VM_DEBUG {
            println!("IR: {:?}", p.instrs);
        }
        self.instruction_pointer = p.start;
        loop {
            if self.instruction_pointer >= p.instrs.len() {
                break;
            }
            if VM_DEBUG {
                println!(
                    "Next Instr: (ip:{}) {:?}",
                    self.instruction_pointer, &p.instrs[self.instruction_pointer]
                );
            }
            let instr = &p.instrs[self.instruction_pointer];
            self.instruction_pointer += 1;
            match instr {
                stack::Instr::NumConst(n) => self.push(VMVal::Number(*n)),
                stack::Instr::BoolConst(b) => self.push(VMVal::Bool(*b)),
                stack::Instr::NoneConst => self.push(VMVal::None),

                stack::Instr::Add => self.bin_op(|l, r| l + r),
                stack::Instr::Sub => self.bin_op(|l, r| l - r),
                stack::Instr::Mul => self.bin_op(|l, r| l * r),
                stack::Instr::Modulo => self.bin_op(|l, r| l % r),
                stack::Instr::Div => self.bin_op(|l, r| l / r),

                stack::Instr::Eq => self.rel_op(|l, r| l == r),
                stack::Instr::Ne => self.rel_op(|l, r| l != r),
                stack::Instr::Lt => self.num_rel_op(|l, r| l < r),
                stack::Instr::Lte => self.num_rel_op(|l, r| l <= r),
                stack::Instr::Gt => self.num_rel_op(|l, r| l > r),
                stack::Instr::Gte => self.num_rel_op(|l, r| l >= r),

                stack::Instr::UnaryNot => {
                    let b = self.pop_bool();
                    self.push(VMVal::Bool(!b))
                }
                stack::Instr::LogicalAnd => {
                    let r = self.pop_bool();
                    let l = self.pop_bool();
                    self.push(VMVal::Bool(l && r))
                }
                stack::Instr::LogicalOr => {
                    let r = self.pop_bool();
                    let l = self.pop_bool();
                    self.push(VMVal::Bool(l || r))
                }
                stack::Instr::CallNative(n) if n == "host_print" => {
                    let v = self.pop();
                    let s = match v {
                        VMVal::Number(n) => n.to_string(),
                        VMVal::Bool(true) => "True".to_owned(),
                        VMVal::Bool(false) => "False".to_owned(),
                        VMVal::None => "None".to_owned(),
                        VMVal::StrRef(idx) => {
                            if let MemVal::Str(s) = &self.heap[idx] {
                                s.to_owned()
                            } else {
                                panic!("expected string at heap location {:?}", idx);
                            }
                        }
                        VMVal::ListRef(idx) => todo!("list print support")
                    };
                    (self.print)(&mut self.s, s)
                }
                stack::Instr::CallNative(n) if n == "len_str" => {
                    let s_ref = self.pop_str();
                    let l = self.heap_as_str(s_ref).len();
                    self.push(VMVal::Number(l as i32));
                }
                stack::Instr::CallNative(n) if n == "len_list" => {
                    let s_ref = self.pop_list();
                    let l = self.heap_as_list(s_ref).len();
                    self.push(VMVal::Number(l as i32));
                }
                stack::Instr::Call { loc, arity } => {
                    self.call_stack.push(CallFrame {
                        return_ip: self.instruction_pointer,
                        return_stack_base: self.stack_base,
                    });
                    self.stack_base = self.stack.len() - arity;
                    loc.update(&mut self.instruction_pointer);
                }
                stack::Instr::Return => {
                    let return_val = self.pop();
                    let frame = self
                        .call_stack
                        .pop()
                        .expect("returning from top-level function");
                    self.stack.truncate(self.stack_base);
                    self.stack_base = frame.return_stack_base;
                    self.instruction_pointer = frame.return_ip;
                    self.push(return_val);
                }
                stack::Instr::Drop => {
                    self.pop();
                }
                stack::Instr::LoadLocal(idx) => {
                    let v = self.stack[*idx + self.stack_base];
                    self.push(v)
                }
                stack::Instr::StoreLocal(idx) => {
                    let v = self.pop();
                    self.stack[*idx + self.stack_base] = v;
                }
                stack::Instr::Jump(loc) => loc.update(&mut self.instruction_pointer),
                stack::Instr::IfJump(loc) => {
                    if self.pop_bool() {
                        loc.update(&mut self.instruction_pointer)
                    }
                }
                stack::Instr::Nop => (),
                stack::Instr::LoadConstant(i) => {
                    let idx = self.alloc(MemVal::from_const(&p.consts[*i]));
                    let stack_ref = match &p.consts[*i] {
                        ConstVal::Str(_) => VMVal::StrRef(idx),
                    };
                    self.push(stack_ref);
                }
                stack::Instr::StrConcat => {
                    let r = self.pop_str();
                    let l = self.pop_str();
                    let mut s = self.heap_as_str(l).to_owned();
                    s.push_str(self.heap_as_str(r));
                    let idx = self.alloc(MemVal::Str(s));
                    self.push(VMVal::StrRef(idx))
                }
                stack::Instr::StrIndex => {
                    let idx = self.pop_num();
                    let s_ref = self.pop_str();
                    let c = self
                        .heap_as_str(s_ref)
                        .chars()
                        .nth(idx as usize)
                        .expect("str index out-of-bounds");
                    let mut s = String::new();
                    s.push(c);
                    let idx = self.alloc(MemVal::Str(s));
                    self.push(VMVal::StrRef(idx));
                }
                stack::Instr::Is => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(VMVal::Bool(l == r && l.is_ref() && r.is_ref()));
                }
                stack::Instr::CallNative(n) => todo!("Unsupported native call: {}", n),
                stack::Instr::ListAlloc => { 
                    let idx = self.alloc(MemVal::List(vec![]));
                    self.push(VMVal::ListRef(idx));
                }
                stack::Instr::ListAppend => {
                    let val = self.pop();
                    let l_ref = self.pop_list();
                    self.heap_as_list_mut(l_ref).push(val);
                }
                stack::Instr::ListIndex => {
                    let idx = self.pop_num();
                    let l_ref = self.pop_list();
                    let v = self.heap_as_list(l_ref)[idx as usize];
                    self.push(v.clone());
                }
                stack::Instr::ListConcat => {
                    let b_ref = self.pop_list();
                    let a_ref = self.pop_list();
                    let a = self.heap_as_list(a_ref);
                    let b = self.heap_as_list(b_ref);
                    let mut c = a.clone();
                    c.append(&mut b.clone());
                    let idx = self.alloc(MemVal::List(c));
                    self.push(VMVal::ListRef(idx));
                }
                stack::Instr::Duplicate => {
                    let v = self.pop();
                    self.push(v.clone());
                    self.push(v);
                }
                stack::Instr::ListAssign => {
                    let l_ref = self.pop_list();
                    let index = self.pop_num();
                    let val = self.pop();
                    let l = self.heap_as_list_mut(l_ref);
                    l[index as usize] = val;
                }
            }
        }
    }

    fn bin_op<F>(&mut self, f: F)
    where
        F: Fn(i32, i32) -> i32,
    {
        let r = self.pop_num();
        let l = self.pop_num();
        self.push(VMVal::Number(f(l, r)));
    }

    fn rel_op<F>(&mut self, f: F)
    where
        F: Fn(ValRef, ValRef) -> bool,
    {
        let r = self.pop();
        let l = self.pop();
        let b = f(self.follow(&l), self.follow(&r));
        self.push(VMVal::Bool(b))
    }

    fn num_rel_op<F>(&mut self, f: F)
    where
        F: Fn(i32, i32) -> bool,
    {
        let r = self.pop_num();
        let l = self.pop_num();
        self.push(VMVal::Bool(f(l, r)))
    }

    fn pop(&mut self) -> VMVal {
        self.stack.pop().expect("empty stack")
    }

    fn follow<'a>(&'a self, v: &'a VMVal) -> ValRef<'a> {
        match v {
            VMVal::StrRef(idx) => ValRef::Mem(&self.heap[*idx]),
            stack => ValRef::Stack(&stack),
        }
    }

    fn pop_bool(&mut self) -> bool {
        if let VMVal::Bool(b) = self.pop() {
            b
        } else {
            panic!("expected bool on stack")
        }
    }

    fn pop_num(&mut self) -> i32 {
        if let VMVal::Number(n) = self.pop() {
            n
        } else {
            panic!("expected int on stack")
        }
    }

    fn pop_str(&mut self) -> usize {
        if let VMVal::StrRef(idx) = self.pop() {
            idx
        } else {
            panic!("expected str ref on stack")
        }
    }

    fn pop_list(&mut self) -> usize {
        if let VMVal::ListRef(idx) = self.pop() {
            idx
        } else {
            panic!("expected list ref on stack")
        }
    }

    fn push(&mut self, v: VMVal) {
        self.stack.push(v)
    }

    fn heap_as_str(&self, idx: usize) -> &str {
        match &self.heap[idx] {
            MemVal::Str(s) => s.as_str(),
            _ => panic!("expected Str on heap, got Unused"),
        }
    }

    fn heap_as_list(&self, idx: usize) -> &Vec<VMVal> {
        match &self.heap[idx] {
            MemVal::List(v) => &v,
            _ => panic!("expected Str on heap, got Unused"),
        }
    }

    fn heap_as_list_mut(&mut self, idx: usize) -> &mut Vec<VMVal> {
        match self.heap[idx] {
            MemVal::List(ref mut v) => v,
            _ => panic!("expected Str on heap, got Unused"),
        }
    }

    fn alloc(&mut self, m: MemVal) -> usize {
        let idx = self.heap.len();
        self.heap.push(m);
        idx
    }

    fn free(&mut self, idx: usize) {
        // previously held value should be dropped?
        self.heap[idx] = MemVal::Unused;
    }
}
impl VM<IOMock> {
    fn new_mock_io() -> VM<IOMock> {
        VM {
            input: Box::new(|_| todo!()),
            print: Box::new(|mock, s| mock.output.push(s)),
            instruction_pointer: 0,
            s: IOMock { output: vec![] },
            stack: vec![],
            stack_base: 0,
            call_stack: vec![],
            consts: vec![],
            globals: vec![],
            heap: vec![],
        }
    }

    pub fn run_with_mock_io(p: &stack::FlatProgram) -> Vec<String> {
        let mut vm = VM::new_mock_io();
        vm.run(p);
        vm.s.output.clone()
    }
}

pub struct IOMock {
    output: Vec<String>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VMVal {
    Number(i32),
    Bool(bool),
    None,
    StrRef(usize),
    ListRef(usize),
}
impl VMVal {
    fn is_ref(&self) -> bool {
        match self {
            VMVal::None => true,
            VMVal::StrRef(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum MemVal {
    Str(String),
    List(Vec<VMVal>),
    Unused
}
impl MemVal {
    pub fn from_const(c: &ConstVal) -> MemVal {
        match c {
            ConstVal::Str(s) => MemVal::Str(s.clone())
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ValRef<'a> {
    Mem(&'a MemVal),
    Stack(&'a VMVal),
}
