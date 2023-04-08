use middle::stack::{self, MemVal};

const VM_DEBUG: bool = false;

pub struct VM<S> 
{
    input: Box<dyn FnMut(&mut S) -> String>,
    print: Box<dyn FnMut(&mut S,String)>,
    instruction_pointer: usize,
    stack: Vec<VMVal>,
    consts: Vec<MemVal>,
    globals: Vec<MemVal>,
    heap: Vec<MemVal>,
    pub s: S
}
impl<S> VM<S> 
{
    fn run(&mut self, p: &stack::FlatProgram)
    {
        if VM_DEBUG {
            println!("IR: {:?}", p.instrs);
        }
        loop {
            if self.instruction_pointer >= p.instrs.len() {
                break;
            }
            if VM_DEBUG {
                println!("Next Instr: (ip:{}) {:?}", self.instruction_pointer, &p.instrs[self.instruction_pointer]);
            }
            match &p.instrs[self.instruction_pointer] {
                stack::Instr::NumConst(n) => self.push(VMVal::Number(*n)),
                stack::Instr::BoolConst(b) => self.push(VMVal::Bool(*b)),
                stack::Instr::NoneConst => self.push(VMVal::None),

                stack::Instr::Add => self.bin_op(|l,r| l + r),
                stack::Instr::Sub => self.bin_op(|l,r| l - r),
                stack::Instr::Mul => self.bin_op(|l,r| l * r),
                stack::Instr::Modulo => self.bin_op(|l,r| l % r),
                stack::Instr::Div => self.bin_op(|l,r| l / r),

                stack::Instr::Eq => self.rel_op(|l,r| l == r),
                stack::Instr::Ne => self.rel_op(|l,r| l != r),
                stack::Instr::Lt => self.num_rel_op(|l,r| l < r),
                stack::Instr::Lte => self.num_rel_op(|l,r| l <= r),
                stack::Instr::Gt => self.num_rel_op(|l,r| l > r),
                stack::Instr::Gte => self.num_rel_op(|l,r| l >= r),

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
                stack::Instr::Call(n) if n == "host_print" => {
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
                    };
                    (self.print)(&mut self.s, s)
                }
                stack::Instr::Call(_) => todo!(),
                stack::Instr::Drop => { self.pop(); }
                stack::Instr::LoadLocal(idx) => {
                    let v = self.stack[*idx];
                    self.push(v)
                }
                stack::Instr::StoreLocal(idx) => {
                    let v = self.pop();
                    self.stack[*idx] = v;
                }
                stack::Instr::Jump(stack::InstrLocation::InstrOffset(off)) => self.instruction_pointer = self.instruction_pointer.checked_add_signed(*off).expect("ip overflow"),
                stack::Instr::IfJump(stack::InstrLocation::InstrOffset(off)) => {
                    if self.pop_bool() {
                        self.instruction_pointer = self.instruction_pointer.checked_add_signed(*off).expect("ip overflow")
                    }
                },
                stack::Instr::Nop => (),
                stack::Instr::LoadConstant(i) => {
                    let idx = self.heap.len();
                    self.heap.push(p.consts[*i].clone());
                    let stack_ref = match &p.consts[*i] {
                        MemVal::Str(_) => VMVal::StrRef(idx),
                        MemVal::Unused => panic!("Loading uninitialized constant"),
                    };
                    self.push(stack_ref);
                }
           }
           self.instruction_pointer += 1;
        }
    }

    fn bin_op<F>(&mut self, f: F) 
    where 
        F: Fn(i32, i32) -> i32
    {
        let r = self.pop_num();
        let l = self.pop_num();
        self.push(VMVal::Number(f(l, r)));
    }

    fn rel_op<F>(&mut self, f: F)
    where
        F: Fn(VMVal, VMVal) -> bool
    {
        let r = self.pop();
        let l = self.pop();
        self.push(VMVal::Bool(f(l,r)))
    }

    fn num_rel_op<F>(&mut self, f: F)
    where
        F: Fn(i32, i32) -> bool
    {
        let r = self.pop_num();
        let l = self.pop_num();
        self.push(VMVal::Bool(f(l,r)))
    }

    fn pop(&mut self) -> VMVal {
        self.stack.pop().expect("empty stack")
    }

    fn pop_bool(&mut self) -> bool {
        if let VMVal::Bool(b) = self.stack.pop().expect("non empty stack") {
            b
        } else {
            panic!("expected bool on stack")
        }
    }

    fn pop_num(&mut self) -> i32 {
        if let VMVal::Number(n) = self.stack.pop().expect("non empty stack") {
            n
        } else {
            panic!("expected bool on stack")
        }
    }

    fn push(&mut self, v: VMVal) {
        self.stack.push(v)
    }
}
impl VM<IOMock> 
{
    fn new_mock_io() -> VM<IOMock> {
        VM { input: Box::new(|_| todo!()), print: Box::new(|mut mock,s| mock.output.push(s)), instruction_pointer: 0, s: IOMock { output: vec![] }, stack: vec![], consts: vec![], globals: vec![], heap: vec![] }
    }

    pub fn run_with_mock_io(p: &stack::FlatProgram) -> Vec<String> {
        let mut vm = VM::new_mock_io();
        vm.run(p);
        vm.s.output.clone()
    }
}

pub struct IOMock {
    output: Vec<String>
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VMVal {
    Number(i32),
    Bool(bool),
    None,
    StrRef(usize)
}