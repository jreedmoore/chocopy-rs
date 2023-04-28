use std::fmt::Display;

use crate::stack::{self, BlockLocation};

pub fn check_exhaustive_returns(prog: &stack::Program) -> Result<(), AnalyzeError> {
    for func in &prog.funcs {
        check_exhaustive_returns_func(func)?
    }
    Ok(())
}

// Analyze the control flow graph of a function's blocks and identify if there are any reachable blocks with no out-edges
// Such a block has no path to return
fn check_exhaustive_returns_func(func: &stack::Function) -> Result<(), AnalyzeError> {
    let mut seen = vec![false; func.blocks.len()];
    let mut stack = vec![0];
    while let Some(next) = stack.pop() {
        if seen[next] {
            continue;
        }
        seen[next] = true;
        let mut out_edge = false;
        for instr in &func.blocks[next].instrs {
            match instr {
                stack::Instr::Return => out_edge = true,
                stack::Instr::IfJump(BlockLocation::BlockOffset(off)) => {
                    out_edge = true;
                    stack.push(next.checked_add_signed(*off).expect("block offset overflow"))
                }
                stack::Instr::IfJump(BlockLocation::Named(_)) => return Err(AnalyzeError::NamedJumpedOutsideOfCall),
                stack::Instr::Jump(BlockLocation::BlockOffset(off)) => {
                    out_edge = true;
                    stack.push(next.checked_add_signed(*off).expect("block offset overflow"))
                }
                stack::Instr::Jump(BlockLocation::Named(_)) => return Err(AnalyzeError::NamedJumpedOutsideOfCall),
                _ => (),
            }
        }
        if !out_edge && func.label != "entry" {
            return Err(AnalyzeError::FunctionWithoutReturn(func.label.to_owned()))
        }
    }
    Ok(())
}

#[derive(Debug)]
pub enum AnalyzeError {
    FunctionWithoutReturn(String),
    NamedJumpedOutsideOfCall,
}
impl Display for AnalyzeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for AnalyzeError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        self.source()
    }

    fn description(&self) -> &str {
        "description() is deprecated; use Display"
    }
}