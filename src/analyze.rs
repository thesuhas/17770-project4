use std::default;

use orca_wasm::ir::module::LocalOrImport;
use wasmparser::Operator;
use orca_wasm::ir::module::module_functions::LocalFunction;
use orca_wasm::Module;

pub struct AbstractState<T> {
    pub stack: Vec<T>,
    pub locals: Vec<T>,
    // pub heap: Vec<T>, // might want a different repr for heap stuff
}

pub struct AnalysisData {
    pub ref_counts: RefCounts,
    pub escape_analysis: EscapeState,
}

#[derive(Eq, PartialEq)]
pub enum ContType {
    Block,
    Loop,
    Func,LocalFunction
}

type ContIndex = usize;
type JumpIndex = usize;

// TODO: worth tracking exits?
// Nodes of the graph
pub struct Continuation {
    pub pc: usize, // Exit instruction of the block. Start of loop or end of Block/Func
    // bc structured control flow, we know all inflows will be resolved 
    // before current has to be, except for Loop, for which we need a fixpoint
    pub inflows: Vec<JumpIndex>, // Edges that lead into the block
    pub ty: ContType,
}

// Edges in the CFG
pub struct Jump<T> {
    pub pc: usize, // Source instruction that the edge starts from
    pub target: ContIndex, // Continuation Block that this Edge targets
    pub is_conditional: bool,
    pub state: T,
}

pub struct Analysis<'a, T> {
    pub func: &'a LocalFunction<'a>,
    pub continuations: Vec<Continuation>,
    pub jumps: Vec<Jump<T>>,
}

pub struct TotalAnalyzer<'a, T> {
    pub module: Module<'a>,
    pub analyses: Vec<Analysis<'a, T>>,
}

impl<'a, T: Default> TotalAnalyzer<'a, T> {
    pub fn init_analysis(module: Module<'a>) -> Self {
        // let wasm = wat::parse_file(path).expect("unable to convert");
        // let module = Module::parse(&wasm, false).expect("Error parsing");
        let mut analyses = vec![];
        for func in module.functions.iter() {
            if func.is_local() {
                let local_func = func.unwrap_local();
                analyses.push(Analysis::init(local_func));
            }
        }
        TotalAnalyzer {
            module,
            analyses,
        }
    }
}

impl<'a, T: Default> Analysis<'a, T> {
    pub fn init(func: &'a LocalFunction) -> Self {
        let mut conts = vec![Continuation{
            pc: func.body.instructions.len(),
            inflows: vec![],
            ty: ContType::Func,
        }];
        let mut jumps = vec![];

        let mut ctl_stack = vec![0];

        for (pc, instr) in func.body.instructions.iter().enumerate() {
            use Operator::*;

            match &instr.extract_op() {
                Block { .. } => {
                    ctl_stack.push(conts.len());
                    conts.push(Continuation {
                        pc: 0, // to be filled in when we find the end
                        inflows: vec![], // to be pushed later
                        ty: ContType::Block,
                    });
                }
                Loop { .. } => {
                    ctl_stack.push(conts.len());
                    conts.push(Continuation {
                        pc, // continuation pc of loop is the loop start
                        inflows: vec![], // to be pushed later
                        ty: ContType::Loop,
                    });
                }
                op @ (Br { relative_depth } | BrIf { relative_depth }) => {
                    let target_cont_idx = ctl_stack[ctl_stack.len() - 1 - *relative_depth as usize];
                    let target_cont = &conts[target_cont_idx];
                    jumps.push(Jump {
                        pc,
                        target: target_cont.pc,
                        is_conditional: matches!(op, BrIf { .. }),
                        state: T::default(),
                    });
                }
                End => {
                    let idx = ctl_stack.pop();
                    let cont = &mut conts[idx];
                    if cont.ty == ContType::Block {
                        cont.pc = pc;
                    }
                }
                _ => {},
            }
        }

        Analysis {
            func,
            continuations: conts,
            jumps,
        }
    }

    pub fn run(func: &'a LocalFunction) -> Analysis<'a, T> {
        // run each instruction tracking data, merge when there
        // are multiple inflows, find fixpoint for loops
        todo!();
    }
}

pub struct RefCounts {
    pub counts: AbstractState<usize>,
}

pub struct EscapeState {
    pub escaped: AbstractState<bool>,
}

impl AnalysisData {
    pub fn execOp<'a>(&mut self, op: Operator<'a>) {
        use Operator::*;

        match op {
            Nop => {},   
            _ => {},
        }
    }
}
