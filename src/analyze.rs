use wasmparser::Operator;
use orca_wasm::ir::module::module_functions::LocalFunction;

pub struct AbstractState<T> {
    pub stack: Vec<T>,
    pub locals: Vec<T>,
    // pub heap: Vec<T>, // might want a different repr for heap stuff
}

pub struct AnalysisData {
    pub ref_counts: RefCounts,
    pub escape_analysis: EscapeState,
}

pub enum ContType {
    Block,
    Loop,
    Func,
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

impl<'a, T: Default> Analysis<'a, T> {
    pub fn init_continuations(func: &'a LocalFunction) -> (Vec<Continuation>, Vec<Jump<T>>) {
        let mut conts = vec![Continuation{
            pc: func.body.instructions.len(),
            inflows: vec![],
            ty: ContType::Func,
        }];

        let mut jumps = vec![];

        // TODO: straightforward iterate over instructions, fill in graph edges

        (conts, jumps)
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
