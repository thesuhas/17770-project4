#![allow(dead_code)]

use orca_wasm::ir::module::module_functions::LocalFunction;
use orca_wasm::ir::module::LocalOrImport;
use orca_wasm::Module;
use orca_wasm::ir::id::FunctionID;
use wasmparser::Operator;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ContType {
    Block,
    Loop,
    Func,
}

type ContIndex = usize;
type JumpIndex = usize;

pub trait State: Clone {
    fn exec_op<'a>(&mut self, op: Operator<'a>);
    fn from_func(module: &Module, local_func: &LocalFunction) -> Self;
    fn merge(&mut self, other: &Self); // TODO: idk if it should always just be two at a time
}

// TODO: worth tracking exits?
// Nodes of the graph
#[derive(Debug)]
pub struct Continuation<T: State> {
    pub pc: usize, // either end of a block/fun or, start of a loop
    pub fallthru_pc: Option<usize>, // essentially first label following a continuation
    // bc structured control flow, we know all inflows will be resolved
    // before current has to be, except for Loop, for which we need a fixpoint
    pub inflows: Vec<JumpIndex>, // Edges that lead into the block
    pub entry_state: Option<T>,
    pub ty: ContType,
}

impl<T: State> Continuation<T> {
    pub fn merge_states(&mut self, jumps: &[Jump<T>]) {
        let state = self.entry_state.as_mut().expect("attempted to merge cont whose entry state was not set");
        // jumps from the future will still have unresolved state,
        // but that should only happen for Loop
        let resolved_in_states =
            self.inflows.iter().filter_map(|jmp_idx| jumps[*jmp_idx].state.as_ref());
        for jmp_state in resolved_in_states {
            state.merge(jmp_state);
        }
    }
}

// Edges in the CFG
#[derive(Debug)]
pub struct Jump<T> {
    pub pc: usize,         // Source instruction that the edge starts from
    pub target: ContIndex, // Continuation Block that this Edge targets
    pub is_conditional: bool,
    pub state: Option<T>,
}

#[derive(Debug)]
pub struct Analysis<T: State> {
    pub func_id: FunctionID,
    pub continuations: Vec<Continuation<T>>,
    pub jumps: Vec<Jump<T>>,
    pub pc_conts: Vec<Option<ContIndex>>,
    pub pc_jumps: Vec<Option<JumpIndex>>,
}

#[derive(Debug)]
pub struct TotalAnalyzer<'a, T: State> {
    pub module: Module<'a>,
    pub analyses: Vec<Analysis<T>>,
}

impl<'a, T: State> TotalAnalyzer<'a, T> {
    pub fn init_analysis(module: Module<'a>) -> Self {
        let mut analyses = vec![];
        for func in module.functions.iter() {
            if func.is_local() {
                let local_func = func.unwrap_local();
                analyses.push(Analysis::init(&module, local_func));
            }
        }
        TotalAnalyzer { module, analyses }
    }
}

impl<T: State> Analysis<T> {
    pub fn init(module: &Module, func: &LocalFunction) -> Self {
        let mut pc_conts = vec![None; func.body.num_instructions];
        let mut pc_jumps = vec![None; func.body.num_instructions];
        let mut conts = vec![
            Continuation {
                pc: func.body.num_instructions,
                fallthru_pc: None,
                inflows: vec![],
                ty: ContType::Func,
                entry_state: None,
            },
        ];
        pc_conts[0] = Some(0);

        let mut jumps = vec![];
        let mut ctl_stack = vec![0];
        let mut current_cont = 0;

        for (pc, instr) in func.body.instructions.iter().enumerate() {
            use Operator::*;

            match &instr.extract_op() {
                Block { .. } => {
                    ctl_stack.push(conts.len());
                    conts.push(Continuation {
                        pc: 0,           // to be filled in when we find the end
                        fallthru_pc: None,
                        inflows: vec![], // to be pushed later
                        ty: ContType::Block,
                        entry_state: None,
                    });
                }
                Loop { .. } => {
                    conts[current_cont].fallthru_pc = Some(pc);
                    current_cont = conts.len();
                    pc_conts[pc] = Some(conts.len());
                    ctl_stack.push(conts.len());
                    conts.push(Continuation {
                        pc,              // continuation pc of loop is the loop start
                        fallthru_pc: None,
                        inflows: vec![], // to be pushed later
                        ty: ContType::Loop,
                        entry_state: None,
                    });
                }
                op @ (Br { relative_depth } | BrIf { relative_depth }) => {
                    let target_cont_idx = ctl_stack[ctl_stack.len() - 1 - *relative_depth as usize];
                    let target_cont = &mut conts[target_cont_idx];
                    pc_jumps[pc] = Some(jumps.len());
                    target_cont.inflows.push(jumps.len());
                    jumps.push(Jump {
                        pc,
                        target: target_cont.pc,
                        is_conditional: matches!(op, BrIf { .. }),
                        state: None,
                    });
                }
                End => {
                    let new_cont_idx = ctl_stack.pop().unwrap();
                    let cont_ty = conts[new_cont_idx].ty;
                    if cont_ty == ContType::Block {
                        pc_conts[pc] = Some(new_cont_idx);
                        conts[current_cont].fallthru_pc = Some(pc);
                        current_cont = new_cont_idx;
                        conts[new_cont_idx].pc = pc;
                    }
                }
                _ => {}
            }
        }

        Analysis {
            func_id: func.func_id,
            continuations: conts,
            jumps,
            pc_conts,
            pc_jumps,
        }
    }

    pub fn run(&mut self, module: &Module) {
        // run each instruction tracking data, merge when there
        // are multiple inflows, find fixpoint for loops
        let func = module.functions.get(self.func_id);
        let local_func = func.unwrap_local();
        let code = &local_func.body.instructions;
        let nconts = self.continuations.len();

        // we start loop with continuations.len() so we can use the FuncStart continuation
        for ci in 0..nconts {
            let cont = &mut self.continuations[ci];
            let mut pc = if cont.ty == ContType::Func { 0 } else { cont.pc };
            if cont.ty == ContType::Func {
                cont.entry_state = Some(T::from_func(module, local_func));
            }
            let mut state = {
                debug_assert!(cont.entry_state.is_some(), "reached cont before entry state was set");
                cont.merge_states(&self.jumps);
                cont.entry_state.as_ref().cloned().unwrap()
            };

            while pc < cont.fallthru_pc.unwrap_or(code.len()) {
                let current_op = code[pc].extract_op();

                match current_op {
                    Operator::Loop { .. } | Operator::End => {
                        let next_cont_idx = self.pc_conts[pc].expect("all loop and end should have a cont");
                        self.continuations[next_cont_idx].entry_state = Some(state.clone());
                        break;
                    }
                    op => state.exec_op(op),
                }
                pc += 1;
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Annotation {
    pub alloc_id: Option<usize>,
}

#[derive(Default, Debug, Clone)]
pub struct AnalysisData {
    pub ref_counts: RefCounts,
    pub abstract_stack: Vec<AbstractSlot>,
    pub abstract_locals: Vec<AbstractSlot>,
    pub abstract_globals: Vec<AbstractSlot>,
    pub instr_annotations: Vec<Annotation>,
}

#[derive(Debug, Clone)]
pub struct Object {
    refcount: usize,
}

#[derive(Default, Debug, Clone)]
pub struct RefCounts {
    pub objects: Vec<Object>,
}

#[derive(Default, Debug, Clone)]
pub struct AbstractSlot {
    reference: Option<usize>,
}

impl AbstractSlot {
    fn new_ref(obj: usize) -> Self {
        AbstractSlot { reference: Some(obj) }
    }

    fn empty() -> Self {
        AbstractSlot { reference: None }
    }
}

trait MergeVal: Sized {
    fn merge(&self, rhs: &Self) -> Self;
    fn merge_slice(l: &[Self], r: &[Self]) -> Vec<Self> {
        l.iter().zip(r.iter()).map(|(l, r)| l.merge(r)).collect()
    }
    fn merge_into_slice(l: &mut [Self], r: &[Self]) {
        l.iter_mut().zip(r.iter()).for_each(|(l, r)| {
            *l = l.merge(r)
        })
    }
}

impl<T: PartialEq + Clone> MergeVal for Option<T> {
    fn merge(&self, rhs: &Self) -> Self {
        let pick_one = self.as_ref().xor(rhs.as_ref()).cloned();
        // flat bc (==) where both are Some is the same as (==) where both are None
        let both_equal = (self == rhs).then(|| self.as_ref().cloned()).flatten();
        pick_one.or(both_equal)
    }
}

impl MergeVal for AbstractSlot {
    fn merge(&self, rhs: &Self) -> Self {
        AbstractSlot {
            reference: self.reference.merge(&rhs.reference)
        }
    }
}

impl State for AnalysisData {
    fn from_func(module: &Module, local_func: &LocalFunction) -> Self {
        // the stack and globals may contain references from other functions,
        // which we don't have to track since those are already escaped anyway
        AnalysisData {
            abstract_stack: vec![AbstractSlot::empty(); local_func.args.len()],
            abstract_locals: vec![AbstractSlot::empty(); local_func.body.num_locals as usize],
            abstract_globals: vec![AbstractSlot::empty(); module.globals.len()],
            ref_counts: RefCounts { objects: vec![] },
            instr_annotations: vec![],
        }
    }

    fn merge(&mut self, other: &Self) {
        assert_eq!(self.abstract_stack.len(), other.abstract_stack.len());
        assert_eq!(self.abstract_locals.len(), other.abstract_locals.len());
        assert_eq!(self.abstract_globals.len(), other.abstract_globals.len());
        MergeVal::merge_into_slice(&mut self.abstract_stack, &other.abstract_stack);
        MergeVal::merge_into_slice(&mut self.abstract_locals, &other.abstract_locals);
        MergeVal::merge_into_slice(&mut self.abstract_globals, &other.abstract_globals);
    }

    fn exec_op<'a>(&mut self, op: Operator<'a>) {
        use Operator::*;

        match op {
            I32Const { .. } => { // TODO: all ops that incr stack length
                self.abstract_stack.push(AbstractSlot::empty());
            }
            I32Add => { // TODO: all ops that decr stack length
                self.abstract_stack.pop();
            }
            StructNew { .. } | StructNewDefault { .. } => {
                let obj_idx = self.ref_counts.objects.len();
                self.ref_counts.objects.push(Object { refcount: 0 });
                self.abstract_stack.push(AbstractSlot::new_ref(obj_idx));
            }
            LocalGet { local_index } => {
                let entry = self.abstract_locals[local_index as usize].clone();
                self.abstract_stack.push(entry);
            }
            LocalSet { local_index } => {
                let entry = self.abstract_stack.pop().unwrap().clone();
                self.abstract_locals[local_index as usize] = entry;
            }
            GlobalGet { global_index } => {
                let entry = self.abstract_globals[global_index as usize].clone();
                self.abstract_stack.push(entry);
            }
            GlobalSet { global_index } => {
                let entry = self.abstract_stack.pop().unwrap().clone();
                self.abstract_globals[global_index as usize] = entry;
            }
            _ => {},
        }
    }
}