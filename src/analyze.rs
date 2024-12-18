#![allow(dead_code)]

use orca_wasm::ir::id::FunctionID;
use orca_wasm::ir::module::module_functions::LocalFunction;
use orca_wasm::ir::module::LocalOrImport;
use orca_wasm::ir::module::module_types::Types;
use orca_wasm::Module;
use wasmparser::Operator;

use std::collections::{BTreeSet};
use std::fmt::Formatter;

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum ContType {
    Block,
    Loop,
    Func,
}

type ContIndex = usize;
type JumpIndex = usize;

pub trait State: Clone {
    fn exec_op<'a>(&mut self, module: &Module, op: Operator<'a>);
    fn from_func(module: &Module, local_func: &LocalFunction) -> Self;
    fn merge(&mut self, other: &Self); // TODO: idk if it should always just be two at a time
    fn clone_for_next_cont(&self) -> Self;
    fn clone_for_jmp(&self) -> Self;
}

// TODO: worth tracking exits?
// Nodes of the graph
#[derive(Debug)]
pub struct Continuation<T: State> {
    pub pc: usize,                  // either end of a block/fun or, start of a loop
    pub fallthru_pc: Option<usize>, // essentially first label following a continuation
    // bc structured control flow, we know all inflows will be resolved
    // before current has to be, except for Loop, for which we need a fixpoint
    pub inflows: Vec<JumpIndex>, // Edges that lead into the block
    pub entry_state: Option<T>,
    pub ty: ContType,
}

impl<T: State> Continuation<T> {
    pub fn merge_states(&mut self, jumps: &[Jump<T>]) {
        let state = self
            .entry_state
            .as_mut()
            .expect("attempted to merge cont whose entry state was not set");
        // jumps from the future will still have unresolved state,
        // but that should only happen for Loop
        let resolved_in_states = self
            .inflows
            .iter()
            .filter_map(|jmp_idx| jumps[*jmp_idx].state.as_ref());
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
pub struct Stats {
    pub unescaped_aliased: u32,
    pub cross_aliased: u32,
    pub unescaped_unaliased: u32,
    pub always_unescaped: u32,
    pub escaped_allocs: u32,
}

impl Stats {
    pub fn new() -> Self {
        Stats {
            unescaped_aliased: 0,
            cross_aliased: 0,
            unescaped_unaliased: 0,
            always_unescaped: 0,
            escaped_allocs: 0,
        }
    }

    pub fn calculate_percentage(&mut self, total_occurences: u32) {
        self.unescaped_aliased =
            ((self.unescaped_aliased as f64 / total_occurences as f64) * 100f64) as u32;
        self.cross_aliased =
            ((self.cross_aliased as f64 / total_occurences as f64) * 100f64) as u32;
        self.unescaped_unaliased =
            ((self.unescaped_unaliased as f64 / total_occurences as f64) * 100f64) as u32;
        self.always_unescaped =
            ((self.always_unescaped as f64 / total_occurences as f64) * 100f64) as u32;
    }
}

impl std::fmt::Display for Stats {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Percentage of Alias Unescaped accesses: {}%\nPercentage of accesses Aliases across Escaped and Unescaped Refs: {}%\nPercentage of accesses that are unambiguous and unescaped: {}%\nPercentage of accesses that are always unescaped: {}%\nPercentage of allocations that escape: {}\n", self.unescaped_aliased, self.cross_aliased, self.unescaped_unaliased, self.always_unescaped, self.escaped_allocs)
    }
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

impl<'a> TotalAnalyzer<'a, AnalysisData> {
    pub fn get_stats(&self) {
        // Function Level Stats
        let mut mod_stats = Stats::new();
        let mut mod_occurences: u32 = 0;
        println!("===== Function Level Stats =====");
        for (idx, analysis) in self.analyses.iter().enumerate() {
            let mut stats = Stats::new();
            let struct_access = analysis.collect_struct_access_info();
            for access in struct_access.iter() {
                if access.aliased_unescaped() {
                    stats.unescaped_aliased += 1;
                    mod_stats.unescaped_aliased += 1;
                }
                if access.cross_aliased() {
                    stats.cross_aliased += 1;
                    mod_stats.cross_aliased += 1;
                }
                if access.unaliased_unescaped() {
                    stats.unescaped_unaliased += 1;
                    mod_stats.unescaped_unaliased += 1;
                }
                if access.always_unescaped() {
                    stats.always_unescaped += 1;
                    mod_stats.always_unescaped += 1;
                }
            }
            // Calculate percentage and put back into stats
            stats.calculate_percentage(struct_access.len() as u32);
            stats.escaped_allocs = analysis.get_escaped_allocs_percentage();
            // Print it out
            println!("Function {}:", idx);
            println!("{}", stats);
            // Module level updates
            mod_occurences += struct_access.len() as u32;
        }
        println!("===== Module Level Stats ======");
        mod_stats.calculate_percentage(mod_occurences);
        println!("{}", mod_stats);
    }
}

impl<T: State> Analysis<T> {
    pub fn init(module: &Module, func: &LocalFunction) -> Self {
        let mut pc_conts = vec![None; func.body.num_instructions];
        let mut pc_jumps = vec![None; func.body.num_instructions];
        let mut conts = vec![Continuation {
            // the return continuation
            pc: func.body.num_instructions,
            fallthru_pc: None,
            inflows: vec![],
            ty: ContType::Func,
            entry_state: None,
        }];
        pc_conts[0] = Some(0);

        let mut jumps = vec![];
        let mut ctl_stack = vec![0];
        let mut current_cont = 0;

        let mut else_stack = vec![];

        for (pc, instr) in func.body.instructions.iter().enumerate() {
            use Operator::*;

            match &instr.extract_op() {
                Block { .. } => {
                    ctl_stack.push(conts.len());
                    conts.push(Continuation {
                        pc: 0, // to be filled in when we find the end
                        fallthru_pc: None,
                        inflows: vec![], // to be pushed later
                        ty: ContType::Block,
                        entry_state: None,
                    });
                }
                Loop { .. } => {
                    conts[current_cont].fallthru_pc = Some(pc);
                    // loop end continuation
                    ctl_stack.push(conts.len());
                    conts.push(Continuation {
                        pc: 0, // filled on loop end
                        fallthru_pc: None,
                        inflows: vec![], // to be pushed later
                        ty: ContType::Block,
                        entry_state: None,
                    });
                    current_cont = conts.len();
                    pc_conts[pc] = Some(conts.len());

                    // loop start continuation
                    ctl_stack.push(conts.len());
                    conts.push(Continuation {
                        pc, // continuation pc of loop is the loop start
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
                        target: target_cont_idx,
                        is_conditional: matches!(op, BrIf { .. }),
                        state: None,
                    });
                }
                End => {
                    let ended_ctl_idx = ctl_stack.pop().unwrap();
                    let ended_cont_ty = &mut conts[ended_ctl_idx].ty;

                    match ended_cont_ty {
                        ContType::Block => {
                            pc_conts[pc] = Some(ended_ctl_idx);
                            conts[current_cont].fallthru_pc = Some(pc);
                            current_cont = ended_ctl_idx;
                            conts[ended_ctl_idx].pc = pc;

                            if else_stack.last().is_some_and(|i| *i == ended_ctl_idx) {
                                let ifend_cont_idx = else_stack.pop().unwrap() - 1;
                                let ifend_cont: &mut Continuation<T> = &mut conts[ifend_cont_idx];
                                ifend_cont.pc = pc;
                            }
                        }
                        ContType::Loop => {
                            let loop_end_cont_idx = ctl_stack.pop().unwrap();
                            assert_eq!(conts[loop_end_cont_idx].ty, ContType::Block);
                            assert_eq!(loop_end_cont_idx, ended_ctl_idx - 1);

                            conts[ended_ctl_idx].fallthru_pc = Some(pc);

                            pc_conts[pc] = Some(loop_end_cont_idx);
                            conts[loop_end_cont_idx].pc = pc;
                            current_cont = loop_end_cont_idx;
                        }
                        ContType::Func => {},
                    }
                }
                If { .. } => {
                    // the continuation denoting the end of the whole if/else
                    let ifend_cont_idx = conts.len();
                    ctl_stack.push(ifend_cont_idx);
                    conts.push(Continuation {
                        pc: 0,
                        fallthru_pc: None,
                        inflows: vec![],
                        ty: ContType::Block,
                        entry_state: None,
                    });

                    // also push the else cont, which doesnt end up on the ctl stack unless there
                    // is an else
                    let else_cont_idx = conts.len();
                    conts.push(Continuation {
                        pc: 0,
                        fallthru_pc: Some(0),
                        inflows: vec![],
                        ty: ContType::Block,
                        entry_state: None,
                    });

                    pc_jumps[pc] = Some(jumps.len());
                    jumps.push(Jump {
                        pc,
                        target: else_cont_idx,
                        is_conditional: true,
                        state: None,
                    });
                }
                Else => {
                    let ifend_cont_idx = ctl_stack.pop().unwrap();

                    // pushed during If
                    let else_cont_idx = ifend_cont_idx + 1;

                    let before_if_ctl_idx = *ctl_stack.last().unwrap();
                    let before_if_cont = &mut conts[before_if_ctl_idx];
                    before_if_cont.fallthru_pc = Some(pc);

                    ctl_stack.push(else_cont_idx);
                    else_stack.push(else_cont_idx);
                    pc_conts[pc] = Some(else_cont_idx);

                    pc_jumps[pc] = Some(jumps.len());
                    jumps.push(Jump {
                        pc,
                        target: ifend_cont_idx,
                        is_conditional: false,
                        state: None,
                    });
                }
                // TODO: add jump from return to ret cont
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
}

impl<T: State + std::fmt::Debug> Analysis<T> {
    pub fn run(&mut self, module: &Module) {
        // run each instruction tracking data, merge when there
        // are multiple inflows, find fixpoint for loops
        let func = module.functions.get(self.func_id);
        let local_func = func.unwrap_local();
        let code = &local_func.body.instructions;
        let nconts = self.continuations.len();

        // we have to iterate in order of cont start, but they were pushed
        // in order of control nesting
        // skip the 1st el in sort because that's the fn cont which is overloaded
        let mut cont_idxs: Vec<usize> = (0..nconts).collect();
        cont_idxs[1..].sort_by_key(|i| self.continuations[*i].pc);
        let last_cont_idx = *cont_idxs.last().unwrap();

        let return_cont_state_ptr: *mut Option<_> = &mut self.continuations[0].entry_state;

        for ci in cont_idxs {
            let cont = &mut self.continuations[ci];
            let start_pc = if cont.ty == ContType::Func {
                0
            } else {
                cont.pc
            };
            if cont.ty == ContType::Func {
                cont.entry_state = Some(T::from_func(module, local_func));
            }
            let state = {
                debug_assert!(
                    cont.entry_state.is_some(),
                    "reached cont before entry state was set"
                );
                cont.merge_states(&self.jumps);
                cont.entry_state.as_mut().unwrap()
            };

            // TODO: if cont is loop, find a fixed point
            for pc in start_pc..cont.fallthru_pc.unwrap_or(code.len()) {
                let current_op = code[pc].extract_op();

                if let Some(jmp_idx) = self.pc_jumps[pc] {
                    let jmp = &mut self.jumps[jmp_idx];
                    let new = state.clone_for_jmp();
                    match jmp.state.as_mut() {
                        Some(old) => old.merge(&new),
                        None => {
                            jmp.state = Some(new);
                        }
                    }
                }

                state.exec_op(module, current_op);
            }

            if let Some(end_pc) = cont.fallthru_pc {
                let next_cont_idx = self.pc_conts[end_pc].expect("expected cont");
                // TODO: should be merged properly instead of overwriting
                self.continuations[next_cont_idx].entry_state = Some(state.clone_for_next_cont());
            } else {
                assert!(ci == last_cont_idx);
                unsafe {
                    if let Some(ret_state) = (*return_cont_state_ptr).as_mut() {
                        ret_state.merge(&state);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Annotation {
    pub new_alloc_id: Option<usize>,
    pub ty_id: Option<usize>,
    pub possibly_accessed_allocs: BTreeSet<usize>,
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
    pub refcount: usize,
    pub ty_id: usize,
}

#[derive(Default, Debug, Clone)]
pub struct RefCounts {
    pub objects: Vec<Object>,
}

#[derive(Default, Debug, Clone)]
pub struct AbstractSlot {
    references: BTreeSet<usize>,
}

impl AbstractSlot {
    fn new_ref(obj: usize) -> Self {
        AbstractSlot {
            references: std::iter::once(obj).collect(),
        }
    }

    fn empty() -> Self {
        AbstractSlot {
            references: BTreeSet::new(),
        }
    }
}

trait MergeVal: Sized {
    fn merge(&self, rhs: &Self) -> Self;
    fn merge_slice(l: &[Self], r: &[Self]) -> Vec<Self> {
        l.iter().zip(r.iter()).map(|(l, r)| l.merge(r)).collect()
    }
    fn merge_into_slice(l: &mut [Self], r: &[Self]) {
        l.iter_mut()
            .zip(r.iter())
            .for_each(|(l, r)| *l = l.merge(r))
    }
}

// Chooses the Some, or sets None if both exist
impl<T: PartialEq + Clone> MergeVal for Option<T> {
    fn merge(&self, rhs: &Self) -> Self {
        // TODO: incorrect when both are Some
        let pick_one = self.as_ref().xor(rhs.as_ref()).cloned();
        // flat bc (==) where both are Some is the same as (==) where both are None
        let both_equal = (self == rhs).then(|| self.as_ref().cloned()).flatten();
        pick_one.or(both_equal)
    }
}

impl<T: PartialEq + Clone + std::cmp::Ord> MergeVal for BTreeSet<T> {
    fn merge(&self, rhs: &Self) -> Self {
        self.union(rhs).cloned().collect()
    }
}

impl MergeVal for AbstractSlot {
    fn merge(&self, rhs: &Self) -> Self {
        AbstractSlot {
            references: self.references.merge(&rhs.references),
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
        for (left, right) in self
            .ref_counts
                .objects
                .iter_mut()
                .zip(other.ref_counts.objects.iter())
                {
                    left.refcount = left.refcount.max(right.refcount);
                }
    }

    // TODO: pass in Analysis, cont idx
    fn clone_for_next_cont(&self) -> Self {
        let mut new = self.clone();
        new.instr_annotations.clear();
        new
    }

    fn clone_for_jmp(&self) -> Self {
        self.clone_for_next_cont()
    }

    fn exec_op<'a>(&mut self, module: &Module, op: Operator<'a>) {
        use Operator::*;

        let mut annotation = Annotation {
            new_alloc_id: None,
            ty_id: None,
            possibly_accessed_allocs: BTreeSet::new(),
        };
        match op {
            I32Const { .. }
            | F64Const { .. }
            | I64Const { .. }
            | F32Const { .. }
            | MemorySize { .. } => {
                // TODO: all ops that incr stack length
                self.abstract_stack.push(AbstractSlot::empty());
            }
            // Instructions that decr stack length by 2
            I32Store { .. }
            | I32Store8 { .. }
            | I32Store16 { .. }
            | I64Store { .. }
            | I64Store16 { .. }
            | I64Store32 { .. }
            | F32Store { .. }
            | F64Store { .. } => {
                let entry = self.abstract_stack.pop().unwrap();
                self.update_rc(&entry, |rc| *rc -= 1);
                let entry = self.abstract_stack.pop().unwrap();
                self.update_rc(&entry, |rc| *rc -= 1);
            }

            I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32Or | I32Xor
                | I32Shl | I32ShrS | I32ShrU | I32Rotl | I32Rotr | I32Eq | I32Eqz | I32Ne | I32LtS
                | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS | I32GeU | I64Add | I64Sub
                | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64Or | I64Xor | I64Shl
                | I64ShrS | I64ShrU | I64Rotl | I64Rotr | I64Eq | I64Eqz | I64Ne | I64LtS | I64LtU
                | I64GtS | I64GtU | I64LeS | I64LeU | I64GeS | I64GeU | F32Add | F32Sub | F32Mul
                | F32Div | F32Eq | F32Ne | F32Le | F32Lt | F32Ge | F32Gt | F64Add | F64Sub | F64Mul
                | F64Div | F64Eq | F64Ne | F64Le | F64Lt | F64Ge | F64Gt | Drop => {
                    // TODO: all ops that decr stack length
                    let entry = self.abstract_stack.pop().unwrap();
                    self.update_rc(&entry, |rc| *rc -= 1);
                }
            StructNew { struct_type_index } | StructNewDefault { struct_type_index } => {
                let obj_idx = self.ref_counts.objects.len();
                self.ref_counts.objects.push(Object {
                    refcount: 1,
                    ty_id: struct_type_index as usize,
                });
                let num_fields = {
                    let struct_ty = &module.types.types[struct_type_index as usize];
                    if let Types::StructType { fields, .. } = struct_ty {
                        fields.len()
                    } else {
                        panic!()
                    }
                };
                self.abstract_stack.truncate(self.abstract_stack.len() - num_fields);
                self.abstract_stack.push(AbstractSlot::new_ref(obj_idx));
                annotation.new_alloc_id = Some(obj_idx);
                annotation.ty_id = Some(struct_type_index as usize);
            }
            StructGet {
                struct_type_index, ..
            }
            | StructGetS {
                struct_type_index, ..
            }
            | StructGetU {
                struct_type_index, ..
            }
            | StructSet {
                struct_type_index, ..
            } => {
                // Pop the reference from the abstract stack and decrement the ref count
                let entry = self.abstract_stack.pop().unwrap();
                self.update_rc(&entry, |rc| *rc -= 1);
                annotation.ty_id = Some(struct_type_index as usize);
                let possible_refs = &entry.references;
                annotation.possibly_accessed_allocs = possible_refs.clone();

                if !matches!(op, StructSet { .. }) {
                    self.abstract_stack.push(AbstractSlot::empty());
                }
            }
            LocalGet { local_index } => {
                let entry = self.abstract_locals[local_index as usize].clone();
                self.update_rc(&entry, |rc| *rc += 1);
                self.abstract_stack.push(entry);
            }
            LocalSet { local_index } => {
                // refcount doesnt change bc it's just moved
                let entry = self.abstract_stack.pop().unwrap().clone();

                let old_entry =
                    std::mem::replace(&mut self.abstract_locals[local_index as usize], entry);
                self.update_rc(&old_entry, |rc| *rc -= 1);
            }
            GlobalGet { global_index } => {
                let entry = self.abstract_globals[global_index as usize].clone();
                self.update_rc(&entry, |rc| *rc += 1);
                self.abstract_stack.push(entry);
            }
            GlobalSet { global_index } => {
                let entry = self.abstract_stack.pop().unwrap().clone();

                let old_entry =
                    std::mem::replace(&mut self.abstract_globals[global_index as usize], entry);
                self.update_rc(&old_entry, |rc| *rc -= 1);
            }
            Return => {
                for i in 0..self.abstract_locals.len() {
                    let entry = std::mem::take(&mut self.abstract_locals[i]);
                    self.update_rc(&entry, |rc| *rc -= 1);
                }
            }
            End => {} // TODO: stack stuff
            _ => {}
        }
        self.instr_annotations.push(annotation);
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Maybe {
    Yes,
    No,
    Unknown,
}

impl Maybe {
    pub fn maybe_yes(&self) -> bool {
        *self == Maybe::Yes || *self == Maybe::Unknown
    }

    pub fn maybe_no(&self) -> bool {
        *self == Maybe::No || *self == Maybe::Unknown
    }

    pub fn always_yes(&self) -> bool {
        *self == Maybe::Yes
    }

    pub fn always_no(&self) -> bool {
        *self == Maybe::No
    }
}

#[derive(Debug)]
pub struct StructAccessInfo {
    pub escaped: Maybe,
    pub aliased: bool,
}

impl StructAccessInfo {
    pub fn aliased_unescaped(&self) -> bool {
        self.escaped.always_no() && self.aliased
    }

    pub fn cross_aliased(&self) -> bool {
        self.escaped.maybe_yes() && self.aliased
    }

    pub fn unaliased_unescaped(&self) -> bool {
        self.escaped.always_no() && !self.aliased
    }

    pub fn aliased_escaped(&self) -> bool {
        self.escaped.always_yes() && self.aliased
    }

    pub fn always_unescaped(&self) -> bool {
        self.escaped.always_no()
    }
}

struct StructAccessStats {
    pub total_count: usize,
    pub unaliased_unescaped_count: usize,
}

impl AnalysisData {
    pub fn update_rc(&mut self, entry: &AbstractSlot, mut f: impl FnMut(&mut usize)) {
        entry
            .references
            .iter()
            .for_each(|obj| f(&mut self.ref_counts.objects[*obj].refcount))
    }
}

impl Analysis<AnalysisData> {
    pub fn get_escaped_allocs(&self) -> BTreeSet<usize> {
        let ret_cont = &self.continuations[0];
        let mut ret_state = ret_cont.entry_state.as_ref().unwrap().clone();

        dbg!(&ret_state);
        let dead_slots: Vec<AbstractSlot> =
            ret_state.abstract_stack.iter().chain(ret_state.abstract_locals.iter()).cloned().collect();
        for stackref in dead_slots.into_iter() {
            ret_state.update_rc(&stackref, |rc| *rc -= 1);
        }
        ret_state
            .ref_counts
            .objects
            .iter()
            .enumerate()
            .filter_map(|(i, obj)| (obj.refcount > 0).then_some(i))
            .collect()
    }

    pub fn get_escaped_allocs_percentage(&self) -> u32 {
        let ret_cont = &self.continuations[0];
        let ret_state = ret_cont.entry_state.as_ref().unwrap();

        // Count the objects where refcount > 0
        let count_ref_gt_zero = ret_state
            .ref_counts
            .objects
            .iter()
            .filter(|obj| obj.refcount > 0)
            .count();

        // Calculate the percentage
        let total_objects = ret_state.ref_counts.objects.len();

        if total_objects == 0 {
            0 // Avoid division by zero if there are no objects
        } else {
            ((count_ref_gt_zero as f64 / total_objects as f64) * 100.0) as u32
        }
    }

    pub fn collect_struct_access_info(&self) -> Vec<StructAccessInfo> {
        let escaped_allocs = self.get_escaped_allocs();
        self.continuations
            .iter()
            .flat_map(|c| c.entry_state.as_ref().unwrap().instr_annotations.iter())
            .filter_map(
                |annotation| match annotation.possibly_accessed_allocs.len() {
                    0 => None,
                    n => {
                        let aliased = n > 1;
                        let n_escaped = annotation
                            .possibly_accessed_allocs
                            .iter()
                            .filter(|o| escaped_allocs.contains(o))
                            .count();
                        let escaped = if n_escaped == n {
                            Maybe::Yes
                        } else if n_escaped == 0 {
                            Maybe::No
                        } else {
                            Maybe::Unknown
                        };
                        Some(StructAccessInfo { escaped, aliased })
                    }
                },
            )
            .collect()
    }
}
