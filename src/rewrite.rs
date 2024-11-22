use orca_wasm::Module;
use orca_wasm::ir::id::TypeID;
use orca_wasm::ir::module::module_types::Types;
use wasmparser::{FuncType, Operator};

use crate::analyze::{Analysis, AnalysisData, State};

pub struct Rewriter<T: State> {
    pub analysis: Analysis<T>,
    pub alloc_local_idx: Vec<usize>,
}

impl Rewriter<AnalysisData> {
    fn rewrite(&self, module: &mut Module) {
        let fun = module.functions.get(self.analysis.func_id);
        let local_fn = fun.unwrap_local();

        let mut pc = 0;
        let mut nlocals = local_fn.body.num_locals;

        for cont in &self.analysis.continuations {
            let analysis = cont.entry_state.unwrap();
            let annotations = analysis.instr_annotations;
            let aidx = pc;

            while pc < cont.fallthru_pc.unwrap_or(local_fn.body.num_instructions) {
                let op = &local_fn.body.instructions[pc].extract_op();
                let annotation = annotations[pc - aidx];
                match op {
                    Operator::StructNew { struct_type_index } => {
                        let ty = module.types.get(TypeID(*struct_type_index)).unwrap();
                        match ty {
                            Types::StructType { fields, .. } => {
                                let nfields = fields.len() as u32;
                                let alloc_id = annotation.alloc_id.unwrap();
                                nlocals += nfields;
                            }
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
        }
    }
}