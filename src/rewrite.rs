use orca_wasm::ir::id::TypeID;
use orca_wasm::ir::module::module_types::Types;
use orca_wasm::ir::types::InstrumentationMode;
use orca_wasm::opcode::InjectAt;
use orca_wasm::Module;
use std::collections::HashSet;
use wasmparser::Operator;

use crate::analyze::{Analysis, AnalysisData, State};

#[derive(Clone)]
pub struct StructAlloc {
    local_offset: u32,
    num_fields: u32,
}

pub struct Rewriter<'a, T: State> {
    pub analysis: &'a mut Analysis<T>,
    pub struct_allocs: Vec<StructAlloc>,
    pub escaped_types: HashSet<usize>,
}

impl<'a> Rewriter<'a, AnalysisData> {
    pub fn new(analysis: &'a mut Analysis<AnalysisData>) -> Self {
        let num_allocations = analysis
            .continuations
            .iter()
            .map(|cont| cont.entry_state.clone().unwrap())
            .flat_map(|data| data.instr_annotations)
            .filter_map(|annotation| annotation.alloc_id)
            .count();

        let mut escaped_types = HashSet::new();

        for cont in analysis.continuations.iter() {
            let analysis = cont.entry_state.as_ref().unwrap();
            for obj in analysis.ref_counts.objects.iter() {
                // If it is escaping
                if obj.refcount > 0 {
                    // Get the type id
                    escaped_types.insert(obj.ty_id);
                }
            }
        }

        Self {
            analysis,
            struct_allocs: vec![
                StructAlloc {
                    local_offset: 0,
                    num_fields: 0
                };
                num_allocations
            ],
            escaped_types,
        }
    }

    pub fn rewrite(&mut self, module: &mut Module) {
        let mut modifier = module
            .functions
            .get_fn_modifier(self.analysis.func_id)
            .unwrap();

        let mut pc = 0;

        for cont in &self.analysis.continuations {
            let analysis = cont.entry_state.as_ref().unwrap();
            let annotations = &analysis.instr_annotations;
            let aidx = pc;

            while pc < cont.fallthru_pc.unwrap_or(modifier.body.num_instructions) {
                let op = &modifier.body.instructions[pc].extract_op();
                let annotation = &annotations[pc - aidx];
                match op {
                    Operator::StructNew { struct_type_index } => {
                        let ty = module.types.get(TypeID(*struct_type_index)).unwrap();
                        match ty {
                            Types::StructType { fields, .. } => {
                                match annotation.ty_id {
                                    None => {}
                                    Some(ty_id) => {
                                        if self.escaped_types.contains(&ty_id) {
                                            println!(
                                                "Type ID: {} is an escaped type, continuing",
                                                ty_id
                                            );
                                            continue;
                                        }
                                    }
                                }
                                let alloc_id = annotation.alloc_id.unwrap();

                                let num_fields = fields.len() as u32;
                                let local_offset = modifier.body.num_locals;
                                self.struct_allocs[alloc_id] = StructAlloc {
                                    local_offset,
                                    num_fields,
                                };

                                modifier.add_locals(fields);
                                for (i, _field) in fields.iter().rev().enumerate() {
                                    let local_index = local_offset + i as u32;
                                    let local_set = Operator::LocalSet { local_index };
                                    modifier.inject_at(
                                        pc,
                                        InstrumentationMode::Alternate,
                                        local_set,
                                    );
                                }
                                // modifier.inj
                            }
                            _ => todo!(),
                        }
                    }
                    _ => {}
                }
                pc += 1;
            }
        }
    }
}
