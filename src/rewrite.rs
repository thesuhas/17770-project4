use orca_wasm::ir::id::{TypeID, LocalID};
use orca_wasm::ir::module::module_types::Types;
use orca_wasm::ir::types::{InstrumentationMode, Location};
use orca_wasm::opcode::InjectAt;
use orca_wasm::Module;
use orca_wasm::module_builder::AddLocal;
use orca_wasm::ir::types::{DataType, BlockType};
use wasmparser::Operator;
use orca_wasm::opcode::{Instrumenter, Opcode};

use std::collections::BTreeSet;

use crate::analyze::{Analysis, AnalysisData, State};

#[derive(Clone)]
pub struct StructAlloc {
    local_offset: u32,
    num_fields: u32,
}

pub struct Rewriter<'a, T: State> {
    pub analysis: &'a mut Analysis<T>,
    pub struct_allocs: Vec<StructAlloc>,
    pub escaped_allocs: BTreeSet<usize>,
}

impl<'a> Rewriter<'a, AnalysisData> {
    pub fn new(analysis: &'a mut Analysis<AnalysisData>) -> Self {
        let num_allocations = analysis
            .continuations
            .iter()
            .map(|cont| cont.entry_state.clone().unwrap())
            .flat_map(|data| data.instr_annotations)
            .filter_map(|annotation| annotation.new_alloc_id)
            .count();

        // let escaped_allocs = analysis.get_escaped_allocs();
        let escaped_allocs = BTreeSet::new();

        Self {
            analysis,
            struct_allocs: vec![
                StructAlloc {
                    local_offset: 0,
                    num_fields: 0
                };
                num_allocations
            ],
            escaped_allocs,
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
                                let alloc_id = annotation.new_alloc_id.unwrap();
                                if self.escaped_allocs.contains(&alloc_id) {
                                    pc += 1;
                                    continue;
                                }

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

                                // inject our own i31 ref
                                let i32_const = Operator::I32Const { value: alloc_id as i32 };
                                modifier.inject_at(
                                    pc,
                                    InstrumentationMode::Alternate,
                                    i32_const,
                                );
                                modifier.inject_at(pc, InstrumentationMode::Alternate, Operator::RefI31);
                            }
                            _ => todo!(),
                        }
                    }
                    Operator::StructGet { struct_type_index, field_index } => {
                        // TODO: I think this only works when we are certain the struct
                        // is not escaped, otherwise we don't know if the top of the stack
                        // is an i31 or a proper structref. We can probably fix that with
                        // extern.convert_any and any.convert_extern
                        //
                        // I'm assuming the top is an i31 to make things easier and lift some
                        // code

                        let loc = Location::Module { 
                            func_idx: self.analysis.func_id, instr_idx: pc
                        };
                        let possible_allocs = &annotation.possibly_accessed_allocs;
                        if possible_allocs.iter().any(|obj| self.escaped_allocs.contains(obj)) {
                            pc += 1;
                            continue;
                        }

                        // have to add a local so that we can repeatedly compare against
                        // top of stack
                        let reflocal = modifier.add_local(DataType::I32);
                        modifier.set_instrument_mode_at(InstrumentationMode::Alternate, loc);

                        let field_ty = {
                            let struct_ty = &module.types.types[*struct_type_index as usize];
                            if let Types::StructType { fields, .. } = struct_ty {
                                fields[*field_index as usize]
                            } else {
                                panic!()
                            }
                        };
                        let block_ty = BlockType::Type(field_ty);

                        // pseudowasm:
                        // initial stack: [... | i31ref]
                        // i31.get_u -- [... | i32ref]
                        // local.set reflocal -- [...]
                        modifier.i31_get_u();
                        modifier.local_set(reflocal);

                        // could use a br_table? idk for now just use if chain
                        // pseudowasm:
                        // ...
                        // i32.eqz
                        // if
                        //  ...
                        // else
                        //  local.get reflocal
                        //  i32.const (#global_id) -- [... | i32ref, choice]
                        //  i32.sub
                        //  i32.eqz
                        //  if
                        //      local.get field_offs
                        //  else | end
                        //  ...
                        for (i, choice) in possible_allocs.iter().enumerate() {
                            let struct_offs = self.struct_allocs[*choice].local_offset;
                            let field_offs = struct_offs + *field_index as u32;

                            if i < possible_allocs.len() - 1{
                                modifier.local_get(reflocal);
                                modifier.i32_const(*choice as i32);
                                modifier.i32_sub();
                                modifier.i32_eqz();
                                modifier.if_stmt(block_ty);
                            }

                            modifier.local_get(LocalID(field_offs));
                            if i < possible_allocs.len() - 1 {
                                modifier.else_stmt();
                            } else if i > 0 {
                                modifier.end();
                            }
                        }
                    }
                    _ => {}
                }
                pc += 1;
            }
        }
    }
}
