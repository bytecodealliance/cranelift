use std::collections::HashMap;
use cranelift_codegen::ir::*;
use cranelift_codegen::ir::condcodes::IntCC;
use frontend::FunctionBuilder;

type EntryIndex = i64;

#[derive(Debug)]
pub struct Switch {
    cases: HashMap<EntryIndex, Ebb>
}

impl Switch {
    pub fn new() -> Self {
        Switch {
            cases: HashMap::new(),
        }
    }

    pub fn set_entry(&mut self, index: EntryIndex, ebb: Ebb) {
        assert!(self.cases.insert(index, ebb).is_none(), "Tried to set the same entry {} twice", index);
    }

    fn build_cases_tree(self) -> Vec<(EntryIndex, Vec<Ebb>)> {
        println!("before: {:#?}", self.cases);
        let mut cases = self.cases.into_iter().collect::<Vec<(_, _)>>();
        cases.sort_by_key(|&(index, _)| index);

        let mut cases_tree: Vec<Vec<(EntryIndex, Ebb)>> = vec![];
        let mut last_index = None;
        for (index, ebb) in cases {
            if Some(index - 1) > last_index {
                cases_tree.push(Vec::new());
            }
            cases_tree.last_mut().unwrap().push((index, ebb));
            last_index = Some(index);
        }

        println!("after: {:#?}", cases_tree);

        cases_tree
            .into_iter()
            .map(|cases| {
                (
                    cases[0].0,
                    cases.iter().map(|&(_, ebb)| ebb).collect(),
                )
            })
            .collect()
    }

    pub fn emit(self, bx: &mut FunctionBuilder, val: Value, otherwise: Ebb) {
        let cases_tree = self.build_cases_tree();

        // FIXME icmp(_imm) doesn't have encodings for i8 and i16 on x86(_64) yet
        let (val, val_type) = match bx.func.dfg.value_type(val) {
            types::I8 | types::I16 => {
                (
                    bx.ins().uextend(types::I32, val),
                    types::I32,
                )
            }
            val_type => (val, val_type)
        };

        let cases_and_jt_ebbs: Vec<Option<(EntryIndex, Ebb, Vec<Ebb>)>> = cases_tree
            .into_iter()
            .rev()
            .map(|(first_index, ebbs)| {
                if ebbs.len() == 1 {
                    let is_good_val = bx.ins().icmp_imm(IntCC::Equal, val, first_index);
                    bx.ins().brnz(is_good_val, ebbs[0], &[]);
                    None
                } else {
                    let jt_ebb = bx.create_ebb();
                    let is_good_val = bx.ins().icmp_imm(IntCC::UnsignedGreaterThanOrEqual, val, first_index);
                    bx.ins().brnz(is_good_val, jt_ebb, &[]);
                    Some((first_index, jt_ebb, ebbs))
                }
            })
            .collect();

        bx.ins().jump(otherwise, &[]);

        for elem in cases_and_jt_ebbs.into_iter().rev() {
            if let Some((first_index, jt_ebb, ebbs)) = elem {
                let mut jt_data = JumpTableData::new();
                for ebb in ebbs {
                    jt_data.push_entry(ebb);
                }
                let jump_table = bx.create_jump_table(jt_data);

                bx.switch_to_block(jt_ebb);
                let first_index = bx.ins().iconst(val_type, first_index);
                let discr = bx.ins().isub(val, first_index);
                bx.ins().br_table(discr, jump_table);
                bx.ins().jump(otherwise, &[]);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use cranelift_codegen::ir::Function;
    use super::*;

    macro_rules! setup {
        ($default:expr, [$($index:expr,)*]) => {{
            let mut func = Function::new();
            let mut func_ctx = FunctionBuilderContext::new();
            let mut bx = FunctionBuilder::new(&mut func, &mut func_ctx);
            let ebb = bx.create_ebb();
            bx.switch_to_block(ebb);
            let val = bx.ins().iconst(types::I8, 0);
            let mut switch = Switch::new();
            $(
                let ebb = bx.create_ebb();
                switch.set_entry($index, ebb);
            )*
            switch.emit(&mut bx, val, Ebb::with_number($default).unwrap());
            func
                .to_string()
                .trim_start_matches("function u0:0() fast {\n")
                .trim_end_matches("\n}\n")
                .to_string()
        }};
    }

    #[test]
    fn switch_bool() {
        let func = setup!(0, [0, 1,]);
        assert_eq!(func, "    jt0 = jump_table ebb1, ebb2

ebb0:
    v0 = iconst.i8 0
    v1 = uextend.i32 v0
    v2 = icmp_imm uge v1, 0
    brnz v2, ebb3
    jump ebb0

ebb3:
    v3 = iconst.i32 0
    v4 = isub.i32 v1, v3
    br_table v4, jt0
    jump ebb0");
    }

    #[test]
    fn switch_two_gap() {
        let func = setup!(0, [0, 2,]);
        assert_eq!(func, "ebb0:
    v0 = iconst.i8 0
    v1 = uextend.i32 v0
    v2 = icmp_imm eq v1, 2
    brnz v2, ebb2
    v3 = icmp_imm eq v1, 0
    brnz v3, ebb1
    jump ebb0");
    }

    #[test]
    fn switch_many() {
        let func = setup!(0, [0, 1, 5, 7, 10, 11, 12,]);
        assert_eq!(func, "    jt0 = jump_table ebb1, ebb2
    jt1 = jump_table ebb5, ebb6, ebb7

ebb0:
    v0 = iconst.i8 0
    v1 = uextend.i32 v0
    v2 = icmp_imm uge v1, 10
    brnz v2, ebb8
    v3 = icmp_imm eq v1, 7
    brnz v3, ebb4
    v4 = icmp_imm eq v1, 5
    brnz v4, ebb3
    v5 = icmp_imm uge v1, 0
    brnz v5, ebb9
    jump ebb0

ebb9:
    v6 = iconst.i32 0
    v7 = isub.i32 v1, v6
    br_table v7, jt0
    jump ebb0

ebb8:
    v8 = iconst.i32 10
    v9 = isub.i32 v1, v8
    br_table v9, jt1
    jump ebb0");
    }
}
