use super::types::ValueType;

pub struct InstructionList {
    groups: Vec<InstructionGroup>,
}

impl InstructionList {
    pub fn new() -> Self {
        Self { groups: Vec::new() }
    }

    pub fn group(&mut self, name: &'static str, f: impl FnOnce(&mut InstructionGroup)) {
        let mut group = InstructionGroup::new(name);
        f(&mut group);
        self.groups.push(group);
    }
}

pub struct InstructionGroup {
    name: &'static str,
    insts: Vec<Instruction>,
}

impl InstructionGroup {
    fn new(name: &'static str) -> Self {
        Self {
            name,
            insts: Vec::new(),
        }
    }

    pub fn add(&mut self, inst: Instruction) {
        self.insts.push(inst);
    }
}

pub struct Instruction {
    name: &'static str,
    doc: &'static str,
    ins: &'static [()],
    outs: &'static [()],
    constraints: &'static [Constraint],
}

pub struct InstructionBuilder {
    name: &'static str,
    doc: &'static str,
    ins: &'static [()],
    outs: &'static [()],
    constraints: &'static [Constraint],
}

impl InstructionBuilder {
    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            doc: "",
            ins: &[],
            outs: &[],
            constraints: &[],
        }
    }

    pub fn doc(mut self, doc: &'static str) -> Self {
        self.doc = doc;
        self
    }

    pub fn ins(mut self, ins: &'static [()]) -> Self {
        self.ins = ins;
        self
    }

    pub fn outs(mut self, outs: &'static [()]) -> Self {
        self.outs = outs;
        self
    }

    pub fn constraints(mut self, constraints: &'static [Constraint]) -> Self {
        self.constraints = constraints;
        self
    }

    pub fn build(self) -> Instruction {
        Instruction {
            name: self.name,
            doc: self.doc,
            ins: self.ins,
            outs: self.outs,
            constraints: self.constraints,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Constraint {
    WiderOrEq { ty: [ValueType; 2] },
    TypesEqual { ty: [ValueType; 2] },
}

impl Constraint {
    pub fn eval(&self) {
        match self {
            Constraint::WiderOrEq { ty: [ty0, ty1] } => {
                assert!(ty0.width() >= ty1.width());
            }
            Constraint::TypesEqual { ty: [ty0, ty1] } => {
                assert_eq!(ty0, ty1);
            }
        }
    }
}
