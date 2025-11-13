#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub usize);

#[derive(Clone, Copy)]
pub struct Place(pub Symbol);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg(pub Symbol);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, Copy)]
pub enum Typ {
    I8,
}

#[derive(Clone, Copy)]
pub enum Val {
    I8(i8),
}

#[derive(Clone, Copy)]
pub enum Inst {
    Comment([u8; 23]),

    // Place manipulation
    Alloca(Place, Typ),
    Store(Place, Typ, Reg),
    Load(Reg, Typ, Place),

    Imm(Reg, Val),
    Add(Reg, Typ, Reg, Reg),
}

#[derive(Clone, Copy)]
pub enum TermInst {
    Ret(Typ, Reg),
    If { cond: Reg, th: BlockId, el: BlockId },
}

pub struct BasicBlock {
    pub insts: Vec<Inst>,
    pub term: TermInst,
}
