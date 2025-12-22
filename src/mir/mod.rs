#![allow(unused)]

pub mod build;
pub mod print;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub usize);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Place(pub Symbol);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Reg(pub Symbol);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Clone, Copy)]
pub enum Typ {
    I8,
    I64,
    Bool,
    Unit,
}

impl From<Typ> for cranelift::prelude::Type {
    fn from(value: Typ) -> Self {
        use cranelift::prelude::types::*;
        match value {
            Typ::I8 => I8,
            Typ::I64 => I64,
            Typ::Bool => I8,
            Typ::Unit => todo!(),
        }
    }
}

impl From<&Typ> for cranelift::prelude::Type {
    fn from(value: &Typ) -> Self {
        cranelift::prelude::Type::from(*value)
    }
}

#[derive(Clone, Copy)]
pub enum Val {
    I8(i8),
    I64(i64),
    False,
    True,
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
    Lt(Reg, Typ, Reg, Reg),
}

#[derive(Clone, Copy)]
pub enum TermInst {
    Ret(Typ, Reg),
    If { cond: Reg, th: BlockId, el: BlockId },
    Jmp(BlockId),
}

pub struct BasicBlock {
    pub id: BlockId,
    pub insts: Vec<Inst>,
    pub term: TermInst,
}
