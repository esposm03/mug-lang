use std::{collections::HashMap, fmt};

use crate::mir::{BlockId, Inst, Place, Reg, Symbol, TermInst, Typ, Val};

// === BlockId ===

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

// === Val ===

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::I8(x) => write!(f, "{x}"),
            Val::I64(x) => write!(f, "{x}"),
            Val::False => write!(f, "false"),
            Val::True => write!(f, "true"),
        }
    }
}

// === Typ ===

impl fmt::Display for Typ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let desc = match self {
            Typ::I8 => "i8",
            Typ::I64 => "i64",
            Typ::Bool => "bool",
        };

        write!(f, "{desc}",)
    }
}

// === Reg ===

impl Reg {
    fn display<'a>(&self, syms: &'a HashMap<Symbol, String>) -> RegDisplay<'a> {
        RegDisplay { reg: *self, syms }
    }
}

struct RegDisplay<'a> {
    reg: Reg,
    syms: &'a HashMap<Symbol, String>,
}

impl<'a> fmt::Display for RegDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syms[&self.reg.0])
    }
}

// === Place ===

impl Place {
    fn display<'a>(&self, syms: &'a HashMap<Symbol, String>) -> PlaceDisplay<'a> {
        PlaceDisplay { place: *self, syms }
    }
}

struct PlaceDisplay<'a> {
    place: Place,
    syms: &'a HashMap<Symbol, String>,
}

impl<'a> fmt::Display for PlaceDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syms[&self.place.0])
    }
}

// === Inst ===

impl Inst {
    pub fn display<'a>(&self, syms: &'a HashMap<Symbol, String>) -> InstDisplay<'a> {
        InstDisplay { inst: *self, syms }
    }
}

pub struct InstDisplay<'a> {
    inst: Inst,
    syms: &'a HashMap<Symbol, String>,
}

impl<'a> fmt::Display for InstDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inst {
            Inst::Comment(x) => write!(f, "// {}", str::from_utf8(&x).unwrap()),
            Inst::Alloca(place, typ) => {
                let place = place.display(self.syms);
                write!(f, "alloca {typ} {place}",)
            }
            Inst::Store(place, typ, reg) => {
                let place = place.display(self.syms);
                let reg = reg.display(self.syms);
                write!(f, "store {typ} {place} <- {reg}",)
            }
            Inst::Load(reg, typ, place) => {
                let place = place.display(self.syms);
                let reg = reg.display(self.syms);
                write!(f, "load {typ} {reg} <- {place}",)
            }
            Inst::Imm(reg, val) => {
                let reg = reg.display(self.syms);
                write!(f, "imm {reg} = {val}")
            }
            Inst::Add(reg, typ, reg1, reg2) => {
                let reg = reg.display(self.syms);
                let reg1 = reg1.display(self.syms);
                let reg2 = reg2.display(self.syms);
                write!(f, "add {typ} {reg} = {reg1} + {reg2}")
            }
            Inst::Lt(reg, typ, reg1, reg2) => {
                let reg = reg.display(self.syms);
                let reg1 = reg1.display(self.syms);
                let reg2 = reg2.display(self.syms);
                write!(f, "icmp {typ} {reg} = {reg1} < {reg2}")
            }
        }
    }
}

// === TermInst ===

impl TermInst {
    pub fn display<'a>(&self, syms: &'a HashMap<Symbol, String>) -> TermInstDisplay<'a> {
        TermInstDisplay { term: *self, syms }
    }
}

pub struct TermInstDisplay<'a> {
    term: TermInst,
    syms: &'a HashMap<Symbol, String>,
}

impl<'a> fmt::Display for TermInstDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.term {
            TermInst::Ret(typ, reg) => write!(f, "ret {typ} {}", reg.display(self.syms)),
            TermInst::If { cond, th, el } => {
                write!(f, "if {} then {th} else {el}", cond.display(self.syms))
            }
            TermInst::Jmp(block_id) => write!(f, "jmp {block_id}"),
        }
    }
}
