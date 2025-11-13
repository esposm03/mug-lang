use std::collections::HashMap;

use crate::mir::{BasicBlock, BlockId, Inst, Place, Reg, Symbol, TermInst};

pub struct BbBuilder {
    insts: Vec<Inst>,
}

impl BbBuilder {
    pub fn emit(&mut self, inst: Inst) -> &mut Self {
        self.insts.push(inst);
        self
    }

    pub fn comment(&mut self, s: &str) -> &mut Self {
        let b = s.as_bytes().try_into().unwrap();
        self.insts.push(Inst::Comment(b));
        self
    }

    pub fn term(self, term: TermInst) -> BasicBlock {
        BasicBlock {
            insts: self.insts,
            term,
        }
    }
}

pub struct MirBuilder {
    next_symbol: usize,
    next_bblock: usize,
    symbols: HashMap<Symbol, String>,
}

impl MirBuilder {
    pub fn new() -> Self {
        Self {
            next_symbol: 0,
            next_bblock: 0,
            symbols: HashMap::new(),
        }
    }

    fn sym(&mut self, name: &str) -> Symbol {
        let res = Symbol(self.next_symbol);
        self.next_symbol += 1;
        self.symbols.insert(res, name.to_string());
        res
    }

    pub fn place(&mut self, name: &str) -> Place {
        Place(self.sym(name))
    }

    pub fn reg(&mut self, name: &str) -> Reg {
        Reg(self.sym(name))
    }

    pub fn block(&mut self) -> (BlockId, BbBuilder) {
        let id = self.next_bblock;
        self.next_bblock += 1;

        (BlockId(id), BbBuilder { insts: vec![] })
    }
}
