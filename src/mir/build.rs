use std::collections::HashMap;

use crate::mir::{BasicBlock, BlockId, Inst, Place, Reg, Symbol, TermInst};

pub struct BbBuilder {
    id: BlockId,
    insts: Vec<Inst>,
}

impl BbBuilder {
    pub fn id(&self) -> BlockId {
        self.id
    }

    pub fn emit(&mut self, inst: Inst) -> &mut Self {
        self.insts.push(inst);
        self
    }

    pub fn comment(&mut self, src: &str) -> &mut Self {
        let mut dest = [0u8; 23];
        if dest.len() == src.len() {
            dest.copy_from_slice(src.as_bytes());
        } else if dest.len() > src.len() {
            dest[..src.len()].copy_from_slice(src.as_bytes());
        } else {
            dest.copy_from_slice(&src.as_bytes()[..23]);
        }

        self.insts.push(Inst::Comment(dest));
        self
    }

    pub fn term(self, term: TermInst) -> BasicBlock {
        BasicBlock {
            id: self.id,
            insts: self.insts,
            term,
        }
    }
}

pub struct MirBuilder {
    next_symbol: usize,
    next_bblock: usize,
    next_temp: usize,
    symbols: HashMap<Symbol, String>,
    symbols_rev: HashMap<String, Symbol>,
}

impl MirBuilder {
    pub fn new() -> Self {
        Self {
            next_symbol: 0,
            next_bblock: 0,
            next_temp: 0,
            symbols: HashMap::new(),
            symbols_rev: HashMap::new(),
        }
    }

    fn sym(&mut self, name: &str) -> Symbol {
        if let Some(x) = self.symbols_rev.get(name) {
            *x
        } else {
            let res = Symbol(self.next_symbol);
            self.next_symbol += 1;
            self.symbols.insert(res, name.to_string());
            self.symbols_rev.insert(name.to_string(), res);
            res
        }
    }

    pub fn place(&mut self, name: &str) -> Place {
        Place(self.sym(name))
    }

    pub fn reg(&mut self, name: &str) -> Reg {
        Reg(self.sym(name))
    }

    pub fn temp(&mut self) -> Reg {
        let id = self.next_temp;
        self.next_temp += 1;

        self.reg(&format!("temp.{id}"))
    }

    pub fn block(&mut self) -> BbBuilder {
        let id = BlockId(self.next_bblock);
        self.next_bblock += 1;

        BbBuilder { id, insts: vec![] }
    }

    pub fn print_block(&self, bb: &BasicBlock) {
        println!("{}:", bb.id);
        for inst in &bb.insts {
            println!("    {}", inst.display(&self.symbols));
        }
        println!("    {}", bb.term.display(&self.symbols));
    }
}
