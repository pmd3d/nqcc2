use std::collections::HashMap;
use std::fmt;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NodeId {
    Entry,
    Block(usize),
    Exit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleInstr {
    Label(String),
    ConditionalJump(String),
    UnconditionalJump(String),
    Return,
    Other,
}

pub trait Instr: Clone {
    fn simplify(&self) -> SimpleInstr;
}

#[derive(Debug, Clone)]
pub struct BasicBlock<V: Clone, I: Instr> {
    pub id: NodeId,
    pub instructions: Vec<(V, I)>,
    pub preds: Vec<NodeId>,
    pub succs: Vec<NodeId>,
    pub value: V,
}

#[derive(Debug, Clone)]
pub struct Cfg<V: Clone, I: Instr> {
    pub basic_blocks: Vec<(usize, BasicBlock<V, I>)>,
    pub entry_succs: Vec<NodeId>,
    pub exit_preds: Vec<NodeId>,
    pub debug_label: String,
}

impl<V: Clone, I: Instr> Cfg<V, I> {
    pub fn get_succs(&self, id: &NodeId) -> Vec<NodeId> {
        match id {
            NodeId::Entry => self.entry_succs.clone(),
            NodeId::Block(n) => self
                .basic_blocks
                .iter()
                .find(|(i, _)| i == n)
                .map(|(_, b)| b.succs.clone())
                .unwrap_or_default(),
            NodeId::Exit => vec![],
        }
    }

    pub fn get_block_value(&self, blocknum: usize) -> V {
        self.basic_blocks
            .iter()
            .find(|(i, _)| *i == blocknum)
            .map(|(_, b)| b.value.clone())
            .expect("block not found")
    }

    fn add_id(list: &mut Vec<NodeId>, id: NodeId) {
        if !list.contains(&id) {
            list.push(id);
        }
    }

    pub fn add_edge(&mut self, pred: NodeId, succ: NodeId) {
        match pred.clone() {
            NodeId::Entry => Self::add_id(&mut self.entry_succs, succ.clone()),
            NodeId::Block(n) => {
                if let Some((_, blk)) = self.basic_blocks.iter_mut().find(|(i, _)| *i == n) {
                    Self::add_id(&mut blk.succs, succ.clone());
                }
            }
            NodeId::Exit => panic!("malformed CFG"),
        }
        match succ {
            NodeId::Entry => panic!("malformed CFG"),
            NodeId::Block(n) => {
                if let Some((_, blk)) = self.basic_blocks.iter_mut().find(|(i, _)| *i == n) {
                    Self::add_id(&mut blk.preds, pred);
                }
            }
            NodeId::Exit => Self::add_id(&mut self.exit_preds, pred),
        }
    }

    pub fn remove_edge(&mut self, pred: NodeId, succ: NodeId) {
        fn remove_id(list: &mut Vec<NodeId>, id: &NodeId) {
            list.retain(|x| x != id);
        }
        match pred.clone() {
            NodeId::Entry => remove_id(&mut self.entry_succs, &succ),
            NodeId::Block(n) => {
                if let Some((_, blk)) = self.basic_blocks.iter_mut().find(|(i, _)| *i == n) {
                    remove_id(&mut blk.succs, &succ);
                }
            }
            NodeId::Exit => (),
        }
        match succ {
            NodeId::Entry => (),
            NodeId::Block(n) => {
                if let Some((_, blk)) = self.basic_blocks.iter_mut().find(|(i, _)| *i == n) {
                    remove_id(&mut blk.preds, &pred);
                }
            }
            NodeId::Exit => remove_id(&mut self.exit_preds, &pred),
        }
    }

    pub fn update_basic_block(&mut self, block_idx: usize, new_block: BasicBlock<V, I>) {
        if let Some((_, blk)) = self.basic_blocks.iter_mut().find(|(i, _)| *i == block_idx) {
            *blk = new_block;
        }
    }

    pub fn cfg_to_instructions(&self) -> Vec<I> {
        self.basic_blocks
            .iter()
            .flat_map(|(_, blk)| blk.instructions.iter().map(|(_, i)| i.clone()))
            .collect()
    }

    pub fn initialize_annotation<W: Clone>(&self, dummy: W) -> Cfg<W, I> {
        let basic_blocks = self
            .basic_blocks
            .iter()
            .map(|(idx, b)| {
                let instrs = b
                    .instructions
                    .iter()
                    .map(|(_, i)| (dummy.clone(), i.clone()))
                    .collect();
                (
                    *idx,
                    BasicBlock {
                        id: b.id.clone(),
                        instructions: instrs,
                        preds: b.preds.clone(),
                        succs: b.succs.clone(),
                        value: dummy.clone(),
                    },
                )
            })
            .collect();
        Cfg {
            basic_blocks,
            entry_succs: self.entry_succs.clone(),
            exit_preds: self.exit_preds.clone(),
            debug_label: self.debug_label.clone(),
        }
    }

    pub fn strip_annotations(&self) -> Cfg<(), I> {
        self.initialize_annotation(())
    }

    #[allow(unused)]
    pub fn print_graphviz<F>(&self, _pp_val: F)
    where
        F: Fn(&mut fmt::Formatter<'_>, &V) -> fmt::Result,
    {
        // Placeholder: Graphviz output not implemented in Rust port yet.
    }
}

fn partition_into_basic_blocks<I: Instr>(instructions: Vec<I>) -> Vec<Vec<I>> {
    let mut blocks: Vec<Vec<I>> = Vec::new();
    let mut current_block: Vec<I> = Vec::new();
    for instr in instructions.into_iter() {
        match instr.simplify() {
            SimpleInstr::Label(_) => {
                if !current_block.is_empty() {
                    blocks.push(current_block);
                    current_block = Vec::new();
                }
                current_block.push(instr);
            }
            SimpleInstr::ConditionalJump(_) | SimpleInstr::UnconditionalJump(_) | SimpleInstr::Return => {
                current_block.push(instr);
                blocks.push(current_block);
                current_block = Vec::new();
            }
            SimpleInstr::Other => current_block.push(instr),
        }
    }
    if !current_block.is_empty() {
        blocks.push(current_block);
    }
    blocks
}

fn add_all_edges<I: Instr>(cfg: &mut Cfg<(), I>) {
    let mut label_map: HashMap<String, NodeId> = HashMap::new();
    for (_, blk) in cfg.basic_blocks.iter() {
        if let Some((_, first_instr)) = blk.instructions.first() {
            if let SimpleInstr::Label(lbl) = first_instr.simplify() {
                label_map.insert(lbl, blk.id.clone());
            }
        }
    }

    let last_block_idx = cfg.basic_blocks.last().map(|(i, _)| *i).unwrap_or(0);
    let blocks = cfg.basic_blocks.clone();
    for (id_num, block) in blocks {
        let next_block = if id_num == last_block_idx {
            NodeId::Exit
        } else {
            NodeId::Block(id_num + 1)
        };
        let last_instr = block.instructions.last().unwrap().1.clone();
        match last_instr.simplify() {
            SimpleInstr::Return => cfg.add_edge(block.id.clone(), NodeId::Exit),
            SimpleInstr::UnconditionalJump(target) => {
                let target_id = label_map[&target].clone();
                cfg.add_edge(block.id.clone(), target_id);
            }
            SimpleInstr::ConditionalJump(target) => {
                let target_id = label_map[&target].clone();
                cfg.add_edge(block.id.clone(), next_block.clone());
                cfg.add_edge(block.id.clone(), target_id);
            }
            _ => cfg.add_edge(block.id.clone(), next_block),
        }
    }
    if !cfg.basic_blocks.is_empty() {
        cfg.add_edge(NodeId::Entry, NodeId::Block(0));
    }
}

pub fn instructions_to_cfg<I: Instr>(debug_label: &str, instructions: Vec<I>) -> Cfg<(), I> {
    let blocks = partition_into_basic_blocks(instructions);
    let basic_blocks: Vec<(usize, BasicBlock<(), I>)> = blocks
        .into_iter()
        .enumerate()
        .map(|(idx, instrs)| {
            let instrs = instrs.into_iter().map(|i| ((), i)).collect();
            (
                idx,
                BasicBlock {
                    id: NodeId::Block(idx),
                    instructions: instrs,
                    preds: vec![],
                    succs: vec![],
                    value: (),
                },
            )
        })
        .collect();
    let mut cfg = Cfg {
        basic_blocks,
        entry_succs: vec![],
        exit_preds: vec![],
        debug_label: debug_label.to_string(),
    };
    add_all_edges(&mut cfg);
    cfg
}

// Implement Instr for specific instruction types
use crate::tacky;
impl Instr for tacky::Instruction {
    fn simplify(&self) -> SimpleInstr {
        use tacky::Instruction::*;
        match self {
            Label(l) => SimpleInstr::Label(l.clone()),
            Jump(target) => SimpleInstr::UnconditionalJump(target.clone()),
            JumpIfZero(_, target) | JumpIfNotZero(_, target) => {
                SimpleInstr::ConditionalJump(target.clone())
            }
            Return(_) => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }
}

use crate::assembly;
impl Instr for assembly::Instruction {
    fn simplify(&self) -> SimpleInstr {
        use assembly::Instruction::*;
        match self {
            Label(l) => SimpleInstr::Label(l.clone()),
            Jmp(t) => SimpleInstr::UnconditionalJump(t.clone()),
            JmpCC(_, t) => SimpleInstr::ConditionalJump(t.clone()),
            Ret => SimpleInstr::Return,
            _ => SimpleInstr::Other,
        }
    }
}
