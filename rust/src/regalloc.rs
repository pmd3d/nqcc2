use std::collections::{BTreeMap, BTreeSet};

use crate::assembly::{AsmType, Instruction, Operand, Program, Reg, TopLevel};
use crate::assembly_symbols;
use crate::backward_dataflow;
use crate::cfg::{self, BasicBlock, Cfg, NodeId};
use crate::reg_set::RegSet;

// Helper to get all operands used in an instruction
fn get_operands(i: &Instruction) -> Vec<Operand> {
    use Instruction::*;
    match i {
        Mov(_, src, dst)
        | Cvttsd2si(_, src, dst)
        | Cvtsi2sd(_, src, dst)
        | Lea(src, dst) => vec![src.clone(), dst.clone()],
        Movsx { src, dst, .. } | MovZeroExtend { src, dst, .. } => {
            vec![src.clone(), dst.clone()]
        }
        Unary(_, _, op)
        | Idiv(_, op)
        | Div(_, op)
        | SetCC(_, op)
        | Push(op) => vec![op.clone()],
        Binary { src, dst, .. } | Cmp(_, src, dst) => vec![src.clone(), dst.clone()],
        Label(_) | Call(_) | Ret | Cdq(_) | JmpCC(_, _) | Jmp(_) => vec![],
        Pop(_) => panic!("Internal error"),
    }
}

// Map a function over all operands in an instruction
fn replace_ops<F>(f: F, i: &Instruction) -> Instruction
where
    F: Fn(Operand) -> Operand,
{
    use Instruction::*;
    match i {
        Mov(t, src, dst) => Mov(t.clone(), f(src.clone()), f(dst.clone())),
        Movsx { src_type, dst_type, src, dst } => Movsx {
            src_type: src_type.clone(),
            dst_type: dst_type.clone(),
            src: f(src.clone()),
            dst: f(dst.clone()),
        },
        MovZeroExtend { src_type, dst_type, src, dst } => MovZeroExtend {
            src_type: src_type.clone(),
            dst_type: dst_type.clone(),
            src: f(src.clone()),
            dst: f(dst.clone()),
        },
        Lea(src, dst) => Lea(f(src.clone()), f(dst.clone())),
        Cvttsd2si(t, src, dst) => Cvttsd2si(t.clone(), f(src.clone()), f(dst.clone())),
        Cvtsi2sd(t, src, dst) => Cvtsi2sd(t.clone(), f(src.clone()), f(dst.clone())),
        Unary(op, t, operand) => Unary(*op, t.clone(), f(operand.clone())),
        Binary { op, t, src, dst } => Binary { op: *op, t: t.clone(), src: f(src.clone()), dst: f(dst.clone()) },
        Cmp(t, v1, v2) => Cmp(t.clone(), f(v1.clone()), f(v2.clone())),
        Idiv(t, v) => Idiv(t.clone(), f(v.clone())),
        Div(t, v) => Div(t.clone(), f(v.clone())),
        SetCC(code, dst) => SetCC(*code, f(dst.clone())),
        Push(v) => Push(f(v.clone())),
        Label(_) | Call(_) | Ret | Cdq(_) | Jmp(_) | JmpCC(_, _) => i.clone(),
        Pop(_) => panic!("Shouldn't use this yet"),
    }
}

fn cleanup_movs(instructions: Vec<Instruction>) -> Vec<Instruction> {
    instructions
        .into_iter()
        .filter(|i| match i {
            Instruction::Mov(_, src, dst) if src == dst => false,
            _ => true,
        })
        .collect()
}

trait RegType {
    fn suffix() -> &'static str;
    fn all_hardregs() -> &'static [Reg];
    fn caller_saved_regs() -> &'static [Reg];
    fn pseudo_is_current_type(p: &str) -> bool;
}

#[derive(Clone)]
struct Node {
    id: Operand,
    neighbors: BTreeSet<Operand>,
    spill_cost: f64,
    color: Option<usize>,
    pruned: bool,
}

type Graph = BTreeMap<Operand, Node>;

fn regs_to_operands(regs: &[Reg]) -> Vec<Operand> {
    regs.iter().map(|r| Operand::Reg(*r)).collect()
}

fn regs_used_and_written(i: &Instruction) -> (BTreeSet<Operand>, BTreeSet<Operand>) {
    use Instruction::*;
    let (ops_used, ops_written): (Vec<Operand>, Vec<Operand>) = match i {
        Mov(_, src, dst) => (vec![src.clone()], vec![dst.clone()]),
        MovZeroExtend { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        Movsx { src, dst, .. } => (vec![src.clone()], vec![dst.clone()]),
        Cvtsi2sd(_, src, dst) => (vec![src.clone()], vec![dst.clone()]),
        Cvttsd2si(_, src, dst) => (vec![src.clone()], vec![dst.clone()]),
        Binary { src, dst, .. } => (vec![src.clone(), dst.clone()], vec![dst.clone()]),
        Unary(_, _, op) => (vec![op.clone()], vec![op.clone()]),
        Cmp(_, v1, v2) => (vec![v1.clone(), v2.clone()], vec![]),
        SetCC(_, op) => (vec![], vec![op.clone()]),
        Push(v) => (vec![v.clone()], vec![]),
        Idiv(_, op) | Div(_, op) => (
            vec![op.clone(), Operand::Reg(Reg::AX), Operand::Reg(Reg::DX)],
            vec![Operand::Reg(Reg::AX), Operand::Reg(Reg::DX)],
        ),
        Cdq(_) => (vec![Operand::Reg(Reg::AX)], vec![Operand::Reg(Reg::DX)]),
        Call(f) => {
            let used: Vec<Operand> = assembly_symbols::param_regs_used(f)
                .into_iter()
                .filter(|r| RegTypeForInstr::ALL.contains(r))
                .map(Operand::Reg)
                .collect();
            (used, regs_to_operands(RegTypeForInstr::CALLER_SAVED))
        }
        Lea(src, dst) => (vec![src.clone()], vec![dst.clone()]),
        Jmp(_) | JmpCC(_, _) | Label(_) | Ret => (vec![], vec![]),
        Pop(_) => panic!("Internal error"),
    };

    // Convert operands read into registers read
    fn regs_used_to_read(opr: &Operand) -> Vec<Operand> {
        match opr {
            Operand::Pseudo(_) | Operand::Reg(_) => vec![opr.clone()],
            Operand::Memory(r, _) => vec![Operand::Reg(*r)],
            Operand::Indexed { base, index, .. } => vec![Operand::Reg(*base), Operand::Reg(*index)],
            Operand::Imm(_) | Operand::Data(_, _) | Operand::PseudoMem(_, _) => vec![],
        }
    }
    let regs_read1: Vec<Operand> = ops_used.iter().flat_map(regs_used_to_read).collect();

    // operands written may require reading pointer if memory
    fn regs_used_to_update(opr: &Operand) -> (Vec<Operand>, Vec<Operand>) {
        match opr {
            Operand::Pseudo(_) | Operand::Reg(_) => (vec![], vec![opr.clone()]),
            Operand::Memory(r, _) => (vec![Operand::Reg(*r)], vec![]),
            Operand::Indexed { base, index, .. } => {
                (vec![Operand::Reg(*base), Operand::Reg(*index)], vec![])
            }
            Operand::Imm(_) | Operand::Data(_, _) | Operand::PseudoMem(_, _) => (vec![], vec![]),
        }
    }
    let (read_lists, write_lists): (Vec<Vec<Operand>>, Vec<Vec<Operand>>) =
        ops_written.iter().map(regs_used_to_update).unzip();
    let regs_read2: Vec<Operand> = read_lists.into_iter().flatten().collect();
    let regs_written: Vec<Operand> = write_lists.into_iter().flatten().collect();

    (
        regs_read1
            .into_iter()
            .chain(regs_read2.into_iter())
            .collect(),
        regs_written.into_iter().collect(),
    )
}

// Placeholder trait implementation to allow regs_used_and_written to compile
// This struct simply exposes arrays of all hardregs and caller-saved regs.
struct RegTypeForInstr;
impl RegTypeForInstr {
    const ALL: &'static [Reg] = &[Reg::AX, Reg::BX, Reg::CX, Reg::DX, Reg::DI, Reg::SI, Reg::R8, Reg::R9, Reg::R12, Reg::R13, Reg::R14, Reg::R15, Reg::XMM0, Reg::XMM1, Reg::XMM2, Reg::XMM3, Reg::XMM4, Reg::XMM5, Reg::XMM6, Reg::XMM7, Reg::XMM8, Reg::XMM9, Reg::XMM10, Reg::XMM11, Reg::XMM12, Reg::XMM13];
    const CALLER_SAVED: &'static [Reg] = &[Reg::AX, Reg::CX, Reg::DX, Reg::DI, Reg::SI, Reg::R8, Reg::R9, Reg::XMM0, Reg::XMM1, Reg::XMM2, Reg::XMM3, Reg::XMM4, Reg::XMM5, Reg::XMM6, Reg::XMM7, Reg::XMM8, Reg::XMM9, Reg::XMM10, Reg::XMM11, Reg::XMM12, Reg::XMM13];
}

fn analyze_liveness(
    fn_name: &str,
    cfg: Cfg<(), Instruction>,
    all_hardregs: &BTreeSet<Operand>,
) -> Cfg<BTreeSet<Operand>, Instruction> {
    let cfg = cfg.initialize_annotation(BTreeSet::new());

    let meet = |
        cfg: &Cfg<BTreeSet<Operand>, Instruction>,
        block: &BasicBlock<BTreeSet<Operand>, Instruction>,
    | -> BTreeSet<Operand> {
        let all_return_regs: BTreeSet<Operand> =
            regs_to_operands(&assembly_symbols::return_regs_used(fn_name))
                .into_iter()
                .collect();
        let return_regs: BTreeSet<Operand> =
            all_hardregs.intersection(&all_return_regs).cloned().collect();

        let mut out = BTreeSet::new();
        for succ in block.succs.iter() {
            let live = match succ {
                NodeId::Entry => panic!("Internal error: malformed interference graph"),
                NodeId::Exit => return_regs.clone(),
                NodeId::Block(n) => cfg.get_block_value(*n),
            };
            out.extend(live);
        }
        out
    };

    let transfer = |
        block: &BasicBlock<BTreeSet<Operand>, Instruction>,
        end_live_regs: &BTreeSet<Operand>,
    | -> BasicBlock<BTreeSet<Operand>, Instruction> {
        let mut current_live = end_live_regs.clone();
        let mut annotated_rev: Vec<(BTreeSet<Operand>, Instruction)> = Vec::new();
        for (_, instr) in block.instructions.iter().rev() {
            let annotated = (current_live.clone(), instr.clone());
            let (regs_used, regs_written) = regs_used_and_written(instr);
            let without_killed: BTreeSet<Operand> =
                current_live.difference(&regs_written).cloned().collect();
            current_live = without_killed.union(&regs_used).cloned().collect();
            annotated_rev.push(annotated);
        }
        annotated_rev.reverse();
        BasicBlock {
            id: block.id.clone(),
            instructions: annotated_rev,
            preds: block.preds.clone(),
            succs: block.succs.clone(),
            value: current_live,
        }
    };

    backward_dataflow::analyze(cfg, meet, transfer)
}

fn mk_base_graph(all_hardregs: &BTreeSet<Operand>) -> Graph {
    let mut g = BTreeMap::new();
    for r in all_hardregs.iter() {
        let neighbors: BTreeSet<Operand> =
            all_hardregs.iter().filter(|x| *x != r).cloned().collect();
        let nd = Node {
            id: r.clone(),
            neighbors,
            spill_cost: f64::INFINITY,
            color: None,
            pruned: false,
        };
        g.insert(r.clone(), nd);
    }
    g
}

fn get_pseudo_nodes<R: RegType>(
    aliased_pseudos: &BTreeSet<String>,
    instructions: &[Instruction],
) -> Vec<Node> {
    let mut pseudos: BTreeSet<String> = BTreeSet::new();
    for i in instructions {
        for op in get_operands(i) {
            if let Operand::Pseudo(p) = op {
                if R::pseudo_is_current_type(&p)
                    && !assembly_symbols::is_static(&p)
                    && !aliased_pseudos.contains(&p)
                {
                    pseudos.insert(p);
                }
            }
        }
    }

    pseudos
        .into_iter()
        .map(|p| Node {
            id: Operand::Pseudo(p),
            neighbors: BTreeSet::new(),
            spill_cost: 0.0,
            color: None,
            pruned: false,
        })
        .collect()
}

fn add_pseudo_nodes<R: RegType>(
    aliased_pseudos: &BTreeSet<String>,
    mut graph: Graph,
    instructions: &[Instruction],
) -> Graph {
    for nd in get_pseudo_nodes::<R>(aliased_pseudos, instructions) {
        graph.insert(nd.id.clone(), nd);
    }
    graph
}

fn add_edge(graph: &mut Graph, id1: &Operand, id2: &Operand) {
    if graph.contains_key(id1) && graph.contains_key(id2) {
        if let Some(nd1) = graph.get_mut(id1) {
            nd1.neighbors.insert(id2.clone());
        }
        if let Some(nd2) = graph.get_mut(id2) {
            nd2.neighbors.insert(id1.clone());
        }
    }
}

fn add_edges(
    liveness_cfg: &Cfg<BTreeSet<Operand>, Instruction>,
    graph: &mut Graph,
) {
    let mut all_instrs: Vec<(BTreeSet<Operand>, Instruction)> = Vec::new();
    for (_, blk) in liveness_cfg.basic_blocks.iter() {
        all_instrs.extend(blk.instructions.clone());
    }

    for (live_after_instr, instr) in all_instrs.into_iter() {
        let (_, updated_regs) = regs_used_and_written(&instr);
        for l in live_after_instr.iter() {
            match instr {
                Instruction::Mov(_, ref src, _) if src == l => continue,
                _ => {
                    for u in updated_regs.iter() {
                        if l != u && graph.contains_key(l) && graph.contains_key(u) {
                            add_edge(graph, l, u);
                        }
                    }
                }
            }
        }
    }
}

fn build_interference_graph<R: RegType>(
    fn_name: &str,
    aliased_pseudos: &BTreeSet<String>,
    instructions: &[Instruction],
) -> Graph {
    let all_hardregs: BTreeSet<Operand> =
        regs_to_operands(R::all_hardregs()).into_iter().collect();
    let mut graph = mk_base_graph(&all_hardregs);
    graph = add_pseudo_nodes::<R>(aliased_pseudos, graph, instructions);
    let cfg = cfg::instructions_to_cfg(fn_name, instructions.to_vec());
    let liveness_cfg = analyze_liveness(fn_name, cfg, &all_hardregs);
    add_edges(&liveness_cfg, &mut graph);
    graph
}

fn add_spill_costs(graph: &mut Graph, instructions: &[Instruction]) {
    let mut counts: BTreeMap<String, i32> = BTreeMap::new();
    for op in instructions.iter().flat_map(get_operands) {
        if let Operand::Pseudo(p) = op {
            *counts.entry(p).or_insert(0) += 1;
        }
    }
    for nd in graph.values_mut() {
        if let Operand::Pseudo(p) = &nd.id {
            nd.spill_cost = *counts.get(p).unwrap_or(&0) as f64;
        }
    }
}

fn degree(graph: &Graph, id: &Operand) -> usize {
    graph
        .get(id)
        .map(|nd| nd.neighbors.len())
        .unwrap_or(0)
}

fn color_graph(mut graph: Graph, k: usize) -> Graph {
    fn helper(graph: &mut Graph, k: usize) {
        let remaining: Vec<Operand> = graph
            .iter()
            .filter(|(_, nd)| !nd.pruned)
            .map(|(id, _)| id.clone())
            .collect();
        if remaining.is_empty() {
            return;
        }
        let not_pruned = |id: &Operand, g: &Graph| !g.get(id).unwrap().pruned;
        let low_degree = remaining
            .iter()
            .find(|id| {
                let nd = graph.get(*id).unwrap();
                let unpruned: BTreeSet<Operand> =
                    nd.neighbors.iter().filter(|n| not_pruned(n, graph)).cloned().collect();
                unpruned.len() < k
            })
            .cloned();
        let next_node_id = match low_degree {
            Some(id) => id,
            None => {
                // spill metric heuristic
                let mut best_id: Option<Operand> = None;
                let mut best_metric = f64::INFINITY;
                for id in remaining.iter() {
                    let nd = graph.get(id).unwrap();
                    let deg = nd.neighbors.iter().filter(|n| not_pruned(n, graph)).count();
                    let metric = nd.spill_cost / (deg as f64);
                    if metric < best_metric {
                        best_metric = metric;
                        best_id = Some(id.clone());
                    }
                }
                best_id.unwrap()
            }
        };
        if let Some(nd) = graph.get_mut(&next_node_id) {
            nd.pruned = true;
        }
        helper(graph, k);
        // color node
        let mut available: Vec<usize> = (0..k).collect();
        let neighbors = graph
            .get(&next_node_id)
            .unwrap()
            .neighbors
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        for n in neighbors {
            if let Some(c) = graph.get(&n).and_then(|nd| nd.color) {
                available.retain(|col| *col != c);
            }
        }
        if let Some(nd) = graph.get_mut(&next_node_id) {
            nd.pruned = false;
            if let Some(col) = available.first().cloned() {
                nd.color = Some(col);
            }
        }
    }

    helper(&mut graph, k);
    graph
}

fn make_register_map<R: RegType>(
    fn_name: &str,
    graph: &Graph,
) -> BTreeMap<String, Reg> {
    let mut color_to_reg: BTreeMap<usize, Reg> = BTreeMap::new();
    for (id, nd) in graph.iter() {
        if let Operand::Reg(r) = id {
            if let Some(c) = nd.color {
                color_to_reg.insert(c, *r);
            }
        }
    }

    let mut used_callee_saved: RegSet = RegSet::new();
    let mut reg_map: BTreeMap<String, Reg> = BTreeMap::new();
    for nd in graph.values() {
        if let Operand::Pseudo(p) = &nd.id {
            if let Some(c) = nd.color {
                let hr = color_to_reg[&c];
                if !R::caller_saved_regs().contains(&hr) {
                    used_callee_saved.insert(hr);
                }
                reg_map.insert(p.clone(), hr);
            }
        }
    }
    assembly_symbols::add_callee_saved_regs_used(fn_name, &used_callee_saved);
    reg_map
}

fn replace_pseudoregs(instructions: Vec<Instruction>, reg_map: &BTreeMap<String, Reg>) -> Vec<Instruction> {
    fn f(op: Operand, reg_map: &BTreeMap<String, Reg>) -> Operand {
        match op {
            Operand::Pseudo(p) => {
                if let Some(r) = reg_map.get(&p) {
                    Operand::Reg(*r)
                } else {
                    Operand::Pseudo(p)
                }
            }
            other => other,
        }
    }

    let replaced: Vec<Instruction> = instructions
        .into_iter()
        .map(|i| replace_ops(|op| f(op, reg_map), &i))
        .collect();
    cleanup_movs(replaced)
}

fn allocate<R: RegType>(
    fn_name: &str,
    aliased_pseudos: &BTreeSet<String>,
    instructions: Vec<Instruction>,
) -> Vec<Instruction> {
    let mut graph = build_interference_graph::<R>(fn_name, aliased_pseudos, &instructions);
    add_spill_costs(&mut graph, &instructions);
    let k = R::all_hardregs().len();
    let colored_graph = color_graph(graph, k);
    let reg_map = make_register_map::<R>(fn_name, &colored_graph);
    replace_pseudoregs(instructions, &reg_map)
}

struct GP;
impl RegType for GP {
    fn suffix() -> &'static str {
        "gp"
    }
    fn all_hardregs() -> &'static [Reg] {
        &[Reg::AX, Reg::BX, Reg::CX, Reg::DX, Reg::DI, Reg::SI, Reg::R8, Reg::R9, Reg::R12, Reg::R13, Reg::R14, Reg::R15]
    }
    fn caller_saved_regs() -> &'static [Reg] {
        &[Reg::AX, Reg::CX, Reg::DX, Reg::DI, Reg::SI, Reg::R8, Reg::R9]
    }
    fn pseudo_is_current_type(p: &str) -> bool {
        assembly_symbols::get_type(p) != AsmType::Double
    }
}

struct XMM;
impl RegType for XMM {
    fn suffix() -> &'static str {
        "xmm"
    }
    fn all_hardregs() -> &'static [Reg] {
        &[
            Reg::XMM0,
            Reg::XMM1,
            Reg::XMM2,
            Reg::XMM3,
            Reg::XMM4,
            Reg::XMM5,
            Reg::XMM6,
            Reg::XMM7,
            Reg::XMM8,
            Reg::XMM9,
            Reg::XMM10,
            Reg::XMM11,
            Reg::XMM12,
            Reg::XMM13,
        ]
    }
    fn caller_saved_regs() -> &'static [Reg] {
        Self::all_hardregs()
    }
    fn pseudo_is_current_type(p: &str) -> bool {
        assembly_symbols::get_type(p) == AsmType::Double
    }
}

pub fn allocate_registers(
    aliased_vars: &BTreeSet<String>,
    Program(tls): Program,
) -> Program {
    let new_tls: Vec<TopLevel> = tls
        .into_iter()
        .map(|tl| match tl {
            TopLevel::Function { name, global, instructions } => {
                let instrs = allocate::<GP>(&name, aliased_vars, instructions);
                let instrs = allocate::<XMM>(&name, aliased_vars, instrs);
                TopLevel::Function { name, global, instructions: instrs }
            }
            other => other,
        })
        .collect();
    Program(new_tls)
}

