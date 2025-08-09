use crate::cfg::{BasicBlock, Cfg, Instr, NodeId};

pub fn analyze<I, V, FMeet, FTransfer>(
    cfg: Cfg<V, I>,
    meet_fn: FMeet,
    transfer_fn: FTransfer,
) -> Cfg<V, I>
where
    I: Instr,
    V: Clone + PartialEq + Eq + std::hash::Hash + Default,
    FMeet: Fn(&Cfg<V, I>, &BasicBlock<V, I>) -> V,
    FTransfer: Fn(&BasicBlock<V, I>, &V) -> BasicBlock<V, I>,
{
    let mut current_cfg = cfg;
    let mut worklist: Vec<(usize, BasicBlock<V, I>)> = current_cfg.basic_blocks.clone();
    worklist.reverse();
    while let Some((block_idx, blk)) = worklist.pop() {
        let old_annotation = blk.value.clone();
        let live_at_exit = meet_fn(&current_cfg, &blk);
        let new_block = transfer_fn(&blk, &live_at_exit);
        current_cfg.update_basic_block(block_idx, new_block.clone());
        if old_annotation != new_block.value {
            for pred in new_block.preds.iter() {
                match pred {
                    NodeId::Entry => {}
                    NodeId::Exit => panic!("malformed CFG"),
                    NodeId::Block(n) => {
                        if !worklist.iter().any(|(i, _)| i == n) {
                            if let Some((_, b)) =
                                current_cfg.basic_blocks.iter().find(|(i, _)| i == n)
                            {
                                worklist.push((*n, b.clone()));
                            }
                        }
                    }
                }
            }
        }
    }
    current_cfg
}
