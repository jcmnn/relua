use std::collections::{HashMap, HashSet};

use crate::{
    cfg::{Block, BlockId, ControlFlowGraph},
    graph::{Node, NodeId},
};

#[derive(Debug)]
pub struct DominatorTree {
    doms: HashMap<NodeId, NodeId>,
}

impl DominatorTree {
    pub fn from_cfg(cfg: &ControlFlowGraph) -> DominatorTree {
        let mut doms = HashMap::with_capacity(cfg.graph.len());
        // Insert root node
        doms.insert(cfg.root_id(), cfg.root_id());

        let reverse_postorder_nodes: Vec<&Node<Block>> =
            cfg.graph.reverse_postorder_iter().collect();

        let node_rpn_map: HashMap<NodeId, usize> = reverse_postorder_nodes
            .iter()
            .enumerate()
            .map(|(idx, node)| (node.id(), idx))
            .collect();

        //let mut doms: Vec<Option<NodeId>> = vec![None; reverse_postorder_nodes.len()];

        // Returns nearest intersection of two nodes
        fn intersect(
            doms: &mut HashMap<NodeId, NodeId>,
            reverse_postorder_nodes: &[&Node<Block>],
            node_rpn_map: &HashMap<NodeId, usize>,
            b1: usize,
            b2: usize,
        ) -> usize {
            let mut finger1 = b1;
            let mut finger2 = b2;
            while finger1 != finger2 {
                while finger1 > finger2 {
                    finger1 = node_rpn_map[&doms[&reverse_postorder_nodes[finger1].id()]];
                }
                while finger2 > finger1 {
                    finger2 = node_rpn_map[&doms[&reverse_postorder_nodes[finger2].id()]];
                }
            }

            finger1
        }

        let mut changed = true;
        while changed {
            changed = false;

            for (idx, node) in reverse_postorder_nodes.iter().enumerate().skip(1) {
                let mut preds = node.links_from().iter();
                // Get first predecessor, or continue if there are none
                let mut new_idiom = match preds.next() {
                    Some(&pred) => pred,
                    // There should always be at least one link when iterating in reverse postorder
                    None => panic!("No predecessor for {:?}", node.id()),
                };

                for pred in preds {
                    if doms.contains_key(pred) {
                        new_idiom = reverse_postorder_nodes[intersect(
                            &mut doms,
                            &reverse_postorder_nodes,
                            &node_rpn_map,
                            node_rpn_map[pred],
                            node_rpn_map[&new_idiom],
                        )]
                        .id();
                    }
                }

                if doms.insert(node.id(), new_idiom) != Some(new_idiom) {
                    changed = true;
                }
            }
        }
        /*
        DominatorTree {
            doms: doms
                .iter()
                .enumerate()
                .filter_map(|(idx, node_id)| {
                    node_id.and_then(|node_id| Some((reverse_postorder_nodes[idx].id(), node_id)))
                })
                .collect(),
        }*/
        DominatorTree { doms }
    }

    /// Returns the immediate dominator of `node_id`
    pub fn idiom(&self, node_id: NodeId) -> Option<NodeId> {
        self.doms.get(&node_id).copied()
    }

    /// Returns true if `a` is dominates `b`
    pub fn dominates(&self, a: NodeId, b: NodeId) -> bool {
        let mut b = b;
        while b != NodeId::new(0) && a != b {
            b = self.doms[&b];
        }

        a == b
    }
}
