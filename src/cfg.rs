use std::{collections::VecDeque, fmt::Debug};

use crate::{
    graph::{Graph, NodeId},
    Branches, Code, Error, Instruction, InstructionIdx,
};

/// Control flow block
#[derive(Debug)]
pub struct Block {
    pub id: BlockId,
    /// First instruction in the block
    pub first: InstructionIdx,
    /// Last instruction in the block
    pub last: InstructionIdx,
}

impl Block {
    /// Returns an iterator over all instructions in the block
    pub fn iter<'a>(&self, code: &'a Code) -> BlockIter<'a> {
        BlockIter {
            curr: self.first,
            last: self.last,
            code,
            finished: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(NodeId);

/// Iterator over all instructions in a block
pub struct BlockIter<'a> {
    curr: InstructionIdx,
    last: InstructionIdx,
    code: &'a Code,
    finished: bool,
}

impl<'a> BlockIter<'a> {
    fn decode(&mut self) -> Result<Instruction, Error> {
        self.code
            .decode(self.curr)?
            .ok_or(Error::InvalidBranch(self.curr))
    }
}

impl<'a> Iterator for BlockIter<'a> {
    type Item = (InstructionIdx, Result<Instruction, Error>);

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let instruction = match self.decode() {
            Ok(i) => i,
            Err(err) => return Some((self.curr, Err(err))),
        };

        let index = self.curr;

        // Check if this is the last instruction in the block
        if self.curr != self.last {
            // Panic if an invariant is violated. Otherwise, set the next index
            self.curr = match instruction.branches() {
                Branches::None => panic!("Instruction exits flow and isn't last in block"),
                Branches::Flow(tgt) => self.curr + tgt,
                Branches::Branches(_, _) => panic!("Instruction branches and isn't last in block"),
            }
        } else {
            self.finished = true;
        }

        Some((index, Ok(instruction)))
    }
}

#[derive(Debug, Clone, Copy)]
enum Intersect {
    /// Instruction has no sources and exits flow
    Dead,
    /// Instruction has one source and exits flow
    SingleSource,
    /// Instruction has multiple sources and exits flow
    MultiSource,
    /// Instruction continues regular flow and has no sources
    Pass,
    /// Instruction continues regular flow and has one source
    PassSingleSource,
    /// Instruction continues regular flow and has multiple sources
    PassMultiSource,
    /// Instruction branches and has no sources
    Branches,
    /// Instruction branches and has one source
    BranchesSingleSource,
    /// Instruction branches and has multiple sources
    BranchesMultiSource,
}

impl Intersect {
    /// Returns `true` if exits flow (e.g. returns)
    pub fn exits(&self) -> bool {
        matches!(
            self,
            Intersect::Dead | Intersect::SingleSource | Intersect::MultiSource
        )
    }

    /// Returns `true` if branches
    pub fn branches(&self) -> bool {
        matches!(
            self,
            Intersect::Branches | Intersect::BranchesSingleSource | Intersect::BranchesMultiSource
        )
    }

    /// Returns `true` if there are any sources (there is at least one preceding instruction)
    pub fn has_source(&self) -> bool {
        !matches!(
            self,
            Intersect::Dead | Intersect::Pass | Intersect::Branches
        )
    }

    /// Returns `true` if there are multiple sources
    pub fn multi_source(&self) -> bool {
        matches!(
            self,
            Intersect::MultiSource | Intersect::PassMultiSource | Intersect::BranchesMultiSource
        )
    }

    /// Returns `true` if there is a single source
    pub fn single_source(&self) -> bool {
        matches!(
            self,
            Intersect::SingleSource | Intersect::PassSingleSource | Intersect::BranchesSingleSource
        )
    }

    pub fn add_source(&mut self) {
        *self = match self {
            Intersect::Dead => Intersect::SingleSource,
            Intersect::SingleSource | Intersect::MultiSource => Intersect::MultiSource,
            Intersect::Pass => Intersect::PassSingleSource,
            Intersect::PassSingleSource | Intersect::PassMultiSource => Intersect::PassMultiSource,
            Intersect::Branches => Intersect::BranchesSingleSource,
            Intersect::BranchesSingleSource | Intersect::BranchesMultiSource => {
                Intersect::BranchesMultiSource
            }
        }
    }

    pub fn set_branches(&mut self) {
        *self = match self {
            Intersect::Dead | Intersect::Pass => Intersect::Branches,
            Intersect::SingleSource | Intersect::PassSingleSource => {
                Intersect::BranchesSingleSource
            }
            Intersect::MultiSource | Intersect::PassMultiSource => Intersect::BranchesMultiSource,
            Intersect::Branches
            | Intersect::BranchesSingleSource
            | Intersect::BranchesMultiSource => *self,
        }
    }

    pub fn set_flows(&mut self) {
        *self = match self {
            Intersect::Dead | Intersect::Branches | Intersect::Pass => Intersect::Pass,
            Intersect::SingleSource
            | Intersect::PassSingleSource
            | Intersect::BranchesSingleSource => Intersect::PassSingleSource,
            Intersect::MultiSource
            | Intersect::PassMultiSource
            | Intersect::BranchesMultiSource => Intersect::PassMultiSource,
        }
    }
}

/// Control flow graph (CFG)
pub struct ControlFlowGraph {
    pub graph: Graph<Block>,

    /// Mapping from instruction indicies to associated blocks
    block_map: Vec<Option<NodeId>>,

    /// Instruction flow
    intersects: Vec<Intersect>,
}

impl Debug for ControlFlowGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ControlFlowGraph")
            .field("graph", &self.graph)
            .field("block_map", &self.block_map)
            .field("intersects", &self.intersects)
            .finish()
    }
}

/// Builds instruction intersections
fn build_intersects(code: &Code) -> Result<Vec<Intersect>, Error> {
    let mut intersects = vec![Intersect::Dead; code.len()];

    for (idx, res) in code.decode_iter() {
        let instruction = res?;
        match instruction.branches() {
            Branches::None => {}
            Branches::Flow(offset) => {
                intersects[idx.0].set_flows();

                let dst = idx + offset;
                intersects
                    .get_mut(dst.0)
                    .ok_or(Error::InvalidBranch(dst))?
                    .add_source();
            }
            Branches::Branches(a, b) => {
                intersects[idx.0].set_branches();

                for dst in [idx + a, idx + b] {
                    intersects
                        .get_mut(dst.0)
                        .ok_or(Error::InvalidBranch(dst))?
                        .add_source();
                }
            }
        }
    }

    Ok(intersects)
}

impl ControlFlowGraph {
    /// Builds a [ControlFlowGraph] from `code`
    pub fn build(code: &Code) -> Result<ControlFlowGraph, Error> {
        CfgBuilder::build(code)
    }

    /// Returns an iterator over the blocks
    pub fn iter(&self) -> impl Iterator<Item = &Block> {
        self.graph.iter().map(|node| node.get())
    }

    /// Returns the block associated with a [NodeId]
    pub fn get(&self, id: NodeId) -> Option<&Block> {
        self.graph.get(id).map(|n| n.get())
    }

    /// Returns an iterator over all blocks in the graph in reverse postorder
    pub fn reverse_postorder_iter(&self) -> impl Iterator<Item = &Block> {
        self.graph.reverse_postorder_iter().map(|node| node.get())
    }

    /// Returns the root node id
    pub fn root_id(&self) -> NodeId {
        NodeId::new(0)
    }

    /// Returns the block containing an instruction
    pub fn instruction_block(&self, instruction_id: InstructionIdx) -> Option<NodeId> {
        self.block_map
            .get(instruction_id.0)
            .unwrap_or(&None)
            .to_owned()
    }

    /// Returns [NodeId] of next node with a branch or multiple sources
    pub fn next_junction(&self, id: NodeId) -> NodeId {
        // TODO: Verify that we don't skip any nodes with actual instructions!
        let mut node = self.graph.get(id).unwrap();
        loop {
            if node.links_from().len() != 1 || node.links_to().len() != 1 {
                break;
            }
            if node.get().first != node.get().last {
                // TODO: Check that this is just a JMP instruction
                break;
            }
            node = self.graph.get(*node.links_to().first().unwrap()).unwrap();
        }
        node.id()
    }
}

/// Control flow graph builder
struct CfgBuilder<'a> {
    intersects: Vec<Intersect>,
    graph: Graph<Block>,
    block_map: Vec<Option<NodeId>>,
    code: &'a Code,
}

struct MakeBlockResult {
    node_id: NodeId,
    last_index: InstructionIdx,
    last_instruction: Instruction,
}

impl<'a> CfgBuilder<'a> {
    pub fn build(code: &Code) -> Result<ControlFlowGraph, Error> {
        // Build instruction intersections
        let intersects = build_intersects(code)?;

        // Build control flow graph
        let graph = Graph::new();
        let block_map = vec![None; code.len()];

        let mut builder = CfgBuilder {
            intersects,
            graph,
            block_map,
            code,
        };

        builder.make_graph()?;

        Ok(ControlFlowGraph {
            graph: builder.graph,
            block_map: builder.block_map,
            intersects: builder.intersects,
        })
    }

    /// Makes a new block starting with the instruction at `index`
    fn make_block_at(&mut self, index: InstructionIdx) -> Result<MakeBlockResult, Error> {
        assert_eq!(self.block_map[index.0], None);

        let node_id = self.graph.next_id();
        let first = index;

        let mut index = index;
        let mut intersect = self.intersects[index.0];
        let last_instruction = loop {
            // Set block map
            self.block_map[index.0] = Some(node_id);

            // Decode instruction to get next index
            let instruction = self
                .code
                .decode(index)?
                .ok_or(Error::InvalidBranch(index))?;

            if intersect.branches() | intersect.exits() {
                // Last instruction in the block
                break instruction;
            }

            let next_index = match instruction.branches() {
                Branches::None => panic!("Instruction exits but Intersect does not"),
                Branches::Flow(tgt) => index + tgt,
                Branches::Branches(_, _) => panic!("Instruction branches but Intersect does not"),
            };

            // Check if next instruction has multiple sources
            let next_intersect = self.intersects[next_index.0];

            if next_intersect.multi_source() {
                // next_index starts a block
                break instruction;
            }

            index = next_index;
            intersect = next_intersect;
        };

        assert_eq!(
            self.graph.new_node(Block {
                id: BlockId(node_id),
                first,
                last: index
            }),
            node_id
        );

        Ok(MakeBlockResult {
            node_id,
            last_index: index,
            last_instruction,
        })
    }

    /// Iteratively builds the graph structure
    fn make_graph(&mut self) -> Result<(), Error> {
        let mut to_make = VecDeque::new();

        struct MakeBlock {
            src_node: Option<NodeId>,
            /// First instruction of the block
            first: InstructionIdx,
        }

        // First instruction is the start of the root block
        to_make.push_back(MakeBlock {
            src_node: None,
            first: InstructionIdx(0),
        });

        while let Some(MakeBlock { src_node, first }) = to_make.pop_front() {
            if let Some(node_id) = self.block_map[first.0] {
                // Node already exists - link source node
                if let Some(src_node) = src_node {
                    self.graph.link(src_node, node_id);
                }
                continue;
            }
            let MakeBlockResult {
                node_id,
                last_index,
                last_instruction,
            } = self.make_block_at(first)?;

            // Link source node
            if let Some(src_node) = src_node {
                self.graph.link(src_node, node_id);
            }

            // Next instruction(s) will always start a block
            match last_instruction.branches() {
                Branches::None => {}
                Branches::Flow(offset) => to_make.push_back(MakeBlock {
                    src_node: Some(node_id),
                    first: last_index + offset,
                }),
                Branches::Branches(a, b) => {
                    to_make.push_back(MakeBlock {
                        src_node: Some(node_id),
                        first: last_index + a,
                    });
                    to_make.push_back(MakeBlock {
                        src_node: Some(node_id),
                        first: last_index + b,
                    });
                }
            }
        }

        Ok(())
    }
}
