use std::{cell::RefCell, collections::HashMap};

use crate::{
    cfg::ControlFlowGraph,
    dominator::{self, DominatorTree},
    graph::NodeId,
    Code, Error, Instruction, InstructionIdx,
};

#[derive(Debug)]
enum AndOr {
    And(NodeId),
    Or(NodeId),
}

#[derive(Debug)]
enum Conditional {
    Single {
        left: NodeId,
        right: NodeId,
    },
    And {
        next: NodeId,
        fail: NodeId,
        invert: bool,
    },
}

impl Conditional {
    fn combine(&self, b: &Conditional, b_id: NodeId) -> Option<Conditional> {
        match self {
            Conditional::Single {
                left: a_left,
                right: a_right,
            } => match b {
                Conditional::Single {
                    left: b_left,
                    right: b_right,
                } => {
                    if (*a_left == *b_left || *a_left == *b_right) && *a_right == b_id {
                        Some(Conditional::And {
                            next: b_id,
                            fail: *a_left,
                            invert: false,
                        })
                    } else if (*a_right == *b_left || *a_right == *b_right) && *b_left == b_id {
                        Some(Conditional::And {
                            next: b_id,
                            fail: *a_right,
                            invert: true,
                        })
                    } else {
                        None
                    }
                }
                Conditional::And {
                    next,
                    fail,
                    invert: _,
                } => todo!(),
            },
            Conditional::And { next, fail, invert } => todo!(),
        }
    }
}

#[derive(Debug)]
enum Control {
    Pass(NodeId),
    If(RefCell<Conditional>),
    Return,
}

impl Control {
    fn from_instruction(
        idx: InstructionIdx,
        instruction: &Instruction,
        cfg: &ControlFlowGraph,
    ) -> Result<Control, Error> {
        let control = match instruction {
            Instruction::Return { .. } | Instruction::TailCall { .. } => Control::Return,
            Instruction::Jmp { offset } => {
                let tgt = idx + *offset + 1_usize;
                Control::Pass(
                    cfg.instruction_block(tgt)
                        .ok_or(Error::InvalidBranch(tgt))?,
                )
            }
            Instruction::LoadBool {
                skip_next: true, ..
            } => {
                let tgt = idx + 2_usize;
                Control::Pass(
                    cfg.instruction_block(tgt)
                        .ok_or(Error::InvalidBranch(tgt))?,
                )
            }
            Instruction::Eq { .. }
            | Instruction::Lt { .. }
            | Instruction::Le { .. }
            | Instruction::Test { .. }
            | Instruction::TestSet { .. }
            | Instruction::TForLoop { .. } => {
                let left = idx + 1_usize;
                let right = idx + 2_usize;

                let left = cfg
                    .instruction_block(left)
                    .ok_or(Error::InvalidBranch(left))?;

                let right = cfg
                    .instruction_block(right)
                    .ok_or(Error::InvalidBranch(right))?;

                Control::If(RefCell::new(Conditional::Single {
                    left: cfg.next_junction(left),
                    right: cfg.next_junction(right),
                }))
            }
            Instruction::ForLoop { offset, .. } | Instruction::ForPrep { offset, .. } => {
                unimplemented!()
            }
            // When offset is None, the next instruction stores the real value
            Instruction::SetList { offset: None, .. } => {
                let tgt = idx + 2_usize;
                Control::Pass(
                    cfg.instruction_block(tgt)
                        .ok_or(Error::InvalidBranch(tgt))?,
                )
            }
            _ => {
                let tgt = idx + 1_usize;
                Control::Pass(
                    cfg.instruction_block(tgt)
                        .ok_or(Error::InvalidBranch(tgt))?,
                )
            }
        };

        Ok(control)
    }
}

pub struct Decompiler<'a> {
    cfg: &'a ControlFlowGraph,
    code: &'a Code,
    dominator_tree: DominatorTree,
    controls: HashMap<NodeId, Control>,
}

#[derive(Debug)]
pub enum ControlNode {
    If {
        node: NodeId,
        first: NodeId,
        else_end: NodeId,
    },
    And {
        node: NodeId,
        next: NodeId,
        fail: NodeId,
    },
    Else,
    End,
}

struct ControlBuilder<'a> {
    controls: Vec<ControlNode>,
    stack: Vec<Control>,
    last_node: Vec<NodeId>,
    dominator_tree: DominatorTree,
    cfg: &'a ControlFlowGraph,
    code: &'a Code,
}

impl<'a> ControlBuilder<'a> {
    fn new(cfg: &'a ControlFlowGraph, code: &'a Code) -> ControlBuilder<'a> {
        ControlBuilder {
            controls: Vec::new(),
            stack: Vec::new(),
            last_node: Vec::new(),
            dominator_tree: DominatorTree::from_cfg(cfg),
            cfg,
            code,
        }
    }

    fn begin_if(&mut self, node_id: NodeId, left: NodeId, right: NodeId) -> NodeId {
        // Check idioms of branches
        let left_idiom = self.dominator_tree.idiom(left).unwrap();
        let right_idiom = self.dominator_tree.idiom(right).unwrap();

        assert!(left_idiom == node_id || right_idiom == node_id);

        if left_idiom != right_idiom {
            // This might be an 'and' statement
            let (dup_branch, dom_branch, dup_idiom) = match (left_idiom, right_idiom) {
                (li, ri) if li == node_id => (right, left, ri),
                (li, ri) if ri == node_id => (left, right, li),
                _ => panic!("Neither branch is dominated by node"),
            };

            // Verify previous branch?
            println!("{:?}, {:?}, {:?}", dup_branch, dom_branch, dup_idiom);
            println!("{:#?}", self.controls);
            let it = self.controls.iter().rev().enumerate().find(|(_, cont)| matches!(cont, ControlNode::If { node, first, else_end } if *node == dup_idiom && *else_end == dup_branch));
            println!("Found {:?}", it);
            let (idx, parent_node) = it.expect("Failed to find parent if statement");
            assert_eq!(idx, 0);
            self.controls.push(ControlNode::And {
                node: node_id,
                next: dom_branch,
                fail: dup_branch,
            });

            dom_branch
        } else {
            // Add 'if' to control stack

            // Check if either branch has multiple sources
            if self.cfg.graph.get(left).unwrap().links_from().len() > 1 {
                // We don't know how to handle the case where both have multiple sources
                assert!(self.cfg.graph.get(right).unwrap().links_from().len() == 1);
                self.controls.push(ControlNode::If {
                    node: node_id,
                    first: right,
                    else_end: left,
                });

                // Process right first
                //self.process_next(right).unwrap();
                // TODO: End or else?
                right
            } else {
                assert!(self.cfg.graph.get(left).unwrap().links_from().len() == 1);
                self.controls.push(ControlNode::If {
                    node: node_id,
                    first: left,
                    else_end: right,
                });

                left
            }
        }
    }

    fn process_next(&mut self, node_id: NodeId) -> Result<Option<NodeId>, Error> {
        println!("Processing {:?}", node_id);
        let block = self.cfg.graph.get(node_id).unwrap().get();

        match Control::from_instruction(
            block.last,
            &self
                .code
                .decode(block.last)?
                .ok_or(Error::InvalidBranch(block.last))?,
            self.cfg,
        )? {
            Control::Pass(to_id) => Ok(Some(to_id)),
            Control::If(cond) => {
                let cond = cond.borrow();
                if let Conditional::Single { left, right } = *cond {
                    Ok(Some(self.begin_if(node_id, left, right)))
                } else {
                    todo!();
                }
            }
            Control::Return => Ok(None),
        }
    }
}

impl<'a> Decompiler<'a> {
    fn construct_initial_controls(&mut self) -> Result<(), Error> {
        // Iteratively construct control structure
        for node in self.cfg.graph.iter() {
            let id = node.id();
            let block = node.get();

            let instruction = self.code.decode(block.last)?.unwrap();
            let control = Control::from_instruction(block.last, &instruction, self.cfg)?;
            // TODO: Check if control targets match node.links_to()
            self.controls.insert(id, control);
        }

        Ok(())
    }

    fn try_combine(&self, cond: &Conditional, b: NodeId) -> Option<Conditional> {
        if let Control::If(b_cond) = &self.controls[&b] {
            cond.combine(&b_cond.borrow(), b)
        } else {
            None
        }
    }

    fn combine_conditionals(&mut self) -> Result<(), Error> {
        for (node_id, control) in &self.controls {
            // Check if control is an IF
            if let Control::If(conditional) = control {
                let conditional = conditional.borrow();
                if let Conditional::Single { left, right } = *conditional {
                    let left_idiom = self.dominator_tree.idiom(left).unwrap();
                    let right_idiom = self.dominator_tree.idiom(right).unwrap();
                    // We only know how to handle cases where at least one idiom is the conditional
                    assert!(left_idiom == *node_id || right_idiom == *node_id);
                    /*if left_idiom != right_idiom {
                    } else {
                        // if-else statement
                    }*/
                }
            }
        }

        Ok(())
        /*
        for control in self.controls.values() {
            if let Control::If(cond) = control {
                let res = {
                    let borrow = cond.borrow();
                    match &*borrow {
                        Conditional::Single { left, right } => self
                            .try_combine(&borrow, *left)
                            .or_else(|| self.try_combine(&borrow, *right)),
                        _ => unimplemented!(),
                    }
                };
                if let Some(new_cond) = res {
                    cond.replace(new_cond);
                }
            }
        }
        Ok(())*/
    }

    fn decompile(&mut self) -> Result<(), Error> {
        self.construct_initial_controls()?;
        // self.combine_conditionals()?;

        let mut cb = ControlBuilder::new(self.cfg, self.code);
        let mut next_id = NodeId::new(0);
        while let Some(n) = cb.process_next(next_id)? {
            next_id = n;
        }

        Ok(())
    }
}

pub fn decompile(cfg: &ControlFlowGraph, code: &Code) -> Result<(), Error> {
    let mut decompiler = Decompiler {
        cfg,
        code,
        controls: HashMap::new(),
        dominator_tree: DominatorTree::from_cfg(cfg),
    };

    // let dominator_tree = DominatorTree::from_cfg(cfg);
    // println!("{:#?}", dominator_tree);

    decompiler.decompile()?;
    //println!("{:#?}", decompiler.controls);

    Ok(())
}
