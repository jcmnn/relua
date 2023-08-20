use std::{
    collections::{HashSet, VecDeque},
    fmt::{Debug, Display},
    ops::{Index, IndexMut},
};

/// Arena-based graph structure for storing control flow information
#[derive(Debug)]
pub struct Graph<T: Debug> {
    nodes: Vec<Node<T>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(usize);

impl Display for NodeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl NodeId {
    pub(crate) fn new(n: usize) -> NodeId {
        NodeId(n)
    }
}

impl<T: Debug> Graph<T> {
    /// Creates a new empty [Graph]
    pub fn new() -> Graph<T> {
        Graph { nodes: Vec::new() }
    }

    /// Returns the [NodeId] that will be returned on the next call to `Graph::new_node`
    pub fn next_id(&self) -> NodeId {
        NodeId(self.nodes.len())
    }

    /// Returns an iterator over all [Node]s in the graph
    pub fn iter(&self) -> impl Iterator<Item = &Node<T>> {
        self.nodes.iter()
    }

    /// Returns an iterator over all [Node]s in the graph in reverse postorder
    pub fn reverse_postorder_iter(&self) -> ReversePostorderIter<'_, T> {
        ReversePostorderIter::new(self)
    }

    /// Creates a new detached node
    ///
    /// # Examples
    ///
    /// ```
    /// # use relua::graph::Graph;
    /// let mut graph = Graph::new();
    /// let id = graph.new_node(123);
    ///
    /// assert_eq!(*graph[id].get(), 123)
    /// ```
    pub fn new_node(&mut self, node: T) -> NodeId {
        let idx = NodeId(self.nodes.len());
        self.nodes.push(Node::new(idx, node));
        idx
    }

    /// Returns a reference to the node with the associated [NodeId].
    ///
    /// Returns [None] if the association does not exist.
    ///
    /// # Examples
    ///
    /// ```
    /// # use relua::graph::Graph;
    /// let mut graph = Graph::new();
    /// let id = graph.new_node(123);
    ///
    /// assert_eq!(graph.get(id).map(|node| *node.get()), Some(123))
    /// ```
    pub fn get(&self, id: NodeId) -> Option<&Node<T>> {
        self.nodes.get(id.0)
    }

    /// Returns a mutable reference to the node with the associated [NodeId].
    ///
    /// Returns [None] if the association does not exist.
    pub fn node_mut(&mut self, id: NodeId) -> Option<&mut Node<T>> {
        self.nodes.get_mut(id.0)
    }

    /// Creates a link from `from` to `to`
    pub fn link(&mut self, from: NodeId, to: NodeId) {
        assert!(!self.nodes[from.0].links_to.contains(&to));
        assert!(!self.nodes[to.0].links_from.contains(&from));

        self.nodes[from.0].links_to.push(to);
        self.nodes[to.0].links_from.push(from);
    }

    /// Returns `true` if the graph contains no nodes
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Returns the first node in the graph
    pub fn first(&self) -> Option<&Node<T>> {
        self.nodes.first()
    }

    /// Returns the number of [Node]s in the graph
    pub fn len(&self) -> usize {
        self.nodes.len()
    }
}

impl<T: Debug> Default for Graph<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Debug> Index<NodeId> for Graph<T> {
    type Output = Node<T>;

    fn index(&self, index: NodeId) -> &Self::Output {
        &self.nodes[index.0]
    }
}

impl<T: Debug> IndexMut<NodeId> for Graph<T> {
    fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
        &mut self.nodes[index.0]
    }
}

// Traverses a graph in reverse postorder
pub struct ReversePostorderIter<'a, T: Debug> {
    graph: &'a Graph<T>,
    visited: HashSet<NodeId>,
    stack: Vec<NodeId>,
}

impl<'a, T: Debug> ReversePostorderIter<'a, T> {
    fn new(graph: &'a Graph<T>) -> ReversePostorderIter<'a, T> {
        let mut iter = ReversePostorderIter {
            graph,
            visited: HashSet::new(),
            stack: Vec::with_capacity(graph.len()),
        };

        if let Some(root) = graph.first() {
            iter.add_node(root);
        }

        // TODO: visited parameter is no longer needed

        iter
    }

    fn add_node(&mut self, current: &Node<T>) {
        if !self.visited.insert(current.id()) {
            // We already visited this node
            return;
        }

        // Add children first
        for &child_id in current.links_to() {
            if let Some(child) = self.graph.get(child_id) {
                self.add_node(child);
            }
        }

        // Add node to stack
        self.stack.push(current.id());
    }
}

impl<'a, T: Debug> Iterator for ReversePostorderIter<'a, T> {
    type Item = &'a Node<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.stack.pop().and_then(|id| self.graph.get(id))
    }
}

/// A node within a [Graph]
#[derive(Debug)]
pub struct Node<T: Debug> {
    id: NodeId,
    links_to: Vec<NodeId>,
    links_from: Vec<NodeId>,
    data: T,
}

impl<T: Debug> Node<T> {
    /// Creates a new node with no links
    pub fn new(id: NodeId, data: T) -> Node<T> {
        Node {
            id,
            links_to: Vec::new(),
            links_from: Vec::new(),
            data,
        }
    }

    /// Returns all [NodeId]s that the node links to
    pub fn links_to(&self) -> &[NodeId] {
        &self.links_to
    }

    /// Returns all [NodeId]s that link to the node
    pub fn links_from(&self) -> &[NodeId] {
        &self.links_from
    }

    /// Returns a reference the associated data
    pub fn get(&self) -> &T {
        &self.data
    }

    /// Returns a mutable reference to the associated data
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.data
    }

    /// Gets the node's [NodeId]
    pub fn id(&self) -> NodeId {
        self.id
    }
}

impl NodeId {
    pub fn link_to<T: Debug>(&self, graph: &mut Graph<T>, target: NodeId) {
        graph.link(*self, target);
    }
}
