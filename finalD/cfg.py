"""Control flow graph module."""

from iformat import *
from altgraph import Graph


class BT:
    """Block type enumeration."""

    fall = 0
    """fall-through block"""
    one_way = 1
    """one way block"""
    two_way = 2
    """two way block"""


class BasicBlock:
    """BasicBlock class."""

    def __init__(self, instrs, btype):
        """
        Initialize BasicBlock object.

        Arguments:
        instrs -- a list of instructions
        btype -- basic block type (see BT)
        """
        self.instrs = instrs
        self.btype = btype
        self.rev_postorder = None

    def __str__(self):
        """BasicBlock string representation."""
        return str(self.instrs)

    def get_first(self):
        """Return first instruction offset in block."""
        return self.instrs[0]

    def get_last(self):
        """Return last instruction offset in block."""
        return self.instrs[-1]


def get_bt(last_instr):
    """Return block type of block with last instruction last_instr."""
    return {CondJump : BT.two_way,
            Jump : BT.one_way}.get(last_instr.__class__, BT.fall)

def gen_basic_blocks(decoded):
    """Generate basic block from a decoded instruction dictionary."""
    blocks = []
    block_instrs = []
    jump_targets = set(map(lambda jmp: jmp.get_to(),
                           filter(is_jump, decoded.values())))
    decoded_tuples = decoded.items()
    decoded_tuples.sort(key=(lambda t: t[0]))
    for off, instr in decoded_tuples:
        if off in jump_targets:
            if block_instrs:
                blocks.append(BasicBlock(block_instrs, BT.fall))
                block_instrs = []
        block_instrs.append(off)
        if is_jump(instr):
            blocks.append(BasicBlock(block_instrs, get_bt(instr)))
            block_instrs = []
    if block_instrs:
        blocks.append(BasicBlock(block_instrs, BT.fall))
    return blocks

def postorder(graph):
    """
    Return a list of graph's nodes in postorder.

    0 is assumed to be the start node.

    Arguments:
    graph -- a control flow graph
    """
    seen = {}
    def visit(node):
        seen[node] = True
        for child in graph.out_nbrs(node):
            if child not in seen:
                for result in visit(child):
                    yield result
        yield node
    return list(visit(0))

def hide_dead_nodes(graph):
    """
    Hide dead nodes (and corresponding edges) in graph.

    Nodes are considered dead iff they are unreachable from the root node.
    The root node is always reachable.
    """
    change = True
    while change:
        change = False
        for node in graph.node_list():
            if node != 0:
                inc = graph.inc_nbrs(node)
                if (len(inc) == 1 and node in inc) or not inc:
                    graph.hide_node(node)
                    change = True

def rewire_jumps(graph, decoded):
    """
    Rewire jumps in graph for simpler control flow where possible.

    Any kind of jump to another unconditional jump instruction is
    rewired to directly point to the jump target of the second jump.

    TODO: If a two_way fall-through successor is a one_way..

    Arguments:
    graph -- a control flow graph
    decoded -- dictionary of decoded instructions
    """
    change = True
    while change:
        change = False
        for node in graph.node_list():
            block = graph.node_data(node)
            if block.btype in (BT.two_way, BT.one_way):
                node_jump = decoded[block.get_last()]
                if block.btype == BT.one_way:
                    tedge = graph.out_edges(node)[0]
                else:
                    # ugly way of finding the "jump outgoing edge"
                    e = graph.out_edges(node)[0]
                    j = graph.node_data(graph.tail(e)).get_first()
                    if j == node_jump.get_to():
                        tedge = e
                    else:
                        tedge = graph.out_edges(node)[1]
                tnode = graph.tail(tedge)
                tblock = graph.node_data(tnode)
                if tblock.btype == BT.one_way and len(tblock.instrs) == 1:
                    graph.hide_edge(tedge)
                    graph.add_edge(node, graph.out_nbrs(tnode)[0])
                    # let's also change the underlying Jump
                    tnode_jump = decoded[tblock.get_first()]
                    node_jump.set_to(tnode_jump.get_to())
                    change = True

def gen_control_flow_graph(blocks, decoded):
    """
    Generate a control flow graph.

    Arguments:
    blocks -- a list of BasicBlock objects
    decoded -- a dict of offset : Instr
    """
    def index_key(lst, val, key):
        for i, e in enumerate(lst):
            if key(e) == val:
                return i
    graph = Graph.Graph()
    for i, block in enumerate(blocks):
        graph.add_node(i, block)
    for i, block in enumerate(blocks):
        if block.btype == BT.one_way or block.btype == BT.two_way:
            # we know the last Instr is some kind of Jump instance
            jump_target = decoded[block.get_last()].get_to()
            target_index = index_key(blocks, jump_target, BasicBlock.get_first)
            # assert that jump target is valid instruction offset
            # (if not, probably a disassembly error happened before)
            assert(target_index != None)
            graph.add_edge(i, target_index, create_nodes=False)
        # a two-way has a fall-through too!
        if block.btype == BT.fall or block.btype == BT.two_way:
            if i+1 != len(blocks):
                graph.add_edge(i, i+1, create_nodes=False)
    rewire_jumps(graph, decoded)  # simplify "redundant" jumps
    hide_dead_nodes(graph)  # hide all unreachable nodes
    # graph edges have been added, now tag basic blocks with their
    # reverse postorder in the control flow graph
    for order, node in enumerate(reversed(postorder(graph))):
        graph.node_data(node).rev_postorder = order
    return graph

def gen_intervals(graph):
    """Generate the non overlapping graph intervals for graph."""
    def get_nodes_for_interval(interval):
        return [m for m in graph.node_list()
                if m not in interval
                and graph.inc_nbrs(m)  # if [], next pred will always be true
                and all(p in interval for p in graph.inc_nbrs(m))]
    headers = set([0])
    processed = set()
    intervals = []
    while any(h not in processed for h in headers):
        h = headers.difference(processed).pop()
        i_h = [h]
        processed.add(h)
        new_nodes = get_nodes_for_interval(i_h)
        while new_nodes:
            i_h += new_nodes
            new_nodes = get_nodes_for_interval(i_h)
        headers.update([m for m in graph.node_list()
                        if m not in headers
                        and m not in i_h
                        and any(p in i_h for p in graph.inc_nbrs(m))])
        intervals.append(i_h)
    return intervals

def gen_derived_sequence(graph, intervals):
    """
    Generate the derived sequence of graphs of graph.

    Arguments:
    graph -- a graph
    intervals -- the list of graph intervals of graph
    """
    def derive_graph(g, ints):
        ng = Graph.Graph()
        for n in range(len(ints)):
            ng.add_node(n)
        for i, int_i in enumerate(ints):
            for j, int_j in enumerate(ints):
                h_j = int_j[0]
                if i != j and any(n in g.inc_nbrs(h_j) for n in int_i):
                    ng.add_edge(i, j, create_nodes=False)
        return ng
    dsequence = [(graph, intervals)]
    prev_graph = graph
    next_graph = derive_graph(prev_graph, intervals)
    while next_graph.number_of_nodes() != prev_graph.number_of_nodes():
        intervals = gen_intervals(next_graph)
        dsequence.append((next_graph, intervals))
        prev_graph = next_graph
        next_graph = derive_graph(prev_graph, intervals)
    return dsequence

def loop_struct(dseq, decoded):
    """
    Structure loops.

    Find loops, deduce their type (pre-tested, post-tested, endless)
    and mark nodes belonging to loops.

    Arguments:
    dseq -- derived sequence of control flow graphs [(g, intvs), ...]
    decoded -- dictionary of instr offset -> decoded instruction
    """
    for g, intvs in dseq:
        for intv in intvs:
            header_node = intv[0]
            # a latching node is the node where the back-edge to a loop
            # header originates
            latching_node = None
            for header_pred in g.inc_nbrs(header_node):
                if header_pred in intv:
                    # found a loop (header_pred, header_node)
                    latching_node = header_pred
                    # mark_nodes_in_loop(latching_node, header_node, intv)
                    # find loop type here
                    # find loop follow here
