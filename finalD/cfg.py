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

class LT:
    """Loop type enumeration."""
    post_tested = 0
    pre_tested = 1
    endless = 2

class BasicBlock:
    """BasicBlock class."""

    def __init__(self, instrs, btype, type_info=None):
        """
        Initialize BasicBlock object.

        Arguments:
        instrs -- a list of instructions
        btype -- basic block type (see BT)
        """
        self.instrs = instrs
        self.btype = btype
        self.type_info = type_info

        self.rev_postorder = None
        self.idom = None

        self.loop_head = None
        self.loop_latch = None
        self.loop_type = None
        self.loop_follow = None

        self.if_follow = None

    def __str__(self):
        """BasicBlock string representation."""
        return """<BasicBlock
        reverse post-order: %s
        immediate dominator: %s
        btype: %s
        type-info: %s
        loop_head: %s
        loop_latch: %s
        loop_type: %s
        loop_follow: %s
        if_follow: %s
        instrs: %s>""" % \
            (self.rev_postorder,
             self.idom,
             {BT.two_way : "two-way",
              BT.one_way : "one-way",
              BT.fall : "fall-through",
              None: None}[self.btype],
             self.type_info,
             self.loop_head,
             self.loop_latch,
             {LT.post_tested: "post-tested",
              LT.pre_tested: "pre-tested",
              LT.endless: "endless",
              None: None}[self.loop_type],
             self.loop_follow,
             self.if_follow,
             self.instrs)

    def __len__(self):
        """Return len(self.instrs)."""
        return len(self.instrs)

    def get_first(self):
        """Return first instruction offset in block."""
        return self.instrs[0]

    def get_last(self):
        """Return last instruction offset in block."""
        return self.instrs[-1]


def get_bt(last_instr):
    """Return block type of block with last instruction last_instr."""
    return {CondJump : BT.two_way,
            NegCondJump : BT.two_way,
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
            blocks.append(BasicBlock(block_instrs,
                                     get_bt(instr),
                                     instr.info()))
            block_instrs = []
    if block_instrs:
        blocks.append(BasicBlock(block_instrs, BT.fall))
    return blocks

def node_to_revpo(g):
    """Return lambda for Node ID -> reverse post-order number."""
    return lambda n: g.node_data(n).rev_postorder

def get_then_else(g, n):
    """Return (t, e) of two-way node n in graph g."""
    block = g.node_data(n)
    assert(block.btype == BT.two_way)
    positive, target, _ = block.type_info
    s0 = g.out_nbrs(n)[0]
    s1 = g.out_nbrs(n)[1]

    if positive:
        if target == g.node_data(s0).get_first():
            return (s0, s1)
        else:
            return (s1, s0)
    else:
        if target == g.node_data(s0).get_first():
            return (s1, s0)
        else:
            return (s0, s1)

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

# TODO: doesn't theoretically depend on decoded anymore
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
                if tblock.btype == BT.one_way and len(tblock) == 1:
                    graph.hide_edge(tedge)
                    graph.add_edge(node, graph.out_nbrs(tnode)[0])
                    # let's also change the underlying Jump
                    tnode_jump = decoded[tblock.get_first()]
                    node_jump.set_to(tnode_jump.get_to())
                    change = True

# merit?
def eliminate_single_unconditional_jumps(g):
    """Eliminates single unconditional jumps in g."""
    for n in postorder(g):
        block = g.node_data(n)
        if block.btype == BT.one_way and len(block) == 1:
            out = g.out_nbrs(n)[0]
            # change nodes that flow to n to flow directly to out
            for inc in g.inc_edges(n):
                g.add_edge(g.head(inc), out)
                g.hide_edge(inc)

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
    eliminate_single_unconditional_jumps(graph) # eliminate redundant jumps
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
    def derive_graph(g, intvs, first=False):
        ng = Graph.Graph()
        if first:
            for n, intv in enumerate(intvs):
                node_data = []
                for nid in intv:
                    node_data.append(nid)
                ng.add_node(n, node_data)
        else:
            for n, intv in enumerate(intvs):
                node_data = []
                for pn in intv:
                    node_data += g.node_data(pn)
                ng.add_node(n, node_data)
        for i, int_i in enumerate(intvs):
            for j, int_j in enumerate(intvs):
                h_j = int_j[0]
                if i != j and any(n in g.inc_nbrs(h_j) for n in int_i):
                    ng.add_edge(i, j, create_nodes=False)
        return ng
    dsequence = [(graph, intervals)]
    prev_graph = graph
    next_graph = derive_graph(prev_graph, intervals, True)
    while next_graph.number_of_nodes() != prev_graph.number_of_nodes():
        intervals = gen_intervals(next_graph)
        dsequence.append((next_graph, intervals))
        prev_graph = next_graph
        next_graph = derive_graph(prev_graph, intervals)
    return dsequence

def mark_nodes_in_loop(g, latch, head, interval):
    """
    Mark all nodes belonging to loop (latch, head).

    Return the list of loop nodes.
    """
    nodes_in_loop = [head]

    hblock = g.node_data(head)
    hblock.loop_head = head
    hblock.loop_latch = latch

    lblock = g.node_data(latch)

    for n in interval:
        block = g.node_data(n)
        if block.rev_postorder \
                in range(hblock.rev_postorder+1, lblock.rev_postorder+1):
            nodes_in_loop.append(n)
            if not block.loop_head:
                block.loop_head = head
                block.loop_latch = latch
    return nodes_in_loop

def set_loop_type(g, latch, head, nodes_in_loop):
    """Find loop type."""
    lblock = g.node_data(latch)
    hblock = g.node_data(head)
    if lblock.btype == BT.two_way:
        if head == latch:
            lt = LT.post_tested
        elif hblock.btype == BT.two_way:
            if all(hsucc in nodes_in_loop for hsucc in g.out_nbrs(head)):
                lt = LT.post_tested
            else:
                lt = LT.pre_tested
        else:
            lt = LT.post_tested
    else:
        if hblock.btype == BT.two_way:
            lt = LT.pre_tested
        else:
            lt = LT.endless
    hblock.loop_type = lt

def set_loop_follow(g, latch, head, nodes_in_loop):
    """Find loop follow node."""
    hblock = g.node_data(head)
    if hblock.loop_type == LT.pre_tested:
        if g.out_nbrs(head)[0] in nodes_in_loop:
            lf = g.out_nbrs(head)[1]
        else:
            lf = g.out_nbrs(head)[0]
    elif hblock.loop_type == LT.post_tested:
        if g.out_nbrs(latch)[0] in nodes_in_loop:
            lf = g.out_nbrs(latch)[1]
        else:
            lf = g.out_nbrs(latch)[0]
    else:
        lf = None
        for twn in [twn for twn in nodes_in_loop
                    if g.node_data(twn).btype == BT.two_way]:
            n0 = g.out_nbrs(twn)[0]
            n1 = g.out_nbrs(twn)[1]
            if (n0 not in nodes_in_loop) and \
                    (not lf or g.node_data(n0).rev_postorder < lf):
                lf = n0
            elif (n1 not in nodes_in_loop) and \
                    (not lf or g.node_data(n1).rev_postorder < lf):
                lf = n1
    hblock.loop_follow = lf

def loop_struct(dseq):
    """
    Structure loops.

    Find loops, deduce their type (pre-tested, post-tested, endless)
    and mark nodes belonging to loops.

    Arguments:
    dseq -- derived sequence of control flow graphs [(g, intvs), ...]
    """
    top_graph, intvs = dseq.pop(0)
    for intv in intvs:
        head = intv[0]
        latch = None
        for head_pred in top_graph.inc_nbrs(head):
            if head_pred in intv:
                # found a loop (head_pred, head)
                latch = head_pred
                nodes_in_loop = \
                    mark_nodes_in_loop(top_graph, latch, head, intv)
                set_loop_type(top_graph, latch, head, nodes_in_loop)
                set_loop_follow(top_graph, latch, head, nodes_in_loop)
                break
    for g, intvs in dseq:
        for intv in intvs:
            head = g.node_data(intv[0])[0]
            latch = None
            for head_pred in top_graph.inc_nbrs(head):
                top_intv = []
                top_intv.extend(g.node_data(n) for n in intv)
                print top_intv
                if head_pred in top_intv:
                    latch = head_pred
                    nodes_in_loop = \
                        mark_nodes_in_loop(top_graph, latch, head, top_intv)
                    set_loop_type(top_graph, latch, head, nodes_in_loop)
                    set_loop_follow(top_graph, latch, head, nodes_in_loop)

def get_dominators(g):
    """Find and return the dominators for every node in g."""
    dominators = {0 : set([0])}
    for n in g.node_list():
        if n != 0:
            dominators[n] = set(g.node_list())
    change = True
    while change:
        change = False
        for n in g.node_list():
            if n != 0:
                all_preds = g.inc_nbrs(n)
                new_doms = set(dominators[all_preds.pop()])
                for doms_p in [dominators[p] for p in all_preds]:
                    doms_p = doms_p.difference([0])
                    new_doms.intersection_update(doms_p)
                new_doms.add(n)
                if len(new_doms) != len(dominators[n]):
                    dominators[n] = new_doms
                    change = True
    return dominators

def set_immediate_dominators(g):
    """Find and store the immediate dominator for every node in g."""
    dominators = get_dominators(g)
    for n in g.node_list():
        block = g.node_data(n)
        doms_n = dominators[n].difference([n])
        for dom in doms_n:
            if not any(dom in dominators[d] for d in doms_n.difference([dom])):
                block.idom = dom
                break
        if not block.idom:
            block.idom = 0

def two_way_struct(g):
    """Structure two-way conditionals in g."""
#     def in_head_latch(n):
#         """Return True if n is a loop header or latching node."""
#         block = g.node_data(n)
#         return block.loop_latch == n or block.loop_head == n
    unresolved = []
    for m in postorder(g):
        block = g.node_data(m)
        if block.btype == BT.two_way:
            # nodes_desc already sorted, no need for max() below
            dominated_nways = [i for i in g.node_list()
                               if g.node_data(i).idom == m
                               and g.inc_degree(i) >= 2]
            if dominated_nways:
                n = max(dominated_nways, key=node_to_revpo)
                block.if_follow = n
                for unres in unresolved[:]:
                    unres_block = g.node_data(unres)
                    if n > unres:
                        unres_block.if_follow = n
                        unresolved.remove(unres)
            else:
                unresolved.append(m)

# TODO: FIX CONDITIONS
def merge_conditionals(g, n0, n1, type):
    """
    Merge the two conditional jump blocks n0 and n1 into n0.

    Arguments:
    g -- a control flow graph
    n0 -- a node in g (becomes the merge of n0 and n1)
    n1 -- a node in g
    cond -- the new condition for n0
    """
    # does NOT change underlying CondJump instruction yet
    # it should not matter for output generated by the decompiler
    block = g.node_data(n0)
    pos0, targ0, cond0 = block.type_info
    pos1, targ1, cond1 = g.node_data(n1).type_info
    t1, e1 = get_then_else(g, n1)
    if type == 0:
        g.add_edge(n0, e1)
        nt = g.node_data(e1).get_first()
        nc = "!(%s) && (%s)" % (cond0, cond1)
    elif type == 1:
        g.add_edge(n0, t1)
        nt = g.node_data(t1).get_first()
        nc = "(%s) || (%s)" % (cond0, cond1)
    elif type == 2:
        g.add_edge(n0, e1)
        nt = g.node_data(e1).get_first()
        nc = "(%s) && (%s)" % (cond0, cond1)
    else: #elif type == 3:
        g.add_edge(n0, t1)
        nt = g.node_data(t1).get_first()
        nc = "!(%s) || (%s)" % (cond0, cond1)
    block.type_info = (True, nt, nc)
    g.hide_node(n1)

def comp_conds_struct(g):
    """Structure compound conditions in g."""
    change = True
    while change:
        change = False
        for n in postorder(g):
            block = g.node_data(n)
            if block.btype == BT.two_way:
                t, e = get_then_else(g, n)
                tblock = g.node_data(t)
                eblock = g.node_data(e)
                if tblock.btype == BT.two_way \
                        and len(tblock) == 1 \
                        and g.inc_degree(t) == 1:
                    tt, te = get_then_else(g, t)
                    if tt == e:
                        merge_conditionals(g, n, t, 0)
                        change = True
                    elif te == e:
                        merge_conditionals(g, n, t, 1)
                        change = True
                elif eblock.btype == BT.two_way \
                        and len(eblock) == 1 \
                        and g.inc_degree(e) == 1:
                    et, ee = get_then_else(g, e)
                    if et == t:
                        merge_conditionals(g, n, e, 2)
                        change = True
                    elif ee == t:
                        merge_conditionals(g, n, e, 3)
                        change = True
