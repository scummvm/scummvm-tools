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

def gen_control_flow_graph(blocks, decoded):
    """
    Generate a control flow graph.

    Arguments:
    blocks -- a list of BasicBlock objects
    decoded -- a dict of offset : Instr
    """
    edges = []
    def index_key(lst, val, key):
        for i, e in enumerate(lst):
            if key(e) == val:
                return i
    for i, block in enumerate(blocks):
        if block.btype == BT.one_way or block.btype == BT.two_way:
            # we know the last Instr is some kind of Jump instance
            jump_target = decoded[block.get_last()].get_to()
            target_index = index_key(blocks, jump_target, BasicBlock.get_first)
            assert(target_index != None)  # this should be non None
            edges.append((i, target_index))
        # a two-way has a fall-through too!
        if block.btype == BT.fall or block.btype == BT.two_way:
            if i+1 != len(blocks):
                edges.append((i, i+1))
    return Graph.Graph(edges)
