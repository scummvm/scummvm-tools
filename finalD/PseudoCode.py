# DecomPy - Bytecode Decompiler for the ScummVM project
# Copyright (C) 2007 Andreas Scholta

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

"""Pseudo code formatting from control flow graphs."""

from cfg import BT, LT, get_then_else, node_to_revpo, postorder
from iformat import is_jump


class PseudoCode:
    """Control flow graph -> pseudo code translation."""

    def __init__(self, cfg, decoded, indent=2):
        """
        Initialize PseudoCode object.

        Arguments:
        cfg -- a control flow graph with structuring information
        decoded -- a dictionary of offset -> Instr
        indent -- indentation by indent spaces each (default 4)
        """
        self.cfg = cfg
        self.decoded = decoded
        self.indent = indent

        self.traversed = []
        """Hold nodes we already traversed."""

    def generate_code(self):
        """Generate code for cfg."""
        self._write_code(0, 0, None, None)

    def _write(self, code, i, pre="[****]"):
        """Write code at indentation level i."""
        print "%s %s%s" % (pre, " "*(i*self.indent), code)

    def _write_bb(self, bbn, i):
        """
        Write code for bbn.

        Arguments:
        bbn -- node in the control flow graph
        i -- current indentation level
        """
        for instr_off in self.cfg.node_data(bbn).instrs:
            instr = self.decoded[instr_off]
            if not is_jump(instr):
                self._write("%s;" % instr, i, "[%.4X]" % instr_off)

    def _emit_goto(self, n, i):
        """Emit goto to n."""
        off = self.cfg.node_data(n).get_first()
        self._write("goto %.4X;" % off, i)

    def _write_loop(self, bbn, i, latch, ifollow):
        """
        Write code for loop structure rooted at bb.

        Arguments:
        bbn -- node in the control flow graph
        i -- current indentation level
        latch -- latching node of enclosing loop structure (or None)
        ifollow -- follow node of the enclosing if structure (or None)
        """
        self.traversed.append(bbn)
        bb = self.cfg.node_data(bbn)
        head_is_if = False
        # write loop header
        if bb.loop_type == LT.pre_tested:
            self._write_bb(bbn, i)
            t, e = get_then_else(self.cfg, bbn)
            if e == bb.loop_follow:
                fmt = "while (%s) {"
            else:
                fmt = "while (!(%s)) {"
            self._write(fmt % bb.type_info[2], i, "[%.4X]" % bb.get_last())
        elif bb.loop_type == LT.post_tested:
            self._write("do {", i)
            if bbn != bb.loop_latch and bb.btype == BT.two_way:
                self._write_two_way(bbn, i+1, latch, ifollow, True)
                head_is_if = True
            else:
                self._write_bb(bbn, i+1)
        else:
            self._write("for (;;) {", i)
            self._write_bb(bbn, i+1)
        if bb == latch:
            return
        if bb.loop_latch != bbn:
            # loop is several basic blocks
            if not head_is_if:
                succs_asc = self.cfg.out_nbrs(bbn)
                succs_asc.sort(key=node_to_revpo(self.cfg))
                for s in succs_asc:
                    if bb.loop_type != LT.pre_tested or s != bb.loop_follow:
                        if s not in self.traversed:
                            self._write_code(s, i+1, bb.loop_latch, ifollow)
                        else:
                            # s already traversed
                            self._emit_goto(s, i+1)
            else:
                succs_asc = self.cfg.out_nbrs(bb.if_follow)
                succs_asc.sort(key=node_to_revpo(self.cfg))
                if bb.loop_latch == bb.if_follow:
                    if bb.loop_latch not in self.traversed:
                        self._write_bb(bb.if_follow, i+1)
                    else:
                        self._emit_goto(bb.if_follow, i+1)
                else:
                    for s in [bb.if_follow] + succs_asc:
                        if s != bb.loop_follow:
                            if s not in self.traversed:
                                self._write_code(s,
                                                 i+1,
                                                 bb.loop_latch,
                                                 ifollow)
                            else:
                                self._emit_goto(s, i+1)

        # write loop trailer
        if bb.loop_type == LT.pre_tested:
            self._write_bb(bbn, i+1)
            self._write("}", i)
        elif bb.loop_type == LT.post_tested:
            lbb = self.cfg.node_data(bb.loop_latch)
            positive, _, condition = lbb.type_info
            if positive:
                fmt = "} while (%s);"
            else:
                fmt = "} while (!(%s));"
            self._write(fmt % condition, i, "[%.4X]" % lbb.get_last())
        else:
            self._write("}", i)
        if bb.loop_follow not in self.traversed:
            self._write_code(bb.loop_follow, i, latch, ifollow)
        else:
            # loop follow already traversed
            self._emit_goto(bb.loop_follow, i)

    # TODO: fix inverting of conditions
    def _write_two_way(self, bbn, i, latch, ifollow, nofollow=False):
        """
        Write code for tree rooted at bbn.

        Arguments:
        bbn -- node in the control flow graph
        i -- current indentation level
        latch -- latching node of enclosing loop structure (or None)
        ifollow -- follow node of the enclosing if structure (or None)
        nofollow -- do not continue with follow node (default: False)
        """
        bb = self.cfg.node_data(bbn)
        t, e = get_then_else(self.cfg, bbn)
        self._write_bb(bbn, i)
        if bb.if_follow:
            empty_then = False
            if t not in self.traversed:
                # process then clause
                if t != bb.if_follow:
                    self._write("if (%s) {" % bb.type_info[2],
                                i,
                                "[%.4X]" % bb.get_last())
                    self._write_code(t, i+1, latch, bb.if_follow)
                else:
                    # then clause empty, negate condition
                    self._write("if (!(%s)) {" % bb.type_info[2],
                                i,
                                "[%.4X]" % bb.get_last())
                    self._write_code(e, i+1, latch, bb.if_follow)
                    empty_then = True
            else:
                # t already traversed, emit goto
                self._emit_goto(t, i)
            if e not in self.traversed:
                if e != bb.if_follow:
                    self._write("} else {", i)
                    self._write_code(e, i+1, latch, bb.if_follow)
            elif not empty_then:
                self._write("} else {", i)
                self._emit_goto(e, i+1)
            self._write("}", i)
            if not nofollow and \
                    ifollow != bb.if_follow and \
                    bb.if_follow not in self.traversed:
                self._write_code(bb.if_follow, i, latch, ifollow)
        else:
            # no follow..
            self._write("if (%s) {" % bb.type_info[2],
                        i,
                        "[%.4X]" % bb.get_last())
            self._write_code(t, i+1, latch, ifollow)
            self._write("} else {", i)
            self._write_code(e, i+1, latch, ifollow)
            self._write("}", i)

    def _write_one_way(self, bbn, i, latch, ifollow):
        """
        Write code for graph rooted at bbn.

        Arguments:
        bbn -- node in the control flow graph
        i -- current indentation level
        latch -- latching node of enclosing loop structure (or None)
        ifollow -- follow node of the enclosing if structure (or None)
        """
        self._write_bb(bbn, i)
        succs = self.cfg.out_nbrs(bbn)
        if not succs:
            return
        s = succs[0]
        if s not in self.traversed:
            self._write_code(s, i, latch, ifollow)
        else:
            self._emit_goto(s, i)

    def _write_code(self, bbn, i, latch, ifollow):
        """
        Write code for graph rooted at bbn.

        Arguments:
        bbn -- node in the control flow graph
        i -- current indentation level
        latch -- latching node of enclosing loop structure (or None)
        ifollow -- follow node of the enclosing if structure (or None)
        """
        if bbn == None:
            return
        bb = self.cfg.node_data(bbn)
        if bbn == ifollow or bbn in self.traversed:
            return
        self.traversed.append(bbn)
        if bb.loop_head == bbn:
            # bbn is a loop header
            self._write_loop(bbn, i, latch, ifollow)
        elif bb.btype == BT.two_way:
            self._write_two_way(bbn, i, latch, ifollow)
        else:
            self._write_one_way(bbn, i, latch, ifollow)
