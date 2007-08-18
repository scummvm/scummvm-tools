"""Intermediary format of bytecode instructions."""

class Instr:
    """
    Instruction in intermediate format.

    After disassembly, all instructions within a bytecode file will
    have been translated into instances of this class.
    """

    def __init__(self, op, args):
        self.op = op
        self.args = args

    def __str__(self):
        buf = self.op + '('
        first = 1
        for arg in self.args:
            if not first:
                buf += ", "
            first = 0
            buf += "%s" % arg
        buf += ')'
        return buf

class AssgnInstr(Instr):
    """Assignment Instruction in intermediate format."""

    def __init__(self, op,  dst, val):
        Instr.__init__(self, op, [dst, val])

    def __str__(self):
        dst, val = self.args
        return "%s %s %s" % (str(dst), str(self.op), str(val))

class Jump(Instr):
    """Unconditional jump instruction in intermediate format."""

    def __init__(self, to):
        Instr.__init__(self, "goto", [to,])

    def get_to(self):
        return self.args[0]

    def set_to(self, to):
        self.args[0] = to

    def info(self):
        return self.get_to()

    def __str__(self):
        return "goto %.4X" % self.args[0]

class CondJump(Jump):
    """Conditional jump instruction in intermediate format."""

    def __init__(self, to, condition):
        Jump.__init__(self, to)
        self.condition = condition

    def info(self):
        return (True, self.get_to(), self.condition)

    def __str__(self):
        return "when (%s) goto %.4X" % (self.condition, self.args[0])

class NegCondJump(CondJump):
    """Negative conditional jump instruction in intermediate format."""

    def info(self):
        return (False, self.get_to(), self.condition)

    def __str__(self):
        return "unless (%s) goto %.4X" % (self.condition, self.args[0])

def is_jump(instr):
    """Return True if instr is a Jump."""
    return instr.__class__ == Jump \
        or instr.__class__ == CondJump \
        or instr.__class__ == NegCondJump

def is_cond_jump(instr):
    """Return True if instr is a conditional Jump."""
    return instr.__class__ == CondJump \
        or instr.__class__ == NegCondJump
