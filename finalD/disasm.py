"""Disassembler base module."""

import bytecode


## Exceptions ##

class Error(Exception):
    """Base class for exceptions in this module."""
    pass


## Disassembler base class ##

class Disasm:
    """Disassembler base class"""

    def __init__(self, bc):
        """
        Initialize Disasm instance.

        Arguments:
        bc -- instance of bytecode.ByteCode class
        """
        self.decoded = []
        """Hold decoded instructions."""
        self._bc = bc
        """Hold bytecode.ByteCode instance."""

    def decode(self):
        """Decode all instructions."""
        try:
            # Append results of self.decode_next() to self.decoded until
            # an exception is raised.
            while 1:
                self.decoded.append(self._bc.decode_next())
        except bytecode.IncompleteFetchError:
            # If an bytecode.IncompleteFetchError was raised, we are done.
            return self.decoded
