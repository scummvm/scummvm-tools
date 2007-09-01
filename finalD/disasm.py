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
        self.decoded = {}
        """Hold decoded instructions indexed by script offset."""
        self._bc = bc
        """Hold bytecode.ByteCode instance."""

    def decode(self):
        """Decode all instructions."""
        try:
            # Append results of self.decode_next() to self.decoded until
            # an exception is raised.
            while 1:
                pos = self._bc.get_pos()
                self.decoded[pos] = self._bc.decode_next()
        except bytecode.IncompleteFetchError:
            # If an bytecode.IncompleteFetchError was raised, we are done.
            return self.decoded
