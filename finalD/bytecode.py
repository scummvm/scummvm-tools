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

"""Classes and functions for reading and handling of bytecode."""

import array


class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class OutOfBoundsError(Error):
    """Error raised when bytecode is reached."""
    pass

class IncompleteFetchError(Error):
    """Error raised when a fetch is incomplete."""
    pass

class OpCodeNotFoundError(Error):
    """Error raised when an opcode is not implemented."""


class ByteCode:
    """Class for reading bytecode."""

    def __init__(self, init, def_opcode_fetch=0):
        """
        Initialize ByteCode object from init.

        Arguments:
        init -- a list or a string
        def_opcode_fetch -- fetch function

        >>> def_opcode_fetch = bytecode.ByteCode.fetch_u8
        >>> def_opcode_fetch(bytecode.ByteCode([1]))
        1
        """
        self._data = array.array("B", init)
        """Store bytecode in array of unsigned bytes."""
        self._pos = 0
        """Store initial bytecode position."""
        self.optable = {}
        """Map from opcode to opcode handler."""
        self._def_opcode_fetch = \
            def_opcode_fetch or ByteCode.fetch_u8
        """Hold default opcode fetch function."""

        self.populate()

    def register_opcode(self, opcode, handler):
        """Register handler for opcode."""
        self.optable[opcode] = handler

    def register_opcodes(self, opcodes, handler):
        """Register handler for opcodes."""
        for opcode in opcodes:
            self.register_opcode(opcode, handler)

    def populate(self):
        """Populate self.optable"""
        pass

    def decode_next(self, alt_opcode_fetch=0):
        """
        Decode next instruction.

        Arguments:
        alt_opcode_fetch -- opcode fetch function (default 0)

        alt_opcode_fetch, if not zero is used instead of
        self._def_opcode_fetch. (see self.__init__())
        """
        opcode_fetch = alt_opcode_fetch or self._def_opcode_fetch
        opc = opcode_fetch(self)
        if self.optable.has_key(opc):
            return self.optable[opc](opc)
        raise OpCodeNotFoundError, "opcode %d is not handled" % opc

    def __len__(self):
        """Return length of _array."""
        return len(self._data)

    def _fetch_guard(self, n, emsg=''):
        """
        Check if fetching of n bytes is possible.

        Raise IncompleteFetchError exception with emsg if not.
        """
        if self._pos + n > len(self._data):
            raise IncompleteFetchError, \
                ("incomplete fetch of %d bytes" % n) + \
                (emsg and " (" + emsg + ")")

    def seek(self, offset, whence=0):
        """
        Set self._pos according to offset and whence.

        Arguments:
        offset -- offset to seek by
        whence -- specifies the type of offset (default 0)

        whence can be either 0 (self._pos relative seek)
        or 1 (absolute seek).  A negative offset seeks
        backward from self._pos or the end of self._data
        respectively.
        """
        length = len(self._data)
        # self._pos relative seek
        if whence == 0:
            self._pos += offset
            # unless 0 <= self._pos <= length holds,
            # raise exception and clamp self._pos
            if self._pos < 0:
                self._pos = 0
                raise OutOfBoundsError, \
                    "negative index via relative seek by %d" % offset
            if self._pos > length:
                self._pos = length
                raise OutOfBoundsError, \
                    "beyond fence via relative seek by %d" % offset
        # absolute seek
        elif whence == 1:
            if not -length <= offset <= length:
                raise OutOfBoundsError, "absolute seek to %d" % offset
            if offset < 0: offset = length + offset
            self._pos = offset

    ## Basic Data Fetchers
    #
    # TODO: maybe generalize LE/BE reading

    def fetch_u8(self, peek=False):
        """Return next byte as unsigned character."""
        self._fetch_guard(1, 'fetch_u8')
        ret = self._data[self._pos]
        if not peek: self.seek(1)
        return ret

    def fetch_be_u16(self, peek=False):
        """Return next unsigned 16-bit word in BE."""
        self._fetch_guard(2, 'fetch_be_u16')
        ret = (self._data[self._pos] << 8) | self._data[self._pos+1]
        if not peek: self.seek(2)
        return ret

    def fetch_le_u16(self, peek=False):
        """Return next unsigned 16-bit word in LE."""
        self._fetch_guard(2, 'fetch_le_u16')
        ret = self._data[self._pos] | (self._data[self._pos+1] << 8)
        if not peek: self.seek(2)
        return ret

    def fetch_be_u32(self, peek=False):
        """Return next unsigned 32-bit word in BE."""
        self._fetch_guard(4, 'fetch_be_u32')
        ret = (self._data[self._pos] << 24) | \
            (self._data[self._pos+1] << 16) | \
            (self._data[self._pos+2] <<  8) | \
            self._data[self._pos+3]
        if not peek: self.seek(4)
        return ret

    def fetch_le_u32(self, peek=False):
        """Return next unsigned 32-bit word in LE."""
        self._fetch_guard(4, 'fetch_le_u32')
        ret = self._data[self._pos] | \
            (self._data[self._pos+1] << 8) | \
            (self._data[self._pos+2] << 16) | \
            (self._data[self._pos+3] << 24)
        if not peek: self.seek(4)
        return ret

    ## Getters

    def get_pos(self):
        """Return self._pos"""
        return self._pos

    def get_data(self):
        """Return self._data"""
        return self._data
