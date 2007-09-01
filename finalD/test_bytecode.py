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

"""Unit test for bytecode.py"""

import unittest
import bytecode


class TestFetchers(unittest.TestCase):
    test_data_16 = ([0xff, 0xff], [0xa0, 0xa1], [0x00, 0x01])

    results_be_u16 = (2**16-1, 41121, 1)
    results_le_u16 = (2**16-1, 41376, 256)

    test_data_32 = ([0xff, 0xff, 0xff, 0xff],
                    [0x01, 0x02, 0x03, 0x04])

    results_be_u32 = (2**32-1, 16909060)
    results_le_u32 = (2**32-1, 67305985)

    def _run_fetch_test(self, meth, data, results):
        for bytes, expected in zip(data, results):
            self.assertEqual(meth(bytecode.ByteCode(bytes)),
                             expected)

    def test_fetch_be_u16(self):
        self._run_fetch_test(bytecode.ByteCode.fetch_be_u16,
                             self.test_data_16,
                             self.results_be_u16)

    def test_fetch_le_u16(self):
        self._run_fetch_test(bytecode.ByteCode.fetch_le_u16,
                             self.test_data_16,
                             self.results_le_u16)

    def test_fetch_be_u32(self):
        self._run_fetch_test(bytecode.ByteCode.fetch_be_u32,
                             self.test_data_32,
                             self.results_be_u32)

    def test_fetch_le_u32(self):
        self._run_fetch_test(bytecode.ByteCode.fetch_le_u32,
                             self.test_data_32,
                             self.results_le_u32)

    def test_fetch_guard(self):
        self.assertRaises(bytecode.IncompleteFetchError,
                          bytecode.ByteCode([]).fetch_u8)

        bc = bytecode.ByteCode([0, 0, 0])
        bc.fetch_be_u16()
        self.assertRaises(bytecode.IncompleteFetchError, bc.fetch_be_u16)

        bc.seek(0, 1)
        bc.fetch_le_u16()
        self.assertRaises(bytecode.IncompleteFetchError, bc.fetch_le_u16)

        bc = bytecode.ByteCode([0, 0, 0, 0, 0])
        bc.fetch_be_u32()
        self.assertRaises(bytecode.IncompleteFetchError, bc.fetch_be_u32)

        bc.seek(0, 1)
        bc.fetch_le_u32()
        self.assertRaises(bytecode.IncompleteFetchError, bc.fetch_le_u32)

class TestSeek(unittest.TestCase):

    def setUp(self):
        self.bc = bytecode.ByteCode(range(100))

    def test_relative_seek(self):
        self.bc.seek(3)
        self.assertEqual(self.bc.get_pos(), 3)
        self.bc.seek(-3)
        self.assertEqual(self.bc.get_pos(), 0)
        self.bc.seek(len(self.bc))
        self.assertEqual(self.bc.get_pos(), len(self.bc))

    def test_relative_seek_failure(self):
        self.assertRaises(bytecode.OutOfBoundsError,
                          self.bc.seek,
                          -3)
        self.assertEqual(self.bc.get_pos(), 0)
        self.assertRaises(bytecode.OutOfBoundsError,
                          self.bc.seek,
                          len(self.bc)+1)
        self.assertEqual(self.bc.get_pos(), len(self.bc))

    def test_absolute_seek(self):
        for i in range(-len(self.bc), 0):
            self.bc.seek(i, 1)
            self.assertEqual(self.bc.get_pos(), len(self.bc)+i)
        for i in range(len(self.bc)):
            self.bc.seek(i, 1)
            self.assertEqual(self.bc.get_pos(), i)

    def test_absolute_seek_sanity(self):
        for i, j in zip(range(-len(self.bc), 0),
                        range(len(self.bc))):
            self.bc.seek(i, 1)
            a = self.bc.get_pos()
            self.bc.seek(j, 1)
            b = self.bc.get_pos()
            self.assertEqual(a, b)

    def test_absolute_seek_failure(self):
        self.assertRaises(bytecode.OutOfBoundsError,
                          self.bc.seek,
                          -len(self.bc)-1)
        self.assertRaises(bytecode.OutOfBoundsError,
                          self.bc.seek,
                          len(self.bc)+1)

if __name__ == '__main__':
    unittest.main()
