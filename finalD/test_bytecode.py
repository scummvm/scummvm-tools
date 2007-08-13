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
