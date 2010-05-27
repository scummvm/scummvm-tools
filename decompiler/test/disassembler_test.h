/* ScummVM Tools
* Copyright (C) 2010 The ScummVM project
*
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.

* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.

* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
*
* $URL$
* $Id$
*
*/

#include <cxxtest/TestSuite.h>

#include "disassembler/pasc.h"
#include "disassembler/subopcode.h"

class DisassemblerTestSuite : public CxxTest::TestSuite {
public:
	void testDisassembly() {
		try	{
			PasCDisassembler p;
			p.open("decompiler/test/hanoi20.pasb");
			std::vector<Instruction> insts = p.disassemble();
			TS_ASSERT(insts[0]._address == 0);
			TS_ASSERT(insts[0]._name == "PUSH");
			TS_ASSERT(insts[0]._stackChange == 0);
			TS_ASSERT(insts[0]._params[0]._type == kInt);
			TS_ASSERT(insts[0]._params[0]._uint == 0x60);
		} catch (UnknownOpcodeException &uoe) {
			printf("Exception message: %s\n",uoe.what());
			TS_ASSERT(false);
		}	catch (std::exception &ex) {
			printf("Exception message: %s\n",ex.what());
			TS_ASSERT(false);
		}
	}

	void testSubOpcodeDisassembly() {
		try {
			SubOpcodeDisassembler s;
			s.open("decompiler/test/subopcode_test.bin");
			std::vector<Instruction> insts = s.disassemble();
			TS_ASSERT(insts[0]._name == "FOO");
		} catch (...) {
			TS_ASSERT(false);
		}
	}

	void testUnknownOpcodeException() {
		try {
			SubOpcodeDisassembler s;
			s.open("decompiler/test/unknownopcode_test.bin");
			s.disassemble();
			TS_ASSERT(false);
		} catch (UnknownOpcodeException) {
			TS_ASSERT(true);
		} catch (...) {
			TS_ASSERT(false);
		}
	}
};
