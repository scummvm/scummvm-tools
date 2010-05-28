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
#include "decompiler/scummv6/disassembler.h"

class DisassemblerTestSuite : public CxxTest::TestSuite {
public:
	void testDisassembly() {
		try {
			PasCDisassembler p;
			p.open("decompiler/test/hanoi20.pasb");
			std::vector<Instruction> insts = p.disassemble();
			TS_ASSERT(insts[0]._address == 0);
			TS_ASSERT(insts[0]._name == "PUSH");
			TS_ASSERT(insts[0]._stackChange == 0);
			TS_ASSERT(insts[0]._params[0]._type == kInt);
			TS_ASSERT(insts[0]._params[0]._uint == 0x60);
		} catch (UnknownOpcodeException &uoe) {
			printf("Exception message: %s\n", uoe.what());
			TS_ASSERT(false);
		} catch (std::exception &ex) {
			printf("Exception message: %s\n", ex.what());
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

	//This test requires global script 15 from Sam & Max: Hit The Road.
	//1ab08298c9c8fb4c77953756989c7449 *script-15.dmp
	void testScummv6DisassemblerScript15() {
		try {
			ScummV6Disassembler s;
			s.open("decompiler/test/script-15.dmp");
			std::vector<Instruction> insts = s.disassemble();
			TS_ASSERT(insts.size() == 11);
			TS_ASSERT(insts[0]._address == 0 && insts[0]._name == "pushWordVar" && insts[0]._params[0]._ushort == 16384);
			TS_ASSERT(insts[1]._address == 3 && insts[1]._name == "writeWordVar" && insts[1]._params[0]._ushort == 197);
			TS_ASSERT(insts[2]._address == 6 && insts[2]._name == "pushWord" && insts[2]._params[0]._ushort == 0);
			TS_ASSERT(insts[3]._address == 9 && insts[3]._name == "pushWord" && insts[3]._params[0]._ushort == 11);
			TS_ASSERT(insts[4]._address == 12 && insts[4]._name == "pushWord" && insts[4]._params[0]._ushort == 0);
			TS_ASSERT(insts[5]._address == 15 && insts[5]._name == "startScript");
			TS_ASSERT(insts[6]._address == 16 && insts[6]._name == "pushWord" && insts[6]._params[0]._ushort == 0);
			TS_ASSERT(insts[7]._address == 19 && insts[7]._name == "pushWord" && insts[7]._params[0]._ushort == 14);
			TS_ASSERT(insts[8]._address == 22 && insts[8]._name == "pushWord" && insts[8]._params[0]._ushort == 0);
			TS_ASSERT(insts[9]._address == 25 && insts[9]._name == "startScript");
			TS_ASSERT(insts[10]._address == 26 && insts[10]._name == "stopObjectCodeB");
		} catch (...) {
			TS_ASSERT(false);
		}
	}

	//This test requires global script 31 from Sam & Max: Hit The Road.
	//f75f7ce110f378735d449f8eeb4a68e5 *script-31.dmp
	void testScummv6DisassemblerScript31() {
		try {
			ScummV6Disassembler s;
			s.open("decompiler/test/script-31.dmp");
			std::vector<Instruction> insts = s.disassemble();
			TS_ASSERT(insts.size() == 5);
			TS_ASSERT(insts[0]._address == 0 && insts[0]._name == "pushWord" && insts[0]._params[0]._ushort == 0);
			TS_ASSERT(insts[1]._address == 3 && insts[1]._name == "writeWordVar" && insts[1]._params[0]._ushort == 180);
			TS_ASSERT(insts[2]._address == 6 && insts[2]._name == "pushWord" && insts[2]._params[0]._ushort == 0);
			TS_ASSERT(insts[3]._address == 9 && insts[3]._name == "writeWordVar" && insts[3]._params[0]._ushort == 181);
			TS_ASSERT(insts[4]._address == 12 && insts[4]._name == "stopObjectCodeB");
		} catch (...) {
			TS_ASSERT(false);
		}
	}

	//This test requires global script 33 from Sam & Max: Hit The Road.
	//9f09418bf34abbdec0ec54f388d8dca4 *script-33.dmp
	void testScummv6DisassemblerScript33() {
		try {
			ScummV6Disassembler s;
			s.open("decompiler/test/script-33.dmp");
			std::vector<Instruction> insts = s.disassemble();
			TS_ASSERT(insts.size() == 10);
			TS_ASSERT(insts[0]._address == 0 && insts[0]._name == "pushWord" && insts[0]._params[0]._ushort == 0);
			TS_ASSERT(insts[1]._address == 3 && insts[1]._name == "writeWordVar" && insts[1]._params[0]._ushort == 71);
			TS_ASSERT(insts[2]._address == 6 && insts[2]._name == "pushWordVar" && insts[2]._params[0]._ushort == 177);
			TS_ASSERT(insts[3]._address == 9 && insts[3]._name == "writeWordVar" && insts[3]._params[0]._ushort == 173);
			TS_ASSERT(insts[4]._address == 12 && insts[4]._name == "pushWord" && insts[4]._params[0]._ushort == 874);
			TS_ASSERT(insts[5]._address == 15 && insts[5]._name == "writeWordVar" && insts[5]._params[0]._ushort == 177);
			TS_ASSERT(insts[6]._address == 18 && insts[6]._name == "pushWordVar" && insts[6]._params[0]._ushort == 177);
			TS_ASSERT(insts[7]._address == 21 && insts[7]._name == "pushWord" && insts[7]._params[0]._ushort == 93);
			TS_ASSERT(insts[8]._address == 24 && insts[8]._name == "cursorCmd_Image");
			TS_ASSERT(insts[9]._address == 26 && insts[9]._name == "stopObjectCodeB");
		} catch (...) {
			TS_ASSERT(false);
		}
	}
};
