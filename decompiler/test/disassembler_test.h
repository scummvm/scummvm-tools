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
#include "decompiler/kyra/engine.h"

class DisassemblerTestSuite : public CxxTest::TestSuite {
public:
	void testDisassembly() {
		try {
			InstVec insts;
			PasCDisassembler p(insts);
			p.open("decompiler/test/hanoi20.pasb");
			p.disassemble();
			TS_ASSERT(insts[0]->_address == 0);
			TS_ASSERT(insts[0]->_opcode == 0x00);
			TS_ASSERT(insts[0]->_name == "PUSH");
			TS_ASSERT(insts[0]->_stackChange == 0);
			TS_ASSERT(insts[0]->_params[0]->isInteger());
			TS_ASSERT(insts[0]->_params[0]->getSigned() == 0x60);
		} catch (UnknownOpcodeException &uoe) {
			printf("Exception message: %s\n", uoe.what());
			TS_ASSERT(false);
		} catch (std::exception &ex) {
			printf("Exception message: %s\n", ex.what());
			TS_ASSERT(false);
		}
	}

	void testSubOpcodeDisassembly() {
		InstVec insts;
		SubOpcodeDisassembler s(insts);
		s.open("decompiler/test/subopcode_test.bin");
		s.disassemble();
		TS_ASSERT(insts[0]->_name == "FOO");
		TS_ASSERT(insts[0]->_opcode == 0xFFFF);
	}

	void testUnknownOpcodeException() {
		try {
			InstVec insts;
			SubOpcodeDisassembler s(insts);
			s.open("decompiler/test/unknownopcode_test.bin");
			s.disassemble();
			TS_ASSERT(false);
		} catch (UnknownOpcodeException) {
			TS_ASSERT(true);
		}
	}

	// This test requires script-15.dmp from Sam & Max: Hit The Road.
	// 1ab08298c9c8fb4c77953756989c7449 *script-15.dmp
	void testScummv6DisassemblerScript15() {
		InstVec insts;
		Scumm::v6::Scummv6Disassembler s(insts);
		s.open("decompiler/test/script-15.dmp");
		s.disassemble();
		TS_ASSERT(insts.size() == 11);
		TS_ASSERT(insts[0]->_address == 0);
		TS_ASSERT(insts[0]->_opcode == 0x03);
		TS_ASSERT(insts[0]->_name == "pushWordVar");
		TS_ASSERT(insts[0]->_params[0]->getUnsigned() == 16384);
		TS_ASSERT(insts[1]->_address == 3);
		TS_ASSERT(insts[1]->_opcode == 0x43);
		TS_ASSERT(insts[1]->_name == "writeWordVar");
		TS_ASSERT(insts[1]->_params[0]->getUnsigned() == 197);
		TS_ASSERT(insts[2]->_address == 6);
		TS_ASSERT(insts[2]->_opcode == 0x01);
		TS_ASSERT(insts[2]->_name == "pushWord");
		TS_ASSERT(insts[2]->_params[0]->getSigned() == 0);
		TS_ASSERT(insts[3]->_address == 9);
		TS_ASSERT(insts[3]->_opcode == 0x01);
		TS_ASSERT(insts[3]->_name == "pushWord");
		TS_ASSERT(insts[3]->_params[0]->getSigned() == 11);
		TS_ASSERT(insts[4]->_address == 12);
		TS_ASSERT(insts[4]->_opcode == 0x01);
		TS_ASSERT(insts[4]->_name == "pushWord");
		TS_ASSERT(insts[4]->_params[0]->getSigned() == 0);
		TS_ASSERT(insts[5]->_address == 15);
		TS_ASSERT(insts[5]->_opcode == 0x5E);
		TS_ASSERT(insts[5]->_name == "startScript");
		TS_ASSERT(insts[6]->_address == 16);
		TS_ASSERT(insts[6]->_opcode == 0x01);
		TS_ASSERT(insts[6]->_name == "pushWord");
		TS_ASSERT(insts[6]->_params[0]->getSigned() == 0);
		TS_ASSERT(insts[7]->_address == 19);
		TS_ASSERT(insts[7]->_opcode == 0x01);
		TS_ASSERT(insts[7]->_name == "pushWord");
		TS_ASSERT(insts[7]->_params[0]->getSigned() == 14);
		TS_ASSERT(insts[8]->_address == 22);
		TS_ASSERT(insts[8]->_opcode == 0x01);
		TS_ASSERT(insts[8]->_name == "pushWord");
		TS_ASSERT(insts[8]->_params[0]->getSigned() == 0);
		TS_ASSERT(insts[9]->_address == 25);
		TS_ASSERT(insts[9]->_opcode == 0x5E);
		TS_ASSERT(insts[9]->_name == "startScript");
		TS_ASSERT(insts[10]->_address == 26);
		TS_ASSERT(insts[10]->_opcode == 0x66);
		TS_ASSERT(insts[10]->_name == "stopObjectCodeB");
	}

	// This test requires script-31.dmp from Sam & Max: Hit The Road.
	// f75f7ce110f378735d449f8eeb4a68e5 *script-31.dmp
	void testScummv6DisassemblerScript31() {
		InstVec insts;
		Scumm::v6::Scummv6Disassembler s(insts);
		s.open("decompiler/test/script-31.dmp");
		s.disassemble();
		TS_ASSERT(insts.size() == 5);
		TS_ASSERT(insts[0]->_address == 0);
		TS_ASSERT(insts[0]->_opcode == 0x01);
		TS_ASSERT(insts[0]->_name == "pushWord");
		TS_ASSERT(insts[0]->_params[0]->getSigned() == 0);
		TS_ASSERT(insts[1]->_address == 3);
		TS_ASSERT(insts[1]->_opcode == 0x43);
		TS_ASSERT(insts[1]->_name == "writeWordVar");
		TS_ASSERT(insts[1]->_params[0]->getUnsigned() == 180);
		TS_ASSERT(insts[2]->_address == 6);
		TS_ASSERT(insts[2]->_opcode == 0x01);
		TS_ASSERT(insts[2]->_name == "pushWord");
		TS_ASSERT(insts[2]->_params[0]->getSigned() == 0);
		TS_ASSERT(insts[3]->_address == 9);
		TS_ASSERT(insts[3]->_opcode == 0x43);
		TS_ASSERT(insts[3]->_name == "writeWordVar");
		TS_ASSERT(insts[3]->_params[0]->getUnsigned() == 181);
		TS_ASSERT(insts[4]->_address == 12);
		TS_ASSERT(insts[4]->_opcode == 0x66);
		TS_ASSERT(insts[4]->_name == "stopObjectCodeB");
	}

	// This test requires script-33.dmp from Sam & Max: Hit The Road.
	// 9f09418bf34abbdec0ec54f388d8dca4 *script-33.dmp
	void testScummv6DisassemblerScript33() {
		InstVec insts;
		Scumm::v6::Scummv6Disassembler s(insts);
		s.open("decompiler/test/script-33.dmp");
		s.disassemble();
		TS_ASSERT(insts.size() == 10);
		TS_ASSERT(insts[0]->_address == 0);
		TS_ASSERT(insts[0]->_opcode == 0x01);
		TS_ASSERT(insts[0]->_name == "pushWord");
		TS_ASSERT(insts[0]->_params[0]->getSigned() == 0);
		TS_ASSERT(insts[1]->_address == 3);
		TS_ASSERT(insts[1]->_opcode == 0x43);
		TS_ASSERT(insts[1]->_name == "writeWordVar");
		TS_ASSERT(insts[1]->_params[0]->getUnsigned() == 71);
		TS_ASSERT(insts[2]->_address == 6);
		TS_ASSERT(insts[2]->_opcode == 0x03);
		TS_ASSERT(insts[2]->_name == "pushWordVar");
		TS_ASSERT(insts[2]->_params[0]->getUnsigned() == 177);
		TS_ASSERT(insts[3]->_address == 9);
		TS_ASSERT(insts[3]->_opcode == 0x43);
		TS_ASSERT(insts[3]->_name == "writeWordVar");
		TS_ASSERT(insts[3]->_params[0]->getUnsigned() == 173);
		TS_ASSERT(insts[4]->_address == 12);
		TS_ASSERT(insts[4]->_opcode == 0x01);
		TS_ASSERT(insts[4]->_name == "pushWord");
		TS_ASSERT(insts[4]->_params[0]->getSigned() == 874);
		TS_ASSERT(insts[5]->_address == 15);
		TS_ASSERT(insts[5]->_opcode == 0x43);
		TS_ASSERT(insts[5]->_name == "writeWordVar");
		TS_ASSERT(insts[5]->_params[0]->getUnsigned() == 177);
		TS_ASSERT(insts[6]->_address == 18);
		TS_ASSERT(insts[6]->_opcode == 0x03);
		TS_ASSERT(insts[6]->_name == "pushWordVar");
		TS_ASSERT(insts[6]->_params[0]->getUnsigned() == 177);
		TS_ASSERT(insts[7]->_address == 21);
		TS_ASSERT(insts[7]->_opcode == 0x01);
		TS_ASSERT(insts[7]->_name == "pushWord");
		TS_ASSERT(insts[7]->_params[0]->getSigned() == 93);
		TS_ASSERT(insts[8]->_address == 24);
		TS_ASSERT(insts[8]->_opcode == 0x6B99);
		TS_ASSERT(insts[8]->_name == "cursorCommand.setCursorImg");
		TS_ASSERT(insts[9]->_address == 26);
		TS_ASSERT(insts[9]->_opcode == 0x66);
		TS_ASSERT(insts[9]->_name == "stopObjectCodeB");
	}

	// This test requires room-9-202.dmp from Sam & Max: Hit The Road.
	// f010dc659264674a2b6da298acd0b88b *room-9-202.dmp
	void testScummv6StackChangeFixRoom9202() {
		InstVec insts;
		Scumm::v6::Scummv6Disassembler s(insts);
		s.open("decompiler/test/room-9-202.dmp");
		s.disassemble();
		InstIterator it = insts.end();
		it -= 8;
		TS_ASSERT((*it)->_stackChange == -3);
	}

	// This test requires script-30.dmp from Sam & Max: Hit The Road.
	// 6e48faca13e1f6df9341567608962744 *script-30.dmp
	void testScummv6StackChangeFixScript30() {
		InstVec insts;
		Scumm::v6::Scummv6Disassembler s(insts);
		s.open("decompiler/test/script-30.dmp");
		s.disassemble();
		InstIterator it = insts.end();
		it -= 3;
		TS_ASSERT((*it)->_stackChange == -6);
	}

	// This test requires _START04.EMC from the CD demo of
	// Legend of Kyrandia: Hand of Fate, found in MISC_EMC.PAK.
	// Extract using extract_kyra from the scummvm-tools-cli bundle.
	// ba2821ac6da96394ce0af75a3cbe48eb *_START04.EMC
	void testKyra2Start04() {
		InstVec insts;
		Kyra::Kyra2Engine engine;
		Disassembler* s = engine.getDisassembler(insts);
		s->open("decompiler/test/_START04.EMC");
		s->disassemble();

		TS_ASSERT(insts.size() == 481);

		//These scripts are far too big to check all instructions, so we just check a few different ones
		TS_ASSERT(insts[16]->_address == 0x20);
		TS_ASSERT(insts[16]->_opcode == 15);
		TS_ASSERT(insts[16]->_name == "ifNotJmp");
		TS_ASSERT(insts[16]->_stackChange == -1);
		TS_ASSERT(insts[38]->_address == 0x54);
		TS_ASSERT(insts[38]->_opcode == 14);
		TS_ASSERT(insts[38]->_name == "o1_setHandItem");
		TS_ASSERT(insts[38]->_stackChange == 0);

		delete s;
	}
};
