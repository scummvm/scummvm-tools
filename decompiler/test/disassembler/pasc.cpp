/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "pasc.h"

PasCDisassembler::PasCDisassembler(InstVec &insts) : ::SimpleDisassembler(insts) {
}

void PasCDisassembler::doDisassemble() throw(std::exception) {
	START_OPCODES;
		//Basic machine operations
		OPCODE(0x00, "PUSH", PasCFakeInstruction, 0, "i");
		OPCODE(0x01, "POP",	PasCFakeInstruction, 0, "i");
		OPCODE(0x02, "CALL", PasCFakeInstruction, 0, "d");
		OPCODE(0x03, "RETURN", PasCFakeInstruction, 0, "");
		OPCODE(0x04, "HALT", PasCFakeInstruction, 0, "");

		//Jumps
		OPCODE(0x10, "JUMP", PasCFakeInstruction, 0, "a");
		OPCODE(0x11, "JEQ", PasCFakeInstruction, 0, "a");
		OPCODE(0x12, "JAEQ", PasCFakeInstruction, 0, "a");
		OPCODE(0x13, "JGT", PasCFakeInstruction, 0, "a");
		OPCODE(0x14, "JLT", PasCFakeInstruction, 0, "a");
		OPCODE(0x15, "JALT", PasCFakeInstruction, 0, "a");
		OPCODE(0x16, "JAGT", PasCFakeInstruction, 0, "a");

		//Boolean operations
		OPCODE(0x20, "OR", PasCFakeInstruction, -1, "");
		OPCODE(0x21, "AND", PasCFakeInstruction, -1, "");
		OPCODE(0x22, "XOR", PasCFakeInstruction, -1, "");
		OPCODE(0x23, "NOT", PasCFakeInstruction, -1, "");

		//Padding instructions (smaller integer -> larger integer)
		OPCODE(0x30, "SPAD", PasCFakeInstruction, 0, "B");
		OPCODE(0x31, "UPAD", PasCFakeInstruction, 0, "B");

		//32-bit operations
		OPCODE(0x80, "IADD", PasCFakeInstruction, -4, "");
		OPCODE(0x81, "ISUB", PasCFakeInstruction, -4, "");
		OPCODE(0x82, "IMULT", PasCFakeInstruction, -4, "");
		OPCODE(0x83, "IDIV", PasCFakeInstruction, -4, "");
		OPCODE(0x84, "IMOD", PasCFakeInstruction, -4, "");
		OPCODE(0x85, "ISHL", PasCFakeInstruction, -4, "");
		OPCODE(0x86, "ISHR", PasCFakeInstruction, -4, "");
		OPCODE(0x87, "ISTOREA [SB]", PasCFakeInstruction, -4, "i");
		OPCODE(0x88, "ISTOREL [SB]", PasCFakeInstruction, 0, "ii");
		OPCODE(0x89, "ILOADA [SB]", PasCFakeInstruction, 4, "i");
		OPCODE(0x8A, "ISTOREA", PasCFakeInstruction, -4, "d");
		OPCODE(0x8B, "ISTOREL", PasCFakeInstruction, 0, "di");
		OPCODE(0x8C, "ILOADA", PasCFakeInstruction, 4, "d");
		OPCODE(0x8D, "ILOADL", PasCFakeInstruction, 0, "i");
		OPCODE(0x8E, "ICMP", PasCFakeInstruction, -8, "");
		OPCODE(0x8F, "UICMP", PasCFakeInstruction, -8, "");
		OPCODE(0x90, "IDUP", PasCFakeInstruction, 4, "");
		OPCODE(0x91, "IPRINT", PasCFakeInstruction, -4, "");
		OPCODE(0x92, "UIPRINT", PasCFakeInstruction, -4, "");
		OPCODE(0x96, "ISTORE [SB]", PasCFakeInstruction, -8, "");
		OPCODE(0x97, "ISTORE", PasCFakeInstruction, -8, "");
		OPCODE(0x98, "ILOAD [SB]", PasCFakeInstruction, -8, "");
		OPCODE(0x99, "ILOAD", PasCFakeInstruction, -8, "");

		//16-bit operations
		OPCODE(0xA0, "SADD", PasCFakeInstruction, -2, "");
		OPCODE(0xA1, "SSUB", PasCFakeInstruction, -2, "");
		OPCODE(0xA2, "SMULT", PasCFakeInstruction, -2, "");
		OPCODE(0xA3, "SDIV", PasCFakeInstruction, -2, "");
		OPCODE(0xA4, "SMOD", PasCFakeInstruction, -2, "");
		OPCODE(0xA5, "SSHL", PasCFakeInstruction, -2, "");
		OPCODE(0xA6, "SSHR", PasCFakeInstruction, -2, "");
		OPCODE(0xA7, "SSTOREA [SB]", PasCFakeInstruction, -2, "i");
		OPCODE(0xA8, "SSTOREL [SB]", PasCFakeInstruction, 0, "is");
		OPCODE(0xA9, "SLOADA [SB]", PasCFakeInstruction, 2, "i");
		OPCODE(0xAA, "SSTOREA", PasCFakeInstruction, -2, "d");
		OPCODE(0xAB, "SSTOREL", PasCFakeInstruction, 0, "ds");
		OPCODE(0xAC, "SLOADA", PasCFakeInstruction, 2, "d");
		OPCODE(0xAD, "SLOADL", PasCFakeInstruction, 0, "s");
		OPCODE(0xAE, "SCMP", PasCFakeInstruction, -4, "");
		OPCODE(0xAF, "USCMP", PasCFakeInstruction, -4, "");
		OPCODE(0xB0, "SDUP", PasCFakeInstruction, 2, "");
		OPCODE(0xB1, "SPRINT", PasCFakeInstruction, -2, "");
		OPCODE(0xB2, "USPRINT", PasCFakeInstruction, -2, "");
		OPCODE(0xB6, "SSTORE [SB]", PasCFakeInstruction, -6, "");
		OPCODE(0xB7, "SSTORE", PasCFakeInstruction, -6, "");
		OPCODE(0xB8, "SLOAD [SB]", PasCFakeInstruction, -6, "");
		OPCODE(0xB9, "SLOAD", PasCFakeInstruction, -6, "");

		//8-bit operations
		OPCODE(0xC0, "BADD", PasCFakeInstruction, -1, "");
		OPCODE(0xC1, "BSUB", PasCFakeInstruction, -1, "");
		OPCODE(0xC2, "BMULT", PasCFakeInstruction, -1, "");
		OPCODE(0xC3, "BDIV", PasCFakeInstruction, -1, "");
		OPCODE(0xC4, "BMOD", PasCFakeInstruction, -1, "");
		OPCODE(0xC5, "BSHL", PasCFakeInstruction, -1, "");
		OPCODE(0xC6, "BSHR", PasCFakeInstruction, -1, "");
		OPCODE(0xC7, "BSTOREA [SB]", PasCFakeInstruction, -1, "i");
		OPCODE(0xC8, "BSTOREL [SB]", PasCFakeInstruction, 0, "iB");
		OPCODE(0xC9, "BLOADA [SB]", PasCFakeInstruction, 1, "i");
		OPCODE(0xCA, "BSTOREA", PasCFakeInstruction, -1, "d");
		OPCODE(0xCB, "BSTOREL", PasCFakeInstruction, 0, "dB");
		OPCODE(0xCC, "BLOADA", PasCFakeInstruction, 1, "d");
		OPCODE(0xCD, "BLOADL", PasCFakeInstruction, 0, "B");
		OPCODE(0xCE, "SBCMP", PasCFakeInstruction, -2, "");
		OPCODE(0xCF, "BCMP", PasCFakeInstruction, -2, "");
		OPCODE(0xD0, "BDUP", PasCFakeInstruction, 1, "");
		OPCODE(0xD1, "SBPRINT", PasCFakeInstruction, -1, "");
		OPCODE(0xD2, "BPRINT", PasCFakeInstruction, -1, "");
		OPCODE(0xD3, "CPRINT", PasCFakeInstruction, -1, "");
		OPCODE(0xD6, "BSTORE [SB]", PasCFakeInstruction, -5, "");
		OPCODE(0xD7, "BSTORE", PasCFakeInstruction, -5, "");
		OPCODE(0xD8, "BLOAD [SB]", PasCFakeInstruction, -5, "");
		OPCODE(0xD9, "BLOAD", PasCFakeInstruction, -5, "");
	END_OPCODES;
}

ValuePtr PasCDisassembler::readParameter(InstPtr inst, char type) {
	ValuePtr retval = NULL;
	switch (type) {
	case 'a':
		retval = new AddressValue(_f.readUint32LE());
		_address += 4;
		break;
	default: // Defer handling to parent implementation
		retval = SimpleDisassembler::readParameter(inst, type);
		break;
	}
	return retval;
}
