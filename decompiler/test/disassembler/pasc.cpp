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

#include "pasc.h"

PasCDisassembler::PasCDisassembler(InstVec &insts) : ::SimpleDisassembler(insts) {
	_instFactory.addEntry<PasCFakeInstruction>(kCallInst);
	_instFactory.addEntry<PasCFakeInstruction>(kCondJumpInst);
	_instFactory.addEntry<PasCFakeInstruction>(kJumpInst);
	_instFactory.addEntry<PasCFakeInstruction>(kLoadInst);
	_instFactory.addEntry<PasCFakeInstruction>(kStackInst);
	_instFactory.addEntry<PasCFakeInstruction>(kStoreInst);
}

void PasCDisassembler::doDisassemble() throw(std::exception) {
	START_OPCODES;
		//Basic machine operations
		OPCODE(0x00, "PUSH", kStackInst, 0, "i");
		OPCODE(0x01, "POP",	kStackInst, 0, "i");
		OPCODE(0x02, "CALL", kCallInst, 0, "d");
		OPCODE(0x03, "RETURN", kReturnInst, 0, "");
		OPCODE(0x04, "HALT", kKernelCallInst, 0, "");

		//Jumps
		OPCODE(0x10, "JUMP", kJumpInst, 0, "a");
		OPCODE(0x11, "JEQ", kCondJumpInst, 0, "a");
		OPCODE(0x12, "JAEQ", kCondJumpInst, 0, "a");
		OPCODE(0x13, "JGT", kCondJumpInst, 0, "a");
		OPCODE(0x14, "JLT", kCondJumpInst, 0, "a");
		OPCODE(0x15, "JALT", kCondJumpInst, 0, "a");
		OPCODE(0x16, "JAGT", kCondJumpInst, 0, "a");

		//Boolean operations
		OPCODE(0x20, "OR", kBinaryOpInst, -1, "");
		OPCODE(0x21, "AND", kBinaryOpInst, -1, "");
		OPCODE(0x22, "XOR", kBinaryOpInst, -1, "");
		OPCODE(0x23, "NOT", kUnaryOpPreInst, -1, "");

		//Padding instructions (smaller integer -> larger integer)
		OPCODE(0x30, "SPAD", kKernelCallInst, 0, "B");
		OPCODE(0x31, "UPAD", kKernelCallInst, 0, "B");

		//32-bit operations
		OPCODE(0x80, "IADD", kBinaryOpInst, -4, "");
		OPCODE(0x81, "ISUB", kBinaryOpInst, -4, "");
		OPCODE(0x82, "IMULT", kBinaryOpInst, -4, "");
		OPCODE(0x83, "IDIV", kBinaryOpInst, -4, "");
		OPCODE(0x84, "IMOD", kBinaryOpInst, -4, "");
		OPCODE(0x85, "ISHL", kBinaryOpInst, -4, "");
		OPCODE(0x86, "ISHR", kBinaryOpInst, -4, "");
		OPCODE(0x87, "ISTOREA [SB]", kStoreInst, -4, "i");
		OPCODE(0x88, "ISTOREL [SB]", kStoreInst, 0, "ii");
		OPCODE(0x89, "ILOADA [SB]", kLoadInst, 4, "i");
		OPCODE(0x8A, "ISTOREA", kStoreInst, -4, "d");
		OPCODE(0x8B, "ISTOREL", kStoreInst, 0, "di");
		OPCODE(0x8C, "ILOADA", kLoadInst, 4, "d");
		OPCODE(0x8D, "ILOADL", kLoadInst, 0, "i");
		OPCODE(0x8E, "ICMP", kBinaryOpInst, -8, "");
		OPCODE(0x8F, "UICMP", kBinaryOpInst, -8, "");
		OPCODE(0x90, "IDUP", kLoadInst, 4, "");
		OPCODE(0x91, "IPRINT", kKernelCallInst, -4, "");
		OPCODE(0x92, "UIPRINT", kKernelCallInst, -4, "");
		OPCODE(0x96, "ISTORE [SB]", kStoreInst, -8, "");
		OPCODE(0x97, "ISTORE", kStoreInst, -8, "");
		OPCODE(0x98, "ILOAD [SB]", kLoadInst, -8, "");
		OPCODE(0x99, "ILOAD", kLoadInst, -8, "");

		//16-bit operations
		OPCODE(0xA0, "SADD", kBinaryOpInst, -2, "");
		OPCODE(0xA1, "SSUB", kBinaryOpInst, -2, "");
		OPCODE(0xA2, "SMULT", kBinaryOpInst, -2, "");
		OPCODE(0xA3, "SDIV", kBinaryOpInst, -2, "");
		OPCODE(0xA4, "SMOD", kBinaryOpInst, -2, "");
		OPCODE(0xA5, "SSHL", kBinaryOpInst, -2, "");
		OPCODE(0xA6, "SSHR", kBinaryOpInst, -2, "");
		OPCODE(0xA7, "SSTOREA [SB]", kStoreInst, -2, "i");
		OPCODE(0xA8, "SSTOREL [SB]", kStoreInst, 0, "is");
		OPCODE(0xA9, "SLOADA [SB]", kLoadInst, 2, "i");
		OPCODE(0xAA, "SSTOREA", kStoreInst, -2, "d");
		OPCODE(0xAB, "SSTOREL", kStoreInst, 0, "ds");
		OPCODE(0xAC, "SLOADA", kLoadInst, 2, "d");
		OPCODE(0xAD, "SLOADL", kLoadInst, 0, "s");
		OPCODE(0xAE, "SCMP", kBinaryOpInst, -4, "");
		OPCODE(0xAF, "USCMP", kBinaryOpInst, -4, "");
		OPCODE(0xB0, "SDUP", kLoadInst, 2, "");
		OPCODE(0xB1, "SPRINT", kKernelCallInst, -2, "");
		OPCODE(0xB2, "USPRINT", kKernelCallInst, -2, "");
		OPCODE(0xB6, "SSTORE [SB]", kStoreInst, -6, "");
		OPCODE(0xB7, "SSTORE", kStoreInst, -6, "");
		OPCODE(0xB8, "SLOAD [SB]", kLoadInst, -6, "");
		OPCODE(0xB9, "SLOAD", kLoadInst, -6, "");

		//8-bit operations
		OPCODE(0xC0, "BADD", kBinaryOpInst, -1, "");
		OPCODE(0xC1, "BSUB", kBinaryOpInst, -1, "");
		OPCODE(0xC2, "BMULT", kBinaryOpInst, -1, "");
		OPCODE(0xC3, "BDIV", kBinaryOpInst, -1, "");
		OPCODE(0xC4, "BMOD", kBinaryOpInst, -1, "");
		OPCODE(0xC5, "BSHL", kBinaryOpInst, -1, "");
		OPCODE(0xC6, "BSHR", kBinaryOpInst, -1, "");
		OPCODE(0xC7, "BSTOREA [SB]", kStoreInst, -1, "i");
		OPCODE(0xC8, "BSTOREL [SB]", kStoreInst, 0, "iB");
		OPCODE(0xC9, "BLOADA [SB]", kLoadInst, 1, "i");
		OPCODE(0xCA, "BSTOREA", kStoreInst, -1, "d");
		OPCODE(0xCB, "BSTOREL", kStoreInst, 0, "dB");
		OPCODE(0xCC, "BLOADA", kLoadInst, 1, "d");
		OPCODE(0xCD, "BLOADL", kLoadInst, 0, "B");
		OPCODE(0xCE, "SBCMP", kBinaryOpInst, -2, "");
		OPCODE(0xCF, "BCMP", kBinaryOpInst, -2, "");
		OPCODE(0xD0, "BDUP", kLoadInst, 1, "");
		OPCODE(0xD1, "SBPRINT", kKernelCallInst, -1, "");
		OPCODE(0xD2, "BPRINT", kKernelCallInst, -1, "");
		OPCODE(0xD3, "CPRINT", kKernelCallInst, -1, "");
		OPCODE(0xD6, "BSTORE [SB]", kStoreInst, -5, "");
		OPCODE(0xD7, "BSTORE", kStoreInst, -5, "");
		OPCODE(0xD8, "BLOAD [SB]", kLoadInst, -5, "");
		OPCODE(0xD9, "BLOAD", kLoadInst, -5, "");
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
