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

PasCDisassembler::PasCDisassembler(std::vector<Instruction> &insts) : ::SimpleDisassembler(insts) {
}

void PasCDisassembler::doDisassemble() throw(std::exception) {
	START_OPCODES;
		//Basic machine operations
		OPCODE(0x00, "PUSH", kStackInstType, 0, "i");
		OPCODE(0x01, "POP",	kStackInstType, 0, "i");
		OPCODE(0x02, "CALL", kCallInstType, 0, "d");
		OPCODE(0x03, "RETURN", kReturnInstType, 0, "");
		OPCODE(0x04, "HALT", kSpecialCallInstType, 0, "");

		//Jumps
		OPCODE(0x10, "JUMP", kJumpInstType, 0, "d");
		OPCODE(0x11, "JEQ", kCondJumpInstType, 0, "d");
		OPCODE(0x12, "JAEQ", kCondJumpInstType, 0, "d");
		OPCODE(0x13, "JGT", kCondJumpInstType, 0, "d");
		OPCODE(0x14, "JLT", kCondJumpInstType, 0, "d");
		OPCODE(0x15, "JALT", kCondJumpInstType, 0, "d");
		OPCODE(0x16, "JAGT", kCondJumpInstType, 0, "d");

		//Boolean operations
		OPCODE(0x20, "OR", kBinaryOpInstType, -1, "");
		OPCODE(0x21, "AND", kBinaryOpInstType, -1, "");
		OPCODE(0x22, "XOR", kBinaryOpInstType, -1, "");
		OPCODE(0x23, "NOT", kUnaryOpPreInstType, -1, "");

		//Padding instructions (smaller integer -> larger integer)
		OPCODE(0x30, "SPAD", kSpecialCallInstType, 0, "B");
		OPCODE(0x31, "UPAD", kSpecialCallInstType, 0, "B");

		//32-bit operations
		OPCODE(0x80, "IADD", kBinaryOpInstType, -4, "");
		OPCODE(0x81, "ISUB", kBinaryOpInstType, -4, "");
		OPCODE(0x82, "IMULT", kBinaryOpInstType, -4, "");
		OPCODE(0x83, "IDIV", kBinaryOpInstType, -4, "");
		OPCODE(0x84, "IMOD", kBinaryOpInstType, -4, "");
		OPCODE(0x85, "ISHL", kBinaryOpInstType, -4, "");
		OPCODE(0x86, "ISHR", kBinaryOpInstType, -4, "");
		OPCODE(0x87, "ISTOREA [SB]", kStoreInstType, -4, "i");
		OPCODE(0x88, "ISTOREL [SB]", kStoreInstType, 0, "ii");
		OPCODE(0x89, "ILOADA [SB]", kLoadInstType, 4, "i");
		OPCODE(0x8A, "ISTOREA", kStoreInstType, -4, "d");
		OPCODE(0x8B, "ISTOREL", kStoreInstType, 0, "di");
		OPCODE(0x8C, "ILOADA", kLoadInstType, 4, "d");
		OPCODE(0x8D, "ILOADL", kLoadInstType, 0, "i");
		OPCODE(0x8E, "ICMP", kBinaryOpInstType, -8, "");
		OPCODE(0x8F, "UICMP", kBinaryOpInstType, -8, "");
		OPCODE(0x90, "IDUP", kLoadInstType, 4, "");
		OPCODE(0x91, "IPRINT", kSpecialCallInstType, -4, "");
		OPCODE(0x92, "UIPRINT", kSpecialCallInstType, -4, "");
		OPCODE(0x96, "ISTORE [SB]", kStoreInstType, -8, "");
		OPCODE(0x97, "ISTORE", kStoreInstType, -8, "");
		OPCODE(0x98, "ILOAD [SB]", kLoadInstType, -8, "");
		OPCODE(0x99, "ILOAD", kLoadInstType, -8, "");

		//16-bit operations
		OPCODE(0xA0, "SADD", kBinaryOpInstType, -2, "");
		OPCODE(0xA1, "SSUB", kBinaryOpInstType, -2, "");
		OPCODE(0xA2, "SMULT", kBinaryOpInstType, -2, "");
		OPCODE(0xA3, "SDIV", kBinaryOpInstType, -2, "");
		OPCODE(0xA4, "SMOD", kBinaryOpInstType, -2, "");
		OPCODE(0xA5, "SSHL", kBinaryOpInstType, -2, "");
		OPCODE(0xA6, "SSHR", kBinaryOpInstType, -2, "");
		OPCODE(0xA7, "SSTOREA [SB]", kStoreInstType, -2, "i");
		OPCODE(0xA8, "SSTOREL [SB]", kStoreInstType, 0, "is");
		OPCODE(0xA9, "SLOADA [SB]", kLoadInstType, 2, "i");
		OPCODE(0xAA, "SSTOREA", kStoreInstType, -2, "d");
		OPCODE(0xAB, "SSTOREL", kStoreInstType, 0, "ds");
		OPCODE(0xAC, "SLOADA", kLoadInstType, 2, "d");
		OPCODE(0xAD, "SLOADL", kLoadInstType, 0, "s");
		OPCODE(0xAE, "SCMP", kBinaryOpInstType, -4, "");
		OPCODE(0xAF, "USCMP", kBinaryOpInstType, -4, "");
		OPCODE(0xB0, "SDUP", kLoadInstType, 2, "");
		OPCODE(0xB1, "SPRINT", kSpecialCallInstType, -2, "");
		OPCODE(0xB2, "USPRINT", kSpecialCallInstType, -2, "");
		OPCODE(0xB6, "SSTORE [SB]", kStoreInstType, -6, "");
		OPCODE(0xB7, "SSTORE", kStoreInstType, -6, "");
		OPCODE(0xB8, "SLOAD [SB]", kLoadInstType, -6, "");
		OPCODE(0xB9, "SLOAD", kLoadInstType, -6, "");

		//8-bit operations
		OPCODE(0xC0, "BADD", kBinaryOpInstType, -1, "");
		OPCODE(0xC1, "BSUB", kBinaryOpInstType, -1, "");
		OPCODE(0xC2, "BMULT", kBinaryOpInstType, -1, "");
		OPCODE(0xC3, "BDIV", kBinaryOpInstType, -1, "");
		OPCODE(0xC4, "BMOD", kBinaryOpInstType, -1, "");
		OPCODE(0xC5, "BSHL", kBinaryOpInstType, -1, "");
		OPCODE(0xC6, "BSHR", kBinaryOpInstType, -1, "");
		OPCODE(0xC7, "BSTOREA [SB]", kStoreInstType, -1, "i");
		OPCODE(0xC8, "BSTOREL [SB]", kStoreInstType, 0, "iB");
		OPCODE(0xC9, "BLOADA [SB]", kLoadInstType, 1, "i");
		OPCODE(0xCA, "BSTOREA", kStoreInstType, -1, "d");
		OPCODE(0xCB, "BSTOREL", kStoreInstType, 0, "dB");
		OPCODE(0xCC, "BLOADA", kLoadInstType, 1, "d");
		OPCODE(0xCD, "BLOADL", kLoadInstType, 0, "B");
		OPCODE(0xCE, "SBCMP", kBinaryOpInstType, -2, "");
		OPCODE(0xCF, "BCMP", kBinaryOpInstType, -2, "");
		OPCODE(0xD0, "BDUP", kLoadInstType, 1, "");
		OPCODE(0xD1, "SBPRINT", kSpecialCallInstType, -1, "");
		OPCODE(0xD2, "BPRINT", kSpecialCallInstType, -1, "");
		OPCODE(0xD3, "CPRINT", kSpecialCallInstType, -1, "");
		OPCODE(0xD6, "BSTORE [SB]", kStoreInstType, -5, "");
		OPCODE(0xD7, "BSTORE", kStoreInstType, -5, "");
		OPCODE(0xD8, "BLOAD [SB]", kLoadInstType, -5, "");
		OPCODE(0xD9, "BLOAD", kLoadInstType, -5, "");
	END_OPCODES;
}
