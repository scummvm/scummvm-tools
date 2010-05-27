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

std::vector<Instruction> PasCDisassembler::disassemble() {
	START_OPCODES;
		//Basic machine operations
		OPCODE(0x00, "PUSH", kStack, 0, "i");
		OPCODE(0x01, "POP",	kStack, 0, "i");
		OPCODE(0x02, "CALL", kCall, 0, "d"); 
		OPCODE(0x03, "RETURN", kReturn, 0, "");
		OPCODE(0x04, "HALT", kSpecial, 0, "");

		//Jumps
		OPCODE(0x10, "JUMP", kJump, 0, "d");
		OPCODE(0x11, "JEQ", kCondJump, 0, "d");
		OPCODE(0x12, "JAEQ", kCondJump, 0, "d");
		OPCODE(0x13, "JGT", kCondJump, 0, "d");
		OPCODE(0x14, "JLT", kCondJump, 0, "d");
		OPCODE(0x15, "JALT", kCondJump, 0, "d");
		OPCODE(0x16, "JAGT", kCondJump, 0, "d");

		//Boolean operations
		OPCODE(0x20, "OR", kBoolean, -1, "");
		OPCODE(0x21, "AND", kBoolean, -1, "");
		OPCODE(0x22, "XOR", kBoolean, -1, "");
		OPCODE(0x23, "NOT", kBoolean, -1, "");

		//Padding instructions (smaller integer -> larger integer)
		OPCODE(0x30, "SPAD", kSpecial, 0, "B");
		OPCODE(0x31, "UPAD", kSpecial, 0, "B");

		//32-bit operations
		OPCODE(0x80, "IADD", kArithmetic, -4, "");
		OPCODE(0x81, "ISUB", kArithmetic, -4, "");
		OPCODE(0x82, "IMULT", kArithmetic, -4, "");
		OPCODE(0x83, "IDIV", kArithmetic, -4, "");
		OPCODE(0x84, "IMOD", kArithmetic, -4, "");
		OPCODE(0x85, "ISHL", kArithmetic, -4, "");
		OPCODE(0x86, "ISHR", kArithmetic, -4, "");
		OPCODE(0x87, "ISTOREA [SB]", kStore, -4, "i");
		OPCODE(0x88, "ISTOREL [SB]", kStore, 0, "ii");
		OPCODE(0x89, "ILOADA [SB]", kLoad, 4, "i");
		OPCODE(0x8A, "ISTOREA", kStore, -4, "d");
		OPCODE(0x8B, "ISTOREL", kStore, 0, "di");
		OPCODE(0x8C, "ILOADA", kLoad, 4, "d");
		OPCODE(0x8D, "ILOADL", kLoad, 0, "i");
		OPCODE(0x8E, "ICMP", kComparison, -8, "");
		OPCODE(0x8F, "UICMP", kComparison, -8, "");
		OPCODE(0x90, "IDUP", kLoad, 4, "");
		OPCODE(0x91, "IPRINT", kSpecial, -4, "");
		OPCODE(0x92, "UIPRINT", kSpecial, -4, "");
		OPCODE(0x96, "ISTORE [SB]", kStore, -8, "");
		OPCODE(0x97, "ISTORE", kStore, -8, "");
		OPCODE(0x98, "ILOAD [SB]", kLoad, -8, "");
		OPCODE(0x99, "ILOAD", kLoad, -8, "");

		//16-bit operations
		OPCODE(0xA0, "SADD", kArithmetic, -2, "");
		OPCODE(0xA1, "SSUB", kArithmetic, -2, "");
		OPCODE(0xA2, "SMULT", kArithmetic, -2, "");
		OPCODE(0xA3, "SDIV", kArithmetic, -2, "");
		OPCODE(0xA4, "SMOD", kArithmetic, -2, "");
		OPCODE(0xA5, "SSHL", kArithmetic, -2, "");
		OPCODE(0xA6, "SSHR", kArithmetic, -2, "");
		OPCODE(0xA7, "SSTOREA [SB]", kStore, -2, "i");
		OPCODE(0xA8, "SSTOREL [SB]", kStore, 0, "is");
		OPCODE(0xA9, "SLOADA [SB]", kLoad, 2, "i");
		OPCODE(0xAA, "SSTOREA", kStore, -2, "d");
		OPCODE(0xAB, "SSTOREL", kStore, 0, "ds");
		OPCODE(0xAC, "SLOADA", kLoad, 2, "d");
		OPCODE(0xAD, "SLOADL", kLoad, 0, "s");
		OPCODE(0xAE, "SCMP", kComparison, -4, "");
		OPCODE(0xAF, "USCMP", kComparison, -4, "");
		OPCODE(0xB0, "SDUP", kLoad, 2, "");
		OPCODE(0xB1, "SPRINT", kSpecial, -2, "");
		OPCODE(0xB2, "USPRINT", kSpecial, -2, "");
		OPCODE(0xB6, "SSTORE [SB]", kStore, -6, "");
		OPCODE(0xB7, "SSTORE", kStore, -6, "");
		OPCODE(0xB8, "SLOAD [SB]", kLoad, -6, "");
		OPCODE(0xB9, "SLOAD", kLoad, -6, "");		

		//8-bit operations
		OPCODE(0xC0, "BADD", kArithmetic, -1, "");
		OPCODE(0xC1, "BSUB", kArithmetic, -1, "");
		OPCODE(0xC2, "BMULT", kArithmetic, -1, "");
		OPCODE(0xC3, "BDIV", kArithmetic, -1, "");
		OPCODE(0xC4, "BMOD", kArithmetic, -1, "");
		OPCODE(0xC5, "BSHL", kArithmetic, -1, "");
		OPCODE(0xC6, "BSHR", kArithmetic, -1, "");
		OPCODE(0xC7, "BSTOREA [SB]", kStore, -1, "i");
		OPCODE(0xC8, "BSTOREL [SB]", kStore, 0, "iB");
		OPCODE(0xC9, "BLOADA [SB]", kLoad, 1, "i");
		OPCODE(0xCA, "BSTOREA", kStore, -1, "d");
		OPCODE(0xCB, "BSTOREL", kStore, 0, "dB");
		OPCODE(0xCC, "BLOADA", kLoad, 1, "d");
		OPCODE(0xCD, "BLOADL", kLoad, 0, "B");
		OPCODE(0xCE, "SBCMP", kComparison, -2, "");
		OPCODE(0xCF, "BCMP", kComparison, -2, "");
		OPCODE(0xD0, "BDUP", kLoad, 1, "");
		OPCODE(0xD1, "SBPRINT", kSpecial, -1, "");
		OPCODE(0xD2, "BPRINT", kSpecial, -1, "");
		OPCODE(0xD3, "CPRINT", kSpecial, -1, "");
		OPCODE(0xD6, "BSTORE [SB]", kStore, -5, "");
		OPCODE(0xD7, "BSTORE", kStore, -5, "");
		OPCODE(0xD8, "BLOAD [SB]", kLoad, -5, "");
		OPCODE(0xD9, "BLOAD", kLoad, -5, "");
	END_OPCODES;

	return _insts;
}
