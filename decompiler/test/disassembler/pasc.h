/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#ifndef DEC_TEST_DISASM_PASC_H
#define DEC_TEST_DISASM_PASC_H

#include "decompiler/simple_disassembler.h"

class PasCDisassembler : public SimpleDisassembler {
public:
	PasCDisassembler(InstVec &insts);
	void doDisassemble() throw(std::exception);
	ValuePtr readParameter(InstPtr inst, char type);
};

class PasCFakeInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {}
};

#endif
