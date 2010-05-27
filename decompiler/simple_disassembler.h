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

#ifndef DEC_SIMPLE_DISASSEMBLER_H
#define DEC_SIMPLE_DISASSEMBLER_H

#include "disassembler.h"

/**
 * Simple disassembler acting as a base for instruction sets only consisting of simple instructions (opcode params...).
 */
class SimpleDisassembler : public Disassembler {
protected:
	uint32 _address; ///<Variable to maintain the current address.

	/**
	 * Read parameters and associate them with an instruction.
	 * @param inst Pointer to the instruction to associate the parameters with.
	 * @param typeString NUL-terminated string describing the type of each parameter.
	 */
	void readParams(Instruction *inst, char *typeString);
};

#define INC_ADDR _address++;
#define ADD_INST _insts.push_back(Instruction());
#define LAST_INST (_insts[_insts.size()-1])

#define START_OPCODES \
	_address = _addressBase; \
	while (_f.pos() != (int)_f.size()) { \
		uint8 opcode = _f.readByte(); \
		switch (opcode) {
#define END_OPCODES \
		default: \
			throw UnknownOpcodeException(_address, opcode);\
		} \
		INC_ADDR; \
	}

#define OPCODE_BASE(val) case val:

#define OPCODE(val, name, category, stackChange, params) \
	OPCODE_BASE(val)\
		ADD_INST; \
		LAST_INST._address = _address; \
		LAST_INST._stackChange = stackChange; \
		LAST_INST._name = std::string(name); \
		LAST_INST._type = category; \
		readParams(&LAST_INST, (char*)params); \
		break;

#define START_SUBOPCODE(val) \
	OPCODE_BASE(val) \
		opcode = _f.readByte(); \
		switch (opcode) {
#define END_SUBOPCODE \
		default: \
			throw UnknownOpcodeException(_address, opcode);\
		} \
		INC_ADDR; \
		break;

#endif
