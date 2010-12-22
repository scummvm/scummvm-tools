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
	uint32 _address; ///< Variable to maintain the current address.

	/**
	 * Read parameters and associate them with an instruction.
	 *
	 * @param inst       The instruction to associate the parameters with.
	 * @param typeString NUL-terminated string describing the type of each parameter.
	 */
	void readParams(InstPtr inst, const char *typeString);

	/**
	 * Reads data for a single parameter.
	 *
	 * @param inst The instruction the parameter will belong to. Used for reference in parameter reading.
	 * @param type Character describing the type of the parameter.
	 * @return The read data as a ValuePtr.
	 */
	virtual ValuePtr readParameter(InstPtr inst, char type);

public:
	/**
	 * Constructor for SimpleDisassembler.
	 *
	 * @param insts Reference to the vector in which disassembled instructions should be placed.
	 */
	SimpleDisassembler(InstVec &insts);
};

#define INC_ADDR _address++;
#define ADD_INST(category) _insts.push_back(new category());
#define LAST_INST (_insts.back())

#define START_OPCODES \
	_address = _addressBase; \
	while (_f.pos() != (int)_f.size()) { \
		uint32 full_opcode = 0; \
		uint8 opcode = _f.readByte(); \
		std::string opcodePrefix; \
		switch (opcode) {
#define END_OPCODES \
		default: \
			throw UnknownOpcodeException(_address, opcode);\
		} \
		INC_ADDR; \
	}

#define OPCODE_BASE(val) \
	case val: \
		full_opcode = (full_opcode << 8) + val;

#define OPCODE_END break;

#define OPCODE_BODY(name, category, stackChange, params, codeGenData) \
		ADD_INST(category); \
		LAST_INST->_opcode = full_opcode; \
		LAST_INST->_address = _address; \
		LAST_INST->_stackChange = stackChange; \
		LAST_INST->_name = opcodePrefix + std::string(name); \
		LAST_INST->_codeGenData = codeGenData; \
		readParams(LAST_INST, params); \

#define OPCODE_MD(val, name, category, stackChange, params, codeGenData) \
	OPCODE_BASE(val)\
		OPCODE_BODY(name, category, stackChange, params, codeGenData)\
		OPCODE_END;

#define OPCODE(val, name, category, stackChange, params) \
	OPCODE_MD(val, name, category, stackChange, params, "")

#define START_SUBOPCODE_WITH_PREFIX(val,prefix) \
	OPCODE_BASE(val) \
		opcodePrefix = prefix + std::string("."); \
		opcode = _f.readByte(); \
		switch (opcode) {
#define START_SUBOPCODE(val) \
	OPCODE_BASE(val) \
		opcode = _f.readByte(); \
		switch (opcode) {
#define END_SUBOPCODE \
		default: \
			throw UnknownOpcodeException(_address, opcode);\
		} \
		INC_ADDR; \
		OPCODE_END;

#endif
