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

#ifndef DEC_INSTRUCTION_H
#define DEC_INSTRUCTION_H

#include <vector>

#include "common/scummsys.h"

/**
 * Enumeration for categorizing the different kinds of instructions.
 */
enum InstType { 
	kArithmetic,
	kBoolean,
	kComparison,
	kCondJump,
	kJump,
	kLoad,
	kSpecial,
	kStore
};

/**
 * Enumeration for categorizing the different kinds of parameters.
 */
enum ParamType {
	kSByte,
	kByte,
	kShort,
	kUShort,
	kInt,
	kUInt
};

/**
 * Structure for representing a parameter.
 */
struct Parameter {
  ParamType _type; ///<Type of the parameter.
	union {
		int8 _sbyte;
		uint8 _byte;
		int16 _short;
		uint16 _ushort;
		int32 _int;
		uint32 _uint;
	}; ///<Value of the parameter.
};

/**
 * Structure for representing an instruction.
 */
struct Instruction {
	uint32 _address; ///<The instruction address.
	int16 _stackChange; ///<How much this instruction changes the stack pointer by.
	char *_name; ///<The instruction name (opcode name).
	InstType _type; ///<The instruction type.
	std::vector<Parameter> _params; ///<Array of parameters used for the instruction.
};

#endif
