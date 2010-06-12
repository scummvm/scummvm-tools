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

#include <string>
#include <vector>
#include <boost/variant.hpp>

#include "common/scummsys.h"

/**
 * Enumeration for categorizing the different kinds of instructions.
 */
enum InstType {
	kArithmetic,  ///< Arithmetic instruction (+, -, *, etc.).
	kBoolean,     ///< Boolean instruction (AND, OR, etc.).
	kCall,        ///< Regular function call.
	kComparison,  ///< Comparison instruction.
	kCondJump,    ///< Conditional jump (absolute address).
	kCondJumpRel, ///< Conditional jump (relative address).
	kJump,        ///< Unconditional jump (absolute address).
	kJumpRel,     ///< Unconditional jump (relative address).
	kLoad,        ///< Load value to stack.
	kReturn,      ///< Return from regular function call.
	kSpecial,     ///< Special functions.
	kStack,       ///< Stack allocation or deallocation (altering stack pointer).
	kStore        ///< Store value from stack in memory.
};

/**
 * Enumeration for categorizing the different kinds of parameters.
 */
enum ParamType {
	kSByte,  ///< Signed 8-bit integer.
	kByte,   ///< Unsigned 8-bit integer.
	kShort,  ///< Signed 16-bit integer.
	kUShort, ///< Unsigned 16-bit integer.
	kInt,    ///< Signed 32-bit integer.
	kUInt,   ///< Unsigned 32-bit integer.
	kString  ///< Text string.
};

/**
 * Structure for representing a parameter.
 */
struct Parameter {
	ParamType _type;                                   ///< Type of the parameter.
	boost::variant<int32, uint32, std::string> _value; ///< Value of the parameter.

	/**
	 * Gets an int32 stored in the _value variant.
	 * @return The int32 stored in the _value variant.
	 * @throws boost::bad_get if the variant is not storing an int32.
	 */
	int32 getSigned() const { return boost::get<int32>(_value); }

	/**
	 * Gets an uint32 stored in the _value variant.
	 * @return The uint32 stored in the _value variant.
	 * @throws boost::bad_get if the variant is not storing an uint32.
	 */
	uint32 getUnsigned() const { return boost::get<uint32>(_value); }

	/**
	 * Gets an std::string stored in the _value variant.
	 * @return The std::string stored in the _value variant.
	 * @throws boost::bad_get if the variant is not storing an std::string.
	 */
	std::string getString() const { return boost::get<std::string>(_value); }
};

/**
 * Structure for representing an instruction.
 */
struct Instruction {
	uint32 _address;                ///< The instruction address.
	int16 _stackChange;             ///< How much this instruction changes the stack pointer by.
	std::string _name;              ///< The instruction name (opcode name).
	InstType _type;                 ///< The instruction type.
	std::vector<Parameter> _params; ///< Array of parameters used for the instruction.
};

#endif
