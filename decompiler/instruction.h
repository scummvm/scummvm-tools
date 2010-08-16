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
#include <boost/format.hpp>
#include <boost/variant.hpp>

#include "common/scummsys.h"

/**
 * Enumeration for categorizing the different kinds of instructions.
 */
enum InstType {
	kBinaryOpInstType,    ///< Binary operation (e.g. +, &&, etc.), including comparisons.
	kCallInstType,        ///< Regular function call.
	kCondJumpInstType,    ///< Conditional jump (absolute address).
	kCondJumpRelInstType, ///< Conditional jump (relative address).
	kDupInstType,         ///< Instruction duplicates the most recent stack entry.
	kJumpInstType,        ///< Unconditional jump (absolute address).
	kJumpRelInstType,     ///< Unconditional jump (relative address).
	kLoadInstType,        ///< Load value to stack.
	kReturnInstType,      ///< Return from regular function call.
	kSpecialCallInstType, ///< Special functions.
	kStackInstType,       ///< Stack allocation or deallocation (altering stack pointer).
	kStoreInstType,       ///< Store value from stack in memory.
	kUnaryOpPreInstType,  ///< Unary operation (e.g. !) with operator placed before the operator.
	kUnaryOpPostInstType  ///< Unary operation with operator placed after the operator.
};

/**
 * Enumeration for categorizing the different kinds of parameters.
 */
enum ParamType {
	kSByteParamType,  ///< Signed 8-bit integer.
	kByteParamType,   ///< Unsigned 8-bit integer.
	kShortParamType,  ///< Signed 16-bit integer.
	kUShortParamType, ///< Unsigned 16-bit integer.
	kIntParamType,    ///< Signed 32-bit integer.
	kUIntParamType,   ///< Unsigned 32-bit integer.
	kStringParamType  ///< Text string.
};

/**
 * Structure for representing a parameter.
 */
struct Parameter {
	ParamType _type;                                   ///< Type of the parameter.
	boost::variant<int32, uint32, std::string> _value; ///< Value of the parameter.

	Parameter() {}
	Parameter(ParamType type, boost::variant<int32, uint32, std::string> value)
		: _type(type), _value(value) {}

	/**
	 * Gets an int32 stored in the _value variant.
	 *
	 * @return The int32 stored in the _value variant.
	 * @throws boost::bad_get if the variant is not storing an int32.
	 */
	int32 getSigned() const { return boost::get<int32>(_value); }

	/**
	 * Gets an uint32 stored in the _value variant.
	 *
	 * @return The uint32 stored in the _value variant.
	 * @throws boost::bad_get if the variant is not storing an uint32.
	 */
	uint32 getUnsigned() const { return boost::get<uint32>(_value); }

	/**
	 * Gets an std::string stored in the _value variant.
	 *
	 * @return The std::string stored in the _value variant.
	 * @throws boost::bad_get if the variant is not storing an std::string.
	 */
	std::string getString() const { return boost::get<std::string>(_value); }
};

/**
 * Structure for representing an instruction.
 */
struct Instruction {
	uint32 _opcode;                 ///< The instruction opcode.
	uint32 _address;                ///< The instruction address.
	std::string _name;              ///< The instruction name (opcode name).
	InstType _type;                 ///< The instruction type.
	int16 _stackChange;             ///< How much this instruction changes the stack pointer by.
	std::vector<Parameter> _params; ///< Array of parameters used for the instruction.
	std::string _codeGenData;       ///< String containing metadata for code generation. See the extended documentation for details.

	Instruction(uint32 opcode = 0, uint32 address = 0,
			std::string name = "", InstType type = kSpecialCallInstType, int16 stackChange = 0) :
		_opcode(opcode), _address(address), _name(name), _type(type), _stackChange(stackChange) {}

	/**
	 * Operator overload to output a vector to a std::ostream.
	 *
	 * @param output The std::ostream to output to.
	 * @param inst   The Instruction to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, const Instruction &inst) {
		output << boost::format("%08x: %s") % inst._address % inst._name;
		std::vector<Parameter>::const_iterator param;
		for (param = inst._params.begin(); param != inst._params.end(); ++param) {
			if (param != inst._params.begin())
				output << ",";
			if (inst._type == kCondJumpInstType || inst._type == kCondJumpRelInstType || inst._type == kJumpInstType || inst._type == kJumpRelInstType || inst._type == kCallInstType) {
				// Output numerical arguments to jumps in hexadecimal
				switch (param->_type) {
				case kSByteParamType:
				case kShortParamType:
				case kIntParamType:
					output << boost::format(" 0x%X") % param->getSigned();
					break;
				case kByteParamType:
				case kUShortParamType:
				case kUIntParamType:
					output << boost::format(" 0x%X") % param->getUnsigned();
					break;
				default:
					output << " " << param->_value;
					break;
				}
			} else
				output << " " << param->_value;
		}
		output << boost::format(" (%d)") % inst._stackChange << "\n";
		return output;
	}
};

/**
 * Type representing an iterator over Instructions.
 */
typedef std::vector<Instruction>::iterator InstIterator;

/**
 * Type representing a const_iterator over Instructions.
 */
typedef std::vector<Instruction>::const_iterator ConstInstIterator;

#endif
