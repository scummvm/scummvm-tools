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
#include <boost/intrusive_ptr.hpp>
#include <boost/variant.hpp>

#include "common/scummsys.h"
#include "refcounted.h"
#include "value.h"

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
 * Structure for representing an instruction.
 */
struct Instruction : public RefCounted {
	uint32 _opcode;                 ///< The instruction opcode.
	uint32 _address;                ///< The instruction address.
	std::string _name;              ///< The instruction name (opcode name).
	InstType _type;                 ///< The instruction type.
	int16 _stackChange;             ///< How much this instruction changes the stack pointer by.
	std::vector<ValuePtr> _params;  ///< Array of parameters used for the instruction.
	std::string _codeGenData;       ///< String containing metadata for code generation. See the extended documentation for details.

	Instruction(uint32 opcode = 0, uint32 address = 0,
			std::string name = "", InstType type = kSpecialCallInstType, int16 stackChange = 0) :
		_opcode(opcode), _address(address), _name(name), _type(type), _stackChange(stackChange) {}

	/**
	 * Operator overload to output an Instruction to a std::ostream.
	 *
	 * @param output The std::ostream to output to.
	 * @param inst   The Instruction to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, const Instruction *inst) {
		return output << *inst;
	}

	/**
	 * Operator overload to output an Instruction to a std::ostream.
	 *
	 * @param output The std::ostream to output to.
	 * @param inst   The Instruction to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, const Instruction &inst) {
		output << boost::format("%08x: %s") % inst._address % inst._name;
		std::vector<ValuePtr>::const_iterator param;
		for (param = inst._params.begin(); param != inst._params.end(); ++param) {
			if (param != inst._params.begin())
				output << ",";
			output << " " << *param;
		}
		output << boost::format(" (%d)") % inst._stackChange << "\n";
		return output;
	}
};

/**
 * Pointer to an Instruction.
 */
typedef boost::intrusive_ptr<Instruction> InstPtr;

/**
 * Type representing a vector of InstPtrs.
 */
typedef std::vector<InstPtr> InstVec;

/**
 * Type representing an iterator over InstPtrs.
 */
typedef InstVec::iterator InstIterator;

/**
 * Type representing a const_iterator over InstPtrs.
 */
typedef InstVec::const_iterator ConstInstIterator;

#endif
