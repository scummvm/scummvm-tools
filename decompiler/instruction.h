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
#include "wrongtype.h"

class CodeGenerator;
class Engine;

/**
 * Constants for categorizing the different kinds of instructions.
 */
const int kBinaryOpInst = 0;     ///< Binary operation (e.g. +, &&, etc.), including comparisons.
const int kBoolNegateInst = 1;   ///< Boolean negation.
const int kCallInst = 2;         ///< Regular function call.
const int kCondJumpInst = 3;     ///< Conditional jump.
const int kDupInst = 4;          ///< Instruction duplicates the most recent stack entry.
const int kJumpInst = 5;         ///< Unconditional jump.
const int kKernelCallInst = 6;   ///< Kernel functions.
const int kLoadInst = 7;         ///< Load value to stack.
const int kReturnInst = 8;       ///< Return from regular function call.
const int kStackInst = 9;        ///< Stack allocation or deallocation (altering stack pointer)
const int kStoreInst = 10;       ///< Store value from stack in memory.
const int kUnaryOpPreInst = 11;  ///< Unary operation (e.g. !) with operator placed before the operator.
const int kUnaryOpPostInst = 12; ///< Unary operation with operator placed after the operator.

const int kFirstCustomInst = kUnaryOpPostInst + 1; ///< First unused key. Add your custom type keys starting with this value.

struct Instruction;

/**
 * Pointer to an Instruction.
 */
typedef boost::intrusive_ptr<Instruction> InstPtr;

/**
 * Structure for representing an instruction.
 */
struct Instruction : public RefCounted {
public:
	uint32 _opcode;                 ///< The instruction opcode.
	uint32 _address;                ///< The instruction address.
	std::string _name;              ///< The instruction name (opcode name).
	int16 _stackChange;             ///< How much this instruction changes the stack pointer by.
	std::vector<ValuePtr> _params;  ///< Array of parameters used for the instruction.
	std::string _codeGenData;       ///< String containing metadata for code generation. See the extended documentation for details.

	Instruction(uint32 opcode = 0, uint32 address = 0,
			std::string name = "", int16 stackChange = 0) :
		_opcode(opcode), _address(address), _name(name), _stackChange(stackChange) {}

	/**
	 * Operator overload to output an Instruction to a std::ostream.
	 *
	 * @param output The std::ostream to output to.
	 * @param inst   The Instruction to output.
	 * @return The std::ostream used for output.
	 */
	friend std::ostream &operator<<(std::ostream &output, const Instruction *inst) {
		return inst->print(output);
	}

	virtual std::ostream &print(std::ostream &output) const;

	virtual bool isJump() const;
	virtual bool isCondJump() const;
	virtual bool isUncondJump() const;
	virtual bool isStackOp() const;
	virtual bool isFuncCall() const;
	virtual bool isReturn() const;
	virtual bool isKernelCall() const;
	virtual bool isLoad() const;
	virtual bool isStore() const;
	virtual uint32 getDestAddress() const;

	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) = 0;

};

struct JumpInstruction : public Instruction {
public:
	virtual bool isJump() const;
};

struct CondJumpInstruction : public JumpInstruction {
public:
	virtual bool isCondJump() const;
};

struct UncondJumpInstruction : public JumpInstruction {
public:
	virtual bool isUncondJump() const;
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

struct StackInstruction : public Instruction {
public:
	virtual bool isStackOp() const;
};

struct CallInstruction : public Instruction {
public:
	virtual bool isFuncCall() const;
};

struct LoadInstruction : public Instruction {
public:
	virtual bool isLoad() const;
};

struct StoreInstruction : public Instruction {
public:
	virtual bool isStore() const;
};

struct DupInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

struct BoolNegateInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

struct BinaryOpInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

struct ReturnInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
	virtual bool isReturn() const;
};

struct UnaryOpPrefixInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

struct UnaryOpPostfixInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

struct KernelCallInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
	virtual bool isKernelCall() const;
};

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
