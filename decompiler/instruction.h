/* ScummVM Tools
 * Copyright (C) 2010 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef DEC_INSTRUCTION_H
#define DEC_INSTRUCTION_H

#include <string>
#include <vector>
#include <boost/format.hpp>
#include <boost/intrusive_ptr.hpp>

#include "common/scummsys.h"
#include "refcounted.h"
#include "value.h"
#include "wrongtype.h"

class CodeGenerator;
class Engine;

/**
 * Changes whether or not to output the stack effect for an instruction.
 */
void setOutputStackEffect(bool value);

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
const int kLoadInst = 7;         ///< Load value from memory.
const int kReturnInst = 8;       ///< Return from regular function call.
const int kStackInst = 9;        ///< Stack allocation or deallocation (altering stack pointer)
const int kStoreInst = 10;       ///< Store value in memory.
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

	/**
	 * Print the instruction to an std::ostream.
	 *
	 * @param output The std::ostream to write to.
	 * @return The std::ostream used for output.
	 */
	virtual std::ostream &print(std::ostream &output) const;

	/**
	 * Returns whether or not the instruction is a jump of some sort.
	 *
	 * @return True if the instruction is a jump, otherwise false.
	 */
	virtual bool isJump() const;

	/**
	 * Returns whether or not the instruction is a conditional jump.
	 *
	 * @return True if the instruction is a conditional jump, otherwise false.
	 */
	virtual bool isCondJump() const;

	/**
	 * Returns whether or not the instruction is an unconditional jump.
	 *
	 * @return True if the instruction is an unconditional jump, otherwise false.
	 */
	virtual bool isUncondJump() const;

	/**
	 * Returns whether or not the instruction is a stack operation.
	 *
	 * @return True if the instruction is a stack operation, otherwise false.
	 */
	virtual bool isStackOp() const;

	/**
	 * Returns whether or not the instruction is a call to a script function.
	 *
	 * @return True if the instruction is a script function call, otherwise false.
	 */
	virtual bool isFuncCall() const;

	/**
	 * Returns whether or not the instruction is a return statement.
	 *
	 * @return True if the instruction is a return statement, otherwise false.
	 */
	virtual bool isReturn() const;

	/**
	 * Returns whether or not the instruction is a call to a kernel function.
	 *
	 * @return True if the instruction is a kernel function call, otherwise false.
	 */
	virtual bool isKernelCall() const;

	/**
	 * Returns whether or not the instruction is a load operation.
	 *
	 * @return True if the instruction is a load operation, otherwise false.
	 */
	virtual bool isLoad() const;

	/**
	 * Returns whether or not the instruction is a store operation.
	 *
	 * @return True if the instruction is a store operation, otherwise false.
	 */
	virtual bool isStore() const;

	/**
	 * Returns the destination address of a jump instruction.
	 *
	 * @return Destination address of a jump instruction.
	 * @throws WrongTypeException if instruction is not a jump.
	 */
	virtual uint32 getDestAddress() const;

	/**
	 * Process an instruction for code generation.
	 *
	 * @param stack The current stack.
	 * @param engine Pointer to the Engine used for code generation.
	 * @param codeGen Pointer to the CodeGenerator used for code generation.
	 */
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) = 0;
};

/**
 * Instruction performing a jump.
 */
struct JumpInstruction : public Instruction {
};

/**
 * Instruction performing a conditional jump.
 */
struct CondJumpInstruction : public JumpInstruction {
public:
	virtual bool isCondJump() const;
};

/**
 * Instruction performing an unconditional jump.
 */
struct UncondJumpInstruction : public JumpInstruction {
public:
	virtual bool isUncondJump() const;
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Instruction performing a stack operation.
 */
struct StackInstruction : public Instruction {
public:
	virtual bool isStackOp() const;
};

/**
 * Instruction performing a script function call.
 */
struct CallInstruction : public Instruction {
public:
	virtual bool isFuncCall() const;
};

/**
 * Instruction which loads data from memory.
 */
struct LoadInstruction : public Instruction {
public:
	virtual bool isLoad() const;
};

/**
 * Instruction which stores data to memory.
 */
struct StoreInstruction : public Instruction {
public:
	virtual bool isStore() const;
};

/**
 * Instruction duplicating the topmost stack value.
 */
struct DupInstruction : public Instruction {
};

/**
 * Instruction performing boolean negation.
 */
struct BoolNegateInstruction : public Instruction {
};

/**
 * Instruction performing a binary operation.
 */
struct BinaryOpInstruction : public Instruction {
};

/**
 * Instruction performing a unary operation.
 */
struct UnaryOpInstruction : public Instruction {
};

/**
 * Instruction performing a kernel function call.
 */
struct KernelCallInstruction : public Instruction {
public:
	virtual bool isKernelCall() const;
};

/**
 * Default implementation for stack-based instruction duplicating the topmost stack value.
 */
struct DupStackInstruction : public DupInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Default implementation for stack-based instruction performing boolean negation.
 */
struct BoolNegateStackInstruction : public BoolNegateInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Default implementation for stack-based instruction performing a binary operation.
 */
struct BinaryOpStackInstruction : public BinaryOpInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Instruction which returns from a function.
 */
struct ReturnInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
	virtual bool isReturn() const;
};

/**
 * Default implementation for stack-based instruction performing a unary operation, with a prefixed operator.
 */
struct UnaryOpPrefixStackInstruction : public UnaryOpInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Default implementation for stack-based instruction performing a unary operation, with a postfixed operator.
 */
struct UnaryOpPostfixStackInstruction : public UnaryOpInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Default implementation for stack-based instruction performing a kernel function call.
 */
struct KernelCallStackInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
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
