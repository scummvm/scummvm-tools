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

#ifndef KYRA_ENGINE_H
#define KYRA_ENGINE_H

#include "decompiler/engine.h"

#include <string>
#include <vector>

namespace Kyra {

/**
 * KYRA engine.
 */
class Kyra2Engine : public Engine {
public:
	Disassembler *getDisassembler(InstVec &insts);
	CodeGenerator *getCodeGenerator(std::ostream &output);
	void postCFG(InstVec &insts, Graph g);
	bool detectMoreFuncs() const;
	void getVariants(std::vector<std::string> &variants) const;

	std::vector<std::string> _textStrings; ///< Container for strings from the TEXT chunk.
};

/**
 * Kyra2 load instruction.
 */
class Kyra2LoadInstruction : public LoadInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Kyra2 store instruction.
 */
class Kyra2StoreInstruction : public StoreInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Kyra2 stack operation.
 */
class Kyra2StackInstruction : public StackInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Kyra2 conditional jump.
 */
class Kyra2CondJumpInstruction : public CondJumpInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
	virtual uint32 getDestAddress() const;
};

/**
 * Kyra2 unconditional jump.
 */
class Kyra2UncondJumpInstruction : public UncondJumpInstruction {
public:
	bool _isCall;  ///< Whether or not this is really a call to a script function.

	/**
	 * Constructor for Kyra2UncondJumpInstruction.
	 */
	Kyra2UncondJumpInstruction() : _isCall(false) { };
	virtual bool isFuncCall() const;
	virtual bool isUncondJump() const;
	virtual uint32 getDestAddress() const;
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Kyra2 kernel function call.
 */
class Kyra2KernelCallInstruction : public KernelCallInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * Kyra2 instruction with no output.
 */
class Kyra2NoOutputInstruction : public Instruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

} // End of namespace Kyra

#endif
