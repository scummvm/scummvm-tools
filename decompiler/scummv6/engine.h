/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
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

#ifndef SCUMM_V6_ENGINE_H
#define SCUMM_V6_ENGINE_H

#include "../engine.h"
#include "../instruction.h"
#include "../value.h"

namespace Scumm {

namespace v6 {

const int kArrayOpInst = kFirstCustomInst;
const int kIncDecInst = kFirstCustomInst + 1;

/**
 * SCUMMv6 engine.
 */
class Scummv6Engine : public Engine {
public:
	Disassembler *getDisassembler(InstVec &insts);
	CodeGenerator *getCodeGenerator(std::ostream &output);
};

/**
 * SCUMM v6 string value.
 */
class Scummv6StringValue : public StringValue {
public:
	/**
	 * Constructor for Scummv6StringValue.
	 *
	 * @param str The string value.
	 */
	Scummv6StringValue(std::string str) : StringValue(str) { }

	virtual std::ostream &print(std::ostream &output) const;
};

/**
 * SCUMM v6 load instruction.
 */
class Scummv6LoadInstruction : public LoadInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * SCUMM v6 store instruction.
 */
class Scummv6StoreInstruction : public StoreInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * SCUMM v6 stack operation.
 */
class Scummv6StackInstruction : public StackInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * SCUMM v6 conditional jump.
 */
class Scummv6CondJumpInstruction : public CondJumpInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
	virtual uint32 getDestAddress() const;
};

/**
 * SCUMM v6 unconditional jump.
 */
class Scummv6JumpInstruction : public UncondJumpInstruction {
public:
	virtual uint32 getDestAddress() const;
};

/**
 * SCUMM v6 variable increment/decrement.
 */
class Scummv6IncDecInstruction : public UnaryOpPostfixStackInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

/**
 * SCUMM v6 array operation.
 */
class Scummv6ArrayOpInstruction : public StoreInstruction {
public:
	virtual void processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen);
};

} // End of namespace v6

} // End of namespace Scumm

#endif
