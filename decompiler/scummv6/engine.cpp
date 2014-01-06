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

#include "engine.h"
#include "disassembler.h"
#include "codegen.h"

std::ostream &Scumm::v6::Scummv6StringValue::print(std::ostream &output) const {
	return output << _str;
}

Disassembler *Scumm::v6::Scummv6Engine::getDisassembler(InstVec &insts) {
	return new Scummv6Disassembler(insts);
}

CodeGenerator *Scumm::v6::Scummv6Engine::getCodeGenerator(std::ostream &output) {
	return new Scummv6CodeGenerator(this, output);
}

void Scumm::v6::Scummv6LoadInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	Scummv6CodeGenerator *cg = (Scummv6CodeGenerator *)codeGen;
	switch (_opcode) {
	case 0x00: // pushByte
	case 0x01: // pushWord
		stack.push(_params[0]);
		break;
	case 0x02: // pushByteVar
	case 0x03: // pushWordVar
		stack.push(new VarValue(cg->decodeVarName(_params[0]->getUnsigned())));
		break;
	case 0x06: // byteArrayRead
	case 0x07: // wordArrayRead
		{
			ValueList idxs;
			idxs.push_front(stack.pop());
			stack.push(new ArrayValue(cg->decodeArrayName(_params[0]->getUnsigned()), idxs));
			break;
		}
	case 0x0A: // byteArrayIndexedRead
	case 0x0B: // wordArrayIndexedRead
		{
			ValueList idxs;
			idxs.push_front(stack.pop());
			idxs.push_front(stack.pop());
			stack.push(new ArrayValue(cg->decodeArrayName(_params[0]->getUnsigned()), idxs));
			break;
		}
	}
}

void Scumm::v6::Scummv6StoreInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	Scummv6CodeGenerator *cg = (Scummv6CodeGenerator *)codeGen;
	switch (_opcode) {
	case 0x42: // writeByteVar
	case 0x43: // writeWordVar
		{
			ValuePtr p = new VarValue(cg->decodeVarName(_params[0]->getUnsigned()));
			cg->writeAssignment(p, stack.pop());
		}
		break;
	case 0x46: // byteArrayWrite
	case 0x47: // wordArrayWrite
		{
			ValuePtr value = stack.pop();
			ValueList idxs;
			idxs.push_back(stack.pop());
			ValuePtr p = new ArrayValue(cg->decodeArrayName(_params[0]->getUnsigned()), idxs);
			cg->writeAssignment(p, value);
		}
		break;
	case 0x4A: // byteArrayIndexedWrite
	case 0x4B: // wordArrayIndexedWrite
		{
			ValuePtr value = stack.pop();
			ValueList idxs;
			idxs.push_front(stack.pop());
			idxs.push_front(stack.pop());
			ValuePtr p = new ArrayValue(cg->decodeArrayName(_params[0]->getUnsigned()), idxs);
			cg->writeAssignment(p, value);
		}
		break;
	}
}

void Scumm::v6::Scummv6StackInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	stack.pop();
}

void Scumm::v6::Scummv6CondJumpInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	if (_opcode == 0x5D) // jumpFalse
		stack.push(stack.pop()->negate());
}

uint32 Scumm::v6::Scummv6CondJumpInstruction::getDestAddress() const {
	return _params[0]->getUnsigned();
}

uint32 Scumm::v6::Scummv6JumpInstruction::getDestAddress() const {
	return _params[0]->getUnsigned();
}

void Scumm::v6::Scummv6IncDecInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	Scummv6CodeGenerator *cg = (Scummv6CodeGenerator *)codeGen;
	switch (_opcode) {
	case 0x4E: // byteVarInc
	case 0x4F: // wordVarInc
	case 0x56: // byteVarDec
	case 0x57: // wordVarDec
		{
			std::stringstream s;
			ValuePtr p = new UnaryOpValue(new VarValue(cg->decodeVarName(_params[0]->getUnsigned())), _codeGenData, true);
			s << p << ";";
			cg->addOutputLine(s.str());
		}
		break;
	case 0x52: // byteArrayInc
	case 0x53: // wordArrayInc
	case 0x5A: // byteArrayDec
	case 0x5B: // wordArrayDec
		{
			std::stringstream s;
			ValueList idxs;
			idxs.push_front(stack.pop());
			ValuePtr p = new UnaryOpValue(new ArrayValue(cg->decodeVarName(_params[0]->getUnsigned()), idxs), _codeGenData, true);
			s << p << ";";
			cg->addOutputLine(s.str());
		}
		break;
	}
}

void Scumm::v6::Scummv6ArrayOpInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	Scummv6CodeGenerator *cg = (Scummv6CodeGenerator *)codeGen;
	switch (_opcode) {
	case 0xA4CD: // arrayOp_assignString
		{
			ValuePtr value = _params[1];
			ValueList idxs;
			idxs.push_front(stack.pop());
			ValuePtr p = new ArrayValue(cg->decodeArrayName(_params[0]->getUnsigned()), idxs);
			cg->writeAssignment(p, value);
		}
		break;
	case 0xA4D0: // arrayOp_assignIntList
		{
			ValueList idxs;
			idxs.push_front(stack.pop());
			ValuePtr value = cg->createListValue();
			ValuePtr p = new ArrayValue(cg->decodeArrayName(_params[0]->getUnsigned()), idxs);
			cg->writeAssignment(p, value);
		}
		break;
	case 0xA4D4: // arrayOp_assign2DimList
		{
			ValueList idxs;
			idxs.push_front(stack.pop());
			ValuePtr value = cg->createListValue();
			idxs.push_front(stack.pop());
			ValuePtr p = new ArrayValue(cg->decodeArrayName(_params[0]->getUnsigned()), idxs);
			cg->writeAssignment(p, value);
		}
		break;
	}
}
