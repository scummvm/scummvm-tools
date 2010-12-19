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

#include "instruction.h"
#include "codegen.h"
#include "engine.h"

bool outputStackEffect = true;

void setOutputStackEffect(bool value) {
	outputStackEffect = value;
}

bool Instruction::isJump() const {
	return isCondJump() || isUncondJump();
}

bool Instruction::isCondJump() const {
	return false;
}

bool Instruction::isUncondJump() const {
	return false;
}

bool Instruction::isStackOp() const {
	return false;
}

bool Instruction::isFuncCall() const {
	return false;
}

bool Instruction::isReturn() const {
	return false;
}

bool Instruction::isKernelCall() const {
	return false;
}

bool Instruction::isLoad() const {
	return false;
}

bool Instruction::isStore() const {
	return false;
}

uint32 Instruction::getDestAddress() const {
	throw WrongTypeException();
}

std::ostream &Instruction::print(std::ostream &output) const {
	output << boost::format("%08x: %s") % _address % _name;
	std::vector<ValuePtr>::const_iterator param;
	for (param = _params.begin(); param != _params.end(); ++param) {
		if (param != _params.begin())
			output << ",";
		output << " " << *param;
	}
	if (outputStackEffect)
		output << boost::format(" (%d)") % _stackChange;
	return output;
}

bool CondJumpInstruction::isCondJump() const {
	return true;
}

bool UncondJumpInstruction::isUncondJump() const {
	return true;
}

void UncondJumpInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
}

bool StackInstruction::isStackOp() const {
	return true;
}

bool CallInstruction::isFuncCall() const {
	return true;
}

bool LoadInstruction::isLoad() const {
	return true;
}

bool StoreInstruction::isStore() const {
	return true;
}

void DupStackInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	std::stringstream s;
	ValuePtr p = stack.pop()->dup(s);
	if (s.str().length() > 0)
		codeGen->addOutputLine(s.str());
	stack.push(p);
	stack.push(p);
}

void BoolNegateStackInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	stack.push(stack.pop()->negate());
}

void BinaryOpStackInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	ValuePtr op1 = stack.pop();
	ValuePtr op2 = stack.pop();
	if (codeGen->_binOrder == kFIFOArgOrder)
		stack.push(new BinaryOpValue(op2, op1, _codeGenData));
	else if (codeGen->_binOrder == kLIFOArgOrder)
		stack.push(new BinaryOpValue(op1, op2, _codeGenData));
}

bool ReturnInstruction::isReturn() const {
	return true;
}

void ReturnInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	codeGen->addOutputLine("return;");
}

void UnaryOpPrefixStackInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	stack.push(new UnaryOpValue(stack.pop(), _codeGenData, false));
}

void UnaryOpPostfixStackInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	stack.push(new UnaryOpValue(stack.pop(), _codeGenData, true));
}

void KernelCallStackInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	codeGen->_argList.clear();
	bool returnsValue = (_codeGenData.find("r") == 0);
	std::string metadata = (!returnsValue ? _codeGenData : _codeGenData.substr(1));
	for (size_t i = 0; i < metadata.length(); i++)
		codeGen->processSpecialMetadata(this, metadata[i], i);
	stack.push(new CallValue(_name, codeGen->_argList));
	if (!returnsValue) {
		std::stringstream stream;
		stream << stack.pop() << ";";
		codeGen->addOutputLine(stream.str());
	}
}

bool KernelCallInstruction::isKernelCall() const {
	return true;
}
