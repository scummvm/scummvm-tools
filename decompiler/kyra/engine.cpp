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

#include "engine.h"
#include "disassembler.h"
#include "codegen.h"

#include <iostream>
#include <sstream>
#include <boost/format.hpp>

Disassembler *Kyra::Kyra2Engine::getDisassembler(InstVec &insts) {
	return new Kyra2Disassembler(this, insts);
}

CodeGenerator *Kyra::Kyra2Engine::getCodeGenerator(std::ostream &output) {
	return new Kyra2CodeGenerator(this, output);
}

void Kyra::Kyra2Engine::postCFG(InstVec &insts, Graph g) {
	// Add metadata to functions
	for (FuncMap::iterator it = _functions.begin(); it != _functions.end(); ++it) {
		std::stringstream s;
		s << it->second._name << boost::format("sub0x%X") % (*it->second._startIt)->_address;
		it->second._name = s.str();
		int maxArg = 0;
		for (ConstInstIterator instIt = it->second._startIt; instIt != it->second._endIt; ++instIt) {
			if ((*instIt)->_name.compare("pushBPAdd") == 0) {
				if (maxArg < (*instIt)->_params[0]->getSigned()) {
					maxArg = (*instIt)->_params[0]->getSigned();
				}
			}
		}
		it->second._args = maxArg;
		it->second._retVal = true;
		it->second._metadata = std::string(maxArg, 'p');
	}
}

bool Kyra::Kyra2Engine::detectMoreFuncs() const {
	return true;
}

void Kyra::Kyra2Engine::getVariants(std::vector<std::string> &variants) const {
	variants.push_back("kyra2");
	variants.push_back("kyra2-talkie");
}

void Kyra::Kyra2LoadInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	Kyra2CodeGenerator *cg = (Kyra2CodeGenerator *)codeGen;
	switch (_opcode) {
	case 2:
		// If something has been called previously in this group, don't output retval variable
		if (_address <= cg->findFirstCall()->_address)
			stack.push(new VarValue("retval"));
		break;
	case 3:
	case 4:
		stack.push(_params[0]);
		break;
	case 5:
		{
			std::stringstream s;
			s << boost::format("var%d") % _params[0]->getSigned();
			stack.push(new VarValue(s.str()));
		}
		break;
	case 6:
		{
			std::stringstream s;
			s << boost::format("localvar%d") % _params[0]->getSigned();
			stack.push(new VarValue(s.str()));
		}
		break;
	case 7:
		{
			std::stringstream s;
			s << boost::format("param%d") % _params[0]->getSigned();
			stack.push(new VarValue(s.str()));
		}
		break;
	}
}

void Kyra::Kyra2StoreInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	Kyra2CodeGenerator *cg = (Kyra2CodeGenerator *)codeGen;
	switch (_opcode) {
	case 8:
		{
			ValuePtr p = new VarValue("retval");
			cg->writeAssignment(p, stack.pop());
		}
		break;
	case 9:
		{
			std::stringstream s;
			s << boost::format("var%d") % _params[0]->getSigned();
			ValuePtr p = new VarValue(s.str());
			cg->writeAssignment(p, stack.pop());
		}
		break;
	case 10:
		{
			std::stringstream s;
			s << boost::format("localvar%d") % _params[0]->getSigned();
			ValuePtr p = new VarValue(s.str());
			cg->writeAssignment(p, stack.pop());
		}
		break;
	case 11:
		{
			std::stringstream s;
			s << boost::format("param%d") % _params[0]->getSigned();
			ValuePtr p = new VarValue(s.str());
			cg->writeAssignment(p, stack.pop());
		}
		break;
	}
}

void Kyra::Kyra2StackInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	if (_opcode == 12) {
		for (int i = _params[0]->getSigned(); i != 0; --i) {
			if (!stack.empty())
				stack.pop();
		}
	} else if (_opcode == 13) {
		for (int i = 0; i != _params[0]->getSigned(); ++i) {
			std::stringstream s;
			s << boost::format("localvar%d") % i;
			stack.push(new VarValue(s.str()));
		}
	}
}

void Kyra::Kyra2CondJumpInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	stack.push(stack.pop()->negate());
}

uint32 Kyra::Kyra2CondJumpInstruction::getDestAddress() const {
	return _params[0]->getUnsigned();
}

uint32 Kyra::Kyra2UncondJumpInstruction::getDestAddress() const {
	return _params[0]->getUnsigned();
}

void Kyra::Kyra2CallInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	Kyra2CodeGenerator *cg = (Kyra2CodeGenerator *)codeGen;
	cg->_argList.clear();
	Function f = engine->_functions.find(_params[0]->getUnsigned())->second;
	for (size_t i = 0; i < f._metadata.length(); i++)
		cg->processSpecialMetadata(this, f._metadata[i], i);
	stack.push(new CallValue(f._name, cg->_argList));
	// Leave call on stack if this is a condition, or other calls follow in same group
	if (cg->_curGroup->_type == kIfCondGroupType || cg->_curGroup->_type == kWhileCondGroupType || cg->_curGroup->_type == kDoWhileCondGroupType || _address != cg->findLastCall()->_address) {
		return;
	}	else if (!f._retVal) {
		std::stringstream stream;
		stream << stack.pop() << ";";
		cg->addOutputLine(stream.str());
	} else {
		ValuePtr p = new VarValue("retval");
		cg->writeAssignment(p, stack.pop());
	}
}

void Kyra::Kyra2KernelCallInstruction::processInst(ValueStack &stack, Engine *engine, CodeGenerator *codeGen) {
	Kyra2CodeGenerator *cg = (Kyra2CodeGenerator *)codeGen;
	if (_opcode != 14)
		return;
	cg->_argList.clear();
	bool returnsValue = (_codeGenData.find("r") == 0);
	std::string metadata = (!returnsValue ? _codeGenData : _codeGenData.substr(1));
	for (size_t i = 0; i < metadata.length(); i++)
		cg->processSpecialMetadata(this, metadata[i], i);
	stack.push(new CallValue(_name, cg->_argList));
	// Leave call on stack if this is a condition, or other calls follow in same group
	if (cg->_curGroup->_type == kIfCondGroupType || cg->_curGroup->_type == kWhileCondGroupType || cg->_curGroup->_type == kDoWhileCondGroupType || _address != cg->findLastCall()->_address) {
		return;
	} else if (!returnsValue) {
		std::stringstream stream;
		stream << stack.pop() << ";";
		cg->addOutputLine(stream.str());
	} else {
		ValuePtr p = new VarValue("retval");
		cg->writeAssignment(p, stack.pop());
	}
}
