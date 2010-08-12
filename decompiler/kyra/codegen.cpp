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

#include "codegen.h"
#include "engine.h"

std::string Kyra::Kyra2CodeGenerator::constructFuncSignature(const Function &func) {
	std::stringstream s;
	s << func._name << "(";
	for (uint32 i = 0; i != func._args; ++i) {
		if (i != 0)
			s << ", ";
		s << "param" << (i+1);
	}
	s << ") {";
	return s.str();
}

void Kyra::Kyra2CodeGenerator::processInst(const Instruction inst) {
	switch (inst._type) {
	case kLoad:
		switch (inst._opcode) {
		case 2:
			// If something has been called previously in this group, don't output retval variable
			if (inst._address <= findFirstCall()._address)
				_stack.push(new VarEntry("retval"));
			break;
		case 3:
		case 4:
			_stack.push(new IntEntry(inst._params[0].getSigned(), true));
			break;
		case 5:
			{
				std::stringstream s;
				s << boost::format("var%d") % inst._params[0].getSigned();
				_stack.push(new VarEntry(s.str()));
			}
			break;
		case 6:
			{
				std::stringstream s;
				s << boost::format("localvar%d") % inst._params[0].getSigned();
				_stack.push(new VarEntry(s.str()));
			}
			break;
		case 7:
			{
				std::stringstream s;
				s << boost::format("param%d") % inst._params[0].getSigned();
				_stack.push(new VarEntry(s.str()));
			}
			break;
		}
		break;
	case kStore:
		switch (inst._opcode) {
		case 8:
			{
				EntryPtr p = new VarEntry("retval");
				writeAssignment(p, _stack.pop());
			}
			break;
		case 9:
			{
				std::stringstream s;
				s << boost::format("var%d") % inst._params[0].getSigned();
				EntryPtr p = new VarEntry(s.str());
				writeAssignment(p, _stack.pop());
			}
			break;
		case 10:
			{
				std::stringstream s;
				s << boost::format("localvar%d") % inst._params[0].getSigned();
				EntryPtr p = new VarEntry(s.str());
				writeAssignment(p, _stack.pop());
			}
			break;
		case 11:
			{
				std::stringstream s;
				s << boost::format("param%d") % inst._params[0].getSigned();
				EntryPtr p = new VarEntry(s.str());
				writeAssignment(p, _stack.pop());
			}
			break;
		}
		break;
	case kStack:
		if (inst._opcode == 12) {
			for (int i = inst._params[0].getSigned(); i != 0; --i) {
				if (!_stack.empty())
					_stack.pop();
			}
		} else if (inst._opcode == 13) {
			for (int i = 0; i != inst._params[0].getSigned(); ++i) {
				std::stringstream s;
				s << boost::format("localvar%d") % i;
				_stack.push(new VarEntry(s.str()));
			}
		}
		break;
	case kCondJump:
		switch (_curGroup->_type) {
		case kIfCond:
		case kWhileCond:
			break;
		case kDoWhileCond:
			_stack.push(new UnaryOpEntry(_stack.pop(), "!"));
			break;
		default:
			{
				std::stringstream s;
				s << boost::format("WARNING: Couldn't handle conditional jump at address %08X") % inst._address;
				addOutputLine(s.str());
			}
			break;
		}
		break;
	case kCall:
		{
			_argList.clear();
			Function f = _engine->_functions.find(inst._params[0].getUnsigned())->second;
			for (size_t i = 0; i < f._metadata.length(); i++)
				processSpecialMetadata(inst, f._metadata[i], i);
			_stack.push(new CallEntry(f._name, _argList));
			// Leave call on stack if this is a condition, or other calls follow in same group
			if (_curGroup->_type == kIfCond || _curGroup->_type == kWhileCond || _curGroup->_type == kDoWhileCond || inst._address != findLastCall()._address) {
				break;
			}	else if (!f._retVal) {
				std::stringstream stream;
				stream << _stack.pop() << ";";
				addOutputLine(stream.str());
			} else {
				EntryPtr p = new VarEntry("retval");
				writeAssignment(p, _stack.pop());
			}
			break;
		}
	case kSpecial:
		{
			if (inst._opcode != 14)
				return;
			_argList.clear();
			bool returnsValue = (inst._codeGenData.find("r") == 1);
			std::string metadata = (!returnsValue ? inst._codeGenData.substr(1) : inst._codeGenData.substr(2));
			for (size_t i = 0; i < metadata.length(); i++)
				processSpecialMetadata(inst, metadata[i], i);
			_stack.push(new CallEntry(inst._name, _argList));
			// Leave call on stack if this is a condition, or other calls follow in same group
			if (_curGroup->_type == kIfCond || _curGroup->_type == kWhileCond || _curGroup->_type == kDoWhileCond || inst._address != findLastCall()._address) {
				break;
			}	else if (!returnsValue) {
				std::stringstream stream;
				stream << _stack.pop() << ";";
				addOutputLine(stream.str());
			} else {
				EntryPtr p = new VarEntry("retval");
				writeAssignment(p, _stack.pop());
			}
			break;
		}
	default:
		{
			std::stringstream s;
			s << boost::format("WARNING: Unknown opcode %X at address %08X") % inst._opcode % inst._address;
			addOutputLine(s.str());
		}
		break;
	}
}

const Instruction &Kyra::Kyra2CodeGenerator::findFirstCall() {
	ConstInstIterator it = _curGroup->_start;
	do {
		if (it->_type == kCall || (it->_type == kSpecial && it->_opcode == 14))
			return *it;
	} while (it++ != _curGroup->_end);

	return *_curGroup->_start;
}

const Instruction &Kyra::Kyra2CodeGenerator::findLastCall() {
	ConstInstIterator it = _curGroup->_end;
	do {
		if (it->_type == kCall || (it->_type == kSpecial && it->_opcode == 14))
			return *it;
	} while (it-- != _curGroup->_start);

	return *_curGroup->_end;
}

void Kyra::Kyra2CodeGenerator::processSpecialMetadata(const Instruction &inst, char c, int pos) {
	switch (c) {
	case '0':
		_stackOffset = 0;
		break;
	case 'p':
		addArg(_stack.peekPos(_stackOffset++));
		break;
	case 's':
		{
			EntryPtr p = _stack.peekPos(_stackOffset++);
			if (p->_type == seInt) {
				IntEntry *ie = (IntEntry *)p.get();
				addArg(new StringEntry(((Kyra::Kyra2Engine *)_engine)->_textStrings[ie->getValue()]));
			} else {
				EntryList idxs;
				idxs.push_front(p);
				addArg(new ArrayEntry("strings", idxs));
			}
		}
		break;
	default:
		CodeGenerator::processSpecialMetadata(inst, c, pos);
		break;
	}
}
