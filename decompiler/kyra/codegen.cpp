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

const InstPtr Kyra::Kyra2CodeGenerator::findFirstCall() {
	ConstInstIterator it = _curGroup->_start;
	do {
		if ((*it)->isFuncCall() || ((*it)->isKernelCall() && (*it)->_opcode == 14))
			return *it;
	} while (it++ != _curGroup->_end);

	return *_curGroup->_start;
}

const InstPtr Kyra::Kyra2CodeGenerator::findLastCall() {
	ConstInstIterator it = _curGroup->_end;
	do {
		if ((*it)->isFuncCall() || ((*it)->isKernelCall() && (*it)->_opcode == 14))
			return *it;
	} while (it-- != _curGroup->_start);

	return *_curGroup->_end;
}

void Kyra::Kyra2CodeGenerator::processSpecialMetadata(const InstPtr inst, char c, int pos) {
	switch (c) {
	case 'p':
		addArg(_stack.peekPos(pos));
		break;
	case 's':
		{
			ValuePtr p = _stack.peekPos(pos);
			if (p->isInteger()) {
				addArg(new StringValue(((Kyra::Kyra2Engine *)_engine)->_textStrings[p->getUnsigned()]));
			} else {
				ValueList idxs;
				idxs.push_front(p);
				addArg(new ArrayValue("strings", idxs));
			}
		}
		break;
	default:
		CodeGenerator::processSpecialMetadata(inst, c, pos);
		break;
	}
}
