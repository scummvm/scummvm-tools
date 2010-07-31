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

#include <boost/format.hpp>

#include "disassembler.h"

Disassembler::Disassembler() {
	_addressBase = 0;
	_disassemblyDone = false;
}

void Disassembler::open(const char *filename) {
	_f.open(filename, "rb");
}

void Disassembler::doDumpDisassembly(std::ostream &output) {
	InstIterator inst;
	for (inst = _insts.begin(); inst != _insts.end(); ++inst) {
		output << boost::format("%08x: %s") % inst->_address % inst->_name;
		std::vector<Parameter>::iterator param;
		for (param = inst->_params.begin(); param != inst->_params.end(); ++param) {
			if (param != inst->_params.begin())
				output << ",";
			if (inst->_type == kCondJump || inst->_type == kCondJumpRel || inst->_type == kJump || inst->_type == kJumpRel) {
				switch (param->_type) {
				case kSByte:
				case kShort:
				case kInt:
					output << boost::format(" 0x%X") % param->getSigned();
					break;
				case kByte:
				case kUShort:
				case kUInt:
					output << boost::format(" 0x%X") % param->getUnsigned();
					break;
				default:
					output << " " << param->_value;
					break;
				}
			} else
				output << " " << param->_value;
		}
		output << boost::format(" (%d)") % inst->_stackChange << "\n";
	}
}

const std::vector<Instruction> &Disassembler::disassemble() {
	if (!_disassemblyDone)
		doDisassemble();
	_disassemblyDone = true;
	return _insts;
}

void Disassembler::dumpDisassembly(std::ostream &output) {
	if (!_disassemblyDone)
		doDisassemble();
	_disassemblyDone = true;
	doDumpDisassembly(output);
}
