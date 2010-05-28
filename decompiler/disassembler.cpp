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
}

void Disassembler::open(const char *filename) {
	_f.open(filename, "rb");
}

void Disassembler::dumpDisassembly(std::ostream &output) {
	for (size_t i = 0; i < _insts.size(); i++) {
		Instruction inst = _insts[i];
		output << boost::format("%08x: %s ") % inst._address % inst._name;
		for (size_t j = 0; j < inst._params.size(); j++) {
			Parameter p = inst._params[j];
			if (j != 0)
				output << ", ";
			switch(p._type) {
				case kSByte:
					output << p._sbyte;
					break;
				case kByte:
					output << p._byte;
					break;
				case kShort:
					output << p._short;
					break;
				case kUShort:
					output << p._ushort;
					break;
				case kInt:
					output << p._int;
					break;
				case kUInt:
					output << p._uint;
					break;
				case kFloat:
					output << p._float;
					break;
			}
		}
		output << "\n";
	}
}
