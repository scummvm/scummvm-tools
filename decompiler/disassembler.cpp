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

#include "disassembler.h"

Disassembler::Disassembler(InstVec &insts) : _insts(insts) {
	_addressBase = 0;
}

void Disassembler::open(const char *filename) {
	_f.open(filename, "rb");
}

void Disassembler::doDumpDisassembly(std::ostream &output) {
	InstIterator inst;
	for (inst = _insts.begin(); inst != _insts.end(); ++inst) {
		output << *inst;
	}
}

void Disassembler::disassemble() {
	if (_insts.empty()) {
		_f.seek(0, SEEK_SET);
		doDisassemble();
	}
}

void Disassembler::dumpDisassembly(std::ostream &output) {
	disassemble();
	doDumpDisassembly(output);
}
