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
		output << *inst << "\n";
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

Reassembler::Reassembler(InstVec &insts) : Disassembler(insts) { }

void Reassembler::assemble() {
	// Prepare to read the input script
	_f.seek(0, SEEK_SET);
	_binary.clear();

	while(!_f.eos()) {
		try {
			//doAssembly();
			byte data = _f.readByte();
			_binary.push_back(data);
		} catch(Common::FileException e) {
			break;
		}
	}
}

void Reassembler::doDumpBinary(std::ostream &output) {
	for (auto &i : _binary) {
		output << i;
	}
}

void Reassembler::dumpBinary(std::ostream &output) {
	assemble();
	doDumpBinary(output);
}
