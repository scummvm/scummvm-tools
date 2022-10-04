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

#include "reassembler.h"
#include "common/endian.h"

Reassembler::Reassembler(InstVec &insts) : Disassembler(insts) { }

void Reassembler::assemble() {
	// Prepare to read the input script
	_f.seek(0, SEEK_SET);
	_binary.clear();

	while(!_f.eos()) {
		try {
			doAssembly();
		} catch(Common::FileException e) {
			break;
		}
	}

	// TODO: 2nd pass in order to set jump addresses after reading all labels
	// maybe doAssembly() should build a vector labels, and a vector of _binary indecies to label IDs
	// then the 2nd pass could be generically handled here without implementation in the engine code
	for(const auto &j : _jumps) {
		if(j._label.empty())
			continue;
		
		size_t addr = _labels[j._label];
		uint16 u16;
		uint32 u32;

		switch(j.len) {
		case 1:
			_binary[j.start] = addr;
			break;
		case 2:
			u16 = TO_LE_16(addr);
			_binary[j.start] = u16 >> 8;
			_binary[j.start + 1] = u16;
			break;
		case 4:
			u32 = TO_LE_32(addr);
			_binary[j.start] = u32 >> 24;
			_binary[j.start + 1] = u32 >> 16;
			_binary[j.start + 2] = u32 >> 8;
			_binary[j.start + 3] = u32;
			break;
		}
	}
}

void Reassembler::doDumpBinary(std::ostream &output) {
	output.write((char*)_binary.data(), _binary.size());
}

void Reassembler::dumpBinary(std::ostream &output) {
	assemble();
	doDumpBinary(output);
}

std::string Reassembler::readLine() {
	std::string line;
	char c;
	while(!_f.eos()) {
		c = 0;
		try {
			c = _f.readByte();
			if(c == '\n')
				break;
			line += c;
		} catch(Common::FileException e) {
			break;
		}
	}
	return line;
}

std::string Reassembler::splitString(std::string &from, size_t pos, size_t separator_len, bool reverse) {
	if(pos == std::string::npos)
		return std::string();
	
	if(reverse) {
		std::string ret = from.substr(pos + separator_len);
		from = from.substr(0, pos);
		return ret;
	}
	std::string ret = from.substr(0, pos);
	from = from.substr(pos + separator_len);
	return ret;
}

void Reassembler::addInstruction(const std::vector<byte> &bytes, int type, size_t jumpAddrStart, size_t jumpAddrLen, const std::string &label, const std::string &jumpToLabel) {
	if(!label.empty()) {
		_labels.emplace(label, _binary.size());
	}

	if(type == kCallInst || type == kCondJumpInst || type == kJumpInst) {
		Jump j;
		j._label = jumpToLabel;
		j.start = jumpAddrStart + _binary.size();
		j.len = jumpAddrLen;
		_jumps.push_back(j);
	}

	// write the address of our label to the _labels map
	_binary.insert(_binary.end(), bytes.begin(), bytes.end());
}
