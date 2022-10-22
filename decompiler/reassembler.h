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

#ifndef DEC_REASSEMBLER_H
#define DEC_REASSEMBLER_H

#include <iostream>
#include <vector>
#include <unordered_map>

#include "disassembler.h"
#include "instruction.h"
#include "common/file.h"
#include "unknown_opcode.h"
#include "objectFactory.h"

class Reassembler : public Disassembler {
	struct Jump {
		std::string _label;
		size_t start, len;
	};
private:
	std::vector<byte> _binary;
	std::unordered_map<std::string, size_t> _labels;
	std::vector<Jump> _jumps;

protected:
	virtual void doAssembly(const std::string &label, std::string &instruction, const std::vector<std::string> &args, const std::string &comment) throw(std::exception) = 0; // push_back to _binary
	virtual void doDumpBinary(std::ostream &output);
	std::string readLine();
	std::string splitString(std::string &from, size_t pos, size_t separator_len=0, bool reverse=false);
	void addInstruction(const std::vector<byte> &bytes, int type, size_t jumpAddrStart=0, size_t jumpAddrLen=0, const std::string &label="", const std::string &jumpToLabel="");// automatically pair the label with the instruction address
public:
	Reassembler(InstVec &insts);
	void assemble();
	void dumpBinary(std::ostream &output);
	size_t getEndArgument(const std::string &s, size_t start);
};

#endif
