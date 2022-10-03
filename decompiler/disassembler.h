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

#ifndef DEC_DISASSEMBLER_H
#define DEC_DISASSEMBLER_H

#include <iostream>
#include <vector>
#include <unordered_map>

#include "instruction.h"
#include "common/file.h"
#include "unknown_opcode.h"
#include "objectFactory.h"

/**
 * Base class for disassemblers.
 */
class Disassembler {
protected:
	Common::File _f;                              ///< Used to perform file I/O.
	InstVec &_insts;                              ///< Container for disassembled instructions.
	uint32 _addressBase;                          ///< Base address where the script starts.

	/**
	 * Performs disassembly.
	 *
	 * @throws UnknownOpcodeException on unknown opcode. May throw std::exception on other failures.
	 */
	virtual void doDisassemble() throw(std::exception) = 0;

	/**
	 * Outputs the disassembled code.
	 *
	 * @param output The std::ostream to output to.
	 */
	virtual void doDumpDisassembly(std::ostream &output);

public:
	/**
	 * Constructor for Disassembler.
	 *
	 * @param insts Reference to the vector in which disassembled instructions should be placed.
	 */
	Disassembler(InstVec &insts);
	virtual ~Disassembler() {}

	/**
	 * Open a file for disassembly.
	 *
	 * @param filename The file to disassemble.
	 */
	void open(const char *filename);

	/**
	 * Request disassembled instructions.
	 *
	 * @return An std::vector containing the disassembled instructions.
	 */
	void disassemble();

	/**
	 * Outputs the disassembled code. Disassembles code if this has not already been done.
	 *
	 * @param output The std::ostream to output to.
	 */
	void dumpDisassembly(std::ostream &output);
};

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
	virtual void doAssembly() throw(std::exception) = 0; // push_back to _binary
	virtual void doDumpBinary(std::ostream &output);
	std::string readLine();
	std::string splitString(std::string &from, size_t pos, size_t separator_len=0, bool reverse=false);
	void addInstruction(const std::vector<byte> &bytes, int type, size_t jumpAddrStart=0, size_t jumpAddrLen=0, const std::string &label="", const std::string &jumpToLabel="");// automatically pair the label with the instruction address
public:
	Reassembler(InstVec &insts);
	void assemble();
	void dumpBinary(std::ostream &output);
};
#endif
