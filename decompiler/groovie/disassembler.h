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

#ifndef GROOVIE_DISASSEMBLER_H
#define GROOVIE_DISASSEMBLER_H

#include "decompiler/reassembler.h"

namespace Groovie {

struct GroovieOpcode;

/**
 * Disassembler for Groovie scripts.
 */
class GroovieDisassembler : public Reassembler {
public:
	GroovieDisassembler(InstVec &insts, const std::vector<GroovieOpcode> &opcodes);

protected:
	void doDisassemble() throw(UnknownOpcodeException);
	GroovieOpcode getInstruction(const std::string &name) throw(std::exception);
	void doAssembly(const std::string &label, std::string &instruction, const std::vector<std::string> &args, const std::string &comment) throw(std::exception);

	InstPtr readInstruction();
	InstPtr createInstruction(byte opcode);
	void readParams(InstPtr inst, const char *typeString);
	ValuePtr readParameter(char type);

	size_t writeParams(std::vector<byte> &bytes, const char *typeString, const std::vector<std::string> &args, std::string &jumpToLabel);
	void writeParameter(char type, std::vector<byte> &bytes, const std::string &arg, size_t &jumpAddrStart, std::string &jumpToLabel);
	void writeParameterVideoName(std::vector<byte> &bytes, const std::string &arg);
	void writeParameterIndexed(bool allow7C, bool limitVal, bool limitVar, std::vector<byte> &bytes, const std::string &arg);
	void writeParameterArray(std::vector<byte> &bytes, const std::string &arg);

	ValuePtr readParameterIndexed(bool allow7C, bool limitVal, bool limitVar);
	ValuePtr readParameterArray();
	ValuePtr readParameterScriptName();
	ValuePtr readParameterVideoName();

	const std::vector<GroovieOpcode> _opcodes;
	uint32 _address;
	uint32 _maxTargetAddress;
	bool _firstBit;
	uint32 _inputLoopStart;
};

} // End of namespace Groovie

#endif // GROOVIE_DISASSEMBLER_H
