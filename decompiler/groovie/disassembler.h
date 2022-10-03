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

#include "decompiler/disassembler.h"

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
	void doAssembly() throw(std::exception);

	InstPtr readInstruction();
	InstPtr createInstruction(byte opcode);
	void readParams(InstPtr inst, const char *typeString);
	ValuePtr readParameter(char type);

	size_t writeParams(std::vector<byte> &bytes, const char *typeString, const std::string &arguments, std::string &jumpToLabel);
	size_t writeParameter(char type, std::vector<byte> &bytes, const std::string &arguments, size_t argStart, size_t &jumpAddrStart, std::string &jumpToLabel);

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
