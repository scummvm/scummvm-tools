/* ScummVM Tools
 * Copyright (C) 2010 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef GROOVIE_DISASSEMBLER_H
#define GROOVIE_DISASSEMBLER_H

#include "decompiler/disassembler.h"

namespace Groovie {

struct GroovieOpcode;

/**
 * Disassembler for Groovie scripts.
 */
class GroovieDisassembler : public Disassembler {
public:
	GroovieDisassembler(InstVec &insts, const GroovieOpcode *opcodes);

protected:
	void doDisassemble() throw(UnknownOpcodeException);


	InstPtr readInstruction();
	InstPtr createInstruction(byte opcode);
	void readParams(InstPtr inst, const char *typeString);
	ValuePtr readParameter(InstPtr inst, char type);

	ValuePtr readParameterIndexed(bool allow7C, bool limitVal, bool limitVar);
	ValuePtr readParameterArray();
	ValuePtr readParameterScriptName();
	ValuePtr readParameterVideoName();

	const GroovieOpcode *_opcodes;
	uint32 _address;
	uint32 _maxTargetAddress;
	bool _firstBit;
	uint32 _inputLoopStart;
};

} // End of namespace Groovie

#endif // GROOVIE_DISASSEMBLER_H
