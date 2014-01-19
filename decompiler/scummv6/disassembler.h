/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
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

#ifndef DEC_SCUMMV6_DISASM_H
#define DEC_SCUMMV6_DISASM_H

#include "decompiler/simple_disassembler.h"
#include "engine.h"

namespace Scumm {

namespace v6 {

/**
 * Disassembler for SCUMMv6.
 */
class Scummv6Disassembler : public SimpleDisassembler {
public:
	/**
	 * Constructor for Scummv6Disassembler.
	 *
	 * @param insts Reference to the vector in which disassembled instructions should be placed.
	 */
	Scummv6Disassembler(InstVec &insts);

	void doDisassemble() throw(std::exception);

	ValuePtr readParameter(InstPtr inst, char type);

	/**
	 * Determines the actual stack effect of an opcode with a variable stack effect.
	 *
	 * @param it        Iterator pointing to the instruction to be fixed.
	 * @param popBefore Number of pops prior to the variable-length list.
	 * @param popAfter  Number of pops after the variable-length list.
	 * @param pushTotal Number of values pushed from the instruction.
	 */
	void fixStackEffect(InstIterator &it, int popBefore, int popAfter, int pushTotal);
};

} // End of namespace v6

} // End of namespace Scumm

#endif
