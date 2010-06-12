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

#ifndef DEC_DISASSEMBLER_H
#define DEC_DISASSEMBLER_H

#include <iostream>
#include <vector>

#include "instruction.h"
#include "common/file.h"
#include "unknown_opcode.h"

/**
 * Base class for disassemblers.
 */
class Disassembler {
protected:
	Common::File _f;                 ///< Used to perform file I/O.
	std::vector<Instruction> _insts; ///< Container for disassembled instructions.
	uint32 _addressBase;             ///< Base address where the script starts.
	bool _disassemblyDone;           ///< Indicates whether or not disassembly has already been performed.

	/**
	 * Performs disassembly.
	 * @throws UnknownOpcodeException on unknown opcode.
	 */
	virtual void doDisassemble() throw(UnknownOpcodeException) = 0;

	/**
	 * Outputs the disassembled code.
	 *
	 * @param output The std::ostream to output to.
	 */
	virtual void doDumpDisassembly(std::ostream &output);

public:
	Disassembler();
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
	const std::vector<Instruction> &disassemble();

	/**
	 * Outputs the disassembled code. Disassembles code if this has not already been done.
	 *
	 * @param output The std::ostream to output to.
	 */
	void dumpDisassembly(std::ostream &output);
};
#endif
