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

#include <vector>

#include "instruction.h"
#include "common/file.h"
#include "unknown_opcode.h"

/**
 * Base class for disassemblers.
 */
class Disassembler {
protected:
	Common::File _f; ///<Used to perform file I/O.
	std::vector<Instruction> _insts; ///<Container for disassembled instructions.
	uint32 _addressBase; ///<Base address where the script starts.		

public:
	Disassembler();
	virtual ~Disassembler() {}

	/**
	 * Open a file for disassembly.		
	 * @param filename The file to disassemble. 
	 */
	void open(const char *filename);

	/**
	 * Disassembles a file.
	 */
	virtual std::vector<Instruction> disassemble() = 0;

	/**
	 * Outputs the disassembly to a file.
	 * @param filename The file to output the disassembly to.
	 */
	virtual void dumpDisassembly(const char *filename);
};

#endif
