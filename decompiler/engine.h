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

#ifndef ENGINE_H
#define ENGINE_H

#include "disassembler.h"
#include "codegen.h"

/**
 * Base class for engines.
 */
class Engine {
public:
	virtual ~Engine() {}

	/**
	 * Retrieve the disassembler for the engine.
	 *
	 * @return Pointer to a Disassembler for the engine.
	 */
	virtual Disassembler *getDisassembler() const = 0;

	/**
	 * Decode a jump instruction to get the destination address.
	 *
	 * @param it Iterator pointing to the instruction to decode.
	 * @return The destination address of the jump instruction
	 */
	virtual uint32 getDestAddress(ConstInstIterator it) const = 0;

	/**
	 * Retrieve the code generator for the engine.
	 *
	 * @param output The std::ostream to output the code to.
	 * @return Pointer to a CodeGenerator for the engine.
	 */
	virtual CodeGenerator *getCodeGenerator(std::ostream &output) = 0;

	/**
	 * Whether or not code flow analysis is supported for this engine.
	 *
	 * @return True if supported, false if not. If false is returned, code flow analysis should not take place, and -D should be implied.
	 */
	virtual bool supportsCodeFlow() { return true; }

	/**
	 * Whether or not code generation is supported for this engine.
	 *
	 * @return True if supported, false if not. If false is returned, code generation should not take place, and -G should be implied.
	 */
	virtual bool supportsCodeGen() { return true; }
};

#endif
