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

/**
 * Base class for engines.
 */
class Engine {
public:
	virtual ~Engine() {}

	/**
	 * Retrieve the disassembler for the engine.
	 * @return Pointer to a Disassembler for the engine.
	 */
	virtual Disassembler *getDisassembler() const = 0;

	/**
	 * Decode a jump-instruction to get the destination address.
	 * @param it Iterator pointing to the instruction to decode.
	 */
	virtual uint32 getDestAddress(InstIterator it) const = 0;
};

#endif
