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

#ifndef DEC_UNKNOWN_OPCODE_H
#define DEC_UNKNOWN_OPCODE_H

#include <exception>

#include "common/scummsys.h"

/**
 * Exception representing an unknown opcode.
 */
class UnknownOpcodeException : public std::exception {
	uint32 _address; ///< Address where the invalid opcode was found.
	uint8 _opcode;   ///< The value of the invalid opcode.
	char _buf[255];  ///< Buffer for formatting the error message.

public:
	/**
	 * Constructor for UnknownOpcodeException.
	 *
	 * @param address Address where the invalid opcode was found.
	 * @param opcode  The value of the invalid opcode.
	 */
	UnknownOpcodeException(uint32 address, uint8 opcode);

	/**
	 * Description of the exception.
	 */
	virtual const char *what() throw();
};

#endif
