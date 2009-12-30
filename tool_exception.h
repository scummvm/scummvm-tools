/* ScummVM Tools
 * Copyright (C) 2002-2009 The ScummVM project
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

#ifndef TOOL_EXCEPTION_H
#define TOOL_EXCEPTION_H

#include <string>
#include <stdexcept>

/**
 * Throw an exception of this type (or subtype of it), if the tool fails fatally.
 * This type is intended for general errors
 */
class ToolException : public std::runtime_error {
public:
	/**
	 * Construct an exception, with an appropriate error message
	 * A return value for the tool should be supplied if none is appropriate
	 * @todo If the tools are even more C++ized, the tool should decide retcode itself, not by exception
	 *
	 * @param error The error message
	 * @param retcode The return value of the process
	 */
	ToolException(std::string error, int retcode = -1) : std::runtime_error(error), _retcode(retcode) {}

	int _retcode;
};

/**
 * Something unexpected happened while reading / writing to a file
 * Usually premature end, or that it could not be opened (write / read protected)
 */
class AbortException : public ToolException {
public:
	AbortException() : ToolException("Operation was aborted", -2) {}
};

#endif
