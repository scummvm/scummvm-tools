/* tools - Interface for accessing all the tools in a convenient fashion
 * Copyright (C) 2009 The ScummVM project
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
 * $URL
 * $Id
 *
 */

#ifndef TOOLS_H
#define TOOLS_H

#include "tool.h"

/**
 * This class holds a list of all the tools available
 * Used by both the GUI and CLI as a base class to get ahold
 * of all the tools.
 */
class Tools {
public:
	Tools();
	~Tools();

	typedef std::vector<Tool *> ToolList;
	
	/**
	 * Returns a list of the tools that supports opening the input file
	 * specified in the input list.
	 */
	ToolList inspectInput(const Filename &filename, ToolType type = TOOLTYPE_ALL) const;

protected:
	/** List of all tools */
	ToolList _tools;
};

#endif
