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
	virtual ~Tools();

	typedef std::vector<Tool *> ToolList;

	/**
	 * Returns a list of the tools that supports opening the input file
	 * specified in the input list.
	 */
	ToolList inspectInput(const Common::Filename &filename, ToolType type = TOOLTYPE_ALL, bool check_directory = false) const;

protected:
	/** List of all tools */
	ToolList _tools;
};

#endif
