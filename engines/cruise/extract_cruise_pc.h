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

/* Extract data from PC version of Cruise for a Corpse */

#ifndef EXTRACT_CRUISE_PC_H
#define EXTRACT_CRUISE_PC_H

#include "compress.h"

struct t_resource;
typedef t_resource * p_resource;

class ExtractCruisePC : public Tool {
public:
	ExtractCruisePC(const std::string &name = "extract_cruise_pc");

	virtual void execute();
};

#endif
