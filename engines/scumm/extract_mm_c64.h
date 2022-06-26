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

/* Extract data files from C64 version of Maniac Mansion */

#ifndef EXTRACT_MM_C64_H
#define EXTRACT_MM_C64_H

#include "compress.h"

struct t_resource;
typedef t_resource * p_resource;

class ExtractMMC64 : public Tool {
public:
	ExtractMMC64(const std::string &name = "extract_mm_c64");

	virtual void execute();

};

#endif

