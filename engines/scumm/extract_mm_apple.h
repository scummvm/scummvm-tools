/* extract_mm_apple - Extract data files from Apple II version of Maniac Mansion
 * Copyright (C) 2009  The ScummVM Team
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

#ifndef EXTRACT_MM_APPLE_H
#define EXTRACT_MM_APPLE_H

#include "compress.h"

struct t_resource;
typedef t_resource * p_resource;

class ExtractMMApple : public Tool {
public:
	ExtractMMApple(const std::string &name = "extract_mm_apple");

	virtual void execute();

protected:

	void dump_resource(Common::File &input, const char *fn_template, int num, p_resource res);
};

#endif

