/* extract_cruise_pc - Extract data from PC version of Cruise for a Corpse
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
