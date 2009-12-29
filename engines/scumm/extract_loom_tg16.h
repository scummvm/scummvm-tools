/* extract_loom_tg16 - Extract data files from TG16 version of Loom
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

#ifndef EXTRACT_LOOM_TG16_H
#define EXTRACT_LOOM_TG16_H

#include "compress.h"

struct t_resource;
typedef t_resource * p_resource;

class ExtractLoomTG16 : public Tool {
public:
	ExtractLoomTG16(const std::string &name = "extract_loom_tg16");

	virtual void execute();

protected:

	void extract_resource(Common::File &input, Common::File &output, p_resource res);
};

#endif

