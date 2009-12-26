/* extract_gob_stk.h - Extracts the packed files used in the Amiga and AtariST versions
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
 * $URL$
 * $Id$
 *
 */

#ifndef EXTRACT_AGOS_H
#define EXTRACT_AGOS_H

#include "tool.h"

class ExtractAgos : public Tool {
public:
	ExtractAgos(const std::string &name = "extract_agos");

	virtual void execute();

protected:
	size_t _filelen;

	int simon_decr(uint8 *src, uint8 *dest, uint32 srclen);
	uint32 simon_decr_length(uint8 *src, uint32 srclen);
	void *loadfile(const Common::Filename &name);
	void savefile(const Common::Filename &name, void *mem, size_t length);
};

#endif
