/* extract_mm_nes - Extract data files from NES version of Maniac Mansion
 * Copyright (C) 2009 The ScummVM Team
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#ifndef EXTRACT_MM_NES_H
#define EXTRACT_MM_NES_H

#include "compress.h"

typedef enum _res_type {
	NES_UNKNOWN,
	NES_GLOBDATA,
	NES_ROOM,
	NES_SCRIPT,
	NES_SOUND,
	NES_COSTUME,
	NES_ROOMGFX,
	NES_COSTUMEGFX,
	NES_SPRPALS,
	NES_SPRDESC,
	NES_SPRLENS,
	NES_SPROFFS,
	NES_SPRDATA,
	NES_CHARSET,
	NES_PREPLIST
} res_type;

typedef enum _romset {
	ROMSET_USA,
	ROMSET_EUROPE,
	ROMSET_SWEDEN,
	ROMSET_FRANCE,
	ROMSET_GERMANY,
	ROMSET_SPAIN,
	ROMSET_ITALY,
	NUM_ROMSETS
} t_romset;

struct t_resource {
	uint32 offset;
	uint16 length;
};

struct t_resgroup {
	res_type type;
	const struct t_resource *langs[NUM_ROMSETS];
};

class ExtractMMNes : public Tool {
public:
	ExtractMMNes(const std::string &name = "extract_mm_nes");

	virtual void execute();

protected:

	void extract_resource(Common::File &input, Common::File &output, const t_resource *res, res_type type);
	void dump_resource(Common::File &input, const char *fn_template, int num, const t_resource *res, res_type type);
};

#endif

