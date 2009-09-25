/* compress_touche - Compress Touche Speech Data Files
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

#ifndef COMPRESS_TOUCHE_H
#define COMPRESS_TOUCHE_H

#include "compress.h"

class CompressTouche : public CompressionTool {
public:
	CompressTouche(const std::string &name = "compress_touche");

	virtual void execute();

	virtual InspectionMatch inspectInput(const Filename &filename);

protected:

	uint32 compress_sound_data_file(uint32 current_offset, File &output, File &input, uint32 *offs_table, uint32 *size_table, int len);
	void compress_sound_data(Filename *inpath, Filename *outpath);
};

#endif
