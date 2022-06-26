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

/* Compress Touche Speech Data Files */

#ifndef COMPRESS_TOUCHE_H
#define COMPRESS_TOUCHE_H

#include "compress.h"

class CompressTouche : public CompressionTool {
public:
	CompressTouche(const std::string &name = "compress_touche");

	virtual void execute();

	virtual InspectionMatch inspectInput(const Common::Filename &filename);

protected:

	uint32 compress_sound_data_file(uint32 current_offset, Common::File &output, Common::File &input, uint32 *offs_table, uint32 *size_table, int len);
	void compress_sound_data(Common::Filename *inpath, Common::Filename *outpath);
};

#endif
