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

/* Compress Broken Sword II sound clusters into MP3/Ogg Vorbis */

#ifndef COMPRESS_SWORD2_H
#define COMPRESS_SWORD2_H

#include "compress.h"

class CompressSword2 : public CompressionTool {
public:
	CompressSword2(const std::string &name = "compress_sword2");

	virtual void execute();

protected:

	Common::File _input, _output_snd, _output_idx;
	std::string _audioOutputFilename;

	uint32 append_to_file(Common::File &f1, const char *filename);
};

#endif
