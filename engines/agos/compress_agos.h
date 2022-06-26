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

/* Compress Simon the Sorcerer 1/2 digital sound files into compressed audio format */

#ifndef COMPRESS_AGOS_H
#define COMPRESS_AGOS_H

#include "compress.h"

class CompressAgos : public CompressionTool {
public:
	CompressAgos(const std::string &name = "compress_agos");

	virtual void execute();

	bool _convertMac;

protected:
	void parseExtraArguments();

	Common::File _input, _output_idx, _output_snd;

	void end();
	int get_offsets(size_t maxcount, uint32 filenums[], uint32 offsets[]);
	int get_offsets_mac(size_t maxcount, uint32 filenums[], uint32 offsets[]);
	uint32 get_sound(uint32 offset);
	void convert_pc(Common::Filename* inputPath);
	void convert_mac(Common::Filename *inputPath);
};

#endif
