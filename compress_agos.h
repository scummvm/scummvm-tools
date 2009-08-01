/* compress_agos.h - Compress Simon the Sorcerer 1/2 digital sound files into compressed audio format
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
 * $URL
 * $Id
 *
 */

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

	File _input, _output_idx, _output_snd;

	void end();
	int get_offsets(uint32 filenums[], uint32 offsets[]);
	int get_offsets_mac(uint32 filenums[], uint32 offsets[]);
	uint32 get_sound(uint32 offset);
	void convert_pc(Filename* inputPath);
	void convert_mac(Filename *inputPath);
};

#endif
