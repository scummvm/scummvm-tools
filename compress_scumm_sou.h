/* compress_scumm_sou - monster.sou to MP3-compressed monster.so3 converter
 * Copyright (C) 2009 The ScummVM Team
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


#ifndef COMPRESS_SCUMM_SOU_H
#define COMPRESS_SCUMM_SOU_H

#include "compress.h"


class CompressScummSou : public CompressionTool {
public:
	CompressScummSou(const std::string &name = "compress_scumm_sou");

	virtual void execute();

protected:
	std::string _audioOuputFilename;
	File _input, _output_idx, _output_snd;

	void end_of_file(const char *inputPath);
	void append_byte(int size, char buf[]);
	void get_part(const char *inputPath);
};

#endif


