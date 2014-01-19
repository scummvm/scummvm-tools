/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
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

/* monster.sou to MP3-compressed monster.so3 converter */

#ifndef COMPRESS_SCUMM_SOU_H
#define COMPRESS_SCUMM_SOU_H

#include "compress.h"

class CompressScummSou : public CompressionTool {
public:
	CompressScummSou(const std::string &name = "compress_scumm_sou");

	virtual void execute();

protected:
	Common::File _input, _output_idx, _output_snd;
	int _file_size;

	std::string getOutputName() const;
	void end_of_file();
	void append_byte(int size, char buf[]);
	bool get_part();
};

#endif
