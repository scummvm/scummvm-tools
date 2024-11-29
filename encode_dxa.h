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

#ifndef ENCODE_DXA_H
#define ENCODE_DXA_H

#include "compress.h"


enum ScaleMode {
	S_NONE,
	S_INTERLACED,
	S_DOUBLE
};

class EncodeDXA : public CompressionTool {
public:
	EncodeDXA(const std::string &name = "encode_dxa");

	virtual void parseExtraArguments();
	virtual void execute();


protected:
	byte _compType;

	void convertWAV(const Common::Filename *inpath, const Common::Filename* outpath);
	void readVideoInfo(Common::Filename *filename, int &width, int &height, int &framerate, int &frames, ScaleMode &scaleMode);
	int read_png_file(const char* filename, unsigned char *&image, unsigned char *&palette, int &width, int &height);
};

#endif


