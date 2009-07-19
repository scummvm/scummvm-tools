/* encode_dxa - compressor for dxa files
 * Copyright (c) 2009 The ScummVM Team
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

	virtual void execute();


protected:

	void convertWAV(const Filename *inpath, const Filename* outpath);
	void readVideoInfo(Filename *filename, int &width, int &height, int &framerate, int &frames, ScaleMode &scaleMode);
	int read_png_file(const char* filename, unsigned char *&image, unsigned char *&palette, int &width, int &height);
};

#endif


