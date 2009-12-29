/* extract_agos - Extracts the packed files used in the Amiga and AtariST versions
 * Copyright (C) 2004-2006  The ScummVM Team
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <iostream>

#include "extract_agos.h"

ExtractAgos::ExtractAgos(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	_filelen = 0;
	
	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Used to extract Simon the Sorcer and Feeble Files data files.";
	_helptext = "\nUsage: " + getName() + " [-o outputname] infilename\n" + _shorthelp + "\n";
}

// Run the actual tool
void ExtractAgos::execute() {
	// Loop through all input files
	Common::Filename infilename(_inputPaths[0].path);

	uint8 *x = (uint8 *)loadfile(infilename);

	_outputPath.setFullName(infilename.getFullName());

	uint32 decrlen = simon_decr_length(x, (uint32) _filelen);
	uint8 *out = (uint8 *)malloc(decrlen);

	if (out) {
		if (simon_decr(x, out, _filelen)) {
			savefile(_outputPath.getFullPath(), out, decrlen);
		} else {
			free(x);
			free(out);
			error("%s: decrunch error\n", infilename.getFullPath().c_str());
		}

		free(x);
		free(out);
	}
}

#define EndGetM32(a)	((((a)[0])<<24)|(((a)[1])<<16)|(((a)[2])<<8)|((a)[3]))

#define SD_GETBIT(var) do { \
	if (!bits--) { s -= 4; if (s < src) return 0; bb=EndGetM32(s); bits=31; } \
	(var) = bb & 1; bb >>= 1; \
} while (0)

#define SD_GETBITS(var, nbits) do { \
	bc=(nbits); (var)=0; while (bc--) {(var)<<=1; SD_GETBIT(bit); (var)|=bit; } \
} while (0)

#define SD_TYPE_LITERAL (0)
#define SD_TYPE_MATCH   (1)

int ExtractAgos::simon_decr(uint8 *src, uint8 *dest, uint32 srclen) {
	uint8 *s = &src[srclen - 4];
	uint32 destlen = EndGetM32(s);
	uint32 bb, x, y;
	uint8 *d = &dest[destlen];
	uint8 bc, bit, bits, type;

	/* initialise bit buffer */
	s -= 4;
	x = EndGetM32(s);
	bb = x;
	bits = 0;

	do {
		x >>= 1;
		bits++;
	} while (x);

	bits--;

	while (d > dest) {
		SD_GETBIT(x);

		if (x) {
			SD_GETBITS(x, 2);

			if (x == 0) {
				type = SD_TYPE_MATCH;
				x = 9;
				y = 2;
			} else if (x == 1) {
				type = SD_TYPE_MATCH;
				x = 10;
				y = 3;
			} else if (x == 2) {
				type = SD_TYPE_MATCH;
				x = 12;
				SD_GETBITS(y, 8);
			} else {
				type = SD_TYPE_LITERAL;
				x = 8;
				y = 8;
			}
		} else {
			SD_GETBIT(x);

			if (x) {
				type = SD_TYPE_MATCH;
				x = 8;
				y = 1;
			} else {
				type = SD_TYPE_LITERAL;
				x = 3;
				y = 0;
			}
		}

		if (type == SD_TYPE_LITERAL) {
			SD_GETBITS(x, x); y += x;

			if ((int)(y + 1) > (d - dest)) {
				return 0; /* overflow? */
			}

			do {
				SD_GETBITS(x, 8);
				*--d = x;
			} while (y-- > 0);
		} else {
			if ((int)(y + 1) > (d - dest)) {
				return 0; /* overflow? */
			}

			SD_GETBITS(x, x);

			if ((d + x) > (dest + destlen)) {
				return 0; /* offset overflow? */
			}

			do {
				d--;
				*d = d[x];
			} while (y-- > 0);
		}
	}

	/* successful decrunch */
	return 1;
}

uint32 ExtractAgos::simon_decr_length(uint8 *src, uint32 srclen) {
	return EndGetM32(&src[srclen - 4]);
}


/**
 * loadfile(filename) loads a file from disk, and returns a pointer to that
 * loaded file, or returns NULL on failure
 * call free() on ptr to free memory
 * size of loaded file is available in global var 'filelen'
 *
 * @param name The name of the file to be loaded
 */
void *ExtractAgos::loadfile(const Common::Filename &name) {
	Common::File file(name, "rb");

	// Using global here is not pretty
	_filelen = file.size();
	void *mem = malloc(file.size());

	// Read data
	file.read_throwsOnError(mem, _filelen);

	return mem;
}

/**
 * Saves N bytes from the buffer to the target file.
 * Throws FileException on failure
 * 
 * @param name The name of the file to save to
 * @param mem Where to get data from
 * @param length How many bytes to write
 */
void ExtractAgos::savefile(const Common::Filename &name, void *mem, size_t length) {
	Common::File file(name, "wb");
	file.write(mem, length);
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractAgos agos(argv[0]);
	return agos.run(argc, argv);
}
#endif
