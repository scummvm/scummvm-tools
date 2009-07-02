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
#include <stdio.h>
#include <string.h>

#include "util.h"

typedef unsigned int ULONG;
typedef unsigned char UBYTE;

size_t filelen;
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

int simon_decr(UBYTE *src, UBYTE *dest, ULONG srclen) {
	UBYTE *s = &src[srclen - 4];
	ULONG destlen = EndGetM32(s);
	ULONG bb, x, y;
	UBYTE *d = &dest[destlen];
	UBYTE bc, bit, bits, type;

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

ULONG simon_decr_length(UBYTE *src, ULONG srclen) {
	return EndGetM32(&src[srclen - 4]);
}


/* - loadfile(filename) loads a file from disk, and returns a pointer to that
 *   loaded file, or returns NULL on failure
 * - call free() on ptr to free memory
 * - size of loaded file is available in global var 'filelen'
 */
void *loadfile(const char *name) {
	void *mem = NULL;
	FILE *fd;

	fd = fopen(name, "rb");
	if (fd != NULL) {
		if ((fseek(fd, 0, SEEK_END) == 0) && (filelen = ftell(fd))
		&&	(fseek(fd, 0, SEEK_SET) == 0) && (mem = malloc(filelen))) {

			if (fread(mem, 1, filelen, fd) < filelen) {
				free(mem); mem = NULL;
			}
		}

		fclose(fd);
	}

	return mem;
}

/* - savefile(filename, mem, length) saves [length] bytes from [mem] into
 *   the file named by [filename]
 * - returns zero if failed, or non-zero if successful
 */
int savefile(const char *name, void *mem, size_t length) {
	unsigned int bytesWritten;

	FILE *fd = fopen(name, "wb");
	if (fd == NULL) {
		return 0;
	}

	bytesWritten = fwrite(mem, 1, length, fd);
	if (bytesWritten != length) {
		return 0;
	}

	fclose(fd);

	return 1;
}

int export_main(extract_agos)(int argc, char *argv[]) {
	int first_arg = 1;
	int last_arg = argc;

	Filename inpath, outpath;

	// Check if we should display some helpful text
	parseHelpArguments(argv, argc);
	
	// Now we try to find the proper output directory
	// also make sure we skip those arguments
	if (parseOutputDirectoryArguments(&outpath, argv, argc, first_arg))
		first_arg += 2;
	else if (parseOutputDirectoryArguments(&outpath, argv, argc, last_arg - 2))
		last_arg -= 2;
	else
		// Standard output dir
		outpath.setFullPath("out/");

	// Loop through all input files
	for (int parsed_args = first_arg; parsed_args <= last_arg; ++parsed_args) {
		const char *filename = argv[parsed_args];
		UBYTE *x = (UBYTE *) loadfile(filename);

		inpath.setFullPath(filename);
		outpath.setFullName(inpath.getFullName());

		if (x) {
			ULONG decrlen = simon_decr_length(x, (ULONG) filelen);
			UBYTE *out = (UBYTE *) malloc(decrlen);

			if (out) {
				if (simon_decr(x, out, filelen)) {
					savefile(outpath.getFullPath(), out, decrlen);
				}
				else {
					notice("%s: decrunch error\n", filename);
				}

				free((void *) x);
			}
		}
		else {
			notice("Could not load file %s\n", filename);
		}
	}

	return 0;
}

#ifdef UNIX
int main(int argc, char *argv[]) __attribute__((weak));
int main(int argc, char *argv[]) {
	return export_main(extract_agos)(argc, argv);
}
#endif

