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

#include "common/endian.h"
#include "common/str.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main (int argc, char **argv) {
	unsigned char * buf;
	size_t sz;
	FILE *fin;

	if (argc < 3) {
		fprintf (stderr, "USAGE: %s INFILE OUTDIR\n", argv[0]);
		return -1;
	}

	fin = fopen (argv[1], "rb");
	if (fin == NULL) {
		fprintf (stderr, "Unable to open %s: %s\n", argv[1], strerror(errno));
		return -2;
	}
	fseek (fin, 0, SEEK_END);
	sz = ftell (fin);
	fseek (fin, 0, SEEK_SET);

	buf = (unsigned char *) malloc (sz);

	fread (buf, 1, sz, fin);
	fclose (fin);

	if (memcmp (buf, "Pod\0file\0\0\0\0", 12) != 0
	    && memcmp (buf, "Pod File\0\0\0\0", 12) != 0
	    && memcmp (buf, "Pod\0\0\0\0\0\0\0\0\0", 12) != 0) {
		fprintf (stderr, "Bad signature\n");
		return 1;
	}

	int filecnt = READ_LE_UINT32(buf + 12);

	FILE *fout;

	int cur = filecnt * 16 + 16;
	int ctr = 0;

	fprintf (stderr, "%d files\n", filecnt);
      
	for (ctr = 0; ctr < filecnt; ctr++) {
		unsigned char * headptr = buf + 16 + 16 * ctr;
		char c = headptr[12];
		headptr[12] = 0;
		Common::String fn = Common::String::format("%s/%s", argv[2], headptr);
		headptr[12] = c;
		int csz = READ_LE_UINT32(headptr+12);
		fout = fopen(fn.c_str(), "wb");
		if (fout == NULL) {
			fprintf (stderr, "Unable to open %s: %s\n", fn.c_str(), strerror(errno));
			return -3;
		}
		fwrite(buf + cur, csz, 1, fout);
		fclose(fout);
		cur += csz;
	}

	if (cur < (int)sz) {
		Common::String fn = Common::String::format("%s/tail.bin", argv[2]);
		fout = fopen(fn.c_str(), "wb");
		if (fout == NULL) {
			fprintf (stderr, "Unable to open %s: %s\n", fn.c_str(), strerror(errno));
			return -3;
		}
		fwrite(buf + cur, sz - cur, 1, fout);
		fclose(fout);
	}

	return 0;
}
