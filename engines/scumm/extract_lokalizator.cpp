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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main (int argc, char **argv) {
	unsigned char * buf;
	size_t i, sz;
	FILE *fin;
	char fn[1024];

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

	for (i = 0; i < sz; i++) {
	    unsigned int st = 214013 * i + 2531011;
	    int r = (st >> 16) & 0x7FFF;
	    buf[i] = buf[i] ^ r;
	}

	int filecnt = READ_LE_UINT32(buf + 0x14);

	FILE *fout;

	int ctr = 0;

	fprintf (stderr, "%d files\n", filecnt);
      
	for (ctr = 0; ctr < filecnt; ctr++) {
		unsigned char * headptr = buf + 0x18 + 0x4c * ctr;
		char c = headptr[64];
		headptr[64] = 0;
		sprintf (fn, "%s/%s", argv[2], headptr);
		headptr[64] = c;
		int csz = READ_LE_UINT32(headptr+64+4);
		int pos = READ_LE_UINT32(headptr+64+8);
		fout = fopen(fn, "wb");
		if (fout == NULL) {
			fprintf (stderr, "Unable to open %s: %s\n", argv[1], strerror(errno));
			return -3;
		}
		fwrite(buf + pos, csz, 1, fout);
		fclose(fout);

		if (csz > 4 && strcmp((const char*)headptr, "locale.msg") == 0) {
			sprintf (fn, "%s/%s.unscrambled", argv[2], headptr);
			fout = fopen(fn, "wb");
			if (fout == NULL) {
				fprintf (stderr, "Unable to open %s: %s\n", argv[1], strerror(errno));
				return -3;
			}
			uint32 st = 0x12345678;
			for (int p = 0; p < csz - 4; p++) {
				byte x = 0;
				switch (p&3) {
				case 0:
					x = st;
					break;
				case 1:
					x = (st + 35);
					break;
				case 2:
					x = (st + 70);
					break;
				case 3:
					x = (st + 105);
					st += 45707404;
					break;
				}
				buf[pos+4+p] ^= x;
			}

			fwrite(buf + pos, csz, 1, fout);
			fclose(fout);
		}
	}

	return 0;
}
