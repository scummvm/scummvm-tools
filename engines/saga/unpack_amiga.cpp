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
	unsigned char *rtnbuf;
	size_t sz;
	FILE *rtnfin;

	if (argc < 3) {
		fprintf (stderr, "USAGE: %s BASEFILE OUTDIR\n", argv[0]);
		return -1;
	}

	rtnfin = fopen (Common::String::format("%s.rtn", argv[1]).c_str(), "rb");
	if (rtnfin == NULL) {
		fprintf (stderr, "Unable to open %s: %s.rtn\n", argv[1], strerror(errno));
		return -2;
	}
	fseek (rtnfin, 0, SEEK_END);
	sz = ftell (rtnfin);
	fseek (rtnfin, 0, SEEK_SET);

	rtnbuf = (unsigned char *) malloc (sz);

	fread (rtnbuf, 1, sz, rtnfin);
	fclose (rtnfin);

	int rescnt = READ_BE_UINT16(rtnbuf);
	int scriptcnt = READ_BE_UINT16(rtnbuf + 2);

	int ctr = 0;

	fprintf (stderr, "%d resource files, %d script files\n", rescnt, scriptcnt);
      
	for (ctr = 0; ctr < rescnt + scriptcnt; ctr++) {
		unsigned char * headptr = rtnbuf + 4 + 10 * ctr;
		uint32 off = READ_BE_UINT32(headptr);
		uint32 len = READ_BE_UINT32(headptr + 4);
		uint16 vol = READ_BE_UINT16(headptr + 8);
		Common::String finName = Common::String::format("%s.%03d", argv[1], vol);
		Common::String foutName = Common::String::format("%s/%s_%d.bin", argv[2], ctr < rescnt ? "res" : "scr", ctr < rescnt ? ctr : ctr - rescnt);
		FILE *finH = fopen(finName.c_str(), "rb");
		if (!finH) {
			fprintf(stderr, "Can't open %s.%03d", argv[1], vol);
			continue;
		}
		FILE *foutH = fopen(foutName.c_str(), "wb");
		if (!foutH) {
			fprintf(stderr, "Can't open %s.%03d", argv[1], vol);
			continue;
		}
		fseek(finH, off, SEEK_SET);
		char buf[0x400];
		size_t resSz = len;
		while (resSz) {
			int csz = resSz < sizeof(buf) ? resSz : sizeof(buf);
			fread(buf, csz, 1, finH);
			fwrite(buf, csz, 1, foutH);
			resSz -= csz;
		}
		fclose(finH);
		fclose(foutH);
	}

	return 0;
}
