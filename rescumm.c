/* ReScumm - Split one-big-file Macintosh game data into seperate .00x files for ScummVM
 * Copyright (C) 2001-2003  Casey Hutchinson
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $Header$
 *
 */

#include "util.h"

/* this makes rescumm convert extracted file names to lower case */
#define CHANGECASE

int main(int argc, char *argv[])
{
	FILE *ifp, *ofp;
	unsigned long file_record_off, file_record_len;
	unsigned long file_off, file_len;
	unsigned long data_file_len;
	const char *data_file_name;
	char file_name[0x20];
	char *buf;
	unsigned long i;
	int j;

	if (argc != 2) {
		fputs("error: you must specify the mac data file on the command line.\n", stderr);
		fputs(" i.e. % rescumm \"Sam & Max Demo Data\"\n", stderr);
		fputs("\nA note on usage. Some Lucas Arts CDs appear to contains only an application.\n", stderr);
		fputs("They actually contain a seperate data file as an invisible file.\n", stderr);

		exit(1);
	}

	data_file_name = argv[1];
	if ((ifp = fopen(data_file_name, "rb")) == NULL) {
		error("Could not open \'%s\'.", data_file_name);
	}

	/* Get the length of the data file to use for consistency checks */
	data_file_len = fileSize(ifp);

	/* Read offset and length to the file records */
	file_record_off = readUint32BE(ifp);
	file_record_len = readUint32BE(ifp);

	/* Do a quick check to make sure the offset and length are good */
	if (file_record_off + file_record_len > data_file_len) {
		fclose(ifp);
		error("\'%s\'. file records out of bounds.", data_file_name);
	}

	/* Do a little consistancy check on file_record_length */
	if (file_record_len % 0x28) {
		fclose(ifp);
		error("\'%s\'. file record length not multiple of 40.", data_file_name);
	}

	/* Extract the files */
	for (i = 0; i < file_record_len; i += 0x28) {
		/* read a file record */
		if (fseek(ifp, file_record_off + i, SEEK_SET)) {
			fclose(ifp);
			error("Seek error.");
		}
		file_off = readUint32BE(ifp);
		file_len = readUint32BE(ifp);
		fread(file_name, 0x20, 1, ifp);

		if (!file_name[0]) {
			fclose(ifp);
			error("\'%s\'. file has no name.", data_file_name);
		}
		printf("extracting \'%s\'", file_name);

		/* For convenience compatibility with scummvm (and case sensitive
		 * file systems) change the file name to lowercase.
		 *
		 * if i ever add the ability to pass flags on the command
		 * line, i will make this optional, but i really don't 
		 * see the point to bothering
		 */
		for (j = 0; j < 0x20; j++) {
			if (!file_name[j])
				break;
#ifdef CHANGECASE
			file_name[j] = tolower(file_name[j]);
#endif
		}
		if (j == 0x20) {
			file_name[0x1f] = 0;
			fprintf(stderr, "\nwarning: \'%s\'. file name not null terminated.\n", file_name);
			fprintf(stderr, "data file \'%s\' may be not a file rescumm can extract.\n", data_file_name);
		}
		printf(", saving as \'%s\'\n", file_name);

		/* Consistency check. make sure the file data is in the file */
		if (file_off + file_len > data_file_len) {
			fclose(ifp);
			error("\'%s\'. file out of bounds.", data_file_name);
		}

		/* Write a file */
		if (fseek(ifp, file_off, SEEK_SET)) {
			fclose(ifp);
			error("Seek error.");
		}
		ofp = fopen(file_name, "wb");
		if (!(buf = malloc(file_len))) {
			fclose(ifp);
			error("Could not allocate %ld bytes of memory.", file_len);
		}
		fread(buf, 1, file_len, ifp);
		fwrite(buf, 1, file_len, ofp);
		fclose(ofp);
		free(buf);
	}

	fclose(ifp);
	return 0;
}
