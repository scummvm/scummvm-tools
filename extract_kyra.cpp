/* extract_kyra - Extractor for Kyrandia .pak archives
 * Copyright (C) 2004  Johannes Schickel
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

#include "kyra_pak.h"

void showhelp(char* exename)
{
		printf("\nUsage: %s <file> [params]\n", exename);

		printf("\nParams:\n");
		printf("-o <filename>     Extract only <filename>\n");
		printf("-x                Extract all files\n");
		printf("-a                Extract files from the Amiga .PAK files\n");

		exit(2);
}

int main(int argc, char **argv) {
	if (argc < 2) {
		showhelp(argv[0]);
	}

	bool extractAll = false, extractOne = false, isAmiga = false;
	int param;

	for (param = 1; param < argc; param++) {
		if (strcmp(argv[param], "-o") == 0) {
			extractOne = true;
			param++;

			if (param >= argc) {
				printf("You supply a filename with -o\n");
				printf("Example: %s A_E.PAK -o ALGAE.CPS\n", argv[0]);

				exit(-1);
			}
		} else if (strcmp(argv[param], "-x") == 0) {
			extractAll = true;
		} else if (strcmp(argv[param], "-a") == 0) {
			isAmiga = true;
		} else {
			showhelp(argv[0]);
		}
	}

	PAKFile myfile;
	if (!myfile.loadFile(argv[1], isAmiga)) {
		error("Couldn't load file '%s'", argv[1]);
	}

	if(extractAll) {
		myfile.outputAllFiles();
	} else if(extractOne) {
		myfile.outputFile(argv[param]);
	} else {
		myfile.drawFileList();
	}

	return 0;
}
