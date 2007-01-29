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

int main(int argc, char **argv) {
	if (argc < 2) {
		printf("Use:\n"
				"%s filename [OPTIONS]\n"
				"Here are the options, default is listing files to stdout\n"
				"-o xxx   Extract only file 'xxx'\n"
				"-x       Extract all files\n"
				"-a       Use this if you want to extract files from the Amiga .PAK files\n",
				argv[0]);
		return -1;
	}
	
	bool extractAll = false, extractOne = false;
	bool isAmiga = false;
	uint8 param = 0;
	
	// looking for the parameters
	for (int32 pos = 1; pos < argc; ++pos) {
		if (*argv[pos] == '-') {
			if (argv[pos][1] == 'o') {
				extractOne = true;
				param = pos + 1;
				
				if (param >= argc) {
					printf("you have to add a filename to option -o\n"
							"like: unpackkyra A_E.PAK -o ALGAE.CPS\n");
					return -1;
				}
				
				++pos;
			} else if (argv[pos][1] == 'x') {
				extractAll = true;
			} else if (argv[pos][1] == 'a') {
				isAmiga = true;
			}
		}
	}

	PAKFile myfile;
	if (!myfile.loadFile(argv[1], isAmiga)) {
		error("couldn't load file '%s'", argv[1]);
		return -1;
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
