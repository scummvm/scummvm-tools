/* UnPak - Extractor for Kyrandia .pak archives
 * Copyright (C) 2004  Johannes Schickel
 * Copyright (C) 2004-2005  The ScummVM Team
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

#include "extract_kyra.h"

int main(int argc, char **argv) {
	if (argc < 2) {
		printf("Use:\n"
				"%s filename [OPTIONS]\n"
				"Here are the options, default is listing files to stdout\n"
				"-o xxx   Extract only file 'xxx'\n"
				"-x       Extract all files\n",
				argv[0]);
		return false;
	}
	
	bool extractAll = false, extractOne = false;
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
					return false;
				}
				
				++pos;
			} else if (argv[pos][1] == 'x') {
				extractAll = true;
			}
		}
	}

	PAKFile myfile(argv[1]);
	
	if(extractAll) {
		myfile.outputAllFiles();
	} else if(extractOne) {
		myfile.outputFile(argv[param]);
	} else {
		myfile.drawFilelist();
	}

	return true;
}

PAKFile::PAKFile(const char* file) {
	FILE* pakfile = fopen(file, "r");
	
	if (!pakfile) {
		error("couldn't open file '%s'", file);
	}
	
	_open = true;
	
	_buffer = new uint8[fileSize(pakfile)];
	assert(_buffer);
	
	_filesize = fileSize(pakfile);
	fread(_buffer, fileSize(pakfile), 1, pakfile);
	
	fclose(pakfile);
}

void PAKFile::drawFilelist(void) {
	const char* currentName = 0;
	
	uint32 startoffset = TO_LE_32(*(uint32 *)_buffer);
	uint32 endoffset = 0;
	uint8* position = _buffer + 4;
	
	for (;;) {
		uint32 strlgt = strlen((const char*)position);
		currentName = (const char*)position;
		
		if (!(*currentName))
			break;

		position += strlgt + 1;
		// scip offset
		endoffset = TO_LE_32(*(uint32 *)position);
		if (endoffset > _filesize) {
			endoffset = _filesize;
		} else if (endoffset == 0) {
			endoffset = _filesize;
		}
		position += 4;
		
		printf("Filename: %s size: %d\n", currentName, endoffset - startoffset);
		
		if (endoffset == _filesize) {
			break;
		}
		
		startoffset = endoffset;
	}
}

void PAKFile::outputFile(const char* file) {
	const char* currentName = 0;
	
	uint32 startoffset = TO_LE_32(*(uint32 *)_buffer);
	uint32 endoffset = 0;
	uint8* position = _buffer + 4;
	
	for (;;) {
		uint32 strlgt = strlen((const char*)position);
		currentName = (const char*)position;
		
		if (!(*currentName))
			break;

		position += strlgt + 1;
		// scip offset
		endoffset = TO_LE_32(*(uint32 *)position);
		if (endoffset > _filesize) {
			endoffset = _filesize;
		} else if (endoffset == 0) {
			endoffset = _filesize;
		}
		position += 4;
		
		if (!strcmp(currentName, file)) {
			FILE* output = fopen(file, "wb+");
			fwrite(_buffer + startoffset, endoffset - startoffset, 1,output);
			fclose(output);
			return;
		}
		
		if (endoffset == _filesize) {
			break;
		}
		
		startoffset = endoffset;
	}
	
	printf("File '%s' not found in this pakfile", file);
}

void PAKFile::outputAllFiles(void) {
	const char* currentName = 0;
	
	uint32 startoffset = TO_LE_32(*(uint32 *)_buffer);
	uint32 endoffset = 0;
	uint8* position = _buffer + 4;
	
	for (;;) {
		uint32 strlgt = strlen((const char*)position);
		currentName = (const char*)position;
		
		if (!(*currentName))
			break;

		position += strlgt + 1;
		// scip offset
		endoffset = TO_LE_32(*(uint32 *)position);
		if (endoffset > _filesize) {
			endoffset = _filesize;
		} else if (endoffset == 0) {
			endoffset = _filesize;
		}
		position += 4;
		
		FILE* output = fopen(currentName, "wb+");
		fwrite(_buffer + startoffset, endoffset - startoffset, 1,output);
		fclose(output);
		
		if (endoffset == _filesize) {
			break;
		}
		
		startoffset = endoffset;
	}
}
