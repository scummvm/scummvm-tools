/* DeGob - GobEngine Script disassembler
 * Copyright (C) 2008 The ScummVM project
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

#include "degob_script.h"
#include <iostream>

void printHelp(const char *bin);
int getVersion(const char *verStr);
byte *readFile(FILE *tot, uint32 &size);
Script *initScript(byte *totData, uint32 totSize, ExtTable *extTable, int version);
void printInfo(Script &script);

int main(int argc, char **argv) {

	if ((argc < 3) || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
		printHelp(argv[0]);
		return -1;
	}

	int version = getVersion(argv[1]);
	if (version == -1) {
		printHelp(argv[0]);
		return -1;
	}

	FILE *f;
	byte *totData = 0, *extData = 0, *extComData = 0;
	uint32 totSize = 0, extSize = 0, extComSize = 0;

	if (!(f = fopen(argv[2], "r")))
		error("Couldn't open file \"%s\"", argv[2]);
	totData = readFile(f, totSize);
	fclose(f);

	ExtTable *extTable = 0;
	if (argc > 3) {
		if (!(f = fopen(argv[3], "r")))
			error("Couldn't open file \"%s\"", argv[3]);
		extData = readFile(f, extSize);
		fclose(f);

		if (argc > 4) {
			if (!(f = fopen(argv[4], "r")))
				error("Couldn't open file \"%s\"", argv[4]);
			extComData = readFile(f, extComSize);
			fclose(f);
		}

		extTable = new ExtTable(extData, extSize, extComData, extComSize);
	}

	Script *script = initScript(totData, totSize, extTable, version);
	if (!script) {
		printHelp(argv[0]);
		return -1;
	}

	printInfo(*script);
	printf("-----\n");

	script->deGob();

	delete[] totData;
	delete[] extData;
	delete[] extComData;
	delete extTable;
	delete script;
	return 0;
}

void printHelp(const char *bin) {
	printf("Usage: %s <version> <file.tot> [<file.ext>] [<commun.ext>]\n\n", bin);
	printf("The disassembled script will be written to stdout.\n\n");
	printf("Supported versions:\n");
	printf("	Gob1     - Gobliiins 1\n");
	printf("	Gob2     - Gobliins 2\n");
	printf("	Gob3     - Goblins 3\n");
	printf("	Ween     - Ween: The Prophecy\n");
	printf("	Bargon   - Bargon Attack\n");
	printf("	Lost     - Lost in Time\n");
	printf("	Woodruff - The Bizarre Adventures of Woodruff and the Schnibble\n");
	printf("	Dynasty  - The Last Dynasty\n");
	printf("	Urban    - Urban Runner\n");
}

int getVersion(const char *verStr) {
	if (!scumm_stricmp(verStr, "Gob1"))
		return 0;
	else if (!scumm_stricmp(verStr, "Gob2"))
		return 1;
	else if (!scumm_stricmp(verStr, "Gob3"))
		return 2;
	else if (!scumm_stricmp(verStr, "Ween"))
		return 3;
	else if (!scumm_stricmp(verStr, "Bargon"))
		return 4;
	else if (!scumm_stricmp(verStr, "Lost"))
		return 5;
	else if (!scumm_stricmp(verStr, "Woodruff"))
		return 6;
	else if (!scumm_stricmp(verStr, "Dynasty"))
		return 7;
	else if (!scumm_stricmp(verStr, "Urban"))
		return 8;

	return -1;
}

byte *readFile(FILE *tot, uint32 &size) {
	size = fileSize(tot);
	byte *data = new byte[size];

	fread(data, size, 1, tot);
	return data;
}

Script *initScript(byte *totData, uint32 totSize, ExtTable *extTable, int version) {
	switch (version) {
		case 0:
			return new Script_v1(totData, totSize, extTable);
			break;
		case 1:
			return new Script_v2(totData, totSize, extTable);
			break;
		case 2:
			return new Script_v3(totData, totSize, extTable);
			break;
		case 3:
			return new Script_v2(totData, totSize, extTable);
			break;
		case 4:
			return new Script_Bargon(totData, totSize, extTable);
			break;
		case 5:
			return new Script_v3(totData, totSize, extTable);
			break;
		case 6:
			return new Script_v4(totData, totSize, extTable);
			break;
		case 7:
			return new Script_v5(totData, totSize, extTable);
			break;
		case 8:
			return new Script_v6(totData, totSize, extTable);
			break;
	}
	return 0;
}

void printInfo(Script &script) {
	printf("Version (script behaviour): %d\n", script.getVerScript());
	printf("Version (IM/EX loading): %d\n", script.getVerIMEX());
	printf("IM file suffix: %d\n", script.getSuffixIM());
	printf("EX file suffix: %d\n", script.getSuffixEX());

	printf("Game texts: ");
	if (script.getTotTextCount() == 0)
		printf("Read out of language specific files\n");
	else if (script.getTotTextCount() == 0xFFFFFFFF)
		printf("None\n");
	else
		printf("%d, directly embedded in the TOT\n", script.getTotTextCount());

	printf("Resources: ");
	if (script.getTotResOffset() != 0xFFFFFFFF)
		printf("%d, starting at 0x%08X\n", script.getTotResCount(), script.getTotResOffset());
	else
		printf("None\n");

	printf("# of variables: %d (%d bytes)\n", script.getVarsCount(), script.getVarsCount() * 4);
	printf("AnimDataSize: %d bytes\n", script.getAnimDataSize());
	printf("Text center code starts at: 0x%04X\n", script.getTextCenter());
	printf("Script code starts at: 0x%04X\n", script.getStart());
}
