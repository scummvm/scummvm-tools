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

/* GobEngine Script disassembler */

#include <string.h>
#include <stdio.h>

#include "degob_script.h"
#include "common/file.h"
#include "common/util.h"

static void printHelp(const char *bin);
static int getVersion(const char *verStr);
static byte *readFile(const char *filename, uint32 &size);
static Script *initScript(byte *totData, uint32 totSize, ExtTable *extTable, int version);
static void printInfo(Script &script);

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

	byte *totData = 0, *extData = 0, *extComData = 0, *ideData = 0;
	uint32 totSize = 0, extSize = 0, extComSize = 0, ideSize = 0;
	int32 offset = -1;

	totData = readFile(argv[2], totSize);
	std::string ideFilename = argv[2];

	size_t extPos = ideFilename.find_last_of('.');
	if (extPos == std::string::npos)
		ideFilename += ".IDE";
	else {
		ideFilename.replace(extPos, ideFilename.length() - extPos, ".IDE");
	}

	if (Common::Filename(ideFilename).exists())
		ideData = readFile(ideFilename.c_str(), ideSize);

	ExtTable *extTable = 0;
	bool libMode = false;
	if (argc > 3) {
		int n = 3;

		if (!strncmp(argv[n], "-o", 2)) {
			char *strOffset;

			if (strlen(argv[n]) > 2)
				strOffset = argv[n] + 2;
			else if (argc <= (n + 1))
				error("No offset specified");
			else
				strOffset = argv[++n];

			long int tempOffset;

			if ((sscanf(strOffset, "%ld", &tempOffset) != 1) || (tempOffset < 0))
				error("Invalid offset specified");

			offset = (int32) tempOffset;

			n++;
		}

		if (!strncmp(argv[n], "--lib", 5)) {
			libMode = true;
			n++;
		}

		if (argc > n) {

			extData = readFile(argv[n], extSize);

			n++;

			if (argc > n) {
				extComData = readFile(argv[n], extComSize);
			}

			extTable = new ExtTable(extData, extSize, extComData, extComSize);
		}
	}

	Script *script = initScript(totData, totSize, extTable, version);
	if (!script) {
		printHelp(argv[0]);
		return -1;
	}

	if (ideData) {
		script->loadIDE(ideData);
		delete[] ideData;
	}
	printInfo(*script);
	printf("-----\n");

	script->deGob(offset, libMode);

	delete[] totData;
	delete[] extData;
	delete[] extComData;
	delete extTable;
	delete script;
	return 0;
}

void printHelp(const char *bin) {
	printf("Usage: %s <version> <file.tot> [-o <offset>] [--lib] [<file.ext>] [<commun.ext>]\n\n", bin);
	printf("The disassembled script will be written to stdout.\n\n");
	printf("Supported versions:\n");
	printf("	Gob1      - Gobliiins 1\n");
	printf("	Gob2      - Gobliins 2\n");
	printf("	Gob3      - Goblins 3\n");
	printf("	Ween      - Ween: The Prophecy\n");
	printf("	Bargon    - Bargon Attack\n");
	printf("	Fascin    - Fascination\n");
	printf("	Lost      - Lost in Time\n");
	printf("	Woodruff  - The Bizarre Adventures of Woodruff and the Schnibble\n");
	printf("	Dynasty   - The Last Dynasty\n");
	printf("	Urban     - Urban Runner\n");
	printf("	Geisha    - Geisha\n");
	printf("	LittleRed - Once Upon A Time: Little Red Riding Hood\n");
	printf("	Adibou2   - Adibou 2\n");
	printf("\n");
	printf("<file.tot>\n\tthe .tot file to decompile\n\n");
	printf("-o <offset>\n\tscript entry point in the .tot file, defaults to the value read in script header\n\n");
	printf("--lib\n\tlibrary mode: all offsets from .IDE named functions file are used as entry points\n\n");
	printf("<file.ext>\n\texternal script resource file (<script name>.EXT)\n\n");
	printf("<commun.ext>\n\texternal common script resource file (commun.EXn)\n\n");

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
	else if (!scumm_stricmp(verStr, "Fascin"))
		return 5;
	else if (!scumm_stricmp(verStr, "Lost"))
		return 6;
	else if (!scumm_stricmp(verStr, "Woodruff"))
		return 7;
	else if (!scumm_stricmp(verStr, "Dynasty"))
		return 8;
	else if (!scumm_stricmp(verStr, "Urban"))
		return 9;
	else if (!scumm_stricmp(verStr, "Geisha"))
		return 10;
	else if (!scumm_stricmp(verStr, "LittleRed"))
		return 11;
	else if (!scumm_stricmp(verStr, "Adibou2"))
		return 12;

	return -1;
}

byte *readFile(const char *filename, uint32 &size) {
	Common::File f(filename, "rb");
	if (!f.isOpen())
		error("Couldn't open file \"%s\"", filename);

	size = f.size();
	byte *data = new byte[size];
	f.read_noThrow(data, size);
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
			return new Script_Fascin(totData, totSize, extTable);
			break;
		case 6:
			return new Script_v3(totData, totSize, extTable);
			break;
		case 7:
			return new Script_v4(totData, totSize, extTable);
			break;
		case 8:
			return new Script_v5(totData, totSize, extTable);
			break;
		case 9:
			return new Script_v6(totData, totSize, extTable);
			break;
		case 10:
			return new Script_Geisha(totData, totSize, extTable);
			break;
		case 11:
			return new Script_LittleRed(totData, totSize, extTable);
			break;
		case 12:
			return new Script_v7(totData, totSize, extTable);
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
	printf("# of named functions: %d\n", script.getFuncNamesCount());
	printf("AnimDataSize: %d bytes\n", script.getAnimDataSize());
	printf("Text center code starts at: 0x%04X\n", script.getTextCenter());
	printf("Script code starts at: 0x%04X\n", script.getStart());
}
