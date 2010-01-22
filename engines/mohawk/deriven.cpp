/* deriven - Riven script decompiler
 * Copyright (C) 2009 The ScummVM project
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

#include "engines/mohawk/archive.h"
#include "util.h"
#include "utils/file.h"

#include <assert.h>

#define NO_OP "empty"

static const char *opcode_names[] = {
	// 0x00 (0 decimal)
	NO_OP,
	"drawBitmap",
	"switchCard",
	"playSound_Mix",
	// 0x04 (4 decimal)
	"playSound",
	NO_OP,
	NO_OP,
	"setVariable",
	// 0x08 (8 decimal)
	"mohawkSwitch",
	"enableHotspot",
	"disableHotspot",
	NO_OP,
	// 0x0C (12 decimal)
	"clearSLST",
	"changeCursor",
	"delay",
	NO_OP,
	// 0x10 (16 decimal)
	NO_OP,
	"runExternalCommand",
	"transition",
	"refreshCard",
	// 0x14 (20 decimal)
	"disableScreenUpdate",
	"enableScreenUpdate",
	NO_OP,
	NO_OP,
	// 0x18 (24 decimal)
	"incrementVariable",
	NO_OP,
	NO_OP,
	"changeStack",
	// 0x1C (28 decimal)
	"disableMovie",
	"disableAllMovies",
	NO_OP,
	"enableMovie",
	// 0x20 (32 decimal)
	"playMovie",
	"playMovieBg",
	"stopMovie",
	NO_OP,
	// 0x24 (36 decimal)
	"unk_36",						// Unknown
	"fadeAmbientSounds",
	"complexPlayMovie",
	"activatePLST",
	// 0x28 (40 decimal)
	"activateSLST",
	"activateMLSTAndPlay",
	NO_OP,
	"activateBLST",
	// 0x2C (44 decimal)
	"activateFLST",
	"zipMode",
	"activateMLST",
	"activateSLSTWithVolume"
};

void printUsage(const char *appName) {
	printf("Usage: %s <mohawk archive> [CARD or HSPT] [id]\n", appName);
}

Common::StringList getNameList(MohawkArchive *mohawkArchive, uint16 id) {
	MohawkOutputStream nameResource = mohawkArchive->getRawData(ID_NAME, id);
	Common::StringList nameList;

	uint16 namesCount = nameResource.stream->readUint16BE();
	uint16 *stringOffsets = new uint16[namesCount];
	for (uint16 i = 0; i < namesCount; i++)
		stringOffsets[i] = nameResource.stream->readUint16BE();
	nameResource.stream->seek(namesCount * 2, SEEK_CUR);
	int32 curNamesPos = nameResource.stream->pos();

	for (uint32 i = 0; i < namesCount; i++) {
		nameResource.stream->seek(curNamesPos + stringOffsets[i]);

		Common::String name;
		name.clear();
		for (char c = nameResource.stream->readByte(); c; c = nameResource.stream->readByte())
			name += c;
		nameList.push_back(name);
	}

	delete nameResource.stream;
	delete[] stringOffsets;

	return nameList;
}

static void printTabs(byte tabs) {
	for (byte i = 0; i < tabs; i++)
		printf ("\t");
}

void dumpCommands(Common::SeekableReadStream *script, Common::StringList varNames, Common::StringList exNames, byte tabs) {
	uint16 commandCount = script->readUint16BE();

	for (uint16 i = 0; i < commandCount; i++) {
		uint16 command = script->readUint16BE();

		if (command == 8) { // "Switch" Statement
			if (script->readUint16BE() != 2)
				warning ("if-then-else unknown value is not 2");
			uint16 var = script->readUint16BE();
			printTabs(tabs); printf("switch (%s) {\n", varNames[var].c_str());
			uint16 logicBlockCount = script->readUint16BE();
			for (uint16 j = 0; j < logicBlockCount; j++) {
				uint16 varCheck = script->readUint16BE();
				printTabs(tabs + 1);
				if (varCheck == 0xFFFF)
					printf("default:\n");
				else
					printf("case %d:\n", varCheck);
				dumpCommands(script, varNames, exNames, tabs + 2);
				printTabs(tabs + 2); printf("break;\n");
			}
			printTabs(tabs); printf("}\n");
		} else if (command == 7) { // Use the variable name
			script->readUint16BE(); // Skip the opcode var count
			printTabs(tabs);
			uint16 varIndex = script->readUint16BE();
			printf("%s = %d;\n", varNames[varIndex].c_str(), script->readUint16BE());
		} else if (command == 17) { // Use the external command name
			script->readUint16BE(); // Skip the opcode var count
			printTabs(tabs);
			printf("%s(", exNames[script->readUint16BE()].c_str());
			uint16 varCount = script->readUint16BE();
			for (uint16 j = 0; j < varCount; j++) {
				printf("%d", script->readUint16BE());
				if (j != varCount - 1)
					printf(", ");
			}
			printf (");\n");
		} else if (command == 24) { // Use the variable name
			script->readUint16BE(); // Skip the opcode var count
			printTabs(tabs);
			uint16 varIndex = script->readUint16BE();
			printf ("%s += %d;\n", varNames[varIndex].c_str(), script->readUint16BE());
		} else {
			printTabs(tabs);
			uint16 varCount = script->readUint16BE();
			printf("%s(", opcode_names[command]);
			for (uint16 j = 0; j < varCount; j++) {
				printf("%d", script->readUint16BE());
				if (j != varCount - 1)
					printf(", ");
			}
			printf(");\n");
		}
	}
}

void dumpScript(Common::SeekableReadStream *script, Common::StringList varNames, Common::StringList exNames, byte tabs) {
	uint16 scriptCount = script->readUint16BE();

	for (uint16 i = 0; i < scriptCount; i++) {
		printTabs(tabs); printf ("Stream Type %d:\n", script->readUint16BE());
		dumpCommands(script, varNames, exNames, tabs + 1);
	}
}

int main(int argc, char *argv[]) {
	if (argc != 4) {
		printUsage(argv[0]);
		return 1;
	}

	FILE *file = fopen(argv[1], "rb");
	if (!file) {
		printf ("Could not open \'%s\'\n", argv[1]);
		return 1;
	}

	// Open the file as a Mohawk archive
	MohawkArchive *mohawkArchive = new MohawkArchive();
	mohawkArchive->open(new Common::File(file));

	// Load in Variable/External Command Names'
	Common::StringList exNames = getNameList(mohawkArchive, 3);
	Common::StringList varNames = getNameList(mohawkArchive, 4);

	uint32 tag = READ_BE_UINT32(argv[2]);
	uint32 cardId = (uint16)atoi(argv[3]);

	if (tag == ID_CARD) {
		printf("\n\nDumping scripts for card %d!\n", cardId);
		printf("==================================\n\n");

		MohawkOutputStream cardStream = mohawkArchive->getRawData(ID_CARD, cardId);
		cardStream.stream->readUint32BE(); // Skip first 4 bytes

		dumpScript(cardStream.stream, varNames, exNames, 0);

		delete cardStream.stream;
	} else if (tag == ID_HSPT) {
		printf("\n\nDumping scripts for card %d hotspots!\n", cardId);
		printf("===========================================\n\n");

		MohawkOutputStream hsptStream = mohawkArchive->getRawData(ID_HSPT, cardId);
		uint16 hotspotCount = hsptStream.stream->readUint16BE();

		for (uint16 i = 0; i < hotspotCount; i++) {
			printf("Hotspot %d:\n", i);
			hsptStream.stream->seek(22, SEEK_CUR); // Skip non-script related stuff
			dumpScript(hsptStream.stream, varNames, exNames, 1);
		}

		delete hsptStream.stream;
	} else {
		printf("That resource (if it exists) doesn't contain script data!\n");
	}

	exNames.clear();
	varNames.clear();

	return 0;
}
