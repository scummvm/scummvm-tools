/* DeScumm - Scumm Script Disassembler
 * Copyright (C) 2001  Ludvig Strigeus
 * Copyright (C) 2002-2006  The ScummVM Team
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "descumm.h"

#include "common/endian.h"
#include "common/util.h"

// 200kb limit on the input file (we just read it all at once into memory).
// Should be no problem, the biggest scripts I have seen were in COMI and
// went up to 180kb (script-457).
#define MAX_FILE_SIZE (200 * 1024)

void ShowHelpAndExit() {
	printf("SCUMM Script decompiler\n"
			"Syntax:\n"
			"\tdescumm [-o] filename\n"
			"Flags:\n"
			"\t-0\tInput Script is v0 / C64\n"
			"\t-1\tInput Script is v1\n"
			"\t-2\tInput Script is v2\n"
			"\t-3\tInput Script is v3\n"
			"\t-4\tInput Script is v4\n"
			"\t-5\tInput Script is v5\n"
			"\t-6\tInput Script is v6\n"
			"\t-7\tInput Script is v7\n"
			"\t-8\tInput Script is v8\n"
			"\t-gNNN\tInput Script is HE version NNN\n"
			"\t-n\tUse Indy3-256 specific hacks\n"
			"\t-z\tUse Zak256 specific hacks\n"
			"\t-u\tScript is Unblocked/has no header\n"
			"\n"
			"\t-o\tAlways Show offsets\n"
			"\t-i\tDon't output ifs\n"
			"\t-e\tDon't output else\n"
			"\t-f\tDon't output else-if\n"
			"\t-w\tDon't output while\n"
			"\t-b\tDon't output breaks\n"
			"\t-c\tDon't show opcode\n"
			"\t-x\tDon't show offsets\n"
			"\t-h\tHalt on error\n");
	exit(0);
}

int skipVerbHeader_V12(byte *p) {
	byte code;
	int offset = 15;
	int minOffset = 255;

	if (g_options.scriptVersion == 0)
		offset = 14;
	p += offset;

	printf("Events:\n");

	while ((code = *p++) != 0) {
		offset = *p++;
		printf("  %2X - %.4X\n", code, offset);
		if (minOffset > offset)
			minOffset = offset;
	}
	return minOffset;
}

int skipVerbHeader_V34(byte *p) {
	byte code;
	int offset = g_options.GF_UNBLOCKED ? 17 : 19;
	int minOffset = 255;
	p += offset;

	printf("Events:\n");

	while ((code = *p++) != 0) {
		offset = READ_LE_UINT16(p);
		p += 2;
		printf("  %2X - %.4X\n", code, offset);
		if (minOffset > offset)
			minOffset = offset;
	}
	return minOffset;
}

int skipVerbHeader_V567(byte *p) {
	byte code;
	int offset = 8;
	int minOffset = 255;
	p += offset;

	printf("Events:\n");

	while ((code = *p++) != 0) {
		offset = READ_LE_UINT16(p);
		p += 2;
		printf("  %2X - %.4X\n", code, offset);
		if (minOffset > offset)
			minOffset = offset;
	}
	return minOffset;
}

int skipVerbHeader_V8(byte *p) {
	uint32 *ptr;
	uint32 code;
	int offset;
	int minOffset = 255;

	ptr = (uint32 *)p;
	while ((code = READ_LE_UINT32(ptr++)) != 0) {
		offset = READ_LE_UINT32(ptr++);
		printf("  %2d - %.4X\n", code, offset);
		if (minOffset > offset)
			minOffset = offset;
	}
	return minOffset;
}

char *parseCommandLine(int argc, char *argv[]) {
	char *filename = NULL;
	int i;
	char *s;
	for (i = 1; i < argc; i++) {
		s = argv[i];

		if (s && s[0] == '-') {
			s++;
			while (*s) {
				switch (tolower(*s)) {

				case '0':
					g_options.scriptVersion = 0;
					g_jump_opcode = 0x18;
					g_options.GF_UNBLOCKED = true;
					break;
				case '1':
					g_options.scriptVersion = 1;
					g_jump_opcode = 0x18;
					g_options.GF_UNBLOCKED = true;
					break;
				case '2':
					g_options.scriptVersion = 2;
					g_jump_opcode = 0x18;
					g_options.GF_UNBLOCKED = true;
					break;
				case '3':
					g_options.scriptVersion = 3;
					g_jump_opcode = 0x18;
					break;
				case '4':
					g_options.scriptVersion = 4;
					g_jump_opcode = 0x18;
					break;
				case '5':
					g_options.scriptVersion = 5;
					g_jump_opcode = 0x18;
					break;
				case 'n':
					g_options.IndyFlag = 1; // Indy3
					g_options.scriptVersion = 3;
					g_jump_opcode = 0x18;
					break;
				case 'z':
					g_options.ZakFlag = 1; // Zak
					g_options.scriptVersion = 3;
					g_jump_opcode = 0x18;
					break;
				case 'u':
					g_options.GF_UNBLOCKED = true;
					break;

				case '6':
					g_options.scriptVersion = 6;
					g_jump_opcode = 0x73;
					break;
				case '7':
					g_options.scriptVersion = 7;
					g_jump_opcode = 0x73;
					break;
				case '8':
					g_options.scriptVersion = 8;
					g_jump_opcode = 0x66;
					break;

				case 'g':
					g_options.heVersion = atoi(s + 1);
					g_options.scriptVersion = 6;
					g_jump_opcode = 0x73;

					// Skip three digits for HE version
					s += 3;
					break;

				case 'o':
					g_options.alwaysShowOffs = true;
					break;
				case 'i':
					g_options.dontOutputIfs = true;
					break;
				case 'e':
					g_options.dontOutputElse = true;
					break;
				case 'f':
					g_options.dontOutputElseif = true;
					break;
				case 'w':
					g_options.dontOutputWhile = true;
					break;
				case 'b':
					g_options.dontOutputBreaks = true;
					break;
				case 'c':
					g_options.dontShowOpcode = true;
					break;
				case 'x':
					g_options.dontShowOffsets = true;
					break;
				case 'h':
					g_options.haltOnError = true;
					break;
				default:
					ShowHelpAndExit();
				}
				s++;
			}
		} else {
			if (filename)
				ShowHelpAndExit();
			filename = s;
		}
	}

	return filename;
}

void parseHeader() {
	if (g_options.GF_UNBLOCKED) {
		if (g_scriptSize < 4) {
			error("File too small to be a script");
		}
		// Hack to detect verb script: first 4 bytes should be file length
		if (READ_LE_UINT32(g_scriptStart) == g_scriptSize) {
			if (g_options.scriptVersion <= 2)
				currentOpcodeBlockStart = skipVerbHeader_V12(g_scriptStart);
			else
				currentOpcodeBlockStart = skipVerbHeader_V34(g_scriptStart);
		} else {
			g_scriptStart += 4;
		}
	} else if (g_options.scriptVersion >= 5) {
		if (g_scriptSize < (uint)(g_options.scriptVersion == 5 ? 8 : 9)) {
			error("File too small to be a script");
		}

		switch (READ_BE_UINT32(g_scriptStart)) {
		case 'LSC2':
			if (g_scriptSize <= 12) {
				printf("File too small to be a local script\n");
			}
			printf("Script# %d\n", READ_LE_UINT32(g_scriptStart+8));
			g_scriptStart += 12;
			break;											/* Local script */
		case 'LSCR':
			if (g_options.scriptVersion == 8) {
				if (g_scriptSize <= 12) {
					printf("File too small to be a local script\n");
				}
				printf("Script# %d\n", READ_LE_UINT32(g_scriptStart+8));
				g_scriptStart += 12;
			} else if (g_options.scriptVersion == 7) {
				if (g_scriptSize <= 10) {
					printf("File too small to be a local script\n");
				}
				printf("Script# %d\n", READ_LE_UINT16(g_scriptStart+8));
				g_scriptStart += 10;
			} else {
				if (g_scriptSize <= 9) {
					printf("File too small to be a local script\n");
				}
				printf("Script# %d\n", (byte)g_scriptStart[8]);
				g_scriptStart += 9;
			}
			break;											/* Local script */
		case 'SCRP':
			g_scriptStart += 8;
			break;											/* Script */
		case 'ENCD':
			g_scriptStart += 8;
			break;											/* Entry code */
		case 'EXCD':
			g_scriptStart += 8;
			break;											/* Exit code */
		case 'VERB':
			if (g_options.scriptVersion == 8) {
				g_scriptStart += 8;
				currentOpcodeBlockStart = skipVerbHeader_V8(g_scriptStart);
			} else
				currentOpcodeBlockStart = skipVerbHeader_V567(g_scriptStart);
			break;											/* Verb */
		default:
			error("Unknown script type");
		}
	} else {
		if (g_scriptSize < 6) {
			error("File too small to be a script");
		}
		switch (READ_BE_UINT16(g_scriptStart + 4)) {
		case 'LS':
			printf("Script# %d\n", (byte)g_scriptStart[6]);
			g_scriptStart += 7;
			break;			/* Local script */
		case 'SC':
			g_scriptStart += 6;
			break;			/* Script */
		case 'EN':
			g_scriptStart += 6;
			break;			/* Entry code */
		case 'EX':
			g_scriptStart += 6;
			break;			/* Exit code */
		case 'OC':
			currentOpcodeBlockStart = skipVerbHeader_V34(g_scriptStart);
			break;			/* Verb */
		default:
			error("Unknown script type");
		}
	}
}

int main(int argc, char *argv[]) {
	FILE *in;
	byte *fileBuffer;
	char *filename;

	memset(&g_options, 0, sizeof(g_options));
	g_options.scriptVersion = 0xff;

	// Parse the arguments
	filename = parseCommandLine(argc, argv);
	if (!filename || g_options.scriptVersion == 0xff)
		ShowHelpAndExit();

	in = fopen(filename, "rb");
	if (!in) {
		printf("Unable to open %s\n", filename);
		return 1;
	}

	// Read the file into memory
	fileBuffer = (byte *)malloc(MAX_FILE_SIZE);
	g_scriptSize = fread(fileBuffer, 1, MAX_FILE_SIZE, in);
	fclose(in);

	g_scriptStart = fileBuffer;
	g_scriptCurPos = g_scriptStart;
	currentOpcodeBlockStart = 0;

	// Read (and skip over) the file header
	parseHeader();
	g_scriptCurPos = g_scriptStart + currentOpcodeBlockStart;

	while (g_scriptCurPos < g_scriptSize + fileBuffer) {
		byte opcode = *g_scriptCurPos;
		int j = g_blockStack.size();
		char outputLineBuffer[8192] = "";

		switch (g_options.scriptVersion) {
		case 0:
			next_line_V0(outputLineBuffer);
			break;
		case 1:
		case 2:
			next_line_V12(outputLineBuffer);
			break;
		case 3:
		case 4:
		case 5:
			next_line_V345(outputLineBuffer);
			break;
		case 6:
			if (g_options.heVersion >= 100)
				next_line_HE_V100(outputLineBuffer);
			else if (g_options.heVersion >= 72)
				next_line_HE_V72(outputLineBuffer);
			else
				next_line_V67(outputLineBuffer);
			break;
		case 7:
			next_line_V67(outputLineBuffer);
			break;
		case 8:
			next_line_V8(outputLineBuffer);
			break;
		}
		if (outputLineBuffer[0]) {
			writePendingElse();
			if (haveElse) {
				haveElse = false;
				j--;
			}
			outputLine(outputLineBuffer, currentOpcodeBlockStart, opcode, j);
			currentOpcodeBlockStart = get_curoffs();
		}
		while (!g_blockStack.empty() && get_curoffs() >= (int)g_blockStack.top().to) {
			g_blockStack.pop();
			outputLine("}", currentOpcodeBlockStart, -1, g_blockStack.size());
			currentOpcodeBlockStart = get_curoffs();
		}
		fflush(stdout);
	}

	printf("END\n");

/*
	if (g_options.scriptVersion >= 6 && num_stack != 0) {
		printf("Stack count: %d\n", num_stack);
		if (num_stack > 0) {
			printf("Stack contents:\n");
			while (num_stack) {
				outputLineBuffer[0] = 0;
				se_astext(pop(), outputLineBuffer);
				printf("%s\n", outputLineBuffer);
			}
		}
	}
*/
	free(fileBuffer);

	return 0;
}
