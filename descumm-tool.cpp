/* DeScumm - Scumm Script Disassembler
 * Copyright (C) 2001  Ludvig Strigeus
 * Copyright (C) 2002-2005  The ScummVM Team
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

#include "descumm.h"

void ShowHelpAndExit()
{
	printf("SCUMM Script decompiler\n"
			"Syntax:\n"
			"\tdescumm [-o] filename\n"
			"Flags:\n"
		    "\t-0\tInput Script is C64\n"
			"\t-1\tInput Script is v1\n"
			"\t-2\tInput Script is v2\n"
			"\t-3\tInput Script is v3\n"
			"\t-4\tInput Script is v4\n"
			"\t-5\tInput Script is v5\n"
			"\t-6\tInput Script is v6\n"
			"\t-7\tInput Script is v7\n"
			"\t-8\tInput Script is v8\n"
			"\t-p\tInput Script is from Humongous Entertainment game\n"
			"\t-n\tUse Indy3-256 specific hacks\n"
			"\t-z\tUse Zak256 specific hacks\n"
			"\t-u\tScript is Unblocked/has no header\n"
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

int skipVerbHeader_V12(byte *p)
{
	byte code;
	int offset = 15;
	int minOffset = 255;
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

int skipVerbHeader_V34(byte *p)
{
	byte code;
	int offset = GF_UNBLOCKED ? 17 : 19;
	int minOffset = 255;
	p += offset;
	
	printf("Events:\n");

	while ((code = *p++) != 0) {
		offset = TO_LE_16(*(uint16 *)p);
		p += 2;
		printf("  %2X - %.4X\n", code, offset);
		if (minOffset > offset)
			minOffset = offset;
	}
	return minOffset;
}

int skipVerbHeader_V567(byte *p)
{
	byte code;
	int offset = 8;
	int minOffset = 255;
	p += offset;

	printf("Events:\n");

	while ((code = *p++) != 0) {
		offset = TO_LE_16(*(uint16 *)p);
		p += 2;
		printf("  %2X - %.4X\n", code, offset);
		if (minOffset > offset)
			minOffset = offset;
	}
	return minOffset;
}

int skipVerbHeader_V8(byte *p)
{
	uint32 *ptr;
	uint32 code;
	int offset;
	int minOffset = 255;
	
	ptr = (uint32 *)p;
	while ((code = TO_LE_32(*ptr++)) != 0) {
		offset = TO_LE_32(*ptr++);
		printf("  %2d - %.4X\n", code, offset);
		if (minOffset > offset)
			minOffset = offset;
	}
	return minOffset;
}

int main(int argc, char *argv[])
{
	FILE *in;
	byte *mem, *memorg;
	int len;
	char *filename, *buf;
	int i;
	char *s;

	scriptVersion = 0xff;
	heVersion = 0;
	
	// Parse the arguments
	filename = NULL;
	for (i = 1; i < argc; i++) {
		s = argv[i];

		if (s && s[0] == '-') {
			s++;
			while (*s) {
				switch (tolower(*s)) {

				case '0':
					scriptVersion = 0;
					g_jump_opcode = 0x18;
					GF_UNBLOCKED = true;
					break;
				case '1':
					scriptVersion = 1;
					g_jump_opcode = 0x18;
					GF_UNBLOCKED = true;
					break;
				case '2':
					scriptVersion = 2;
					g_jump_opcode = 0x18;
					GF_UNBLOCKED = true;
					break;
				case '3':
					scriptVersion = 3;
					g_jump_opcode = 0x18;
					break;
				case '4':
					scriptVersion = 4;
					g_jump_opcode = 0x18;
					break;
				case '5':
					scriptVersion = 5;
					g_jump_opcode = 0x18;
					break;
				case 'n':
					IndyFlag = 1; // Indy3
					scriptVersion = 3;
					g_jump_opcode = 0x18;
					break;
				case 'z':
					ZakFlag = 1; // Zak
					scriptVersion = 3;
					g_jump_opcode = 0x18;
					break;
				case 'u':
					GF_UNBLOCKED = true;
					break;

				case 'p':
					HumongousFlag = true;
					// Fall through
				case '6':
					scriptVersion = 6;
					g_jump_opcode = 0x73;
					break;
				case '7':
					scriptVersion = 7;
					g_jump_opcode = 0x73;
					break;
				case '8':
					scriptVersion = 8;
					g_jump_opcode = 0x66;
					break;

				case '9':
					heVersion = 72;
					scriptVersion = 6;
					g_jump_opcode = 0x73;
					break;

				case 'o':
					alwaysShowOffs = true;
					break;
				case 'i':
					dontOutputIfs = true;
					break;
				case 'e':
					dontOutputElse = true;
					break;
				case 'f':
					dontOutputElseif = true;
					break;
				case 'w':
					dontOutputWhile = true;
					break;
				case 'b':
					dontOutputBreaks = true;
					break;
				case 'c':
					dontShowOpcode = true;
					break;
				case 'x':
					dontShowOffsets = true;
					break;
				case 'h':
					haltOnError = true;
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

	if (!filename || scriptVersion == 0xff)
		ShowHelpAndExit();

	in = fopen(filename, "rb");
	if (!in) {
		printf("Unable to open %s\n", filename);
		return 1;
	}

	memorg = mem = (byte *)malloc(MAX_FILE_SIZE);
	len = fread(mem, 1, MAX_FILE_SIZE, in);
	fclose(in);
	size_of_code = len;

	buf = (char *)malloc(8192);

	offs_of_line = 0;

	if (GF_UNBLOCKED) {
		if (size_of_code < 4) {
			printf("File too small to be a script\n");
			return 1;
		}
		// Hack to detect verb script: first 4 bytes should be file length
		if (TO_LE_32(*((uint32 *)mem)) == size_of_code) {
			if (scriptVersion <= 2)
				offs_of_line = skipVerbHeader_V12(mem);
			else
				offs_of_line = skipVerbHeader_V34(mem );
		} else {
			mem += 4;
		}
	} else if (scriptVersion >= 5) {
		if (size_of_code < (scriptVersion == 5 ? 8 : 9)) {
			printf("File too small to be a script\n");
			return 1;
		}
	
		switch (TO_BE_32(*((uint32 *)mem))) {
		case 'LSC2':
			printf("Script# %d\n", TO_LE_32(*((int32 *)(mem+8))));
			mem += 12;
			break;											/* Local script */
		case 'LSCR':
		case 'LSC2':
			if (size_of_code < 13) {
				printf("File too small to be a local script\n");
			}
			printf("Script# %d\n", TO_LE_32(*((int32 *)(mem+8))));
			mem += 12;
			break;											/* Local script */
		case 'LSCR':
			if (scriptVersion == 8) {
				if (size_of_code < 13) {
					printf("File too small to be a local script\n");
				}
				printf("Script# %d\n", TO_LE_32(*((int32 *)(mem+8))));
				mem += 12;
			} else if (scriptVersion == 7) {
				if (size_of_code < 11) {
					printf("File too small to be a local script\n");
				}
				printf("Script# %d\n", TO_LE_16(*((int16 *)(mem+8))));
				mem += 10;
			} else {
				if (size_of_code < 10) {
					printf("File too small to be a local script\n");
 				}
				printf("Script# %d\n", (byte)mem[8]);
				mem += 9;
			}
			break;											/* Local script */
		case 'SCRP':
			mem += 8;
			break;											/* Script */
		case 'ENCD':
			mem += 8;
			break;											/* Entry code */
		case 'EXCD':
			mem += 8;
			break;											/* Exit code */
		case 'VERB':
			if (scriptVersion == 8) {
				mem += 8;
				offs_of_line = skipVerbHeader_V8(mem);
			} else
				offs_of_line = skipVerbHeader_V567(mem);
			break;											/* Verb */
		default:
			printf("Unknown script type!\n");
			return 1;
		}
	} else {
		if (size_of_code < 6) {
			printf("File too small to be a script\n");
			return 1;
		}
		switch (TO_LE_16(*((uint16 *)mem + 2))) {
		case MKID('LS'):
			printf("Script# %d\n", (byte)mem[6]);
			mem += 7;
			break;			/* Local script */
		case MKID('SC'):
			mem += 6;
			break;			/* Script */
		case MKID('EN'):
			mem += 6;
			break;			/* Entry code */
		case MKID('EX'):
			mem += 6;
			break;			/* Exit code */
		case MKID('OC'):
			offs_of_line = skipVerbHeader_V34(mem);
			break;			/* Verb */
		default:
			printf("Unknown script type!\n");
			return 1;
		}
	}

	org_pos = mem;
	cur_pos = org_pos + offs_of_line;
	len -= mem - memorg;

	while (cur_pos < mem + len) {
		byte opcode = *cur_pos;
		int j = num_block_stack;
		buf[0] = 0;
		switch (scriptVersion) {
		case 0:
			next_line_V0(buf);
			break;
		case 1:
		case 2:
			next_line_V12(buf);
			break;
		case 3:
		case 4:
		case 5:
			next_line_V345(buf);
			break;
		case 6:
			if (heVersion)
				next_line_HE_V72(buf);
			else
				next_line_V67(buf);
			break;
		case 7:
			next_line_V67(buf);
			break;
		case 8:
			next_line_V8(buf);
			break;
		}
		if (buf[0]) {
			writePendingElse();
			if (haveElse) {
				haveElse = false;
				j--;
			}
			outputLine(buf, offs_of_line, opcode, j);
			offs_of_line = get_curoffs();
		}
		while (indentBlock(get_curoffs())) {
			outputLine("}", offs_of_line, -1, -1);
			offs_of_line = get_curoffs();
		}
		fflush(stdout);
	}

	printf("END\n");
	
/*
	if (scriptVersion >= 6 && num_stack != 0) {
		printf("Stack count: %d\n", num_stack);
		if (num_stack > 0) {
			printf("Stack contents:\n");
			while (num_stack) {
				buf[0] = 0;
				se_astext(pop(), buf);
				printf("%s\n", buf);
			}
		}
	}
*/
	free(memorg);

	return 0;
}
