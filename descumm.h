/* DeScumm - Scumm Script Disassembler
 * Copyright (C) 2001  Ludvig Strigeus
 * Copyright (C) 2002, 2003  The ScummVM Team
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

#ifndef DESCUMM_H
#define DESCUMM_H

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#include <process.h>
#endif

//
// Various utility macros
//

#define ARRAYSIZE(x) ((int)(sizeof(x) / sizeof(x[0])))

typedef unsigned char byte;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned int uint32;
typedef unsigned int uint;
typedef signed char int8;
typedef signed short int16;
typedef signed int int32;

uint32 inline SWAP_32(uint32 a)
{
	return ((a >> 24) & 0xFF) + ((a >> 8) & 0xFF00) + ((a << 8) & 0xFF0000) +
		((a << 24) & 0xFF000000);
}

uint16 inline SWAP_16(uint16 a)
{
	return ((a >> 8) & 0xFF) + ((a << 8) & 0xFF00);
}

#if defined(SCUMM_BIG_ENDIAN)
#define TO_BE_32(a) (a)
#define TO_BE_16(a) (a)
#define TO_LE_32(a) SWAP_32(a)
#define TO_LE_16(a) SWAP_16(a)
#else
#define TO_BE_32(a) SWAP_32(a)
#define TO_BE_16(a) SWAP_16(a)
#define TO_LE_32(a) (a)
#define TO_LE_16(a) (a)
#endif

#define MKID(a) (((a&0xff) << 8) | ((a >> 8)&0xff))


//
// The block stack records jump instructions
//
struct BlockStack {
	unsigned short from;	// From which offset...
	unsigned short to;		// ...to which offset
	bool isWhile;			// Set to true if we think this jump is part of a while loop
};

extern BlockStack *block_stack;
extern int num_block_stack;

//
// Jump decoding auxillaries (used by the code which tries to translate jumps
// back into if / else / while / etc. constructs).
//
extern bool pendingElse, haveElse;
extern int pendingElseTo;
extern int pendingElseOffs;
extern int pendingElseOpcode;
extern int pendingElseIndent;

//
// The opcode of an unconditional jump instruction.
//
extern int g_jump_opcode;

//
// Command line options
//
extern bool alwaysShowOffs;
extern bool dontOutputIfs;
extern bool dontOutputElse;
extern bool dontOutputElseif;
extern bool dontOutputWhile;
extern bool dontShowOpcode;
extern bool dontShowOffsets;
extern bool haltOnError;

//
// The SCUMM version used for the script we are descumming.
//
extern byte scriptVersion;

//
// Various positions / offsets
//
extern byte *cur_pos, *org_pos;
extern int offs_of_line;

//
// Total size of the currently loaded script
//
extern uint size_of_code;

//
// Common
//

extern void outputLine(const char *buf, int curoffs, int opcode, int indent);
extern bool indentBlock(unsigned int cur);

extern char *strecpy(char *buf, const char *src);
extern int get_curoffs();
extern int get_byte();
extern uint get_word();
extern int get_signed_word();

extern bool maybeAddIf(uint cur, uint to);
extern bool maybeAddElse(uint cur, uint to);
extern bool maybeAddElseIf(uint cur, uint elseto, uint to);
extern void writePendingElse();

//
// Entry points for the descumming
//
extern void next_line_V12(char *buf);	// For V1 and V2
extern void next_line_V345(char *buf);	// For V3, V4, V5
extern void next_line_V67();
extern void next_line_V8();
extern char *output;
extern bool HumongousFlag;
extern bool ZakFlag;
extern bool IndyFlag;
extern bool GF_UNBLOCKED;



#endif
