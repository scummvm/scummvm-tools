/* DeScumm - Scumm Script Disassembler
 * Copyright (C) 2001  Ludvig Strigeus
 * Copyright (C) 2002-2004 The ScummVM project
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

#include "util.h"

typedef unsigned int uint;

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
extern int get_word();

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
