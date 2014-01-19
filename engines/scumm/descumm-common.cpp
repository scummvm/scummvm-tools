/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
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

/* Scumm Script Disassembler (common code) */

#include <string.h>
#include <stdio.h>

#include "descumm.h"

#include "common/endian.h"

BlockStack g_blockStack;

bool pendingElse, haveElse;
int pendingElseTo;
int pendingElseOffs;
int pendingElseOpcode;
int pendingElseIndent;

int g_jump_opcode;

Options g_options;

byte *g_scriptCurPos, *g_scriptStart;
int currentOpcodeBlockStart;

uint g_scriptSize;


///////////////////////////////////////////////////////////////////////////

char *strecpy(char *buf, const char *src) {
	strcpy(buf, src);
	return strchr(buf, 0);
}

int get_curoffs() {
	return g_scriptCurPos - g_scriptStart;
}

int get_byte() {
	return (byte)(*g_scriptCurPos++);
}

int get_word() {
	int i;

	if (g_options.scriptVersion == 8) {
		i = (int32)READ_LE_UINT32(g_scriptCurPos);
		g_scriptCurPos += 4;
	} else {
		i = (int16)READ_LE_UINT16(g_scriptCurPos);
		g_scriptCurPos += 2;
	}
	return i;
}

int get_dword() {
	int i;

	i = (int32)READ_LE_UINT32(g_scriptCurPos);
	g_scriptCurPos += 4;
	return i;
}


///////////////////////////////////////////////////////////////////////////

void outputLine(const char *buf, int curoffs, int opcode, int indent) {

	if (buf[0]) {
		assert(curoffs >= 0);
		assert(indent >= 0);

		// Show the offset
		if (!g_options.dontShowOffsets) {
			printf("[%.4X] ", curoffs);
		}

		// Show the opcode value
		if (!g_options.dontShowOpcode) {
			if (opcode != -1)
				printf("(%.2X) ", opcode);
			else
				printf("(**) ");
		}

		// Indent the line as requested ...
		for (int i = 0; i < indent; ++i)
			printf("  ");

		// ... and finally print the actual code
		puts(buf);
	}
}

///////////////////////////////////////////////////////////////////////////

// Returns 0 or 1 depending if it's ok to add a block
bool maybeAddIf(uint cur, uint to) {
	Block p;
	int i;

	if (((to | cur) >> 24) || (to <= cur))
		return false; // Invalid jump

	for (i = 0; i < g_blockStack.size(); ++i) {
		if (to > g_blockStack[i].to)
			return false;
	}

	// Try to determine if this is a while loop. For this, first check if we
	// jump right behind a regular jump, then whether that jump is targeting us.
	if (g_options.scriptVersion == 8) {
		p.isWhile = (*(byte*)(g_scriptStart+to-5) == g_jump_opcode);
		i = (int32)READ_LE_UINT32(g_scriptStart+to-4);
	} else {
		p.isWhile = (*(byte*)(g_scriptStart+to-3) == g_jump_opcode);
		i = (int16)READ_LE_UINT16(g_scriptStart+to-2);
	}

	p.isWhile = p.isWhile && (currentOpcodeBlockStart == (int)to + i);
	p.from = cur;
	p.to = to;

	g_blockStack.push(p);

	return true;
}

// Returns 0 or 1 depending if it's ok to add an else
bool maybeAddElse(uint cur, uint to) {
	int i;

	if (((to | cur) >> 16) || (to <= cur))
		return false;								/* Invalid jump */

	if (g_blockStack.empty())
		return false;								/* There are no previous blocks, so an else is not ok */

	if (cur != g_blockStack.top().to)
		return false;								/* We have no prevoius if that is exiting right at the end of this goto */

	// Don't jump out of previous blocks. In addition, don't jump "onto"
	// the end of a while loop, as that would lead to incorrect output.
	// This test is stronger than the one in maybeAddIf.
	for (i = 0; i < g_blockStack.size() - 1; ++i) {
		if (to > g_blockStack[i].to || (to == g_blockStack[i].to && g_blockStack[i].isWhile))
			return false;
	}

	Block tmp = g_blockStack.pop();
	if (maybeAddIf(cur, to))
		return true;								/* We can add an else */
	g_blockStack.push(tmp);
	return false;									/* An else is not OK here :( */
}

bool maybeAddElseIf(uint cur, uint elseto, uint to) {
	uint k;

	if (((to | cur | elseto) >> 16) || (elseto < to) || (to <= cur))
		return false;								/* Invalid jump */

	if (g_blockStack.empty())
		return false;								/* There are no previous blocks, so an ifelse is not ok */

	if (g_blockStack.top().isWhile)
		return false;

	if (g_options.scriptVersion == 8)
		k = to - 5;
	else
		k = to - 3;

	if (k >= g_scriptSize)
		return false;								/* Invalid jump */

	if (elseto != to) {
		if (g_scriptStart[k] != g_jump_opcode)
			return false;							/* Invalid jump */

		if (g_options.scriptVersion == 8)
			k = to + READ_LE_UINT32(g_scriptStart + k + 1);
		else
			k = to + READ_LE_UINT16(g_scriptStart + k + 1);

		if (k != elseto)
			return false;							/* Not an ifelse */
	}
	g_blockStack.top().from = cur;
	g_blockStack.top().to = to;

	return true;
}

bool maybeAddBreak(uint cur, uint to) {
	if (((to | cur) >> 16) || (to <= cur))
		return false;								/* Invalid jump */

	if (g_blockStack.empty())
		return false;								/* There are no previous blocks, so a break is not ok */

	/* Find the first parent block that is a while and if we're jumping to the end of that, we use a break */
	for (int i = g_blockStack.size() - 1; i >= 0; --i) {
		if (g_blockStack[i].isWhile) {
			if (to == g_blockStack[i].to)
				return true;
			else
				return false;
		}
	}

	return false;
}

void writePendingElse() {
	if (pendingElse) {
		char buf[32];
		sprintf(buf, g_options.alwaysShowOffs ? "} else /*%.4X*/ {" : "} else {", pendingElseTo);
		outputLine(buf, currentOpcodeBlockStart, pendingElseOpcode, pendingElseIndent - 1);
		currentOpcodeBlockStart = pendingElseOffs;
		pendingElse = false;
	}
}

char *put_ascii(char *buf, int i) {
	if (i > 31 && i < 128) {
		// non-printable chars are escaped by backslashes as so: "\x00"
		// backslashes and quote marks are escaped like so: "\\" "\""
		if (i == '\\' || i == '"') {
			buf[0] = '\\';
			buf++;
		}
		buf[0] = i;
		buf[1] = 0;
		return buf + 1;
	}
	return buf + sprintf(buf, "\\x%.2X", i);
}

extern char *get_var(char *buf);
extern char *get_var6(char *buf);

char *get_string(char *buf) {
	byte cmd;
	char *e = buf;
	bool in = false;
	bool in_function = false;
	int i;

	while ((cmd = get_byte()) != 0) {
		if (cmd == 0xFF || cmd == 0xFE) {
			if (in) {
				e += sprintf(e, "\" + ");
				in = false;
			}
			in_function = true;
			i = get_byte();
			switch (i) {
			case 1: // newline
				e += sprintf(e, "newline()");
				break;
			case 2:
				e += sprintf(e, "keepText()");
				break;
			case 3:
				e += sprintf(e, "wait()");
				break;
			case 4:		// addIntToStack
				e += sprintf(e, "getInt(");
				goto addVarToStack;
			case 5:		// addVerbToStack
				e += sprintf(e, "getVerb(");
				goto addVarToStack;
			case 6:		// addNameToStack
				e += sprintf(e, "getName(");
				goto addVarToStack;
			case 7:		// addStringToStack
				e += sprintf(e, "getString(");
			addVarToStack:
				if (g_options.scriptVersion >= 6)  {
					e = get_var6(e);
				} else {
					e = get_var(e);
				}
				e += sprintf(e, ")");
				break;
			case 9:
				e += sprintf(e, "startAnim(%d)", get_word());
				break;
			case 10:
				e += sprintf(e, "sound(");
				// positions 2, 3, 6, 7 are the offset in MONSTER.SOU (LE).
				// positions 10, 11, 14, 15 are the VCTL block size (LE).
				{
					// show the voice's position in the MONSTER.SOU
				    int p = 0;
				    p += get_word();
				    g_scriptCurPos += 2; // skip the next "0xFF 0x0A"
				    p += get_word() << 2;
				    e += sprintf(e, "0x%X, ", p);

				    g_scriptCurPos += 2; // skip the next "0xFF 0x0A"

				    // show the size of the VCTL chunk/lip-synch tags
				    p = 0;
				    p += get_word();
				    g_scriptCurPos += 2; // skip the next "0xFF 0x0A"
				    p += get_word() << 2;
				    e += sprintf(e, "0x%X)", p);
				}
				break;
			case 12:
				e += sprintf(e, "setColor(%d)", get_word());
				break;
			case 13: // was unk2
				e += sprintf(e, "unknown13(%d)", get_word());
				break;
			case 14:
				e += sprintf(e, "setFont(%d)", get_word());
				break;
			case 32: // Workaround for a script bug in Indy3
			case 46: // Workaround for a script bug in Indy3
				if (g_options.scriptVersion == 3 && g_options.IndyFlag) {
					buf += sprintf(buf, "\\x%.2X", 0xE1); // should output German "sz" in-game.
					continue;
				}
				// fall-through
			default:
				e += sprintf(e, "unknown%d(%d)", i, get_word());
			}
		} else {
			if (in_function) {
				e += sprintf(e, " + ");
				in_function = false;
			}
			if (!in) {
				*e++ = '"';
				in = true;
			}
			e = put_ascii(e, cmd);
		}
	}
	if (in)
		*e++ = '"';
	*e = 0;
	return e;
}
