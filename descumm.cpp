/* DeScumm - Scumm Script Disassembler (version 2-5 scripts)
 * Copyright (C) 2001  Ludvig Strigeus
 * Copyright (C) 2002-2003  The ScummVM Team
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

#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#include <process.h>
#endif



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
#define TO_BE_16(x) (x)
#define TO_BE_32(x) (x)
#define TO_LE_16(x) SWAP_16(x)
#define TO_LE_32(x) SWAP_32(x)
#else
#define TO_BE_16(x) SWAP_16(x)
#define TO_BE_32(x) SWAP_32(x)
#define TO_LE_16(x) (x)
#define TO_LE_32(x) (x)
#endif


#define A1B (1<<0)
#define A1W (2<<0)
#define A1V (3<<0)
#define A1VARUNTIL0xFF (4<<0)
#define A1ASCII (5<<0)

#define A2B (1<<4)
#define A2W (2<<4)
#define A2V (3<<4)
#define A2VARUNTIL0xFF (4<<4)
#define A2ASCII (5<<4)

#define A3B (1<<8)
#define A3W (2<<8)
#define A3V (3<<8)
#define A3VARUNTIL0xFF (4<<8)
#define A3ASCII (5<<8)
#define A3ASC (6<<8)

#define A4B (1<<12)
#define A4W (2<<12)
#define A4V (3<<12)
#define A4VARUNTIL0xFF (4<<12)
#define A4ASCII (5<<12)
#define A4ASC (6<<12)

#define A5ASCII (5<<16)

#define ATO (1<<31)
#define ANOLASTPAREN (1<<30)
#define ANOFIRSTPAREN (1<<29)
#define ASTARTCOMMA (1<<28)
#define AVARSTORE (1<<27)
#define MKID(a) (((a&0xff) << 8) | ((a >> 8)&0xff))



void get_tok_V2(char *buf);	// For V2 (and V1?)
void get_tok(char *buf);	// For V3, V4, V5

const int g_jump_opcode = 0x18;

byte *cur_pos, *org_pos;
int curoffs;
int size_of_code;

char *indentbuf;

typedef struct block_stack {
	unsigned short from;
	unsigned short to;
} block_stack;


int NumBlockStack;
block_stack *BlockStack;
byte HaveElse;
byte PendingElse;
int PendingElseTo;
int PendingElseOffs;
int PendingElseOpcode;
int PendingElseIndent;

byte AlwaysShowOffs;
byte DontOutputIfs;
byte DontOutputElse;
byte DontOutputElseif;
byte DontShowOpcode;
byte DontShowOffsets;
byte HaltOnError;
byte ScriptVersion = 3;


int get_curpos();

bool IndyFlag = 0;
bool GF_UNBLOCKED = false;


bool emit_if(char *before, char *after);

int get_byte()
{
	return (byte)(*cur_pos++);
}

int get_word()
{
	int i = TO_LE_16(*((short *)cur_pos));
	cur_pos += 2;
	return i;
}

char *strecpy(char *buf, const char *src)
{
	strcpy(buf, src);
	return strchr(buf, 0);
}


const char *get_num_string(int i)
{
	const char *s;


	if (i & 0x8000) {							/* Bit var */
		if ((i & 0xFFF) >= 0x800)
			s = "??Bit??";
		else
			s = "Bit";
	} else if (i & 0x4000) {
		if ((i & 0xFFF) > 0x10)
			s = "??Local??";
		else
			s = "Local";
	} else {
		if ((i & 0xFFF) >= 0x320)
			s = "??Var??";
		else
			s = "Var";
	}

	if (HaltOnError && (s[0] == '?')) {
		printf("%s out of range, was %d\n", s, i & 0xFFF);
		exit(1);
	}

	return s;
}


char *get_var(char *buf)
{
	char *buf2;
	int i;

	if (ScriptVersion == 2)
		i = get_byte();
	else
		i = get_word();

	// FIXME this should be for zak256 as well
	if ((i & 0x8000) && (GF_UNBLOCKED)) 
		sprintf(buf, "Var[%d Bit %d", (i & 0x0FFF) >> 4, i & 0x000F);
	else
		sprintf(buf, "%s[%d", get_num_string(i), i & 0xFFF);

	buf2 = strchr(buf, 0);

	if (i & 0x2000) {
		int j = get_word();
		if (j & 0x2000) {
			j ^= 0x2000;
			sprintf(buf2, " + %s[%d]", get_num_string(j), j & 0xFFF);
		} else
			sprintf(buf2, " + %d", j & 0xFFF);
	}
	strcat(buf2, "]");

	return strchr(buf2, 0);

}

char *get_var_or_word(char *buf, char condition)
{
	if (condition)
		get_var(buf);
	else
		sprintf(buf, "%d", get_word());
	return strchr(buf, 0);
}

char *get_var_or_byte(char *buf, char condition)
{
	if (condition)
		get_var(buf);
	else
		sprintf(buf, "%d", get_byte());
	return strchr(buf, 0);
}

char *get_var_until_0xff(char *buf)
{
	int i;
	int j = 0;
	bool first = true;

	buf = strecpy(buf, "[");
	do {
		i = get_byte();
		if (i == 0xFF)
			break;
		if (!first)
			buf = strecpy(buf, ",");
		first = false;
		buf = get_var_or_word(buf, i & 0x80);
		j++;
		if (j > 16) {
			printf("ERROR: too many variables in argument list!\n");
			if (HaltOnError)
				exit(1);
			break;
		}
	} while (1);

	return strecpy(buf, "]");
}

char *putascii(char *buf, int i)
{
	if (i > 31 && i < 128) {
		buf[0] = i;
		buf[1] = 0;
		return buf + 1;
	}
	return buf + sprintf(buf, "^%d", i);
}

char *get_ascii(char *buf)
{
	int i;

	buf = strecpy(buf, "\"");

	do {
		i = get_byte();
		if (!i)
			break;
		buf = putascii(buf, i);
		if (i == 255) {
			buf = putascii(buf, get_byte());
			buf = putascii(buf, get_byte());
			buf = putascii(buf, get_byte());
		}
	} while (1);

	return strecpy(buf, "\"");
}



char *add_a_tok(char *buf, int type)
{
	switch (type) {
	case 1:
		buf += sprintf(buf, "%d", get_byte());
		break;
	case 2:
		buf += sprintf(buf, "%d", get_word());
		break;
	case 3:
		buf = get_var(buf);
		break;
	case 4:
		buf = get_var_until_0xff(buf);
		break;
	case 5:
		buf = get_ascii(buf);
		break;
	case 6:
		buf = putascii(buf, get_byte());
		break;
	}
	return buf;
}

char *do_tok(char *buf, const char *text, int args)
{
	char *buforg = buf;


	if (args & AVARSTORE) {
		buf = get_var(buf);
		buf = strecpy(buf, " = ");
	}

	if (text)
		buf = strecpy(buf, text);

	if (!(args & ANOFIRSTPAREN))
		buf = strecpy(buf, "(");

	if (args & ASTARTCOMMA)
		buf = strecpy(buf, ",");

	if (args & 0xF) {
		buf = add_a_tok(buf, args & 0xF);
		if (args & 0xF0) {
			buf = add_a_tok(strecpy(buf, ","), (args >> 4) & 0xF);
			if (args & 0xF00) {
				buf = add_a_tok(strecpy(buf, ","), (args >> 8) & 0xF);
				if (args & 0xF000) {
					buf = add_a_tok(strecpy(buf, ","), (args >> 12) & 0xF);
					if (args & 0xF0000)
						buf = add_a_tok(strecpy(buf, ","), (args >> 16) & 0xF);
				}
			}
		}
	}

	if (args & ATO) {
		char before[256];
		char after[256];
		char tmp[256];
		strcpy(tmp, buforg);
		emit_if(before, after);
		sprintf(buforg, "%s%s%s", before, tmp, after);
	} else if (!(args & ANOLASTPAREN)) {
		buf = strecpy(buf, ")");
	}

	return strchr(buf, 0);
}

#if 0
void output_expr_text(int offs, char *data)
{
	printf("[%.4X] (%.2X) EXPRESSION_MODE: %s\n", offs, *((byte *)(org_pos + offs)), data);
}

void output_ext_opcode(int offs, char *data)
{
	printf("[%.4X] (%.2X) ACTORSET: %s\n", offs, *(org_pos + offs), data);
}
#endif


#define INDENT_SIZE 2



char *GetIndentString(int i)
{
	char *c = indentbuf;
	i += i;
	if (!c)
		indentbuf = c = (char *)malloc(127 * INDENT_SIZE + 1);
	if (i >= 127 * INDENT_SIZE)
		i = 127 * INDENT_SIZE;
	if (i < 0)
		i = 0;
	memset(c, 32, i);
	c[i] = 0;
	return c;
}



block_stack *PushBlockStackItem()
{
	if (!BlockStack)
		BlockStack = (block_stack *) malloc(256 * sizeof(block_stack));

	if (NumBlockStack >= 256) {
		printf("BlockStack full!\n");
		exit(0);
	}
	return &BlockStack[NumBlockStack++];
}

/* Returns 0 or 1 depending if it's ok to add a block */
int RequestIfAdd(unsigned int cur, unsigned int to)
{
	int i;
	block_stack *p;

	if (((to | cur) >> 16) || (to <= cur))
		return 0;										/* Invalid jump */

	for (i = 0, p = BlockStack; i < NumBlockStack; i++, p++) {
		if (to > p->to)
			return 0;
	}

	p = PushBlockStackItem();
	p->from = cur;
	p->to = to;
	return 1;
}

int IndentBlock(unsigned int cur)
{
	block_stack *p;

	if (!NumBlockStack)
		return 0;

	p = &BlockStack[NumBlockStack - 1];
	if (cur < p->to)
		return 0;

	NumBlockStack--;
	return 1;
}

/* Returns 0 or 1 depending if it's ok to add an else */
int RequestElseAdd(int cur, int to)
{
	block_stack *p;
	int i;

	if (((to | cur) >> 16) || (to <= cur))
		return 0;										/* Invalid jump */

	if (!NumBlockStack)
		return 0;										/* There are no previous blocks, so an else is not ok */

	p = &BlockStack[NumBlockStack - 1];
	if (cur != p->to)
		return 0;										/* We have no prevoius if that is exiting right at the end of this goto */

	NumBlockStack--;
	i = RequestIfAdd(cur, to);
	if (i)
		return i;										/* We can add an else */

	NumBlockStack++;

	return 0;											/* An else is not OK here :( */
}

int RequestElseIfAdd(int cur, int elseto, int to)
{
	int k;
	block_stack *p;

	if (((to | cur | elseto) >> 16) || (elseto < to) || (to <= cur))
		return 0;										/* Invalid jump */

	if (!NumBlockStack)
		return 0;										/* There are no previous blocks, so an ifelse is not ok */


	k = to - 3;
	if (k < 0 || k >= size_of_code)
		return 0;										/* Invalid jump */

	if (org_pos[k] != g_jump_opcode)
		return 0;										/* Invalid jump */

	k = to + *((short *)(org_pos + k + 1));

	if (k != elseto)
		return 0;										/* Not an ifelse */

	p = &BlockStack[NumBlockStack - 1];
	p->from = cur;
	p->to = to;

	return 1;
}



void OutputLine(char *buf, int curoffs, int opcode, int indent)
{

	char *s;

	if (buf[0]) {
		if (indent == -1)
			indent = NumBlockStack;
		if (curoffs == -1)
			curoffs = get_curpos();

		s = GetIndentString(indent);

		if (DontShowOpcode) {
			if (DontShowOffsets)
				printf("%s%s\n", s, buf);
			else
				printf("[%.4X] %s%s\n", curoffs, s, buf);
		} else {
			char buf2[4];
			if (opcode != -1)
				sprintf(buf2, "%.2X", opcode);
			else
				strcpy(buf2, "**");
			if (DontShowOffsets)
				printf("(%s) %s%s\n", buf2, s, buf);
			else
				printf("[%.4X] (%s) %s%s\n", curoffs, buf2, s, buf);
		}
	}
}


void WritePendingElse()
{
	if (PendingElse) {
		char buf[32];
		sprintf(buf, AlwaysShowOffs ? "} else /*%.4X*/ {" : "} else {", PendingElseTo);
		OutputLine(buf, PendingElseOffs, PendingElseOpcode, PendingElseIndent - 1);
		PendingElse = 0;
	}
}

int HavePendingElse()
{
	return PendingElse;
}

void do_decodeparsestring_v2(char *buf, byte opcode)
{
	byte buffer[256];
	byte *ptr = buffer;
	byte c;

	while (( c = get_byte())) {
		if (c & 0x80) {
			*ptr++ = c & 0x7F;
			*ptr++ = ' ';
		} else if (c < 8) {
			*ptr++ = 0xFF;
			*ptr++ = c;
			if (c > 3) {
				*ptr++ = 0;
				*ptr++ = get_byte();
			}
		} else
			*ptr++ = c;
	}
	*ptr = 0;

	strcat(buf, (char *)buffer);
}

void do_actorset_v2(char *buf, byte opcode)
{
	buf = do_tok(buf, "ActorSet", ((opcode & 0x80) ? A1V : A1B) | ANOLASTPAREN);
	buf = strecpy(buf, ",[");
	int arg = get_byte();

	int subop = get_byte();

	switch(subop) {
		case 1:
			buf = do_tok(buf, "Sound", ((arg & 0x40) ? A1V : A1B));
			break;
		case 2:
			buf = do_tok(buf, "Colour", A1B | ((arg & 0x40) ? A2V : A2B));
			break;
		case 3:
			buf = do_tok(buf, "Name", A1ASCII);
			break;
		case 4:
			buf = do_tok(buf, "Costume", ((arg & 0x40) ? A1V : A1B));
			break;
		case 5:
			buf = do_tok(buf, "TalkColor", ((arg & 0x40) ? A1V : A1B));
			break;
		default:
			buf += sprintf(buf, "Unknown%.2X()", opcode);
	}
	strecpy(buf, "]);");

}

void do_actorset(char *buf, byte opcode)
{
	static const byte convertTable[20] =
		{ 1, 0, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20 };

	char first = 1;

	buf = do_tok(buf, "ActorSet", ((opcode & 0x80) ? A1V : A1B) | ANOLASTPAREN);

	buf = strecpy(buf, ",[");

	do {
		opcode = get_byte();
		if (opcode == 0xFF)
			break;
		if (!first)
			buf = strecpy(buf, ",");
		first = 0;

		// FIXME - this really should be a check for GF_SMALL_HEADER instead!
		if (ScriptVersion < 5)
			opcode = (opcode & 0xE0) | convertTable[(opcode & 0x1F) - 1];

		switch (opcode & 0x1F) {
		case 0x00:
			buf = do_tok(buf, "Unknown", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x01:
			buf = do_tok(buf, "Costume", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x02:
			buf = do_tok(buf, "WalkSpeed", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
			break;
		case 0x03:
			buf = do_tok(buf, "Sound", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x04:
			buf = do_tok(buf, "WalkAnimNr", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x05:
			buf =
				do_tok(buf, "TalkAnimNr", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
			break;
		case 0x06:
			buf = do_tok(buf, "StandAnimNr", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x07:
			buf =
				do_tok(buf, "Nothing",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
							 ((opcode & 0x20) ? A3V : A3B));
			break;
		case 0x08:
			buf = do_tok(buf, "Init", 0);
			break;
		case 0x09:
			buf = do_tok(buf, "Elevation", ((opcode & 0x80) ? A1V : A1W));
			break;
		case 0x0A:
			buf = do_tok(buf, "DefaultAnims", 0);
			break;
		case 0x0B:
			buf = do_tok(buf, "Palette", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
			break;
		case 0x0C:
			buf = do_tok(buf, "TalkColor", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x0D:
			buf = do_tok(buf, "Name", A1ASCII);
			break;
		case 0x0E:
			buf = do_tok(buf, "InitAnimNr", ((opcode & 0x80) ? A1V : A1B));
			break;
//    case 0x0F: buf=do_tok(buf, "Unk3_Set21_22to1", A1VARUNTIL0xFF); break;
		case 0x10:
			buf = do_tok(buf, "Width", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x11:
			// FIXME for GID_MONKEY_VGA / GID_MONKEY_EGA this is:
			if (ScriptVersion == 5)
				buf = do_tok(buf, "Scale", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
			else
				buf = do_tok(buf, "Scale", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x12:
			buf = do_tok(buf, "NeverZClip", 0);
			break;
		case 0x13:
			buf = do_tok(buf, "SetZClip", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x14:
			buf = do_tok(buf, "IgnoreBoxes", 0);
			break;
		case 0x15:
			buf = do_tok(buf, "FollowBoxes", 0);
			break;
		case 0x16:
			buf = do_tok(buf, "AnimSpeed", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x17:
			buf=do_tok(buf, "ShadowMode", ((opcode & 0x80) ? A1V : A1B));
			break;
		default:
			buf += sprintf(buf, "Unknown%.2X()", opcode);
		}
	} while (1);

	strecpy(buf, "]);");

}

int NumInExprStack;
char **ExprStack;

void AddToExprStack(char *s)
{
	int l;
	char *m;

	if (!ExprStack)
		ExprStack = (char **)malloc(sizeof(char *) * 256);

	if (NumInExprStack >= 256) {
		printf("Expression stack overflow!\n");
		exit(0);

	}
	m = NULL;
	l = strlen(s);
	if (l) {
		l++;
		m = (char *)malloc(l);
		memcpy(m, s, l);
	}
	ExprStack[NumInExprStack++] = m;
}

char *GetFromExprStack(char *buf)
{
	char *s;

	if (NumInExprStack <= 0) {
		printf("Expression stack is empty!\n");
		exit(0);
	}

	s = ExprStack[--NumInExprStack];
	buf = strecpy(buf, s);
	free(s);
	return buf;
}


void do_expr_code(char *buf)
{
	int i;
	char *s;
	char *buf2;

	char tmp[256];

	//j = cur_pos - org_pos - 1;
#define NEW_EXPR_MODE 1
#if NEW_EXPR_MODE
	buf = strecpy(buf, "Exprmode ");
	buf = get_var(buf);
	buf = strecpy(buf, " = ");

	NumInExprStack = 0;

	do {
		i = get_byte();
		if (i == 0xFF)
			break;
		switch (i & 0x1F) {
		case 0x1:
			get_var_or_word(buf, i & 0x80);
			AddToExprStack(buf);
			break;

		case 0x2:
			s = " + ";
			goto do_oper;

		case 0x3:
			s = " - ";
			goto do_oper;

		case 0x4:
			s = " * ";
			goto do_oper;

		case 0x5:
			s = " / ";
		do_oper:;
			buf2 = strecpy(buf, "(");
			GetFromExprStack(tmp);
			buf2 = GetFromExprStack(buf2);
			buf2 = strecpy(buf2, s);
			buf2 = strecpy(buf2, tmp);
			strecpy(buf2, ")");
			AddToExprStack(buf);
			break;

		case 0x6:
			buf2 = strecpy(buf, "<");
			if (ScriptVersion == 2)
				get_tok_V2(buf2);
			else
				get_tok(buf2);
			strecpy(strchr(buf2, 0), ">");
			AddToExprStack(buf);
			break;

		default:
			printf("Warning, Invalid expression code %.2X\n", i);
		}

	} while (1);

	strcpy(GetFromExprStack(buf), ";");
#else


	strcpy(buf, "START, DEST=");
	get_var(strchr(buf, 0));
	output_expr_text(j, buf);

	do {
		j = cur_pos - org_pos;

		i = get_byte();
		if (i == 0xFF)
			break;
		switch (i) {
		case 0x1:
		case 0x81:
			strcpy(buf, "PUSH ");
			get_var_or_word(strchr(buf, 0), i & 0x80);
			break;
		case 0x2:
			strcpy(buf, "ADD");
			break;
		case 0x3:
			strcpy(buf, "SUB");
			break;
		case 0x4:
			strcpy(buf, "MUL");
			break;
		case 0x5:
			strcpy(buf, "DIV");
			break;
		case 0x6:
			sprintf(buf, "CALL (%.2X) ", *cur_pos);
			if (ScriptVersion == 2)
				get_tok_V2(strchr(buf, 0));
			else
				get_tok(strchr(buf, 0));
			break;
		default:
			sprintf(buf, "UNKNOWN %d", i);
		}

		output_expr_text(j, buf);

	} while (1);

	output_expr_text(j, "END");
#endif
}


void do_load_code_to_string(char *buf, byte opcode)
{

	buf = strchr(strcpy(buf, "PutCodeInString("), 0);
	buf = get_var_or_byte(buf, opcode & 0x80);
	buf = strchr(strcpy(buf, ", "), 0);
	buf = get_ascii(buf);
	strcpy(buf, ");");
}

void do_resource_v2(char *buf, byte opcode)
{
	int resid = get_byte();
	int subop = get_byte();

	if (((subop & 0x0F) == 0) || ((subop & 0x0F) == 1)) {
		switch (subop & 0xF1) {
			case 96:
				do_tok(buf, "lockSound", ((resid & 0x80) ? A1V : A1B));
				break;
			case 97:
				do_tok(buf, "unlockSound", ((resid & 0x80) ? A1V : A1B));
				break;
			case 80:
				do_tok(buf, "lockScript", ((resid & 0x80) ? A1V : A1B));
				break;
			case 81:
				do_tok(buf, "unlockScript", ((resid & 0x80) ? A1V : A1B));
				break;
			case 32:
				do_tok(buf, "lockCostume", ((resid & 0x80) ? A1V : A1B));
				break;
			case 33:
				do_tok(buf, "unlockCostume", ((resid & 0x80) ? A1V : A1B));
				break;
			case 48:
				do_tok(buf, "lockRoom", ((resid & 0x80) ? A1V : A1B));
				break;
			case 49:
				do_tok(buf, "unlockRoom", ((resid & 0x80) ? A1V : A1B));
				break;
			default:
				sprintf(buf, "UnknownResLockUnlockCommand%.2X", subop & 0xF1);
		}
	} else {
		switch (subop & 0xF1) {
			case 96:
			case 80:
			case 32:
			case 48:
				break;
			case 97:
				do_tok(buf, "loadSoundRes", ((resid & 0x80) ? A1V : A1B));
				break;
			case 81:
				do_tok(buf, "loadScriptRes", ((resid & 0x80) ? A1V : A1B));
				break;
			case 33:
				do_tok(buf, "loadCostumeRes", ((resid & 0x80) ? A1V : A1B));
				break;
			case 49:
				do_tok(buf, "loadRoomRes", ((resid & 0x80) ? A1V : A1B));
				break;
			default:
				sprintf(buf, "UnknownResLoadNukeCommand%.2X", subop & 0xF1);
		}
	}
}

void do_resource(char *buf, byte opco)
{
	// FIXME:
	// 1) This is out of date compared to script_v5.cp
	// 2) the token's should all get a prefix, so that we don't mix up the
	//    "real" loadRoom with the one here.
	char opcode = get_byte();
	int sub_op;
	if (ScriptVersion != 5)
		sub_op = opcode & 0x3F;	// FIXME - actually this should only be done for Zak256
	else
		sub_op = opcode & 0x1F;

	switch (sub_op) {
	case 0x1:
		do_tok(buf, "loadScript", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x2:
		do_tok(buf, "loadSound", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x3:
		do_tok(buf, "loadCostume", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x4:
		do_tok(buf, "loadRoom", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x5:
		do_tok(buf, "nukeScript", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x6:
		do_tok(buf, "nukeSound", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x7:
		do_tok(buf, "nukeCostume", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x8:
		do_tok(buf, "nukeRoom", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x9:
		do_tok(buf, "lockScript", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0xA:
		do_tok(buf, "lockSound", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0xB:
		do_tok(buf, "lockCostume", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0xC:
		do_tok(buf, "lockRoom", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0xD:
		do_tok(buf, "unlockScript", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0xE:
		do_tok(buf, "unlockSound", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0xF:
		do_tok(buf, "unlockCostume", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x10:
		do_tok(buf, "unlockRoom", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x11:
		do_tok(buf, "clearHeap", 0);
		break;
	case 0x12:
		do_tok(buf, "loadCharset", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x13:
		do_tok(buf, "nukeCharset", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x14:
		do_tok(buf, "loadFlObject", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x22 + 1:
		do_tok(buf, "resUnk1", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x23 + 1:
		do_tok(buf, "resUnk2", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) | A3B);
		break;
	case 0x24 + 1:
		do_tok(buf, "resUnk3", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;


	default:
		do_tok(buf, "resUnk", ((opcode & 0x80) ? A1V : A1B));
		break;
	}

}

void do_cc(char *buf)
{
	int j, i = get_byte();

	buf += sprintf(buf, "PseudoRoom(%d", i);

	do {
		j = get_byte();
		if (!j)
			break;

		if (j & 128)
			buf += sprintf(buf, ",%d", j & 127);
		else
			buf = strecpy(buf, ",IG");
	} while (1);

	strcpy(buf, ")");
}

void do_room_ops(char *buf)
{
	int opcode = get_byte();

	//buf+=sprintf(buf, "SubCode33%.2X", opcode);

	switch (opcode & 0x1F) {
	case 0x01:
		do_tok(buf, "RoomScroll", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;
	case 0x02:
		do_tok(buf, "RoomColor", 0);
		break;
	case 0x03:
		do_tok(buf, "SetScreen", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;
	case 0x04:
		buf =
			do_tok(buf, "SetPalColor",
						 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W) |
						 ((opcode & 0x20) ? A3V : A3W) | ANOLASTPAREN);
		opcode = get_byte();
		buf = do_tok(buf, NULL, ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x05:
		do_tok(buf, "ShakeOn", 0);
		break;
	case 0x06:
		do_tok(buf, "ShakeOff", 0);
		break;
	case 0x07:
		do_tok(buf, "Unused", 0);
		break;
	case 0x08:
		buf =
			do_tok(buf, "roomFunc2",
						 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
						 ((opcode & 0x20) ? A3V : A3B));
		break;
	case 0x09:
		buf = do_tok(buf, "saveLoad?", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x0A:
		buf = do_tok(buf, "screenEffect?", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x0B:
		buf =
			do_tok(buf, "roomFunc2",
						 ANOLASTPAREN | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ?
																														 A2V : A2W) |
						 ((opcode & 0x20) ? A3V : A3W));
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B) |
						 ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x0C:
		buf =
			do_tok(buf, "roomFunc3",
						 ANOLASTPAREN | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ?
																														 A2V : A2W) |
						 ((opcode & 0x20) ? A3V : A3W));
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B) |
						 ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x0D:
		do_tok(buf, "roomops:13", ((opcode & 0x80) ? A1V : A1B) | A2ASCII);
		break;
	case 0x0E:
		do_tok(buf, "roomops:14", ((opcode & 0x80) ? A1V : A1B) | A2ASCII);
		break;

	case 0x0F:
		buf = do_tok(buf, "palManipulate", ANOLASTPAREN | ((opcode & 0x80) ? A1V : A1B));
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ANOLASTPAREN | ((opcode & 0x80) ?
																													 A1V : A1B) |
						 ((opcode & 0x40) ? A2V : A2B));
		opcode = get_byte();
		buf = do_tok(buf, NULL, ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x10:
		do_tok(buf, "colorCycleDelay", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	default:
		strcpy(buf, "Unknown??");
	}

	strcat(buf, ")");
}

void do_room_ops_old(char *buf, byte master_opcode)
{
	/* FIXME - this function is not complete yet! */
	char	a[256];
	char	b[256];
	
	if (ScriptVersion == 2) {
		get_var_or_byte(a, (master_opcode & 0x80));
		get_var_or_byte(b, (master_opcode & 0x40));
	} else if (ScriptVersion == 3) {
		get_var_or_word(a, (master_opcode & 0x80));
		get_var_or_word(b, (master_opcode & 0x40));
	}

	int opcode = get_byte();

	//buf+=sprintf(buf, "SubCode33%.2X", opcode);

	switch (opcode & 0x1F) {
	case 0x01:
		if (ScriptVersion > 3) {
			get_var_or_word(a, (master_opcode & 0x80));
			get_var_or_word(b, (master_opcode & 0x40));
		}
		buf = strecpy(buf, "RoomScroll(");
		buf = strecpy(buf, a);
		buf = strecpy(buf, ",");
		buf = strecpy(buf, b);
		buf = strecpy(buf, ")");
		break;
	case 0x02:
		if (ScriptVersion > 3) {
			get_var_or_word(a, (master_opcode & 0x80));
			get_var_or_word(b, (master_opcode & 0x40));
		}
		buf = strecpy(buf, "RoomColor(");
		buf = strecpy(buf, a);
		buf = strecpy(buf, ",");
		buf = strecpy(buf, b);
		buf = strecpy(buf, ")");
		break;
	case 0x03:
		if (ScriptVersion > 3) {
			get_var_or_word(a, (master_opcode & 0x80));
			get_var_or_word(b, (master_opcode & 0x40));
		}
		buf = strecpy(buf, "SetScreen(");
		buf = strecpy(buf, a);
		buf = strecpy(buf, ",");
		buf = strecpy(buf, b);
		buf = strecpy(buf, ")");
		break;
	case 0x04:
		if (ScriptVersion > 3) {
			get_var_or_word(a, (master_opcode & 0x80));
			get_var_or_word(b, (master_opcode & 0x40));
		}
		buf = strecpy(buf, "SetPalColor(");
		buf = strecpy(buf, a);
		buf = strecpy(buf, ",");
		buf = strecpy(buf, b);
		buf = strecpy(buf, ")");
		break;
	case 0x05:
		do_tok(buf, "ShakeOn", 0);
		break;
	case 0x06:
		do_tok(buf, "ShakeOff", 0);
		break;
/*
	case 0x07:
		do_tok(buf, "Unused", 0);
		break;
	case 0x08:
		buf =
			do_tok(buf, "roomFunc2",
						 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
						 ((opcode & 0x20) ? A3V : A3B));
		break;
	case 0x09:
		buf = do_tok(buf, "saveLoad?", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x0A:
		buf = do_tok(buf, "screenEffect?", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x0B:
		buf =
			do_tok(buf, "roomFunc2",
						 ANOLASTPAREN | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ?
																														 A2V : A2W) |
						 ((opcode & 0x20) ? A3V : A3W));
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B) |
						 ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x0C:
		buf =
			do_tok(buf, "roomFunc3",
						 ANOLASTPAREN | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ?
																														 A2V : A2W) |
						 ((opcode & 0x20) ? A3V : A3W));
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B) |
						 ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x0D:
		do_tok(buf, "save-string", ((opcode & 0x80) ? A1V : A1B) | A2ASCII);
		break;
	case 0x0E:
		do_tok(buf, "load-string", ((opcode & 0x80) ? A1V : A1B) | A2ASCII);
		break;

	case 0x0F:
		buf = do_tok(buf, "palManipulate", ANOLASTPAREN | ((opcode & 0x80) ? A1V : A1B));
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ANOLASTPAREN | ((opcode & 0x80) ?
																													 A1V : A1B) |
						 ((opcode & 0x40) ? A2V : A2B));
		opcode = get_byte();
		buf = do_tok(buf, NULL, ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B));
		break;
*/
	case 0x10:
		do_tok(buf, "colorCycleDelay", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	default:
		// strcpy(buf, "Unknown??");
		// printf("UGH, unknown room op %d\n", opcode & 0x1F);
		// exit(1);
		sprintf(buf, "UnknownRoomCommand%.2X", opcode);
	}
}

void do_cursor_command(char *buf)
{
	int opcode = get_byte();

	switch (opcode & 0x1f) {
	case 0x01:
		do_tok(buf, "CursorShow", 0);
		break;
	case 0x02:
		do_tok(buf, "CursorHide", 0);
		break;
	case 0x03:
		do_tok(buf, "UserputOn", 0);
		break;
	case 0x04:
		do_tok(buf, "UserputOff", 0);
		break;
	case 0x05:
		do_tok(buf, "CursorSoftOn", 0);
		break;
	case 0x06:
		do_tok(buf, "CursorSoftOff", 0);
		break;
	case 0x07:
		do_tok(buf, "UserputSoftOn", 0);
		break;
	case 0x08:
		do_tok(buf, "UserputSoftOff", 0);
		break;

	case 0x0A:
		do_tok(buf, "SetCursorImg", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x0B:
		do_tok(buf, "SetCursorHotspot",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
					 ((opcode & 0x20) ? A3V : A3B));
		break;
	case 0x0C:
		do_tok(buf, "InitCursor", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x0D:
		do_tok(buf, "InitCharset", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x0E:
		if (GF_UNBLOCKED || IndyFlag)
			do_tok(buf, "LoadCharset", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		else
			do_tok(buf, "CursorCommand", A1VARUNTIL0xFF);
		break;
	default:
		sprintf(buf, "UnknownCursorCommand%.2X", opcode);
	}
}

void do_verbops_v2(char *buf, byte opcode)
{
	int subop = get_byte();

	buf = do_tok(buf, "VerbOps[", ANOLASTPAREN);
	switch (subop) {
		case 0:
			buf = do_tok(buf, "Delete", (opcode & 0x80) ? A1V : A1B);
			break;
		case 0xFF:
			buf = do_tok(buf, "State", A1B | A2B);
			break;
		default:
			buf = do_tok(buf, "New", A1B | A2B | ((opcode & 0x80) ? A3V : A3B) | A4B | A5ASCII);
	}
	
}

void do_verbops(char *buf, byte opcode)
{
	char first = 1;

	buf = do_tok(buf, "VerbOps", ((opcode & 0x80) ? A1V : A1B) | ANOLASTPAREN);
	buf = strecpy(buf, ",[");

	do {
		opcode = get_byte();
		if (opcode == 0xFF)
			break;
		if (!first)
			buf = strecpy(buf, ",");
		first = 0;
		switch (opcode & 0x1F) {
		case 0x1:
			buf = do_tok(buf, "Image", (opcode & 0x80) ? A1V : A1W);
			break;
		case 0x2:
			buf = do_tok(buf, "Text", A1ASCII);
			break;
		case 0x3:
			buf = do_tok(buf, "Color", (opcode & 0x80) ? A1V : A1B);
			break;
		case 0x4:
			buf = do_tok(buf, "HiColor", (opcode & 0x80) ? A1V : A1B);
			break;
		case 0x5:
			buf = do_tok(buf, "SetXY", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
			break;
		case 0x6:
			buf = do_tok(buf, "On", 0);
			break;
		case 0x7:
			buf = do_tok(buf, "Off", 0);
			break;
		case 0x8:
			buf = do_tok(buf, "Delete", 0);
			break;
		case 0x9:
			buf = do_tok(buf, "New", 0);
			break;
		case 0x10:
			buf = do_tok(buf, "DimColor", (opcode & 0x80) ? A1V : A1B);
			break;
		case 0x11:
			buf = do_tok(buf, "Dim", 0);
			break;
		case 0x12:
			buf = do_tok(buf, "Key", (opcode & 0x80) ? A1V : A1B);
			break;
		case 0x13:
			buf = do_tok(buf, "Center", 0);
			break;
		case 0x14:
			buf = do_tok(buf, "SetToString", (opcode & 0x80) ? A1V : A1W);
			break;
		case 0x16:
			buf =
				do_tok(buf, "SetToObject", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
			break;
		case 0x17:
			buf = do_tok(buf, "BackColor", (opcode & 0x80) ? A1V : A1B);
			break;
		default:
			buf += sprintf(buf, "Invalid%.2X()", opcode);
		}

	} while (1);
	strecpy(buf, "]);");
}

void do_print_ego(char *buf, byte opcode)
{
	int i;

	char first = 1;

	if (opcode == 0xD8) {
		buf = strecpy(buf, "printEgo([");
	} else {
		buf = do_tok(buf, "print", ((opcode & 0x80) ? A1V : A1B) | ANOLASTPAREN);
		buf = strecpy(buf, ",[");
	}

	do {
		int opcode = get_byte();
		if (opcode == 0xFF)
			break;

		if (!first)
			buf = strecpy(buf, ",");
		first = 0;

		switch (opcode & 0x1f) {
		case 0x0:
			buf = do_tok(buf, "Pos", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
			break;
		case 0x1:
			buf = do_tok(buf, "Color", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x2:
			buf = do_tok(buf, "Clipped", ((opcode & 0x80) ? A1V : A1W));
			break;
		case 0x3:
			buf = do_tok(buf, "Erase?", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
			break;
		case 0x4:
			buf = do_tok(buf, "Center", 0);
			break;
		case 0x6:
			if (GF_UNBLOCKED)
				buf = do_tok(buf, "Spacing?", ((opcode & 0x80) ? A1V: A1W));
			else
				buf = do_tok(buf, "Left", 0);
			break;
		case 0x7:
			buf = do_tok(buf, "Overhead", 0);
			break;
		case 0x8:
			buf = do_tok(buf, "PlayCDTrack", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
			break;
		case 0xF:{
				buf = strecpy(buf, "Text(\"");
				while (1) {
					i = get_byte();
					if (!i)
						break;
					buf = putascii(buf, i);
					if (i == 255) {
						i = get_byte();
						buf = putascii(buf, i);
						if (i == 0 || i != 2 && i != 3 && i != 8) {
							buf = putascii(buf, get_byte());
							buf = putascii(buf, get_byte());
						}
					}
				}
				buf = strecpy(buf, "\")");
			}
			goto exit_proc;
		default:
			buf += sprintf(buf, "Unknown%.2X()", opcode);
			goto exit_proc;
		}
	} while (1);

exit_proc:;
	buf = strecpy(buf, "]);");

}

int get_curpos()
{
	return cur_pos - org_pos;
}

int get_gotopos()
{
	int j = get_word();
	return (short)(j + get_curpos());
}

bool emit_if(char *before, char *after)
{
	int i = get_gotopos();

	before[0] = 0;
	after[0] = 0;

	if (!DontOutputElseif && HavePendingElse()) {
		if (RequestElseIfAdd(get_curpos(), PendingElseTo, i)) {
			sprintf(after, AlwaysShowOffs ? ") /*%.4X*/ {" : ") {", i);
			strcpy(before, "} else ");
			PendingElse = 0;
			HaveElse = 1;
			return true;
		}
	}

	if (!DontOutputIfs && RequestIfAdd(get_curpos(), i)) {
		sprintf(after, AlwaysShowOffs ? ") /*%.4X*/ {" : ") {", i);
		return true;
	}

	sprintf(after, ") goto %.4X;", i);
	return false;
}



void do_if_code(char *buf, byte opcode)
{
	char tmp2[256];
	char var[256];
	char before[256], after[256];
	byte neg;
	int txt;

	const char *cmp_texts[8] = {
		" >= ", " < ", " <= ", " > ", " == ", " != ", "!", "",
	};

	var[0] = 0;
	if (opcode != 0x28 && opcode != 0xA8)
		get_var(var);

	switch (opcode & 0x7F) {
	case 0x38:
		txt = 0;
		break;											/* lessOrEqual */
	case 0x04:
		txt = 2;
		break;											/* isGreaterEqual */
	case 0x08:
		txt = 5;
		break;											/* isNotEqual */
	case 0x48:
		txt = 4;
		break;											/* isEqual */
	case 0x78:
		txt = 1;
		break;											/* isGreater */
	case 0x44:
		txt = 3;
		break;											/* isLess */
	case 0x28:
		txt = opcode & 128 ? 7 : 6;
		break;
	default:
		/* Exit, this should never happen, only if my code is buggy */
		printf("Unknown IF code %x", opcode);
		exit(0);
	}

	if (opcode == 0x28 || opcode == 0xA8) {
		get_var(tmp2);
	} else {
		get_var_or_word(tmp2, opcode & 0x80);
	}

	neg = emit_if(before, after) ^ 1;

	sprintf(buf, "%sif (%s%s%s%s", before, var, cmp_texts[txt ^ neg], tmp2, after);
}

void do_if_state_code(char *buf, byte opcode)
{
	char tmp2[256];
	char var[256];
	char before[256], after[256];
	byte neg;
	int state;

	const char *cmp_texts[2] = {
		" == ", " != "
	};

	var[0] = 0;
	get_var_or_word(var, opcode & 0x80);

	if (ScriptVersion > 2) {
		switch (opcode & 0x2F) {
		case 0x0f:
			neg = 0;
			break;
		case 0x2f:
			neg = 1;
			break;
		default:
			/* Exit, this should never happen, only if my code is buggy */
			printf("Unknown IF code %x", opcode);
			exit(1);
		}

		get_var_or_word(tmp2, opcode & 0x40);
	} else {
		switch (opcode) {
		case 0x3f:
		case 0x7f:
		case 0xbf:
			state = 1;
			neg = 1;
			break;
		case 0x5f:
		case 0xdf:
			state = 2;
			neg = 1;
			break;
		case 0x27:
		case 0x2f:
		case 0xaf:
			state = 4;
			neg = 1;
			break;
		case 0x0f:
		case 0x8f:
			state = 8;
			neg = 1;
			break;
		case 0xff:
			state = 1;
			neg = 0;
			break;
		case 0x1f:
		case 0x9f:
			state = 2;
			neg = 0;
			break;
		case 0x6f:
		case 0xef:
			state = 4;
			neg = 0;
			break;
		case 0x4f:
		case 0xcf:
			state = 8;
			neg = 0;
			break;
		default:
			/* Exit, this should never happen, only if my code is buggy */
			printf("Unknown IF code %x", opcode);
			exit(1);
		}
		
		sprintf(tmp2, "%d", state);
	}

	neg = neg ^ emit_if(before, after) ^ 1;

	sprintf(buf, "%sif (getState(%s)%s%s%s", before, var, cmp_texts[neg], tmp2, after);
}

void do_unconditional_jump(char *buf, byte opcode)
{
	int i = get_gotopos();
	int j = get_curpos();

	if (i == j) {
		sprintf(buf, "/* goto %.4X; */", i);
	} else if (!DontOutputElse && RequestElseAdd(j, i)) {
		PendingElse = 1;
		PendingElseTo = i;
		PendingElseOffs = j - 3;
		PendingElseOpcode = opcode;
		PendingElseIndent = NumBlockStack;
		buf[0] = 0;
	} else {
		sprintf(buf, "goto %.4X;", i);
	}
}


void do_varset_code(char *buf, byte opcode)
{
	char *s;

	if ((ScriptVersion == 2)
		&& ((opcode & 0x7F) == 0x0A
		 || (opcode & 0x7F) == 0x2A
		 || (opcode & 0x7F) == 0x6A)) {
		 
		int i = get_byte();
		buf += sprintf(buf, "Var[Var[%d]]", i);
	} else
		buf = get_var(buf);

	switch (opcode & 0x7F) {
	case 0x0A:
	case 0x1A:
		s = " = ";
		break;											/* move */
	case 0x1B:
		s = " *= ";
		break;											/* mul */
	case 0x3A:
	case 0x6A:
		s = " -= ";
		break;											/* sub */
	case 0x57:
		s = " |= ";
		break;											/* or */
	case 0x2A:
	case 0x5A:
		s = " += ";
		break;											/* add */
	case 0x5B:
		s = " /= ";
		break;											/* divide */
	case 0x17:
		s = " &= ";
		break;											/* and */
	case 0x46:
		if (opcode & 128)
			s = "--";
		else
			s = "++";
		break;											/* increment & decrement */
	default:
		/* Exit, this should never happen, only if my code is buggy */
		printf("Unknown VARSET code %x", opcode);
		exit(0);
	}

	buf = strecpy(buf, s);


	if ((opcode & 127) != 0x46) {	/* increment or decrement */
		buf = get_var_or_word(buf, opcode & 0x80);
	}
	strecpy(buf, ";");
}

void do_matrix_ops(char *buf, byte opcode)
{
	opcode = get_byte();

	switch (opcode & 0x1F) {
	case 0x1:
		do_tok(buf, "setBoxFlags", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x2:
		do_tok(buf, "setBoxScale", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x3:
		do_tok(buf, "SetBoxSlot", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x4:
		do_tok(buf, "createBoxMatrix", 0);
		break;
	default:
		sprintf(buf, "SetBoxUnknown%.2X", opcode);
	}
}

void get_tok_V2(char *buf)
{
	byte opcode = get_byte();

	switch (opcode) {
	
	case 0x58:
		do_tok(buf, "beginOverride", 0);
		break;
	case 0x52:
	case 0xD2:
		do_tok(buf, "actorFollowCamera", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x15:
	case 0x55:
	case 0x95:
	case 0xD5:
		do_tok(buf, "actorFromPos",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) | AVARSTORE);
		break;
	case 0x13:
	case 0x53:
	case 0x93:
	case 0xD3:
		// actorSet
		do_actorset_v2(buf, opcode);
		break;

	case 0x2A:
	case 0xAA:
		// addDirect
	case 0x3A:
	case 0xBA:
		//subtract
	case 0x6A:
	case 0xEA:
		//subDirect
	case 0x0A:
	case 0x8A:
		// assignVarWordDirect
	case 0x1A:
	case 0x5A:
	case 0x9A:
	case 0xDA:
		do_varset_code(buf, opcode);
		break;

	case 0x11:
	case 0x51:
	case 0x91:
	case 0xD1:
		// animateActor
		do_tok(buf, "animateActor", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
/*
	case 0x2C:
		// assignVarByte
		break;
*/
	case 0x80:
		do_tok(buf, "breakHere", 0);
		break;
	case 0x4A:
	case 0xCA:
		// chainScript
		do_tok(buf, "chainScript", ((opcode & 0x80) ? A1V : A1B));
		break;
/*
	case 0x77:
	case 0xF7:
		// clearState01
		break;
	case 0x67:
	case 0xE7:
		// clearState04
		break;
	case 0xC7:
		// clearState08
		break;
*/
	case 0x60:
	case 0xE0:
		//cursorCommand
		sprintf(buf, "UnknownCursorCommand%.2X", opcode);
		break;
	case 0x40:
		sprintf(buf, "cutscene");
		break;

	case 0x46:
	case 0xC6:
		// increment / decrement
		do_varset_code(buf, opcode);
		break;

	case 0x2E: {
		//delay
		int d = get_byte();
		d |= get_byte() << 8;
		d |= get_byte() << 16;
		d = 0xFFFFFF - d;
		sprintf(buf, "delay(%d)", d);
		break;
	}

	case 0x2B:
		do_tok(buf, "delayVariable", A1V);
		break;

	case 0x19:
	case 0x39:
	case 0x59:
	case 0x79:
	case 0x99:
	case 0xB9:
	case 0xD9:
	case 0xF9:{
			buf = strecpy(buf, "doSentence(");
			if (!(opcode & 0x80) && (*cur_pos == 0xFB) || *cur_pos == 0xFC) {
				strcpy(buf, "STOP)");
				cur_pos++;
			} else {
				do_tok(buf, "",
							 ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B) |
							 ((opcode & 0x40) ? A2V : A2W) | ((opcode & 0x20) ? A3V : A3W) | A4B);
			}
		}
		break;

	case 0x05:
	case 0x25:
	case 0x45:
	case 0x65:
	case 0x85:
	case 0xA5:
	case 0xC5:
	case 0xE5:
		//drawObject
		buf = do_tok(buf, "drawObject",
					 ((opcode & 0x80) ? A1V : A1B) |
					 ((opcode & 0x40) ? A2V : A2B) |
					 ((opcode & 0x20) ? A3V : A3B));
		break;
/*	
	case 0xAC:
		//drawSentence
		break;
*/
	case 0x5C:
	case 0x6B:
	case 0x6E:
	case 0xAB:
	case 0xDC:
	case 0xEB:
	case 0xEE:
		//dummy
		sprintf(buf, "dummy(%.2X)", opcode);
		
		break;
/*		
	case 0xC0:
		//endCutscene
		break;
*/

	case 0x09:
	case 0x49:
	case 0x89:
	case 0xC9:
		do_tok(buf, "faceActor", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x35:
	case 0x75:
	case 0xB5:
	case 0xF5:
		do_tok(buf, "findObject",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;
	case 0x71:
	case 0xF1:
		do_tok(buf, "getActorCostume", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x06:
	case 0x86:
		do_tok(buf, "getActorElevation", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x63:
	case 0xE3:
		do_tok(buf, "getActorFacing", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x56:
	case 0xD6:
		do_tok(buf, "getActorMoving", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x03:
	case 0x83:
		do_tok(buf, "getActorRoom", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
/*
	case 0x7B:
	case 0xFB:
		//getActorWalkBox
		break;	
*/
	case 0x43:
	case 0xC3:
		do_tok(buf, "getActorX", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x23:
	case 0xA3:
		do_tok(buf, "getActorY", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x31:
	case 0xB1:
		// FIXME
		do_tok(buf, "getBitVar", AVARSTORE | A1B | A2B | ((opcode & 0x80) ? A3V : A3B));
		break;

	case 0x66:
	case 0xE6:
		do_tok(buf, "getClosestObjActor", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x34:
	case 0x74:
	case 0xB4:
	case 0xF4:
		do_tok(buf, "getDist",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;
	case 0x10:
	case 0x90:
		do_tok(buf, "getObjectOwner", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x6C:
	case 0xEC:
		//getObjY
		do_tok(buf, "getObjY", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x16:
	case 0x96:
		do_tok(buf, "getRandomNr", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

/*
	case 0x1D:
	case 0x5D:
	case 0x9D:
	case 0xDD:
		//ifClassOfIs
		break;
*/
		
	case 0x3F:
	case 0x7F:
	case 0xBF:
		//ifNotState01
	case 0x5F:
	case 0xDF:
		//ifNotState02
	case 0x27:
	case 0x2F:
	case 0xAF:
		//ifNotState04
	case 0x0F:
	case 0x8F:
		//ifNotState08
	case 0xFF:
		//ifState01
	case 0x1F:
	case 0x9F:
		//ifState02
	case 0x6F:
	case 0xEF:
		//ifState04
	case 0x4F:
	case 0xCF:
		//ifState08
		do_if_state_code(buf, opcode);
		break;
		
	case 0x48:
	case 0xC8:
		//isEqual
	case 0x78:
	case 0xF8:
		//isGreater
	case 0x04:
	case 0x84:
		//isGreaterEqual
	case 0x44:
	case 0xC4:
		//isLess
	case 0x08:
	case 0x88:
		//isNotEqual
	case 0x38:
	case 0xB8:
		//lessOrEqual
	case 0x28:
		//equalZero
	case 0xA8:
		//notEqualZero
		do_if_code(buf, opcode);
		break;

	case 0x68:
	case 0xE8:
		do_tok(buf, "isScriptRunning", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x7C:
	case 0xFC:
		do_tok(buf, "isSoundRunning", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x18:
		// jumpRelative
		do_unconditional_jump(buf, opcode);
		break;

	case 0x70:
	case 0xF0:
		buf = do_tok(buf, "lights", ((opcode & 0x80) ? A1V : A1B) | A2B | A3B);
		break;
	case 0x72:
	case 0xF2:
		do_tok(buf, "loadRoom", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x24:
	case 0x64:
	case 0xA4:
	case 0xE4:
		//loadRoomWithEgo
		buf =
			do_tok(buf, "loadRoomWithEgo",
						 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B) | ANOLASTPAREN);
		do_tok(buf, NULL, A1B | A2B | ANOFIRSTPAREN | ASTARTCOMMA);
		break;
/*			
	case 0x30:
	case 0xB0:
		//matrixOps
		break;
*/			
	case 0x12:
	case 0x92:
		//panCameraTo
		do_tok(buf, "panCameraTo", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x50:
	case 0xD0:
		//pickupObject
		do_tok(buf, "pickupObject", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x14:
	case 0x94:
		// print
		sprintf(buf, "print(\"");
		do_decodeparsestring_v2(buf, opcode);
		strcat(buf, "\")");
		break;
	case 0xD8:
		//printEgo
		sprintf(buf, "printEgo(\"");
		do_decodeparsestring_v2(buf, opcode);
		strcat(buf, "\")");
		break;
		
	case 0xCC:
		// pseudoRoom
		do_cc(buf);
		break;
	case 0x01:
	case 0x21:
	case 0x41:
	case 0x61:
	case 0x81:
	case 0xA1:
	case 0xC1:
	case 0xE1:
		do_tok(buf, "putActor",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
					 ((opcode & 0x20) ? A3V : A3B));
		break;
	case 0x0E:
	case 0x4E:
	case 0x8E:
	case 0xCE:
		//putActorAtObject
		do_tok(buf, "putActorAtObject", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) );
		break;
	case 0x2D:
	case 0x6D:
	case 0xAD:
	case 0xED:
		do_tok(buf, "putActorInRoom", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x0C:
	case 0x8C:
		//resourceRoutines
		do_resource_v2(buf, opcode);
		break;
	case 0x98:
		do_tok(buf, "restart", 0);
		break;

	case 0x33:
	case 0x73:
	case 0xB3:
	case 0xF3:
		do_room_ops_old(buf, opcode);
		break;
/*		
	case 0x22:
	case 0xA2:
		//saveLoadGame
		break;
	
	case 0x3D:
	case 0x7D:
	case 0xBD:
	case 0xFD:
		//setActorElevation
		break;
		
	case 0x1B:
	case 0x5B:
	case 0x9B:
	case 0xDB:
		//setBitVar
		break;
*/
	case 0x32:
	case 0xB2:
		//setCameraAt
		do_tok(buf, "setCameraAt", ((opcode & 0x80) ? A1V : A1B));
		break;
/*			
	case 0x54:
	case 0xD4:
		//setObjectName
		break;
*/		
	case 0x0B:
	case 0x4B:
	case 0x8B:
	case 0xCB:
		//setObjY
		do_tok(buf, "setObjY", ((opcode & 0x80) ? A1V : A1W) | A2B);
		break;
	case 0x29:
	case 0x69:
	case 0xA9:
	case 0xE9:
		//setOwnerOf
		do_tok(buf, "setOwnerOf", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
		break;
		
	case 0x37:
		//setState01
		do_tok(buf, "setState01", A1W);
		break;
	case 0x57:
	case 0x97:
	case 0xB7:
	case 0xD7:
		//setState02
		do_tok(buf, "setState02", A1W);
		break;
	case 0x17:
	case 0xA7:
		//setState04
		do_tok(buf, "setState04", A1W);
		break;
	case 0x07:
	case 0x47:
	case 0x87:
		//setState08
		do_tok(buf, "setState08", A1W);
		break;
	case 0x26:
	case 0xA6: {
			int i;
			char first = 1;

			buf = do_tok(buf, "setVarRange", A1V | ANOLASTPAREN);
			i = get_byte();

			buf += sprintf(buf, ",%d,[", i);

			while (i > 0) {
				if (!first)
					buf = strecpy(buf, ",");
				first = 0;

				buf += sprintf(buf, "%d", (opcode & 0x80) ? get_word() : get_byte());
				i--;
			}

			strcpy(buf, "];");

		}
		break;

	case 0x02:
	case 0x82:
		do_tok(buf, "startMusic", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x42:
	case 0xC2:
		//startScript
		do_tok(buf, "startScript", ((opcode & 0x80) ? A1V : A1B));
		break;
		
	case 0x1C:
	case 0x9C:
		do_tok(buf, "startSound", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x20:
		do_tok(buf, "stopMusic", 0);
		break;
	case 0x00:
	case 0xA0:
		do_tok(buf, "stopObjectCode", 0);
		break;
	case 0x62:
	case 0xE2:
		do_tok(buf, "stopScript", ((opcode & 0x80) ? A1V : A1B));
		break;
		
	case 0x3C:
	case 0xBC:
		do_tok(buf, "stopSound", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x7A:
	case 0xFA:
		// verbOps
		do_verbops_v2(buf, opcode);
		break;

	case 0x3B:
	case 0xBB:
		do_tok(buf, "waitForActor", ((opcode & 0x80) ? A1V : A1B));
		break;
			
	case 0x4C:
		do_tok(buf, "waitForSentence", 0);
		break;

	case 0xAE:
		do_tok(buf, "waitForMessage", 0);
		break;

	case 0x3E:
	case 0x5E:
	case 0x7E:
	case 0x9E:
	case 0xBE:
	case 0xDE:
	case 0xFE:
		do_tok(buf, "walkActorTo",
					 ((opcode & 0x80) ? A1V : A1B) |
					 ((opcode & 0x40) ? A2V : A2B) | 
					 ((opcode & 0x20) ? A3V : A3B));
		break;

	case 0x0D:
	case 0x4D:
	case 0x8D:
	case 0xCD:
		do_tok(buf, "walkActorToActor",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) | A3B);
		break;			
	case 0x36:
	case 0x76:
	case 0xB6:
	case 0xF6:
		// walkActorToObject
		break;
	default:
		printf("Unknown opcode %.2X\n", opcode);
		exit(1);
	}
}

void get_tok(char *buf)
{
	byte opcode = get_byte();

	switch (opcode) {

	case 0x00:
	case 0xA0:
		do_tok(buf, "stopObjectCode", 0);
		break;

	case 0x01:
	case 0x21:
	case 0x41:
	case 0x61:
	case 0x81:
	case 0xA1:
	case 0xC1:
	case 0xE1:
		do_tok(buf, "putActor",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W) |
					 ((opcode & 0x20) ? A3V : A3W));
		break;

	case 0x15:
	case 0x55:
	case 0x95:
	case 0xD5:
		do_tok(buf, "actorFromPos",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W) | AVARSTORE);
		break;

	case 0x03:
	case 0x83:
		do_tok(buf, "getActorRoom", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;


	case 0x38:
	case 0xB8:
	case 0x04:
	case 0x84:
	case 0x08:
	case 0x88:
	case 0x48:
	case 0xC8:
	case 0x44:
	case 0xC4:
	case 0x78:
	case 0xF8:
	case 0x28:
	case 0xA8:
		do_if_code(buf, opcode);
		break;

	case 0x05:
	case 0x45:
	case 0x85:
	case 0xC5:
		if (ScriptVersion == 5) {
			buf = do_tok(buf, "drawObject", ((opcode & 0x80) ? A1V : A1W) | ANOLASTPAREN);
			opcode = get_byte();
			switch (opcode & 0x1F) {
			case 1:
				do_tok(buf, ", setXY(",
							 ANOLASTPAREN | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1W) |
							 ((opcode & 0x40) ? A2V : A2W));
				break;
			case 2:
				do_tok(buf, ", setImage(", ANOLASTPAREN | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1W));
				break;
			}
			strcat(buf, "));");

		} else {
			buf = do_tok(buf, "drawObject",
						 ((opcode & 0x80) ? A1V : A1W) |
						 ((opcode & 0x40) ? A2V : A2W) |
						 ((opcode & 0x20) ? A3V : A3W));
		}
		break;

	case 0x25:
	case 0x65:
	case 0xA5:
	case 0xE5:
		if (ScriptVersion == 5) {
			do_tok(buf, "pickupObject", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
		} else {
			buf = do_tok(buf, "drawObject",
						 ((opcode & 0x80) ? A1V : A1W) |
						 ((opcode & 0x40) ? A2V : A2W) |
						 ((opcode & 0x20) ? A3V : A3W));
		}
		break;

	case 0x06:
	case 0x86:
		do_tok(buf, "getActorElevation", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x07:
	case 0x47:
	case 0x87:
	case 0xC7:
		do_tok(buf, "setState", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x09:
	case 0x49:
	case 0x89:
	case 0xC9:
		do_tok(buf, "faceActor", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x0A:
	case 0x8A:
	case 0x2A:
	case 0xAA:
	case 0x4A:
	case 0xCA:
	case 0x6A:
	case 0xEA:
		do_tok(buf, "startScript", ((opcode & 0x80) ? A1V : A1B) | A2VARUNTIL0xFF);
		break;


	case 0x0B:
	case 0x4B:
	case 0x8B:
	case 0xCB:
		do_tok(buf, "getVerbEntryPoint",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;

	case 0x0c:
	case 0x8C:
		do_resource(buf, opcode);
		break;

	case 0x0D:
	case 0x4D:
	case 0x8D:
	case 0xCD:
		do_tok(buf, "walkActorToActor",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) | A3B);
		break;											/* arg1=actor, arg2=actor */

	case 0x0F:
	case 0x8F:
		if (ScriptVersion == 5) {
			do_tok(buf, "getObjectState", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
			break;
		}
	case 0x2F:
	case 0x4F:
	case 0x6F:
	case 0xAF:
	case 0xCF:
	case 0xEF:
		do_if_state_code(buf, opcode);
		break;

	case 0x10:
	case 0x90:
		do_tok(buf, "getObjectOwner", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
		break;


	case 0x14:
	case 0xD8:
	case 0x94:
		do_print_ego(buf, opcode);
		break;

	case 0x17:
	case 0x97:
	case 0x1A:
	case 0x9A:
	case 0x1B:
	case 0x9B:
	case 0x3A:
	case 0xBA:
	case 0x46:
	case 0x57:
	case 0xD7:
	case 0x5A:
	case 0xDA:
	case 0x5B:
	case 0xDB:
	case 0xC6:
		do_varset_code(buf, opcode);
		break;

	case 0x18:
		do_unconditional_jump(buf, opcode);
		break;

	case 0x1D:
	case 0x9D:
		do_tok(buf, "if ClassOfIs", ((opcode & 0x80) ? A1V : A1W) | A2VARUNTIL0xFF | ATO);
		break;											/* arg1=object; vararg=classes to test; arg3=jumpoffs */

	case 0x1E:
	case 0x3E:
	case 0x5E:
	case 0x7E:
	case 0x9E:
	case 0xBE:
	case 0xDE:
	case 0xFE:
		do_tok(buf, "walkActorTo",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W) |
					 ((opcode & 0x20) ? A3V : A3W));
		break;

	case 0x24:
	case 0x64:
	case 0xA4:
	case 0xE4:
		buf =
			do_tok(buf, "loadRoomWithEgo",
						 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B) | ANOLASTPAREN);
		do_tok(buf, NULL, A1W | A2W | ANOFIRSTPAREN | ASTARTCOMMA);
		break;

	case 0x2C:
		do_cursor_command(buf);
		break;

	case 0x40:
		do_tok(buf, "cutscene", A1VARUNTIL0xFF);
		break;

	case 0x42:
	case 0xC2:
		do_tok(buf, "chainScript", ((opcode & 0x80) ? A1V : A1B) | A2VARUNTIL0xFF);
		break;

	case 0x56:
	case 0xD6:
		do_tok(buf, "getActorMoving", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x72:
	case 0xF2:
		do_tok(buf, "loadRoom", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x63:
	case 0xE3:
		do_tok(buf, "getActorFacing", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x66:
	case 0xE6:
		do_tok(buf, "getClosestObjActor", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x6C:
	case 0xEC:
		do_tok(buf, "getActorWidth", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x71:
	case 0xF1:
		do_tok(buf, "getActorCostume", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x3B:
	case 0xBB:
		if (IndyFlag)
			do_tok(buf, "waitForActor", ((opcode & 0x80) ? A1V : A1B));
		else
			do_tok(buf, "getActorScale", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0xAE:{
			byte opcode;
			if (IndyFlag)
				opcode = 2;
			else
				opcode = get_byte();

			switch (opcode) {
			case 0x01:
			case 0x81:
				do_tok(buf, "WaitForActor", ((opcode & 0x80) ? A1V : A1B));
				break;
			case 0x02:
				do_tok(buf, "WaitForMessage", 0);
				break;
			case 0x03:
				do_tok(buf, "WaitForCamera", 0);
				break;
			case 0x04:
				do_tok(buf, "WaitForSentence", 0);
				break;
			default:
				do_tok(buf, "UnknownWait", 0);
			}
		}
		break;


	case 0x34:
	case 0x74:
	case 0xB4:
	case 0xF4:
		do_tok(buf, "getDist",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;


	case 0x36:
	case 0x76:
	case 0xB6:
	case 0xF6:
		do_tok(buf, "walkActorToObject", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W));
		break;

	case 0x37:
	case 0x77:
	case 0xB7:
	case 0xF7:
		do_tok(buf, "startObject",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B) | A3VARUNTIL0xFF);
		break;

	case 0x19:
	case 0x39:
	case 0x59:
	case 0x79:
	case 0x99:
	case 0xB9:
	case 0xD9:
	case 0xF9:{
			buf = strecpy(buf, "doSentence(");
			// FIXME: this is not exactly what ScummVM does...
			if (!(opcode & 0x80) && (*cur_pos == 0xFE)) {
				strcpy(buf, "STOP)");
				cur_pos++;
			} else {
				do_tok(buf, "",
							 ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B) |
							 ((opcode & 0x40) ? A2V : A2W) | ((opcode & 0x20) ? A3V : A3W));
			}
		}
		break;


	case 0x62:
	case 0xE2:
		do_tok(buf, "stopScript", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0xAc:
		do_expr_code(buf);
		break;

	case 0x11:
	case 0x51:
	case 0x91:
	case 0xD1:
		do_tok(buf, "animateCostume", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x80:
		do_tok(buf, "breakHere", 0);
		break;

	case 0xc0:
		do_tok(buf, "endCutscene", 0);
		break;

	case 0x27:{									/* String management subcode */
			switch ((opcode = get_byte()) & 0x1F) {
			case 0x01:
				do_load_code_to_string(buf, opcode);
				break;
			case 0x02:
				do_tok(buf, "CopyString", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
				break;
			case 0x03:
				do_tok(buf, "SetStringChar",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
							 ((opcode & 0x20) ? A3V : A3B));
				break;									/* string(arg1)[arg2] = arg3 */
			case 0x04:
				do_tok(buf, "GetStringChar",
							 AVARSTORE | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
				break;									/* arg1 = string(arg2)[arg3] */
			case 0x05:
				do_tok(buf, "CreateString", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
				break;									/* arg1=index, arg2=size */
			default:
				do_tok(buf, "StringFuncUnknown", 0);
			}
		}
		break;

	case 0x13:
	case 0x53:
	case 0x93:
	case 0xD3:
		do_actorset(buf, opcode);
		break;

	case 0x20:
		do_tok(buf, "stopMusic", 0);
		break;

	case 0x70:
	case 0xF0:
		buf = do_tok(buf, "lights", ((opcode & 0x80) ? A1V : A1B) | A2B | A3B);
		break;

	case 0x3F:
	case 0x7F:
	case 0xBF:
	case 0xFF:
		buf =
			do_tok(buf, "drawBox",
						 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W) | ANOLASTPAREN);
		opcode = get_byte();
		do_tok(buf, NULL,
					 ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1W) |
					 ((opcode & 0x40) ? A2V : A2W) | ((opcode & 0x20) ? A3V : A3B));
		break;


	case 0x02:
	case 0x82:
		do_tok(buf, "startMusic", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0xCC:
		do_cc(buf);
		break;

	case 0x33:
	case 0x73:
	case 0xB3:
	case 0xF3:
		if (ScriptVersion == 5)
			do_room_ops(buf);
		else
			do_room_ops_old(buf, opcode);
		break;

	case 0x68:
	case 0xE8:
		do_tok(buf, "isScriptRunning", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x2E:{
			int d;
			d = get_byte();
			d |= get_byte() << 8;
			d |= get_byte() << 16;
			sprintf(buf, "Delay(%d);", d);
			break;
		}

	case 0x29:
	case 0x69:
	case 0xA9:
	case 0xE9:
		do_tok(buf, "setOwnerOf", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x52:
	case 0xD2:
		do_tok(buf, "actorFollowCamera", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x58:
		if (!cur_pos)
			do_tok(buf, "StopOverride", A1B | A2B | A3W);
		else
			do_tok(buf, "Override", A1B);
		break;

	case 0x1C:
	case 0x9C:
		do_tok(buf, "startSound", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x4C:
		do_tok(buf, "soundKludge", A1VARUNTIL0xFF);
		break;

	case 0x3C:
	case 0xBC:
		do_tok(buf, "stopSound", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x98:
		do_tok(buf, "quitPauseRestart", A1B);
		break;

	case 0x7B:
	case 0xFB:
		do_tok(buf, "getActorWalkBox", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x43:
	case 0xC3:
		if (IndyFlag)
			do_tok(buf, "getActorX", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		else
			do_tok(buf, "getActorX", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x23:
	case 0xA3:
		if (IndyFlag)
			do_tok(buf, "getActorY", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		else
			do_tok(buf, "getActorY", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x7A:
	case 0xFA:
		do_verbops(buf, opcode);
		break;

	case 0x2D:
	case 0x6D:
	case 0xAD:
	case 0xED:
		do_tok(buf, "putActorInRoom", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x54:
	case 0xD4:
		do_tok(buf, "setObjectName", ((opcode & 0x80) ? A1V : A1W) | A2ASCII);
		break;

	case 0x5D:
	case 0xDD:
		do_tok(buf, "setClass", ((opcode & 0x80) ? A1V : A1W) | A2VARUNTIL0xFF);
		break;

	case 0x35:
	case 0x75:
	case 0xB5:
	case 0xF5:
		do_tok(buf, "findObject",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;

	case 0x26:
	case 0xA6:{
			int i;
			char first = 1;

			buf = do_tok(buf, "setVarRange", A1V | ANOLASTPAREN);
			i = get_byte();

			buf += sprintf(buf, ",%d,[", i);

			while (i > 0) {
				if (!first)
					buf = strecpy(buf, ",");
				first = 0;

				buf += sprintf(buf, "%d", (opcode & 0x80) ? get_word() : get_byte());
				i--;
			}

			strcpy(buf, "];");

		}
		break;

	case 0x16:
	case 0x96:
		do_tok(buf, "getRandomNr", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x2B:
		do_tok(buf, "delayVariable", A1V);
		break;

	case 0x0E:
	case 0x4E:
	case 0x8E:
	case 0xCE:
		do_tok(buf, "putActorAtObject", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W));
		break;

	case 0x12:
	case 0x92:
		do_tok(buf, "panCameraTo", ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x32:
		do_tok(buf, "setCameraAt", ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x30:
	case 0xB0:
		if (ScriptVersion == 5)
			do_matrix_ops(buf, opcode);
		else
			do_tok(buf, "setBoxFlags", ((opcode & 0x80) ? A1V : A1B) | A2B);
		break;

	case 0x7C:
	case 0xFC:
		do_tok(buf, "isSoundRunning", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x31:
	case 0xB1:
		do_tok(buf, "getInventoryCount", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x1f:
	case 0x5f:
	case 0x9f:
	case 0xdf:
		do_tok(buf, "isActorInBox",
					 ATO | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x22:
	case 0xA2:
		do_tok(buf, "getAnimCounter", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x3d:
	case 0x7d:
	case 0xBD:
	case 0xFD:
		do_tok(buf, "findInventory",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0xAB:{
			int code;
			opcode = get_byte();
			code =
				((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
				((opcode & 0x20) ? A3V : A3B);
			switch (opcode & 0x1F) {
			case 0x01:
				do_tok(buf, "SaveRestoreVerbsA", code);
				break;
			case 0x02:
				do_tok(buf, "SaveRestoreVerbsB", code);
				break;
			case 0x03:
				do_tok(buf, "SaveRestoreVerbsC", code);
				break;
			default:
				do_tok(buf, "SaveRestoreVerbsUnknown", 0);
			}
		}
		break;

	case 0x60:
	case 0xE0:
		do_tok(buf, "freezeScripts", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x6E:
	case 0xEE:
		do_tok(buf, "stopObjectScript", ((opcode & 0x80) ? A1V : A1W));
		break;

		// dodgy?
	case 0x5C:{
	case 0xDC:
			int d = get_byte();
			if ((d & 0x1F) == 3)
				do_tok(buf, "oldRoomEffect-set", ((opcode & 0x80) ? A1V : A1W));
			else
				do_tok(buf, "oldRoomEffect-fadein", ((opcode & 0x80) ? A1V : A1W));
			break;
		}

	case 0x50:
	case 0xD0:
		do_tok(buf, "pickupObject", ((opcode & 0x80) ? A1V : A1W));
		break;

	default:
		if (HaltOnError) {
			printf("Unknown opcode %.2X\n", opcode);
			exit(1);
		}
		sprintf(buf, "Unknown opcode %.2X!", opcode);
	}
}

void ShowHelpAndExit()
{
	printf("SCUMM Script discompiler\n"
				 "Syntax:\n"
				 "\tdescumm [-o] filename\n"
				 "Flags:\n"
				 "\t-2\tInput Script is v2\n"
				 "\t-3\tInput Script is v3\n"
				 "\t-5\tInput Script is v5\n"
				 "\t-n\tUse Indy3-256 specific hacks\n"
				 "\t-u\tScript is Unblocked/has no header\n"
				 "\t-o\tAlways Show offsets\n"
				 "\t-i\tDon't output ifs\n"
				 "\t-e\tDon't output else\n"
				 "\t-f\tDon't output else-if\n"
				 "\t-c\tDon't show opcode\n" 
				 "\t-x\tDon't show offsets\n" 
				 "\t-h\tHalt on error\n");
	exit(0);
}

int skipVerbHeader(byte *p)
{
	byte code;
	int offset = 19;

// two bytes obj id

	printf("Events:\n");

	while ((code = *p++) != 0) {
		offset = TO_LE_16(*(unsigned short *)p);
		printf("  %2X - %.4X\n", code, offset);
		p += sizeof(unsigned short);
	}
	return offset;
}

byte *skipVerbHeader_V5(byte *p)
{
	byte code;
	byte *p2 = p;
	int hdrlen;

	while ((code = *p2++) != 0) {
		p2 += sizeof(unsigned short);
	}

	printf("Events:\n");

	hdrlen = p2 - p + 8;

	while ((code = *p++) != 0) {
		printf("  %2X - %.4X\n", code, *(unsigned short *)p - hdrlen);
		p += sizeof(unsigned short);
	}
	return p;
}


int main(int argc, char *argv[])
{
	FILE *in;
	byte *mem, *memorg;
	int len;
	char *buf;										/* token buffer */
	char *filename;
	int i;
	char *s;

	filename = NULL;
	IndyFlag = 0;
	/* Parse the arguments */
	for (i = 1; i < argc; i++) {
		s = argv[i];

		if (s && s[0] == '-') {
			s++;
			while (*s) {
				switch (tolower(*s)) {
				case '2':
					ScriptVersion = 2;
					GF_UNBLOCKED = true;
					break;
				case '3':
					ScriptVersion = 3;
					break;
				case '5':
					ScriptVersion = 5;
					break;
				case 'n':
					IndyFlag = 1; // Indy3
					break;
				case 'u':
					GF_UNBLOCKED = true;
					break;

				case 'o':
					AlwaysShowOffs = 1;
					break;
				case 'i':
					DontOutputIfs = 1;
					break;
				case 'e':
					DontOutputElse = 1;
					break;
				case 'f':
					DontOutputElseif = 1;
					break;
				case 'c':
					DontShowOpcode = 1;
					break;
				case 'x':
					DontShowOffsets = 1;
					break;
				case 'h':
					HaltOnError = 1;
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

	if (!filename)
		ShowHelpAndExit();

	in = fopen(filename, "rb");
	if (!in) {
		printf("Unable to open %s\n", filename);
		return 1;
	}

	memorg = mem = (byte *)malloc(65536);
	len = fread(mem, 1, 65536, in);
	
	fclose(in);
	size_of_code = len;

	buf = (char *)malloc(4096);

	if (GF_UNBLOCKED) {
		mem += 4;
	} else if (ScriptVersion == 5) {
		switch (TO_BE_32(*((uint32 *)mem))) {
		case 'LSCR':
			printf("Script# %d\n", (byte)mem[8]);
			mem += 9;
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
			mem = skipVerbHeader_V5(mem + 8);
			break;											/* Verb */
		default:
			printf("Unknown script type!\n");
			exit(0);
		}
	} else {
		switch (TO_LE_16(*((uint16 *)mem + 2))) {
			case MKID('LS'):
				printf("Script# %d\n", (byte)mem[8]);
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
				mem += skipVerbHeader(mem + 19);
				break;			/* Verb */
			default:
				printf("Unknown script type!\n");
				exit(0);
			}
	}

	cur_pos = mem;
	org_pos = mem;

	len -= mem - memorg;

	do {
		int j = NumBlockStack;
		byte opcode = *cur_pos;
		curoffs = cur_pos - mem;
		if (ScriptVersion == 2)
			get_tok_V2(buf);
		else
			get_tok(buf);
		if (buf[0]) {
			WritePendingElse();
			if (HaveElse) {
				HaveElse = 0;
				j--;
			}
			OutputLine(buf, curoffs, opcode, j);
		}
		while (IndentBlock(get_curpos())) {
			OutputLine("}", -1, -1, -1);
		}

		fflush(stdout);
	} while (cur_pos < mem + len);


	printf("END\n");

	free(memorg);

	return 0;
}
