/* DeScumm - Scumm Script Disassembler (version 5 scripts)
 * Copyright (C) 2001  Ludvig Strigeus
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

#include <string.h>
#include <stdio.h>

//#ifdef UNIX
#include <ctype.h>
//#endif

#ifdef WIN32
#include <io.h>
#include <process.h>
#endif

#include <stdlib.h>

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

#define ATO (1<<31)
#define ANOLASTPAREN (1<<30)
#define ANOFIRSTPAREN (1<<29)
#define ASTARTCOMMA (1<<28)
#define AVARSTORE (1<<27)


#define uchar unsigned char
#define uint unsigned int
#define ushort unsigned short

typedef unsigned char byte;

void get_tok(char *buf);

#define JUMP_OPCODE 0x18

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


int get_curpos();

bool emit_if(char *before, char *after);

#if defined(SCUMM_BIG_ENDIAN)

unsigned long inline TO_LE_32(unsigned long a)
{
	return ((a >> 24) & 0xFF) + ((a >> 8) & 0xFF00) + ((a << 8) & 0xFF0000) +
		((a << 24) & 0xFF000000);
}

unsigned short inline TO_LE_16(unsigned short a)
{
	return ((a >> 8) & 0xFF) + ((a << 8) & 0xFF00);
}

#endif

int get_byte()
{
	return (byte) (*cur_pos++);
}

int get_word()
{
#if defined(SCUMM_BIG_ENDIAN)
	int i = TO_LE_16(*((short *)cur_pos));
#else
	int i = *((short *)cur_pos);
#endif
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

	int i = get_word();

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
		buf[0] = i, buf[1] = 0;
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
				if (args & 0xF000)
					buf = add_a_tok(strecpy(buf, ","), (args >> 12) & 0xF);
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
	printf("[%.4X] (%.2X) EXPRESSION_MODE: %s\n", offs,
				 *((byte *) (org_pos + offs)), data);
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

	if (org_pos[k] != JUMP_OPCODE)
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
		sprintf(buf, AlwaysShowOffs ? "} else /*%.4X*/ {" : "} else {",
						PendingElseTo);
		OutputLine(buf, PendingElseOffs, PendingElseOpcode,
							 PendingElseIndent - 1);
		PendingElse = 0;
	}
}

int HavePendingElse()
{
	return PendingElse;
}


void do_actorset(char *buf, byte opcode)
{
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
		switch (opcode & 0x1F) {
		case 0x01:
			buf = do_tok(buf, "Costume", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x02:
			buf =
				do_tok(buf, "WalkSpeed",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
			break;
		case 0x03:
			buf = do_tok(buf, "Sound", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x04:
			buf = do_tok(buf, "WalkAnimNr", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x05:
			buf =
				do_tok(buf, "TalkAnimNr",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
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
			buf =
				do_tok(buf, "Palette",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
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
			buf =
				do_tok(buf, "Scale",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
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
//    case 0x17: buf=do_tok(buf, "SetAD8", ((opcode&0x80)?A1V:A1B)); break;
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


void do_resource(char *buf, byte opco)
{
	char opcode = get_byte();
	switch (opcode & 31) {
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

void do_33(char *buf)
{
	int opcode = get_byte();

	//buf+=sprintf(buf, "SubCode33%.2X", opcode);

	switch (opcode & 0x1F) {
	case 0x01:
		do_tok(buf, "RoomScroll",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;
	case 0x02:
		do_tok(buf, "RoomColor", 0);
		break;
	case 0x03:
		do_tok(buf, "SetScreen",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
		break;
	case 0x04:
		buf =
			do_tok(buf, "SetPalColor",
						 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W) |
						 ((opcode & 0x20) ? A3V : A3W) | ANOLASTPAREN);
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1W));
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
		buf =
			do_tok(buf, "saveLoad?",
						 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
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
		buf =
			do_tok(buf, "palManipulate",
						 ANOLASTPAREN | ((opcode & 0x80) ? A1V : A1B));
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ANOLASTPAREN | ((opcode & 0x80) ?
																													 A1V : A1B) |
						 ((opcode & 0x40) ? A2V : A2B));
		opcode = get_byte();
		buf =
			do_tok(buf, NULL,
						 ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x10:
		do_tok(buf, "colorCycleDelay",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	default:
		strcpy(buf, "Unknown??");
	}

	strcat(buf, ")");
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
		do_tok(buf, "SetCursorImg",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
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
		do_tok(buf, "CursorCommand", A1VARUNTIL0xFF);
		break;
	default:
		sprintf(buf, "UnknownCursorCommand%.2X", opcode);
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
			buf =
				do_tok(buf, "SetXY",
							 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
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
				do_tok(buf, "SetToObject",
							 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
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
			buf =
				do_tok(buf, "Pos",
							 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
			break;
		case 0x1:
			buf = do_tok(buf, "Color", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x2:
			buf = do_tok(buf, "Clipped", ((opcode & 0x80) ? A1V : A1W));
			break;
		case 0x4:
			buf = do_tok(buf, "Center", 0);
			break;
		case 0x6:
			buf = do_tok(buf, "Left", 0);
			break;
		case 0x7:
			buf = do_tok(buf, "Overhead", 0);
			break;
		case 0x8:
			buf =
				do_tok(buf, "Unk8",
							 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
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
	return j + get_curpos();
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

	switch (opcode & 127) {
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

	sprintf(buf, "%sif (%s%s%s%s", before, var, cmp_texts[txt ^ neg], tmp2,
					after);
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

	buf = get_var(buf);

	switch (opcode & 127) {
	case 0x1A:
		s = " = ";
		break;											/* move */
	case 0x1B:
		s = " *= ";
		break;											/* mul */
	case 0x3A:
		s = " -= ";
		break;											/* sub */
	case 0x57:
		s = " |= ";
		break;											/* or */
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
		do_tok(buf, "SetBoxTo",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x2:
		do_tok(buf, "SetBoxScale",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x3:
		do_tok(buf, "SetBoxSlot",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x4:
		do_tok(buf, "SetBoxPath", 0);
		break;
	default:
		sprintf(buf, "SetBoxUnknown%.2X", opcode);
	}

}

void get_tok(char *buf)
{
	byte opcode = get_byte();

	switch (opcode) {

	case 0x00:
		do_tok(buf, "StopObjectScript", 0);
		break;
	case 0xA0:
		do_tok(buf, "StopScript", 0);
		break;

	case 0x01:
	case 0x21:
	case 0x41:
	case 0x61:
	case 0x81:
	case 0xA1:
	case 0xC1:
	case 0xE1:
		do_tok(buf, "PutActor",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W) |
					 ((opcode & 0x20) ? A3V : A3W));
		break;

	case 0x15:
	case 0x55:
	case 0x95:
	case 0xD5:
		do_tok(buf, "GetActorFromPos",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W) |
					 AVARSTORE);
		break;

	case 0x03:
	case 0x83:
		do_tok(buf, "GetActorLocation",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
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
	case 0x85:{

			buf =
				do_tok(buf, "DrawObject",
							 ((opcode & 0x80) ? A1V : A1W) | ANOLASTPAREN);
			opcode = get_byte();
			switch (opcode & 0x1F) {
			case 1:
				do_tok(buf, ", setXY(",
							 ANOLASTPAREN | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1W) |
							 ((opcode & 0x40) ? A2V : A2W));
				break;
			case 2:
				do_tok(buf, ", setImage(",
							 ANOLASTPAREN | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1W));
				break;
			}
			strcat(buf, "));");

		}
		break;

	case 0x06:
	case 0x86:
		do_tok(buf, "GetActorElevation",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x07:
	case 0x47:
	case 0x87:
	case 0xC7:
		do_tok(buf, "SetState",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x09:
	case 0x49:
	case 0x89:
	case 0xC9:
		do_tok(buf, "FaceActor",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x0A:
	case 0x8A:
		do_tok(buf, "startScriptAA",
					 ((opcode & 0x80) ? A1V : A1B) | A2VARUNTIL0xFF);
		break;
	case 0x2A:
	case 0xAA:
		do_tok(buf, "startScriptAB",
					 ((opcode & 0x80) ? A1V : A1B) | A2VARUNTIL0xFF);
		break;
	case 0x4A:
	case 0xCA:
		do_tok(buf, "startScriptBA",
					 ((opcode & 0x80) ? A1V : A1B) | A2VARUNTIL0xFF);
		break;
	case 0x6A:
	case 0xEA:
		do_tok(buf, "startScriptBB",
					 ((opcode & 0x80) ? A1V : A1B) | A2VARUNTIL0xFF);
		break;


	case 0x0B:
	case 0x4B:
	case 0x8B:
	case 0xCB:
		do_tok(buf, "getVerbEntryPoint",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V
																												: A2W));
		break;

	case 0x0c:
	case 0x8C:
		do_resource(buf, opcode);
		break;

	case 0x0D:
	case 0x4D:
	case 0x8D:
	case 0xCD:
		do_tok(buf, "WalkActorToActor",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
					 A3B);
		break;											/* arg1=actor, arg2=actor */

	case 0x0F:
	case 0x8F:
		do_tok(buf, "getObjectState", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
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
		do_tok(buf, "if ClassOfIs",
					 ((opcode & 0x80) ? A1V : A1W) | A2VARUNTIL0xFF | ATO);
		break;											/* arg1=object; vararg=classes to test; arg3=jumpoffs */

	case 0x1E:
	case 0x3E:
	case 0x5E:
	case 0x7E:
	case 0x9E:
	case 0xBE:
	case 0xDE:
	case 0xFE:
		do_tok(buf, "WalkTo",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W) |
					 ((opcode & 0x20) ? A3V : A3W));
		break;

	case 0x24:
	case 0x64:
	case 0xA4:
	case 0xE4:
		buf =
			do_tok(buf, "loadRoomWithEgo",
						 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B) |
						 ANOLASTPAREN);
		do_tok(buf, NULL, A1W | A2W | ANOFIRSTPAREN | ASTARTCOMMA);
		break;

	case 0x25:
	case 0x65:
	case 0xA5:
	case 0xE5:
		do_tok(buf, "PickUp",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x2C:
		do_cursor_command(buf);
		break;

	case 0x40:
		do_tok(buf, "runCutscene", A1VARUNTIL0xFF);
		break;

	case 0x42:
	case 0xC2:
		do_tok(buf, "chainScript",
					 ((opcode & 0x80) ? A1V : A1B) | A2VARUNTIL0xFF);
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
		do_tok(buf, "GetActorFacing", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x6C:
	case 0xEC:
		do_tok(buf, "GetActorWidth", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x71:
	case 0xF1:
		do_tok(buf, "GetActorCostume", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0xAE:{
			switch (opcode = get_byte()) {
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
		do_tok(buf, "GetDistActorToObject",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V
																												: A2W));
		break;


	case 0x36:
	case 0x76:
	case 0xB6:
	case 0xF6:
		do_tok(buf, "WalkActorToObject",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W));
		break;

	case 0x37:
	case 0x77:
	case 0xB7:
	case 0xF7:
		do_tok(buf, "StartObject",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B) |
					 A3VARUNTIL0xFF);
		break;

	case 0x19:
	case 0x39:
	case 0x59:
	case 0x79:
	case 0x99:
	case 0xB9:
	case 0xD9:
	case 0xF9:{
			buf = strecpy(buf, "DoSentence(");
			if (!(opcode & 0x80) && (*cur_pos == 254)) {
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
		do_tok(buf, "StopScript", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0xAc:
		do_expr_code(buf);
		break;

	case 0x11:
	case 0x51:
	case 0x91:
	case 0xD1:
		do_tok(buf, "ActorAnimate",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x80:
		do_tok(buf, "Break", 0);
		break;

	case 0xc0:
		do_tok(buf, "EndCutscene", 0);
		break;

	case 0x27:{									/* String management subcode */
			switch ((opcode = get_byte()) & 0x1F) {
			case 0x01:
				do_load_code_to_string(buf, opcode);
				break;
			case 0x02:
				do_tok(buf, "CopyString",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
				break;
			case 0x03:
				do_tok(buf, "SetStringChar",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) |
							 ((opcode & 0x20) ? A3V : A3B));
				break;									/* string(arg1)[arg2] = arg3 */
			case 0x04:
				do_tok(buf, "GetStringChar",
							 AVARSTORE | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ?
																														A2V : A2B));
				break;									/* arg1 = string(arg2)[arg3] */
			case 0x05:
				do_tok(buf, "CreateString",
							 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
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
		do_tok(buf, "StopMusic", 0);
		break;

	case 0x3F:
	case 0x7F:
	case 0xBF:
	case 0xFF:
		buf =
			do_tok(buf, "DrawBox",
						 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W) |
						 ANOLASTPAREN);
		opcode = get_byte();
		do_tok(buf, NULL,
					 ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1W) |
					 ((opcode & 0x40) ? A2V : A2W) | ((opcode & 0x20) ? A3V : A3B));
		break;


	case 0x02:
	case 0x82:
		do_tok(buf, "StartMusic", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0xCC:
		do_cc(buf);
		break;

	case 0x33:
		do_33(buf);
		break;

	case 0x68:
	case 0xE8:
		do_tok(buf, "GetScriptRunning",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
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
		do_tok(buf, "SetOwnerOf",
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x52:
	case 0xD2:
		do_tok(buf, "CameraFollowActor", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x58:
		if (!cur_pos)
			do_tok(buf, "StopOverride", A1B | A2B | A3W);
		else
			do_tok(buf, "Override", A1B);
		break;

	case 0x1C:
	case 0x9C:
		do_tok(buf, "StartSound", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x4C:
		do_tok(buf, "SoundKludge", A1VARUNTIL0xFF);
		break;

	case 0x3C:
	case 0xBC:
		do_tok(buf, "StopSound", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x98:
		do_tok(buf, "QuitPauseRestart", A1B);
		break;

	case 0x7B:
	case 0xFB:
		do_tok(buf, "GetActorBox", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x43:
	case 0xc3:
		do_tok(buf, "GetActorX", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x23:
	case 0xA3:
		do_tok(buf, "GetActorY", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x7A:
	case 0xFA:
		do_verbops(buf, opcode);
		break;

	case 0x2D:
	case 0x6D:
	case 0xAD:
	case 0xED:
		do_tok(buf, "PutActorInRoom",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x54:
	case 0xD4:
		do_tok(buf, "SetObjectName", ((opcode & 0x80) ? A1V : A1W) | A2ASCII);
		break;

	case 0x5D:
	case 0xDD:
		do_tok(buf, "SetClass", ((opcode & 0x80) ? A1V : A1W) | A2VARUNTIL0xFF);
		break;

	case 0x35:
	case 0x75:
	case 0xB5:
	case 0xF5:
		do_tok(buf, "FindObject",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V
																												: A2W));
		break;

	case 0x26:
	case 0xA6:{
			int i;
			char first = 1;

			buf = do_tok(buf, "SetVarRange", A1V | ANOLASTPAREN);
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
		do_tok(buf, "GetRandomNr", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x2B:
		do_tok(buf, "DelayJiffies", A1V);
		break;

	case 0x0E:
	case 0x4E:
	case 0x8E:
	case 0xCE:
		do_tok(buf, "PutActorAtObject",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W));
		break;

	case 0x12:
	case 0x92:
		do_tok(buf, "PanCameraTo", ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x32:
		do_tok(buf, "SetCameraAt", ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x30:
		do_matrix_ops(buf, opcode);
		break;

	case 0x7C:
		do_tok(buf, "IsSoundRunning", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x31:
	case 0xB1:
		do_tok(buf, "getInventoryCount",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x1f:
	case 0x5f:
	case 0x9f:
	case 0xdf:
		do_tok(buf, "IsActorInBox",
					 ATO | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V :
																									A2B));
		break;

	case 0x22:
	case 0xA2:
		do_tok(buf, "GetAnimCounter", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x3d:
	case 0x7d:
	case 0xBD:
	case 0xFD:
		do_tok(buf, "GetInventoryItem",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V
																												: A2B));
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
		do_tok(buf, "FreezeScripts", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x6E:
	case 0xEE:
		do_tok(buf, "StopScriptNr", ((opcode & 0x80) ? A1V : A1W));
		break;


	case 0x2f:
	case 0x45:
	case 0x4F:
	case 0x50:
	case 0x5C:
	case 0x6F:
	case 0xAF:
	case 0xC5:
	case 0xCF:
	case 0xD0:
	case 0xDC:
	case 0xEF:
		if (HaltOnError) {
			printf("Illegal opcode %.2X\n", opcode);
			exit(1);
		}
		sprintf(buf, "Illegal opcode %.2X", opcode);
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
				 "\t-o\tAlways Show offsets\n"
				 "\t-i\tDon't output ifs\n"
				 "\t-e\tDon't output else\n"
				 "\t-f\tDon't output else-if\n"
				 "\t-c\tDon't show opcode\n"
				 "\t-x\tDon't show offsets\n" "\t-h\tHalt on error\n");
	exit(0);
}

byte *skipVerbHeader(byte * p)
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

	/* Parse the arguments */
	for (i = 1; i < argc; i++) {
		s = argv[i];

		if (s && s[0] == '-') {
			s++;
			while (*s) {
				switch (tolower(*s)) {
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

	memorg = mem = (byte *) malloc(65536);
	len = fread(mem, 1, 65536, in);
	fclose(in);
	size_of_code = len;

	buf = (char *)malloc(4096);

#if defined(SCUMM_BIG_ENDIAN)
	switch (TO_LE_32(*((long *)mem))) {
#else
	switch (*((long *)mem)) {
#endif
	case 'RCSL':
		printf("Script# %d\n", (unsigned char)mem[8]);
		mem += 9;
		break;											/* Local script */
	case 'PRCS':
		mem += 8;
		break;											/* Script */
	case 'DCNE':
		mem += 8;
		break;											/* Entry code */
	case 'DCXE':
		mem += 8;
		break;											/* Exit code */
	case 'BREV':
		mem = skipVerbHeader(mem + 8);
		break;											/* Verb */
	default:
		printf("Unknown script type!\n");
		exit(0);
	}

	cur_pos = mem;
	org_pos = mem;

	len -= mem - memorg;

	do {
		int j = NumBlockStack;
		byte opcode = *cur_pos;
		curoffs = cur_pos - mem;
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
