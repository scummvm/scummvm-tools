/* DeScumm - Scumm Script Disassembler (version 6 scripts)
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
#include <assert.h>


typedef unsigned char byte;
typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned long uint32;
typedef unsigned int uint;
typedef signed char int8;
typedef signed short int16;
typedef signed long int32;

#define JUMP_OPCODE 0x73

#if defined(SCUMM_BIG_ENDIAN)

uint32 inline TO_LE_32(uint32 a)
{
	return ((a >> 24) & 0xFF) + ((a >> 8) & 0xFF00) + ((a << 8) & 0xFF0000) +
		((a << 24) & 0xFF000000);
}

uint16 inline TO_LE_16(uint16 a)
{
	return ((a >> 8) & 0xFF) + ((a << 8) & 0xFF00);
}

#endif

struct StackEnt {
	byte type;
	long data;
	StackEnt *left, *right;
	char *str;
	StackEnt **list;
};

struct BlockStack {
	unsigned short from;
	unsigned short to;
};

enum {
	seInt = 1,
	seVar = 2,
	seArray = 3,
	seBinary = 4,
	seUnary = 5,
	seComplex = 6,
	seStackList = 7,
	seDup = 8,
	seNeg = 9,
};

enum {
	isZero,
	isEqual,
	isNotEqual,
	isGreater,
	isLess,
	isLessEqual,
	isGreaterEqual,
	operAdd,
	operSub,
	operMul,
	operDiv,
	operLand,
	operLor,
};

static const char *oper_list[] = {
	"0==",
	"==",
	"!=",
	">",
	"<",
	"<=",
	">=",
	"+",
	"-",
	"*",
	"/",
	"&&",
	"||"
};

StackEnt *stack[128];
int num_stack;
byte *cur_pos, *org_pos;
int size_of_code;

char *output;

bool pendingElse, haveElse;
int pendingElseTo;
int pendingElseOffs;
int pendingElseOpcode;
int pendingElseIndent;


int get_curoffs()
{
	return cur_pos - org_pos;
}

byte alwaysShowOffs;
byte dontOutputIfs;
byte dontOutputElse;
byte dontOutputElseif;
byte dontShowOpcode;
byte dontShowOffsets;
byte haltOnError;
byte scriptVersion = 6;

BlockStack *block_stack;
int num_block_stack;

const char *var_names[] = {
	/* 0 */
	NULL,
	"g_ego",
	"g_camera_cur_pos",
	"g_have_msg",
	/* 4 */
	"g_room",
	"g_override",
	NULL,
	NULL,
	/* 8 */
	"g_num_actor",
	NULL,
	"g_drive_number",
	"g_timer_1",
	/* 12 */
	"g_timer_2",
	"g_timer_3",
	NULL,
	NULL,
	/* 16 */
	NULL,
	"g_camera_min",
	"g_camera_max",
	"g_timer_next",
	/* 20 */
	"g_virtual_mouse_x",
	"g_virtual_mouse_y",
	"g_room_resource",
	"g_last_sound",
	/* 24 */
	"g_cutsceneexit_key",
	"g_talk_actor",
	"g_camera_fast",
	"g_scroll_script",
	/* 28 */
	"g_entry_script",
	"g_entry_script_2",
	"g_exit_script",
	"g_exit_script_2",
	/* 32 */
	"g_verb_script",
	"g_sentence_script",
	"g_hook_script",
	"g_begin_cutscene_script",
	/* 36 */
	"g_end_cutscene_script",
	"g_char_inc",
	"g_walkto_obj",
	"g_debug_mode",
	/* 40 */
	"g_heap_space",
	"g_scr_width",
	"g_restart_key",
	"g_pause_key",
	/* 44 */
	"g_mouse_x",
	"g_mouse_y",
	"g_timer",
	"g_timer_4",
	/* 48 */
	NULL,
	"g_video_mode",
	"g_save_load_key",
	"g_fixed_disk",
	/* 52 */
	"g_cursor_state",
	"g_user_put",
	"g_scr_height",
	NULL,
	/* 56 */
	"g_sound_thing",
	"g_talkstop_key",
	NULL,
	NULL,
	/* 60 */
	NULL,
	NULL,
	NULL,
	NULL,
	/* 64 */
	"g_sound_param",
	"g_sound_param_2",
	"g_sound_param_3",
	"g_mouse_present",
	/* 68 */
	"g_performance_1",
	"g_performance_2",
	NULL,
	"g_save_load_thing",
	/* 72 */
	"g_new_room",
	NULL,
	NULL,
	NULL,
	/* 76 */
	"g_ems_space"
};

const char *getVarName(uint var)
{
	if (var >= sizeof(var_names) / sizeof(var_names[0]))
		return NULL;
	return var_names[var];
}

void push(StackEnt * se)
{
	assert(se);
	stack[num_stack++] = se;
}

StackEnt *pop()
{
	if (num_stack == 0) {
		printf("No items on stack to pop!\n");
		exit(1);
	}
	return stack[--num_stack];
}

void invalidop(const char *cmd, int op)
{
	if (cmd)
		printf("invalid opcode %s:%d\n", cmd, op);
	else
		printf("invalid opcode %d\n", op);
	exit(1);
}

char *strecpy(char *buf, const char *src)
{
	strcpy(buf, src);
	return strchr(buf, 0);
}

int get_byte()
{
	return (byte)(*cur_pos++);
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


byte *skipVerbHeader(byte *p)
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

StackEnt *se_new(int type)
{
	StackEnt *se = (StackEnt *) malloc(sizeof(StackEnt));
	se->type = type;
	return se;
}

void se_free(StackEnt * se)
{
	free(se);
}

StackEnt *se_neg(StackEnt * se)
{
	StackEnt *s = se_new(seNeg);
	s->left = se;
	return s;
}

StackEnt *se_int(int i)
{
	StackEnt *se = se_new(seInt);
	se->data = i;
	return se;
}

StackEnt *se_var(int i)
{
	StackEnt *se = se_new(seVar);
	se->data = i;
	return se;
}

StackEnt *se_array(int i, StackEnt * dim2, StackEnt * dim1)
{
	StackEnt *se = se_new(seArray);
	se->left = dim2;
	se->right = dim1;
	se->data = i;
	return se;
}

StackEnt *se_oper(StackEnt * a, int op)
{
	StackEnt *se = se_new(seUnary);
	se->data = op;
	se->left = a;
	return se;
}

StackEnt *se_oper(StackEnt * a, int op, StackEnt * b)
{
	StackEnt *se = se_new(seBinary);
	se->data = op;
	se->left = a;
	se->right = b;
	return se;
}

StackEnt *se_complex(char *s)
{
	StackEnt *se = se_new(seComplex);
	se->str = strdup(s);
	return se;
}


char *se_astext(StackEnt * se, char *where, bool wantparens = true)
{
	int i;
	int var;
	const char *s;

	switch (se->type) {
	case seInt:
		where += sprintf(where, "%ld", se->data);
		break;
	case seVar:
		if (!(se->data & 0xF000)) {
			var = se->data & 0xFFF;
			if ((s = getVarName(var)) != NULL)
				where = strecpy(where, s);
			else
				where += sprintf(where, "var[%ld]", se->data & 0xFFF);
		} else if (se->data & 0x8000) {
			where += sprintf(where, "bitvar[%ld]", se->data & 0x7FFF);
		} else if (se->data & 0x4000) {
			where += sprintf(where, "localvar[%ld]", se->data & 0xFFF);
		} else {
			where += sprintf(where, "??var??[%ld]", se->data & 0xFFFF);
		}
		break;
	case seArray:
		if (se->left) {
			where += sprintf(where, "array[%ld][", se->data);
			where = se_astext(se->left, where);
			where = strecpy(where, "][");
			where = se_astext(se->right, where);
			where = strecpy(where, "]");
		} else {
			where += sprintf(where, "array[%ld][", se->data);
			where = se_astext(se->right, where);
			where = strecpy(where, "]");
		}
		break;
	case seUnary:
		where += sprintf(where, "%s ", oper_list[se->data]);
		where = se_astext(se->left, where);
		break;
	case seBinary:
		if (wantparens)
			*where++ = '(';
		where = se_astext(se->left, where);
		where += sprintf(where, " %s ", oper_list[se->data]);
		where = se_astext(se->right, where);
		if (wantparens)
			*where++ = ')';
		*where = 0;
		break;
	case seComplex:
		where = strecpy(where, se->str);
		break;
	case seStackList:
		*where++ = '[';
		for (i = se->data; --i >= 0;) {
			where = se_astext(se->list[i], where);
			if (i)
				*where++ = ',';
		}
		*where++ = ']';
		*where = 0;
		break;
	case seDup:
		where += sprintf(where, "dup[%ld]", se->data);
		break;
	case seNeg:
		*where++ = '!';
		where = se_astext(se->left, where);
		break;
	}
	return where;
}

void kill(StackEnt * se)
{
	if (se->type != seDup) {
		char *e = strecpy(output, "kill(");
		e = se_astext(se, e);
		strcpy(e, ")");
		se_free(se);
	} else {
		push(se);
	}
}

void doAssign(StackEnt * dst, StackEnt * src)
{
	if (src->type == seDup && dst->type == seDup) {
		dst->data = src->data;
		return;
	}
	char *e = se_astext(dst, output);
	e = strecpy(e, " = ");
	se_astext(src, e);
}

void doAdd(StackEnt * se, int val)
{
	char *e = se_astext(se, output);
	sprintf(e, " += %d", val);
}

int dupindex;

StackEnt *dup(StackEnt * se)
{
	if (se->type == seInt)
		return se;

	StackEnt *dse = se_new(seDup);
	dse->data = ++dupindex;
	doAssign(dse, se);
	return dse;
}

void writeArray(int i, StackEnt * dim2, StackEnt * dim1, StackEnt * value)
{
	StackEnt *array = se_array(i, dim2, dim1);
	doAssign(array, value);
	se_free(array);
}

void writeVar(int i, StackEnt * value)
{
	StackEnt *se = se_var(i);
	doAssign(se, value);
	se_free(se);
}

void addArray(int i, StackEnt * dim1, int val)
{
	StackEnt *array = se_array(i, NULL, dim1);
	doAdd(array, val);
	se_free(array);
}

void addVar(int i, int val)
{
	StackEnt *se = se_var(i);
	doAdd(se, val);
	se_free(se);
}

StackEnt *se_get_string()
{
	byte cmd;
	char buf[1024];
	char *e = buf;
	bool in = false;
	int i;

	while ((cmd = get_byte()) != 0) {
		if (cmd == 0xFF || cmd == 0xFE) {
			if (in) {
				*e++ = '"';
				in = false;
			}
			i = get_byte();
			switch (i) {
			case 3:
				e += sprintf(e, ":wait:");
				break;
			case 1:
				e += sprintf(e, ":newline:");
				break;
			case 2:
				e += sprintf(e, ":keeptext:");
				break;
			case 9:
				e += sprintf(e, ":startanim=%d:", get_word());
				break;
			case 10:
				e += sprintf(e, ":sound:");
				cur_pos += 14;
				break;
			case 14:
				e += sprintf(e, ":setfont=%d:", get_word());
				break;
			case 12:
				e += sprintf(e, ":setcolor=%d:", get_word());
				break;
			case 13:
				e += sprintf(e, ":unk2=%d:", get_word());
				break;
			default:
				e += sprintf(e, ":unk%d=%d:", i, get_word());
			}
		} else {
			if (!in) {
				*e++ = '"';
				in = true;
			}
			*e++ = cmd;
		}
	}
	if (in)
		*e++ = '"';
	*e = 0;
	return se_complex(buf);
}

StackEnt *se_get_list()
{
	StackEnt *se = se_new(seStackList);
	StackEnt *senum = pop();
	int num, i;

	if (senum->type != seInt) {
		printf("stackList with variable number of arguments, cannot disassemble");
		exit(1);
	}
	se->data = num = senum->data;
	se->list = (StackEnt **) calloc(num, sizeof(StackEnt *));

	for(i = 0; i < num; i++) {
		se->list[i] = pop();
	}
	return se;
}

void ext(const char *fmt)
{
	bool wantresult;
	byte cmd, extcmd;
	const char *extstr = NULL;
	const char *prep = NULL;
	StackEnt *args[10];
	int numArgs = 0;
	char *e;

	/* return the result? */
	wantresult = false;
	if (*fmt == 'r') {
		wantresult = true;
		fmt++;
	}

	while ((cmd = *fmt++) != '|') {
		if (cmd == 'x' && !extstr) {
			extstr = fmt;
			fmt += strlen(fmt) + 1;

			/* extended thing */
			extcmd = get_byte();

			/* locate our extended item */
			while ((cmd = *fmt++) != extcmd) {
				/* scan until we find , or \0 */
				while ((cmd = *fmt++) != ',') {
					if (cmd == 0) {
						invalidop(extstr, extcmd);
					}
				}
			}
			/* found a command, continue at the beginning */
			continue;
		}
		if (cmd == 'm' && !prep) {
			prep = fmt;
			fmt += strlen(fmt) + 1;
			continue;
		}

		if (cmd == 'p') {
			args[numArgs++] = pop();
		} else if (cmd == 's') {
			args[numArgs++] = se_get_string();
		} else if (cmd == 'w') {
			args[numArgs++] = se_int(get_word());
		} else if (cmd == 'l') {
			args[numArgs++] = se_get_list();
		} else if (cmd == 'j') {
			args[numArgs++] = se_int(get_word());
		} else {
			printf("error in argument string '%s', character '%c' unknown\n", fmt, cmd);
		}
	}

	/* create a string from the arguments */

	e = (char *)output;
	if (prep)
		e = strecpy(e, prep);
	while ((cmd = *fmt++) != 0 && cmd != ',')
		*e++ = cmd;
	*e++ = '(';
	while (--numArgs >= 0) {
		e = se_astext(args[numArgs], e);
		if (numArgs)
			*e++ = ',';
	}
	*e++ = ')';
	*e = 0;

	if (wantresult) {
		push(se_complex((char *)output));
		output[0] = 0;
		return;
	}
}

BlockStack *pushBlockStackItem()
{
	if (!block_stack)
		block_stack = (BlockStack *) malloc(256 * sizeof(BlockStack));

	if (num_block_stack >= 256) {
		printf("BlockStack full!\n");
		exit(0);
	}
	return &block_stack[num_block_stack++];
}


bool maybeAddIf(unsigned int cur, unsigned int to)
{
	int i;
	BlockStack *p;

	if (((to | cur) >> 16) || (to <= cur))
		return 0;										/* Invalid jump */

	for (i = 0, p = block_stack; i < num_block_stack; i++, p++) {
		if (to > p->to)
			return false;
	}

	p = pushBlockStackItem();
	p->from = cur;
	p->to = to;
	return true;
}

/* Returns 0 or 1 depending if it's ok to add an else */
bool maybeAddElse(unsigned int cur, unsigned int to)
{
	BlockStack *p;
	bool i;

	if (((to | cur) >> 16) || (to <= cur))
		return false;								/* Invalid jump */

	if (!num_block_stack)
		return false;								/* There are no previous blocks, so an else is not ok */

	p = &block_stack[num_block_stack - 1];
	if (cur != p->to)
		return false;								/* We have no prevoius if that is exiting right at the end of this goto */

	num_block_stack--;
	i = maybeAddIf(cur, to);
	if (i)
		return i;										/* We can add an else */
	num_block_stack++;
	return false;									/* An else is not OK here :( */
}



int maybeAddElseIf(unsigned int cur, unsigned int elseto, unsigned int to)
{
	unsigned int k;
	BlockStack *p;

	if (((to | cur | elseto) >> 16) || (elseto < to) || (to <= cur))
		return false;								/* Invalid jump */

	if (!num_block_stack)
		return false;								/* There are no previous blocks, so an ifelse is not ok */

	k = to - 3;
	if ((uint) k >= (uint) size_of_code)
		return false;								/* Invalid jump */

	if (org_pos[k] != JUMP_OPCODE)
		return false;								/* Invalid jump */

	k = to + *((short *)(org_pos + k + 1));

	if (k != elseto)
		return false;								/* Not an ifelse */

	p = &block_stack[num_block_stack - 1];
	p->from = cur;
	p->to = to;

	return true;
}


void jump()
{
	int cur = get_curoffs() + 2;
	int to = cur + (int16)get_word();

	if (!dontOutputElse && maybeAddElse(cur, to)) {
		pendingElse = true;
		pendingElseTo = to;
		pendingElseOffs = cur - 1;
		pendingElseOpcode = JUMP_OPCODE;
		pendingElseIndent = num_block_stack;
	} else {
		sprintf(output, "jump %x", to);
	}
}

void jumpif(StackEnt * se, bool when)
{
	int cur = get_curoffs() + 2;
	int to = cur + (int16)get_word();
	char *e = output;

	if (!dontOutputElseif && pendingElse) {
		if (maybeAddElseIf(cur, pendingElseTo, to)) {
			pendingElse = false;
			haveElse = true;
			e = strecpy(e, "} else if (");
			if (when)
				se = se_neg(se);
			e = se_astext(se, e, false);
			sprintf(e, alwaysShowOffs ? ") /*%.4X*/ {" : ") {", to);
			return;
		}
	}

	if (!dontOutputIfs && maybeAddIf(cur, to)) {
		e = strecpy(e, "if (");
		if (when)
			se = se_neg(se);
		e = se_astext(se, e, false);
		sprintf(e, alwaysShowOffs ? ") /*%.4X*/ {" : ") {", to);
		return;
	}

	e = strecpy(e, when ? "if (" : "if (!");
	e = se_astext(se, e);
	sprintf(e, ") goto %x", to);
}


void next_line()
{
	byte code = get_byte();
	StackEnt *se_a;

	switch (code) {
	case 0x0:
		push(se_int(get_byte()));
		break;
	case 0x1:
		push(se_int(get_word()));
		break;
	case 0x2:
		push(se_var(get_byte()));
		break;
	case 0x3:
		push(se_var(get_word()));
		break;
	case 0x6:
		push(se_array(get_byte(), NULL, pop()));
		break;
	case 0x7:
		push(se_array(get_word(), NULL, pop()));
		break;
	case 0xA:
		push(se_array(get_byte(), pop(), pop()));
		break;
	case 0xB:
		push(se_array(get_word(), pop(), pop()));
		break;
	case 0xC:
		se_a = dup(pop());
		push(se_a);
		push(se_a);
		break;
	case 0xD:
		push(se_oper(pop(), isZero));
		break;
	case 0xE:
		push(se_oper(pop(), isEqual, pop()));
		break;
	case 0xF:
		push(se_oper(pop(), isNotEqual, pop()));
		break;
	case 0x10:
		push(se_oper(pop(), isGreater, pop()));
		break;
	case 0x11:
		push(se_oper(pop(), isLess, pop()));
		break;
	case 0x12:
		push(se_oper(pop(), isLessEqual, pop()));
		break;
	case 0x13:
		push(se_oper(pop(), isGreaterEqual, pop()));
		break;
	case 0x14:
		push(se_oper(pop(), operAdd, pop()));
		break;
	case 0x15:
		push(se_oper(pop(), operSub, pop()));
		break;
	case 0x16:
		push(se_oper(pop(), operMul, pop()));
		break;
	case 0x17:
		push(se_oper(pop(), operDiv, pop()));
		break;
	case 0x18:
		push(se_oper(pop(), operLand, pop()));
		break;
	case 0x19:
		push(se_oper(pop(), operLor, pop()));
		break;
	case 0x1A:
		kill(pop());
		break;
	case 0x42:
		writeVar(get_byte(), pop());
		break;
	case 0x43:
		writeVar(get_word(), pop());
		break;
	case 0x46:
		writeArray(get_byte(), NULL, pop(), pop());
		break;
	case 0x47:
		writeArray(get_word(), NULL, pop(), pop());
		break;
	case 0x4A:
		writeArray(get_byte(), pop(), pop(), pop());
		break;
	case 0x4B:
		writeArray(get_word(), pop(), pop(), pop());
		break;
	case 0x4E:
		addVar(get_byte(), 1);
		break;
	case 0x4F:
		addVar(get_word(), 1);
		break;
	case 0x52:
		addArray(get_byte(), pop(), 1);
		break;
	case 0x53:
		addArray(get_word(), pop(), 1);
		break;
	case 0x56:
		addVar(get_byte(), -1);
		break;
	case 0x57:
		addVar(get_word(), -1);
		break;
	case 0x5A:
		addArray(get_byte(), pop(), -1);
		break;
	case 0x5B:
		addArray(get_word(), pop(), -1);
		break;
	case 0x5C:
		jumpif(pop(), true);
		break;
	case 0x5D:
		jumpif(pop(), false);
		break;
	case 0x5E:
		ext("lpp|startScriptEx");
		break;
	case 0x5F:
		ext("lp|startScript");
		break;
	case 0x60:
		ext("lppp|startObject");
		break;
	case 0x61:
		ext("pp|setObjectState");
		break;
	case 0x62:
		ext("ppp|setObjectXY");
		break;
		/* *** */
	case 0x65:
		ext("|stopObjectCodeA");
		break;
	case 0x66:
		ext("|stopObjectCodeB");
		break;
	case 0x67:
		ext("|endCutscene");
		break;
	case 0x68:
		ext("l|beginCutscene");
		break;
	case 0x69:
		ext("|stopMusic");
		break;
	case 0x6A:
		ext("p|freezeUnfreeze");
		break;
	case 0x6B:
		ext("x" "cursorCommand\0"
				"\x90|cursorOn,"
				"\x91|cursorOff,"
				"\x92|userPutOn,"
				"\x93|userPutOff,"
				"\x94|softCursorOn,"
				"\x95|softCursorOff,"
				"\x96|softUserputOn,"
				"\x97|softUserputOff,"
				"\x99pp|setCursorImg,"
				"\x9App|setCursorHotspot," "\x9Cp|initCharset," "\x9Dl|charsetColors," "\xD6p|new_unk_1");
		break;
	case 0x6C:
		ext("|break");
		break;
	case 0x6D:
		ext("rlp|ifClassOfIs");
		break;
	case 0x6E:
		ext("lp|setClass");
		break;
	case 0x6F:
		ext("rp|getState");
		break;
	case 0x70:
		ext("pp|setState");
		break;
	case 0x71:
		ext("pp|setOwner");
		break;
	case 0x72:
		ext("rp|getOwner");
		break;
	case 0x73:
		jump();
		break;
	case 0x74:
		ext("p|startSound");
		break;
	case 0x75:
		ext("p|stopSound");
		break;
	case 0x76:
		ext("p|startMusic");
		break;
	case 0x77:
		ext("p|stopObjectScript");
		break;
	case 0x78:
		ext("p|panCameraTo");
		break;
	case 0x79:
		ext("p|actorFollowCamera");
		break;
	case 0x7A:
		ext("p|setCameraAt");
		break;
	case 0x7B:
		ext("p|loadRoom");
		break;
	case 0x7C:
		ext("p|stopScript");
		break;
	case 0x7D:
		ext("ppp|walkActorToObj");
		break;
	case 0x7E:
		ext("ppp|walkActorTo");
		break;
	case 0x7F:
		ext("pppp|putActorInRoom");
		break;
	case 0x80:
		ext("ppp|putActorAtObject");
		break;
	case 0x81:
		ext("pp|faceActor");
		break;
	case 0x82:
		ext("pp|animateActor");
		break;
	case 0x83:
		ext("pppp|doSentence");
		break;
	case 0x84:
		ext("pp|pickupObject");
		break;
	case 0x85:
		ext("pppp|loadRoomWithEgo");
		break;
	case 0x87:
		ext("rp|getRandomNumber");
		break;
	case 0x88:
		ext("rpp|getRandomNumberRng");
		break;
	case 0x8A:
		ext("rp|getActorMoving");
		break;
	case 0x8B:
		ext("rp|getScriptRunning");
		break;
	case 0x8C:
		ext("rp|getActorRoom");
		break;
	case 0x8D:
		ext("rp|getObjectX");
		break;
	case 0x8E:
		ext("rp|getObjectY");
		break;
	case 0x8F:
		ext("rp|getObjectDir");
		break;
	case 0x90:
		ext("rp|getActorWalkBox");
		break;
	case 0x91:
		ext("rp|getActorCostume");
		break;
	case 0x92:
		ext("rpp|findInventory");
		break;
	case 0x93:
		ext("rp|getInventoryCount");
		break;
	case 0x94:
		ext("rpp|getVerbFromXY");
		break;
	case 0x95:
		ext("|beginOverride");
		break;
	case 0x96:
		ext("|endOverride");
		break;
	case 0x97:
		ext("ps|setObjectName");
		break;
	case 0x98:
		ext("rp|isSoundRunning");
		break;
	case 0x99:
		ext("pl|setBoxFlags");
		break;
	case 0x9A:
		ext("|createBoxMatrix");
		break;
	case 0x9B:
		ext("x" "resourceRoutines\0"
				"\x64p|loadScript,"
				"\x65p|loadSound,"
				"\x66p|loadCostume,"
				"\x67p|loadRoom,"
				"\x68p|nukeScript,"
				"\x69p|nukeSound,"
				"\x6Ap|nukeCostume,"
				"\x6Bp|nukeRoom,"
				"\x6Cp|lockScript,"
				"\x6Dp|lockSound,"
				"\x6Ep|lockCostume,"
				"\x6Fp|lockRoom,"
				"\x70p|unlockScript,"
				"\x71p|unlockSound,"
				"\x72p|unlockCostume,"
				"\x73p|unlockRoom," "\x75p|loadCharset," "\x76p|nukeCharset," "\x77pp|unkResProc");
		break;
	case 0x9C:
		ext("x" "roomOps\0"
				"\xACpp|roomScroll,"
				"\xAEpp|setScreen,"
				"\xAFpppp|setPalColor,"
				"\xB0|shakeOn,"
				"\xB1|shakeOff,"
				"\xB3ppp|unkRoomFunc2,"
				"\xB4pp|saveLoadThing,"
				"\xB5p|screenEffect,"
				"\xB6ppppp|unkRoomFunc2,"
				"\xB7ppppp|unkRoomFunc3,"
				"\xBApppp|palManipulate," "\xBBpp|colorCycleDelay," "\xD5p|setPalette");
		break;
	case 0x9D:
		ext("x" "actorSet\0"
				"\xC5p|setCurActor,"
				"\x4Cp|setActorCostume,"
				"\x4Dpp|setActorWalkSpeed,"
				"\x4El|setActorSound,"
				"\x4Fp|setActorWalkFrame,"
				"\x50pp|setActorTalkFrame,"
				"\x51p|setActorStandFrame,"
				"\x52ppp|actorSet:82:??,"
				"\x53|initActor,"
				"\x54|setActorElevation,"
				"\x55|setActorDefAnim,"
				"\x56pp|setActorPalette,"
				"\x57p|setActorTalkColor,"
				"\x58s|setActorName,"
				"\x59p|setActorInitFrame,"
				"\x5Bp|setActorWidth,"
				"\x5Cp|setActorScale,"
				"\x5D|setActorNeverZClip,"
				"\x5Ep|setActorNeverZClip,"
				"\x5F|setActorIgnoreBoxes,"
				"\x60|setActorFollowBoxes,"
				"\x61|setActorAnimSpeed,"
				"\x62|setActorData8,"
				"\x63pp|setActorTalkPos,"
				"\xD7|setActorNew3On," "\xD8|setActorNew3Off," "\xD9|initActorLittle," "\xE3p|setActorLayer");
		break;
	case 0x9E:
		ext("x" "verbOps\0"
				"\xC4p|setCurVerb,"
				"\x7Cp|verbLoadImg,"
				"\x7Ds|verbLoadString,"
				"\x7Ep|verbSetColor,"
				"\x7Fp|verbSetHiColor,"
				"\x80pp|verbSetXY,"
				"\x81|verbSetCurmode1,"
				"\x82|verbSetCurmode0,"
				"\x83|verbKill,"
				"\x84|verbInit,"
				"\x85p|verbSetDimColor,"
				"\x86|verbSetCurmode2,"
				"\x87p|verbSetKey,"
				"\x88p|verbSetCenter,"
				"\x89p|verbSetToString,"
				"\x8Bpp|verbSetToObject," "\x8Cp|verbSetBkColor," "\xFF|verbRedraw");
		break;
	case 0x9F:
		ext("rpp|getActorFromXY");
		break;
	case 0xA0:
		ext("rpp|findObject");
		break;
	case 0xA1:
		ext("lp|pseudoRoom");
		break;
	case 0xA2:
		ext("rp|getActorElevation");
		break;
	case 0xA3:
		ext("rpp|getVerbEntrypoint");
		break;
	case 0xA4:
		ext("x" "arrayOps\0" "\xCDwps|arrayOps205," "\xD0wpl|arrayOps208," "\xD4wplp|arrayOps212");
		break;
	case 0xA5:
		ext("x" "saveRestoreVerbs\0"
				"\x8Dppp|saveRestoreA," "\x8Eppp|saveRestoreB," "\x8Fppp|saveRestoreC");
		break;
	case 0xA6:
		ext("ppppp|drawBox");
		break;
	case 0xA8:
		ext("rp|getActorWidth");
		break;
	case 0xA9:
		ext("x" "wait\0"
				"\xA8pj|waitForActor," "\xA9|waitForMessage," "\xAA|waitForCamera," "\xAB|waitForSentence");
		break;
	case 0xAA:
		ext("rp|getActorScaleX");
		break;
	case 0xAB:
		ext("rp|getActorAnimCounter1");
		break;
	case 0xAC:
		ext("l|soundKludge");
		break;
	case 0xAD:
		ext("rlp|isAnyOf");
		break;
	case 0xAE:
		ext("x" "quitPauseRestart\0" "\x9E|pauseGame," "\xA0|shutDown");
		break;
	case 0xAF:
		ext("rp|isActorInBox");
		break;
	case 0xB0:
		ext("p|delay");
		break;
	case 0xB1:
		ext("p|delayLonger");
		break;
	case 0xB2:
		ext("p|delayVeryLong");
		break;
	case 0xB3:
		ext("|stopSentence");
		break;
	case 0xB4:
		ext("m" "print_0_\0"
				"x" "print_0\0"
				"\x41pp|XY,"
				"\x42p|color,"
				"\x43p|right,"
				"\x45|center,"
				"\x47|left," "\x48|overhead," "\x4A|new3," "\x4Bs|msg," "\xFE|begin," "\xFF|end");
		break;
	case 0xB5:
		ext("m" "print_1_\0"
				"x" "print_1\0"
				"\x41pp|XY,"
				"\x42p|color,"
				"\x43p|right,"
				"\x45|center,"
				"\x47|left," "\x48|overhead," "\x4A|new3," "\x4Bs|msg," "\xFE|begin," "\xFF|end");
		break;
	case 0xB6:
		ext("m" "print_2_\0"
				"x" "print_2\0"
				"\x41pp|XY,"
				"\x42p|color,"
				"\x43p|right,"
				"\x45|center,"
				"\x47|left," "\x48|overhead," "\x4A|new3," "\x4Bs|msg," "\xFE|begin," "\xFF|end");
		break;
	case 0xB7:
		ext("m" "print_3_\0"
				"x" "print_3\0"
				"\x41pp|XY,"
				"\x42p|color,"
				"\x43p|right,"
				"\x45|center,"
				"\x47|left," "\x48|overhead," "\x4A|new3," "\x4Bs|msg," "\xFE|begin," "\xFF|end");
		break;
	case 0xB8:
		ext("m" "print_actor_\0"
				"x" "print_actor\0"
				"\x41pp|XY,"
				"\x42p|color,"
				"\x43p|right,"
				"\x45|center,"
				"\x47|left," "\x48|overhead," "\x4A|new3," "\x4Bs|msg," "\xFEp|begin," "\xFF|end");
		break;
	case 0xB9:
		ext("m" "print_ego_\0"
				"x" "print_ego\0"
				"\x41pp|XY,"
				"\x42p|color,"
				"\x43p|right,"
				"\x45|center,"
				"\x47|left," "\x48|overhead," "\x4A|new3," "\x4Bs|msg," "\xFE|begin," "\xFF|end");
		break;
	case 0xBA:
		ext("ps|talkActor");
		break;
	case 0xBB:
		ext("s|talkEgo");
		break;
	case 0xBC:
		ext("x" "dim\0"
				"\xC7pw|dimType5,"
				"\xC8pw|dimType1,"
				"\xC9pw|dimType2," "\xCApw|dimType3," "\xCBpw|dimType4," "\xCCpw|nukeArray");
		break;
	case 0xBE:
		ext("lpp|startObjectQuick");
		break;
	case 0xBF:
		ext("lp|startScriptQuick");
		break;
	case 0xC0:
		ext("x" "dim2\0"
				"\xC7ppw|dim2Type5,"
				"\xC8ppw|dim2Type1," "\xC9ppw|dim2Type2," "\xCAppw|dim2Type3," "\xCBppw|dim2Type4");
		break;
	case 0xC4:
		ext("rp|abs");
		break;
	case 0xC5:
		ext("rpp|getDistObjObj");
		break;
	case 0xC6:
		ext("rppp|getDistObjPt");
		break;
	case 0xC7:
		ext("rpppp|getDistPtPt");
		break;
	case 0xC9:
		ext("l|miscOps");
		break;
	case 0xCA:
		ext("p|breakXTimes");
		break;
	case 0xCB:
		ext("lp|pickOneOf");
		break;
	case 0xCC:
		ext("plp|pickOneOfDefault");
		break;
	default:
		invalidop(NULL, code);
		break;
	}
}



char *indentbuf;

#define INDENT_SIZE 2
char *getIndentString(int i)
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

void outputLine(char *buf, int curoffs, int opcode, int indent)
{

	char *s;

	if (buf[0]) {
		if (indent == -1)
			indent = num_block_stack;
		if (curoffs == -1)
			curoffs = get_curoffs();

		s = getIndentString(indent);

		if (dontShowOpcode) {
			if (dontShowOffsets)
				printf("%s%s\n", s, buf);
			else
				printf("[%.4X] %s%s\n", curoffs, s, buf);
		} else {
			char buf2[4];
			if (opcode != -1)
				sprintf(buf2, "%.2X", opcode);
			else
				strcpy(buf2, "**");
			if (dontShowOffsets)
				printf("(%s) %s%s\n", buf2, s, buf);
			else
				printf("[%.4X] (%s) %s%s\n", curoffs, buf2, s, buf);
		}
	}
}

void writePendingElse()
{
	if (pendingElse) {
		char buf[32];
		sprintf(buf, alwaysShowOffs ? "} else /*%.4X*/ {" : "} else {", pendingElseTo);
		outputLine(buf, pendingElseOffs, pendingElseOpcode, pendingElseIndent - 1);
		pendingElse = false;
	}
}

bool indentBlock(unsigned int cur)
{
	BlockStack *p;

	if (!num_block_stack)
		return false;

	p = &block_stack[num_block_stack - 1];
	if (cur < p->to)
		return false;

	num_block_stack--;
	return true;
}




void ShowHelpAndExit()
{
	printf("DOTT Script discompiler\nSyntax:\n"
				 "\tdottdis [-o] filename\nFlags:\n"
				 "\t-o\tAlways Show offsets\n"
				 "\t-i\tDon't output ifs\n"
				 "\t-e\tDon't output else\n"
				 "\t-f\tDon't output else-if\n"
				 "\t-c\tDon't show opcode\n"
				 "\t-x\tDon't show offsets\n"
				 "\t-h\tHalt on error\n"
				 "\t-7\tAssume V7 scripts\n");
	exit(0);
}

int main(int argc, char *argv[])
{
	int i;
	char *s;
	char *filename, *buf;
	FILE *in;
	byte *mem, *memorg;
	int len;
	int curoffs;

	/* Parse the arguments */
	filename = NULL;
	for (i = 1; i < argc; i++) {
		s = argv[i];

		if (s && s[0] == '-') {
			s++;
			while (*s) {
				switch (tolower(*s)) {
				case 'o':
					alwaysShowOffs = 1;
					break;
				case 'i':
					dontOutputIfs = 1;
					break;
				case 'e':
					dontOutputElse = 1;
					break;
				case 'f':
					dontOutputElseif = 1;
					break;
				case 'c':
					dontShowOpcode = 1;
					break;
				case 'x':
					dontShowOffsets = 1;
					break;
				case 'h':
					haltOnError = 1;
					break;
				case '7':
					scriptVersion = 7;
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

	output = buf = (char *)malloc(8192);

#if defined(SCUMM_BIG_ENDIAN)
	switch (TO_LE_32(*((long *)mem))) {
#else
	switch (*((long *)mem)) {
#endif
	case 'RCSL':
		if (scriptVersion == 7) {
			printf("Script# %d\n", mem[8] + (mem[9] << 8));
			mem += 10;
		} else {
			printf("Script# %d\n", (unsigned char)mem[8]);
			mem += 9;
		}
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

	curoffs = 0;

	do {
		byte opcode = *cur_pos;
		int j = num_block_stack;
		buf[0] = 0;
		next_line();
		if (buf[0]) {
			writePendingElse();
			if (haveElse) {
				haveElse = false;
				j--;
			}
			outputLine(buf, curoffs, opcode, j);
			curoffs = get_curoffs();
			while (indentBlock(get_curoffs())) {
				outputLine("}", -1, -1, -1);
			}

			fflush(stdout);
		}
	} while (cur_pos < mem + len);

	printf("END\n");

	free(memorg);

	return 0;
}
