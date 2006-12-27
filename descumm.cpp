/* DeScumm - Scumm Script Disassembler (version 2-5 scripts)
 * Copyright (C) 2001  Ludvig Strigeus
 * Copyright (C) 2002-2006  The ScummVM Team
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

#include "descumm.h"

/*
  Similar to the code that detects "head" while loops like this:
  while(Condition) {
  }
  add code to detect tail while loops, i.e. something of the kind
  do {
  } while(Condition);
  In V2-V5, those are the most frequent type of loops (and in V6-V8 they
  sometimes occur, too).

  However, implementing this might be quite tricky, and require us to refactor the
  code, because unlike a "head if/while", we don't know we are inside a loop until its
  end. This means a problem for indention, and when outputing the initial "do {".
  To solve this, we could implement some sort of look ahead; but that would be very
  complicated, because essentially we have to perform full parsing of the data anyway. 
  Instead of doing multiple look aheads, one could also do a 2 pass descumming:
  In pass 1, we find all jump statement, and identify all jump targets. From this data
  we can work backwards to detect all loops (and also if/else/elsif).
  Yet another approach would be to not emit any lines until we fully descummed the script.
  Instead, we keep each line in a line buffer, with indention but with an associated
  "indention" variable. When we discover a do/while loop, we can then insert a "do {"
  line and increase the indention of all intermediate lines. However this approach 
  needs a lot of memory, and won't output anything until the script is fully descummed,
  which is annoying when debugging descumm.

*/

enum TokenType {
	TOK_BYTE = 1,
	TOK_WORD = 2,
	TOK_VAR = 3,
	TOK_LIST = 4,
	TOK_ASCII = 5,
	TOK_CHAR = 6
};


#define A1B (TOK_BYTE<<0)
#define A1W (TOK_WORD<<0)
#define A1V (TOK_VAR<<0)
#define A1LIST (TOK_LIST<<0)
#define A1ASCII (TOK_ASCII<<0)

#define A2B (TOK_BYTE<<4)
#define A2W (TOK_WORD<<4)
#define A2V (TOK_VAR<<4)
#define A2LIST (TOK_LIST<<4)
#define A2ASCII (TOK_ASCII<<4)

#define A3B (TOK_BYTE<<8)
#define A3W (TOK_WORD<<8)
#define A3V (TOK_VAR<<8)
#define A3LIST (TOK_LIST<<8)
#define A3ASCII (TOK_ASCII<<8)
#define A3CHAR (TOK_CHAR<<8)

#define A4B (TOK_BYTE<<12)
#define A4W (TOK_WORD<<12)
#define A4V (TOK_VAR<<12)
#define A4LIST (TOK_LIST<<12)
#define A4ASCII (TOK_ASCII<<12)
#define A4CHAR (TOK_CHAR<<12)

#define A5ASCII (TOK_ASCII<<16)

#define ATO (1<<31)
#define ANOLASTPAREN (1<<30)
#define ANOFIRSTPAREN (1<<29)
#define ASTARTCOMMA (1<<28)
#define AVARSTORE (1<<27)



void emit_if(char *buf, char *condition);


const char *var_names0[] = {
	/* 0 */
	"VAR_EGO",
	"VAR_RESULT",
	"VAR_CAMERA_POS_X",
	"VAR_HAVE_MSG",
	/* 4 */
	"VAR_ROOM",
	"VAR_ACTIVE_ACTOR",
	"VAR_OVERRIDE",
	NULL,
	/* 8 */
	"VAR_IS_SOUND_RUNNING",
	"VAR_ACTIVE_VERB",
	"VAR_CHARCOUNT",
	NULL
};

const char *var_names2[] = {
	/* 0 */
	"VAR_EGO",
	"VAR_RESULT",
	"VAR_CAMERA_POS_X",
	"VAR_HAVE_MSG",
	/* 4 */
	"VAR_ROOM",
	"VAR_OVERRIDE",
	"VAR_MACHINE_SPEED",
	"VAR_CHARCOUNT",
	/* 8 */
	"VAR_ACTIVE_VERB",
	"VAR_ACTIVE_OBJECT1",
	"VAR_ACTIVE_OBJECT2",
	"VAR_NUM_ACTOR",
	/* 12 */
	"VAR_CURRENT_LIGHTS",
	"VAR_CURRENTDRIVE",
	NULL,
	NULL,
	/* 16 */
	NULL,
	"VAR_MUSIC_TIMER",
	"VAR_VERB_ALLOWED",
	"VAR_ACTOR_RANGE_MIN",
	/* 20 */
	"VAR_ACTOR_RANGE_MAX",
	NULL,
	NULL,
	"VAR_CAMERA_MIN_X",
	/* 24 */
	"VAR_CAMERA_MAX_X",
	"VAR_TIMER_NEXT",
	"VAR_SENTENCE_VERB",
	"VAR_SENTENCE_OBJECT1",
	/* 28 */
	"VAR_SENTENCE_OBJECT2",
	"VAR_SENTENCE_PREPOSITION",
	"VAR_VIRT_MOUSE_X",
	"VAR_VIRT_MOUSE_Y",
	/* 32 */
	"VAR_CLICK_AREA",
	"VAR_CLICK_VERB",
	NULL,
	"VAR_CLICK_OBJECT",
	/* 36 */
	"VAR_ROOM_RESOURCE",
	"VAR_LAST_SOUND",
	"VAR_BACKUP_VERB",
	"VAR_KEYPRESS",
	/* 40 */
	"VAR_CUTSCENEEXIT_KEY",
	"VAR_TALK_ACTOR",
	NULL,
	NULL
};

const char *var_names3[] = {
	/* 0 */
	"VAR_RESULT",
	"VAR_EGO",
	"VAR_CAMERA_POS_X",
	"VAR_HAVE_MSG",
	/* 4 */
	"VAR_ROOM",
	"VAR_OVERRIDE",
	"VAR_MACHINE_SPEED",
	"VAR_ME",
	/* 8 */
	"VAR_NUM_ACTOR",
	"VAR_CURRENT_LIGHTS",
	"VAR_CURRENTDRIVE",
	"VAR_TMR_1",
	/* 12 */
	"VAR_TMR_2",
	"VAR_TMR_3",
	"VAR_MUSIC_TIMER",
	"VAR_ACTOR_RANGE_MIN",
	/* 16 */
	"VAR_ACTOR_RANGE_MAX",
	"VAR_CAMERA_MIN_X",
	"VAR_CAMERA_MAX_X",
	"VAR_TIMER_NEXT",
	/* 20 */
	"VAR_VIRT_MOUSE_X",
	"VAR_VIRT_MOUSE_Y",
	"VAR_ROOM_RESOURCE",
	"VAR_LAST_SOUND",
	/* 24 */
	"VAR_CUTSCENEEXIT_KEY",
	"VAR_TALK_ACTOR",
	"VAR_CAMERA_FAST_X",
	NULL,
	/* 28 */
	"VAR_ENTRY_SCRIPT",
	"VAR_ENTRY_SCRIPT2",
	"VAR_EXIT_SCRIPT",
	"VAR_EXIT_SCRIPT2",
	/* 32 */
	"VAR_VERB_SCRIPT",
	"VAR_SENTENCE_SCRIPT",
	"VAR_INVENTORY_SCRIPT",
	"VAR_CUTSCENE_START_SCRIPT",
	/* 36 */
	"VAR_CUTSCENE_END_SCRIPT",
	"VAR_CHARINC",
	"VAR_WALKTO_OBJ",
	NULL,
	/* 40 */
	NULL,
	NULL,
	"VAR_RESTART_KEY",
	"VAR_PAUSE_KEY",
	/* 44 */
	"VAR_MOUSE_X",
	"VAR_MOUSE_Y",
	"VAR_TIMER",
	"VAR_TMR_4",
	/* 48 */
	"VAR_SOUNDCARD",
	"VAR_VIDEOMODE",
	NULL,
	NULL
};

const char *var_names4[] = {
	/* 0 */
	"VAR_RESULT",
	"VAR_EGO",
	"VAR_CAMERA_POS_X",
	"VAR_HAVE_MSG",
	/* 4 */
	"VAR_ROOM",
	"VAR_OVERRIDE",
	"VAR_MACHINE_SPEED",
	"VAR_ME",
	/* 8 */
	"VAR_NUM_ACTOR",
	"VAR_CURRENT_LIGHTS",
	"VAR_CURRENTDRIVE",
	"VAR_TMR_1",
	/* 12 */
	"VAR_TMR_2",
	"VAR_TMR_3",
	"VAR_MUSIC_TIMER",
	"VAR_ACTOR_RANGE_MIN",
	/* 16 */
	"VAR_ACTOR_RANGE_MAX",
	"VAR_CAMERA_MIN_X",
	"VAR_CAMERA_MAX_X",
	"VAR_TIMER_NEXT",
	/* 20 */
	"VAR_VIRT_MOUSE_X",
	"VAR_VIRT_MOUSE_Y",
	"VAR_ROOM_RESOURCE",
	"VAR_LAST_SOUND",
	/* 24 */
	"VAR_CUTSCENEEXIT_KEY",
	"VAR_TALK_ACTOR",
	"VAR_CAMERA_FAST_X",
	"VAR_SCROLL_SCRIPT",
	/* 28 */
	"VAR_ENTRY_SCRIPT",
	"VAR_ENTRY_SCRIPT2",
	"VAR_EXIT_SCRIPT",
	"VAR_EXIT_SCRIPT2",
	/* 32 */
	"VAR_VERB_SCRIPT",
	"VAR_SENTENCE_SCRIPT",
	"VAR_INVENTORY_SCRIPT",
	"VAR_CUTSCENE_START_SCRIPT",
	/* 36 */
	"VAR_CUTSCENE_END_SCRIPT",
	"VAR_CHARINC",
	"VAR_WALKTO_OBJ",
	"VAR_DEBUGMODE",
	/* 40 */
	"VAR_HEAPSPACE",
	NULL,
	"VAR_RESTART_KEY",
	"VAR_PAUSE_KEY",
	/* 44 */
	"VAR_MOUSE_X",
	"VAR_MOUSE_Y",
	"VAR_TIMER",
	"VAR_TMR_4",
	/* 48 */
	"VAR_SOUNDCARD",
	"VAR_VIDEOMODE",
	"VAR_MAINMENU_KEY",
	"VAR_FIXEDDISK",
	/* 52 */
	"VAR_CURSORSTATE",
	"VAR_USERPUT",
	"VAR_V5_TALK_STRING_Y",
	/* Loom CD specific */
	NULL,
	/* 56 */
	NULL,
	NULL,
	NULL,
	NULL,
	/* 60 */
	"VAR_NOSUBTITLES",
	NULL,
	NULL,
	NULL,
	/* 64 */
	"VAR_SOUNDPARAM",
	"VAR_SOUNDPARAM2",
	"VAR_SOUNDPARAM3",
	NULL
};

const char *var_names5[] = {
	/* 0 */
	"VAR_RESULT",
	"VAR_EGO",
	"VAR_CAMERA_POS_X",
	"VAR_HAVE_MSG",
	/* 4 */
	"VAR_ROOM",
	"VAR_OVERRIDE",
	"VAR_MACHINE_SPEED",
	"VAR_ME",
	/* 8 */
	"VAR_NUM_ACTOR",
	"VAR_CURRENT_LIGHTS",
	"VAR_CURRENTDRIVE",
	"VAR_TMR_1",
	/* 12 */
	"VAR_TMR_2",
	"VAR_TMR_3",
	"VAR_MUSIC_TIMER",
	"VAR_ACTOR_RANGE_MIN",
	/* 16 */
	"VAR_ACTOR_RANGE_MAX",
	"VAR_CAMERA_MIN_X",
	"VAR_CAMERA_MAX_X",
	"VAR_TIMER_NEXT",
	/* 20 */
	"VAR_VIRT_MOUSE_X",
	"VAR_VIRT_MOUSE_Y",
	"VAR_ROOM_RESOURCE",
	"VAR_LAST_SOUND",
	/* 24 */
	"VAR_CUTSCENEEXIT_KEY",
	"VAR_TALK_ACTOR",
	"VAR_CAMERA_FAST_X",
	"VAR_SCROLL_SCRIPT",
	/* 28 */
	"VAR_ENTRY_SCRIPT",
	"VAR_ENTRY_SCRIPT2",
	"VAR_EXIT_SCRIPT",
	"VAR_EXIT_SCRIPT2",
	/* 32 */
	"VAR_VERB_SCRIPT",
	"VAR_SENTENCE_SCRIPT",
	"VAR_INVENTORY_SCRIPT",
	"VAR_CUTSCENE_START_SCRIPT",
	/* 36 */
	"VAR_CUTSCENE_END_SCRIPT",
	"VAR_CHARINC",
	"VAR_WALKTO_OBJ",
	"VAR_DEBUGMODE",
	/* 40 */
	"VAR_HEAPSPACE",
	NULL,
	"VAR_RESTART_KEY",
	"VAR_PAUSE_KEY",
	/* 44 */
	"VAR_MOUSE_X",
	"VAR_MOUSE_Y",
	"VAR_TIMER",
	"VAR_TMR_4",
	/* 48 */
	"VAR_SOUNDCARD",
	"VAR_VIDEOMODE",
	"VAR_MAINMENU_KEY",
	"VAR_FIXEDDISK",
	/* 52 */
	"VAR_CURSORSTATE",
	"VAR_USERPUT",
	NULL,
	NULL,
	/* 56 */
	"VAR_SOUNDRESULT",
	"VAR_TALKSTOP_KEY",
	NULL,
	"VAR_FADE_DELAY",
	/* 60 */
	"VAR_NOSUBTITLES",
	NULL,
	NULL,
	NULL,
	/* 64 */
	"VAR_SOUNDPARAM",
	"VAR_SOUNDPARAM2",
	"VAR_SOUNDPARAM3",
	"VAR_INPUTMODE",
	/* 68 */
	"VAR_MEMORY_PERFORMANCE",
	"VAR_VIDEO_PERFORMANCE",
	"VAR_ROOM_FLAG",
	"VAR_GAME_LOADED",
	/* 72 */
	"VAR_NEW_ROOM",
	NULL,
	NULL,
	NULL,
	/* 76 */
	NULL,
	NULL,
	NULL,
	NULL
};


const char *get_num_string(int i)
{
	const char *s;

	if (i & 0x8000) {							/* Bit var */
		i &= 0xFFF;
		if (i >= 0x800)
			s = "??Bit??";
		else
			s = "Bit";
	} else if (i & 0x4000) {
		i &= g_options.IndyFlag ? 0xF : 0xFFF;
		if (i > 0x10)
			s = "??Local??";
		else
			s = "Local";
	} else {
		i &= 0xFFF;
		if (i >= 0x320)
			s = "??Var??";
		else
			s = "Var";
	}

	if (g_options.haltOnError && (s[0] == '?')) {
		error("%s out of range, was %d", s, i);
	}

	return s;
}


char *get_var(char *buf)
{
	int i;

	if (g_options.scriptVersion <= 2)
		i = get_byte();
	else
		i = (uint16)get_word();
		
	assert(i >= 0);

	if (g_options.scriptVersion >= 5 && 
			i < ARRAYSIZE(var_names5) && var_names5[i]) {
		buf += sprintf(buf, var_names5[i]);
		return buf;
	} else if (g_options.scriptVersion >= 4 && 
			i < ARRAYSIZE(var_names4) && var_names4[i]) {
		buf += sprintf(buf, var_names4[i]);
		return buf;
	} else if (g_options.scriptVersion >= 3 && 
			i < ARRAYSIZE(var_names3) && var_names3[i]) {
		buf += sprintf(buf, var_names3[i]);
		return buf;
	} else if (g_options.scriptVersion >= 1 &&
			i < ARRAYSIZE(var_names2) && var_names2[i]) {
		buf += sprintf(buf, var_names2[i]);
		return buf;
	} else if (g_options.scriptVersion == 0 &&
			i < ARRAYSIZE(var_names0) && var_names0[i]) {
		buf += sprintf(buf, var_names0[i]);
		return buf;
	} else if (g_options.scriptVersion <= 2 && g_options.ZakFlag && (i == 234 || i == 235)) {
		buf += sprintf(buf, (i == 234) ? "ZERO" : "ONE");
		return buf;
	} else if ((i & 0x8000) && (g_options.GF_UNBLOCKED || g_options.ZakFlag))
		buf += sprintf(buf, "Var[%d Bit %d", (i & 0x0FFF) >> 4, i & 0x000F);
	else
		buf += sprintf(buf, "%s[%d", get_num_string(i), i & 0xFFF);

	if (i & 0x2000) {
		int j = get_word();
		if (j & 0x2000) {
			j ^= 0x2000;
			sprintf(buf, " + %s[%d]", get_num_string(j), j & 0xFFF);
		} else
			sprintf(buf, " + %d", j & 0xFFF);
	}
	strcat(buf, "]");

	return strchr(buf, 0);

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

char *get_list(char *buf)
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
			if (g_options.haltOnError)
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
			i = get_byte();
			buf = putascii(buf, i);

			// Workaround for a script bug in Indy3
			if (i == 46 && g_options.scriptVersion == 3 && g_options.IndyFlag)
				continue;

			if (i != 1 && i != 2 && i != 3 && i != 8) {
				buf = putascii(buf, get_byte());
				buf = putascii(buf, get_byte());
			}
		}
	} while (1);

	return strecpy(buf, "\"");
}



char *add_a_tok(char *buf, int type)
{
	switch (type) {
	case TOK_BYTE:
		buf += sprintf(buf, "%d", get_byte());
		break;
	case TOK_WORD:
		buf += sprintf(buf, "%d", get_word());
		break;
	case TOK_VAR:
		buf = get_var(buf);
		break;
	case TOK_LIST:
		buf = get_list(buf);
		break;
	case TOK_ASCII:
		buf = get_ascii(buf);
		break;
	case TOK_CHAR:
		error("this code seems to be dead");
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
		char tmp[256];
		strcpy(tmp, buforg);
		emit_if(buforg, tmp);
	} else if (!(args & ANOLASTPAREN)) {
		buf = strecpy(buf, ")");
	}

	return strchr(buf, 0);
}

void do_decodeparsestring_v2(char *buf, byte opcode)
{
	byte c;
	bool flag;

	while ((c = get_byte())) {
		flag = (c & 0x80) != 0;
		c &= 0x7f;

		if (c < 8) {
			buf += sprintf(buf, "^%d", c);
			if (c > 3) {
				buf += sprintf(buf, "^%d", get_byte());
			}
		} else
			*buf++ = c;
		if (flag)
			*buf++ = ' ';
	}
	*buf = 0;
}

void do_actorops_v12(char *buf, byte opcode)
{
	buf = strecpy(buf, "ActorOps(");
	buf = get_var_or_byte(buf, opcode & 0x80);
	buf = strecpy(buf, ",[");

	char arg[256];
	get_var_or_byte(arg, opcode & 0x40);

	int subop = get_byte();
	switch(subop) {
		case 1:
			buf += sprintf(buf, "Sound(%s)", arg);
			break;
		case 2:
			if (g_options.scriptVersion == 1)
				buf += sprintf(buf, "Color(%s)", arg);
			else
				buf += sprintf(buf, "Color(%d, %s)", get_byte(), arg);
			break;
		case 3:
			buf = do_tok(buf, "Name", A1ASCII);
			break;
		case 4:
			buf += sprintf(buf, "Costume(%s)", arg);
			break;
		case 5:
			buf += sprintf(buf, "TalkColor(%s)", arg);
			break;
		default:
			error("do_actorops_v12: unknown subop %d", subop);
	}
	strecpy(buf, "]);");
}

void do_actorops(char *buf, byte opcode)
{
	static const byte convertTable[20] =
		{ 1, 0, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20 };

	char first = 1;

	buf = do_tok(buf, "ActorOps", ((opcode & 0x80) ? A1V : A1B) | ANOLASTPAREN);

	buf = strecpy(buf, ",[");

	do {
		opcode = get_byte();
		if (opcode == 0xFF)
			break;
		if (!first)
			buf = strecpy(buf, ",");
		first = 0;

		// FIXME - this really should be a check for GF_SMALL_HEADER instead!
		if (g_options.scriptVersion < 5)
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
//    		case 0x0F: buf=do_tok(buf, "PaletteList", A1LIST); break;
		case 0x10:
			buf = do_tok(buf, "Width", ((opcode & 0x80) ? A1V : A1B));
			break;
		case 0x11:
			if (g_options.scriptVersion == 5)
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

static int g_numInExprStack;
static char *g_exprStack[256];

void pushExprStack(char *s) {
	assert(g_numInExprStack < 256);
	g_exprStack[g_numInExprStack++] = strdup(s);
}

char *popExprStack(char *buf) {
	char *s;

	if (g_numInExprStack <= 0) {
		printf("Expression stack is empty!\n");
		exit(0);
	}

	s = g_exprStack[--g_numInExprStack];
	buf = strecpy(buf, s);
	free(s);
	return buf;
}


void do_expr_code(char *buf)
{
	int i;
	const char *s;
	char *buf2;

	char tmp[256];

	buf = strecpy(buf, "Exprmode ");
	buf = get_var(buf);
	buf = strecpy(buf, " = ");

	g_numInExprStack = 0;

	do {
		i = get_byte();
		if (i == 0xFF)
			break;
		switch (i & 0x1F) {
		case 0x1:
			get_var_or_word(buf, i & 0x80);
			pushExprStack(buf);
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
			popExprStack(tmp);
			buf2 = popExprStack(buf2);
			buf2 = strecpy(buf2, s);
			buf2 = strecpy(buf2, tmp);
			strecpy(buf2, ")");
			pushExprStack(buf);
			break;

		case 0x6:
			buf2 = strecpy(buf, "<");
			if (g_options.scriptVersion <= 2)
				next_line_V12(buf2);
			else
				next_line_V345(buf2);
			strecpy(strchr(buf2, 0), ">");
			pushExprStack(buf);
			break;

		default:
			printf("Warning, Invalid expression code %.2X\n", i);
		}

	} while (1);

	strcpy(popExprStack(buf), ";");
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
	const char *resTypes[] = {
			"UnkResType0",
			"UnkResType1",
			"Costume",
			"Room",
			"UnkResType4",
			"Script",
			"Sound"
		};
	char resid[256];
	int subop;

	get_var_or_byte(resid, opcode & 0x80);
	subop = get_byte();

	int type = subop >> 4;

	if (((subop & 0x0F) == 0) || ((subop & 0x0F) == 1)) {
		if (subop & 1)
			buf += sprintf(buf, "load");
		else
			buf += sprintf(buf, "nuke");
		assert(0 <= type && type < ARRAYSIZE(resTypes));
		buf += sprintf(buf, resTypes[type]);
		buf += sprintf(buf, "(%s)", resid);
	} else {
		if (subop & 1)
			buf += sprintf(buf, "lock");
		else
			buf += sprintf(buf, "unlock");
		assert(0 <= type && type < ARRAYSIZE(resTypes));
		buf += sprintf(buf, resTypes[type]);
		buf += sprintf(buf, "(%s)", resid);
	}
}

void do_resource(char *buf, byte opco)
{
	char opcode = get_byte();
	int subop;
	if (g_options.scriptVersion != 5)
		subop = opcode & 0x3F;	// FIXME - actually this should only be done for Zak256
	else
		subop = opcode & 0x1F;

	buf += sprintf(buf, "Resource.");

	switch (subop) {
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
		do_tok(buf, "loadFlObject", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W));
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
		error("do_resource: unhandled subop %d\n", subop);
		break;
	}

}

void do_pseudoRoom(char *buf)
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
		buf = do_tok(buf, NULL, ASTARTCOMMA | ANOFIRSTPAREN | ((opcode & 0x80) ? A1V : A1B));
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
			do_tok(buf, "RoomIntensity",
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
			do_tok(buf, "setRGBRoomIntensity",
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
			do_tok(buf, "setRoomShadow",
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
		do_tok(buf, "saveString", ((opcode & 0x80) ? A1V : A1B) | A2ASCII);
		break;
	case 0x0E:
		do_tok(buf, "loadString", ((opcode & 0x80) ? A1V : A1B) | A2ASCII);
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

void do_room_ops_old(char *buf, byte opcode)
{
	char	a[256];
	char	b[256];
	
	if (g_options.scriptVersion <= 2) {
		get_var_or_byte(a, (opcode & 0x80));
		get_var_or_byte(b, (opcode & 0x40));
	} else if (g_options.scriptVersion == 3) {
		get_var_or_word(a, (opcode & 0x80));
		get_var_or_word(b, (opcode & 0x40));
	}

	opcode = get_byte();
	switch (opcode & 0x1F) {
	case 0x01:
		if (g_options.scriptVersion > 3) {
			get_var_or_word(a, (opcode & 0x80));
			get_var_or_word(b, (opcode & 0x40));
		}
		buf = strecpy(buf, "RoomScroll(");
		buf = strecpy(buf, a);
		buf = strecpy(buf, ",");
		buf = strecpy(buf, b);
		buf = strecpy(buf, ")");
		break;
	case 0x02:
		if (g_options.scriptVersion > 3) {
			get_var_or_word(a, (opcode & 0x80));
			get_var_or_word(b, (opcode & 0x40));
		}
		buf = strecpy(buf, "RoomColor(");
		buf = strecpy(buf, a);
		buf = strecpy(buf, ",");
		buf = strecpy(buf, b);
		buf = strecpy(buf, ")");
		break;
	case 0x03:
		if (g_options.scriptVersion > 3) {
			get_var_or_word(a, (opcode & 0x80));
			get_var_or_word(b, (opcode & 0x40));
		}
		buf = strecpy(buf, "SetScreen(");
		buf = strecpy(buf, a);
		buf = strecpy(buf, ",");
		buf = strecpy(buf, b);
		buf = strecpy(buf, ")");
		break;
	case 0x04:
		if (g_options.scriptVersion > 3) {
			get_var_or_word(a, (opcode & 0x80));
			get_var_or_word(b, (opcode & 0x40));
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
	default:
		error("do_room_ops_old: unknown subop %d", opcode & 0x1F);
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
		if (g_options.scriptVersion == 3)
			do_tok(buf, "LoadCharset", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		else
			do_tok(buf, "CursorCommand", A1LIST);
		break;
	default:
		sprintf(buf, "UnknownCursorCommand%.2X", opcode);
	}
}

void do_verbops_v2(char *buf, byte opcode)
{
	int subop = get_byte();

	buf = do_tok(buf, "VerbOps", ANOLASTPAREN);
	switch (subop) {
		case 0:
			buf = do_tok(buf, "Delete", (opcode & 0x80) ? A1V : A1B);
			break;
		case 0xFF:
			buf = do_tok(buf, "State", A1B | A2B);
			break;
		default:
			buf += sprintf(buf, "New-%d", subop);
			buf = do_tok(buf, "", A1B | A2B | ((opcode & 0x80) ? A3V : A3B) | A4B | A5ASCII);
	}
	strecpy(buf, ")");
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
	strecpy(buf, "])");
}

void do_print_ego(char *buf, byte opcode)
{
	char first = 1;

	if (opcode == 0xD8) {
		buf = strecpy(buf, "printEgo([");
	} else {
		buf = do_tok(buf, "print", ((opcode & 0x80) ? A1V : A1B) | ANOLASTPAREN);
		buf = strecpy(buf, ",[");
	}

	do {
		opcode = get_byte();
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
			buf = do_tok(buf, "RestoreBG", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2W));
			break;
		case 0x4:
			buf = do_tok(buf, "Center", 0);
			break;
		case 0x6:
			if (g_options.GF_UNBLOCKED)
				buf = do_tok(buf, "Height", ((opcode & 0x80) ? A1V: A1W));
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
				buf = strecpy(buf, "Text(");
				buf = get_ascii(buf);
				buf = strecpy(buf, ")");
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

void do_unconditional_jump(char *buf)
{
	int offset = get_word();
	int cur = get_curoffs();
	int to = cur + offset;

	if (offset == 0) {
		sprintf(buf, "/* goto %.4X; */", to);
	} else if (!g_options.dontOutputElse && maybeAddElse(cur, to)) {
		pendingElse = true;
		pendingElseTo = to;
		pendingElseOffs = cur;
		pendingElseOpcode = g_jump_opcode;
		pendingElseIndent = g_blockStack.size();
		buf[0] = 0;
	} else {
		if (!g_blockStack.empty() && !g_options.dontOutputWhile) {
			Block p = g_blockStack.top();
			if (p.isWhile && cur == (int)p.to)
				return;		// A 'while' ends here.
		}
		sprintf(buf, "goto %.4X;", to);
	}
}

void emit_if(char *buf, char *condition)
{
	int offset = get_word();
	int cur = get_curoffs();
	int to = cur + offset;

	if (!g_options.dontOutputElseif && pendingElse) {
		if (maybeAddElseIf(cur, pendingElseTo, to)) {
			pendingElse = false;
			haveElse = true;
			buf = strecpy(buf, "} else if (");
			buf = strecpy(buf, condition);
			sprintf(buf, g_options.alwaysShowOffs ? ") /*%.4X*/ {" : ") {", to);
			return;
		}
	}

	if (!g_options.dontOutputIfs && maybeAddIf(cur, to)) {
		if (!g_options.dontOutputWhile && g_blockStack.top().isWhile) {
			buf = strecpy(buf, "while (");
		} else
			buf = strecpy(buf, "if (");
		buf = strecpy(buf, condition);
		sprintf(buf, g_options.alwaysShowOffs ? ") /*%.4X*/ {" : ") {", to);
		return;
	}

//	buf = strecpy(buf, negate ? "if (" : "unless (");
	buf = strecpy(buf, "unless (");
	buf = strecpy(buf, condition);
	sprintf(buf, ") goto %.4X;", to);
}

void do_if_code(char *buf, byte opcode)
{
	char var[256];
	char tmp[256], tmp2[256];
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
		error("Unknown IF code %x", opcode);
	}

	if (opcode == 0x28 || opcode == 0xA8) {
		get_var(tmp2);
	} else {
		if (g_options.scriptVersion == 0)
			get_var_or_byte(tmp2, opcode & 0x80);
		else
			get_var_or_word(tmp2, opcode & 0x80);
	}

	sprintf(tmp, "%s%s%s", var, cmp_texts[txt], tmp2);
	emit_if(buf, tmp);
}

void do_if_active_object(char *buf, byte opcode)
{
	char tmp[256];

	int obj = get_byte();
	sprintf(tmp, "activeObject2 == %d", obj);

	emit_if(buf, tmp);
}

void do_if_state_code(char *buf, byte opcode)
{
	char var[256];
	char tmp[256], tmp2[256];
	byte neg;
	int state = 0;

	var[0] = 0;
	if (g_options.scriptVersion == 0) {
		if (opcode & 0x40)
			sprintf(var, "activeObject");
		else
			sprintf(var, "%d", get_byte());
	} else {
		get_var_or_word(var, opcode & 0x80);
	}

	if (g_options.scriptVersion > 2) {
		switch (opcode & 0x2F) {
		case 0x0f:
			neg = 0;
			break;
		case 0x2f:
			neg = 1;
			break;
		default:
			/* Exit, this should never happen, only if my code is buggy */
			error("Unknown IF code %x", opcode);
		}

		get_var_or_byte(tmp2, opcode & 0x40);
	} else {
		if (g_options.scriptVersion == 0) {
			switch (opcode) {
			case 0x7f:
			case 0xbf:
				state = 2;
				neg = 1;
				break;
			case 0x9f:
			case 0xdf:
				state = 4;
				neg = 1;
				break;
			case 0xaf:
			case 0xef:
				state = 8;
				neg = 1;
				break;
			case 0x3f:
			case 0xff:
				state = 2;
				neg = 0;
				break;
			case 0x1f:
			case 0x5f:
				state = 4;
				neg = 0;
				break;
			case 0x2f:
			case 0x6f:
				state = 8;
				neg = 0;
				break;
			default:
				/* Exit, this should never happen, only if my code is buggy */
				error("Unknown IF code %x", opcode);
			}
		} else {
			switch (opcode) {
			case 0x3f:
			case 0xbf:
				state = 1;
				neg = 1;
				break;
			case 0x5f:
			case 0xdf:
				state = 2;
				neg = 1;
				break;
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
			case 0x7f:
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
				error("Unknown IF code %x", opcode);
			}
		}
	}

	if (g_options.scriptVersion > 2)
		sprintf(tmp, "getState(%s)%s%s", var, neg ? " != " : " == ", tmp2);
	else
		sprintf(tmp, "%sgetState%02d(%s)", neg ? "!" : "", state, var);
	emit_if(buf, tmp);
}

void do_varset_code(char *buf, byte opcode)
{
	const char *s;

	if ((g_options.scriptVersion <= 2)
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
	case 0x2C:
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
		error("Unknown VARSET code %x", opcode);
	}

	buf = strecpy(buf, s);


	if ((g_options.scriptVersion <= 2) && (opcode & 0x7F) == 0x2C) { /* assignVarByte */
		sprintf(buf, "%d", get_byte());
		buf = strchr(buf, 0);
	} else if ((opcode & 0x7F) != 0x46) {	/* increment or decrement */
		if (g_options.scriptVersion == 0)
			buf = get_var_or_byte(buf, opcode & 0x80);
		else
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

void next_line_V12(char *buf)
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
		do_actorops_v12(buf, opcode);
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
	case 0x2C:
		// assignVarByte
		do_varset_code(buf, opcode);
		break;
	case 0x80:
		do_tok(buf, "breakHere", 0);
		break;
	case 0x4A:
	case 0xCA:
		do_tok(buf, "chainScript", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x60:
	case 0xE0:
		//do_tok(buf, "cursorCommand", ((opcode & 0x80) ? A1V : A1W));
		if (opcode & 0x80) {
			char tmp[256];
			get_var(tmp);
			buf += sprintf(buf, "cursorCommand(Hi(%s), Lo(%s))", tmp, tmp);
		} else {
			int val = get_word();
			buf += sprintf(buf, "cursorCommand(%d, %d)", (val >> 8) & 0xFF, val & 0xFF);
		}

		break;
	case 0x40:
		sprintf(buf, "cutscene");
		break;
	case 0xC0:
		sprintf(buf, "endCutscene");
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
			if (!(opcode & 0x80) && *g_scriptCurPos == 0xFC) {
				strcpy(buf, "STOP)");
				g_scriptCurPos++;
			} else if (!(opcode & 0x80) && *g_scriptCurPos == 0xFB) {
				strcpy(buf, "RESET)");
				g_scriptCurPos++;
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
					 ((opcode & 0x80) ? A1V : A1W) |
					 ((opcode & 0x40) ? A2V : A2B) |
					 ((opcode & 0x20) ? A3V : A3B));
		break;
	case 0xAC:
		//drawSentence
		do_tok(buf, "drawSentence", 0);
		break;
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
	case 0x7B:
	case 0xFB:
		do_tok(buf, "getActorWalkBox", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;	
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
		do_tok(buf, "getBitVar", AVARSTORE | A1W | ((opcode & 0x80) ? A2V : A2B));
		break;

	case 0x1B:
	case 0x5B:
	case 0x9B:
	case 0xDB:
		do_tok(buf, "setBitVar", A1W | ((opcode & 0x80) ? A2V : A2B) | ((opcode & 0x40) ? A3V : A3B));
		break;

	case 0x66:
	case 0xE6:
		do_tok(buf, "getClosestObjActor", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
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
		do_tok(buf, "getObjPreposition", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x16:
	case 0x96:
		do_tok(buf, "getRandomNr", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x1D:
	case 0x5D:
	case 0x9D:
	case 0xDD:
		//ifClassOfIs
		do_tok(buf, "classOfIs", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B) | ATO);
		break;
		
	case 0x3F:
	case 0xBF:
		//ifNotState01
	case 0x5F:
	case 0xDF:
		//ifNotState02
	case 0x2F:
	case 0xAF:
		//ifNotState04
	case 0x0F:
	case 0x8F:
		//ifNotState08
	case 0x7F:
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
		do_unconditional_jump(buf);
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

	case 0x30:
	case 0xB0:
		do_tok(buf, "setBoxFlags", ((opcode & 0x80) ? A1V : A1B) | A2B);
		break;

	case 0x12:
	case 0x92:
		//panCameraTo
		do_tok(buf, "panCameraTo", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x50:
	case 0xD0:
		//pickupObject
		do_tok(buf, "pickupObject", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x14:
	case 0x94:
		// print
		buf += sprintf(buf, "print(");
		buf = get_var_or_byte(buf, (opcode & 0x80));
		buf += sprintf(buf, ",\"");
		do_decodeparsestring_v2(buf, opcode);
		strcat(buf, "\")");
		break;
	case 0xD8:
		//printEgo
		buf += sprintf(buf, "printEgo(\"");
		do_decodeparsestring_v2(buf, opcode);
		strcat(buf, "\")");
		break;
		
	case 0xCC:
		// pseudoRoom
		do_pseudoRoom(buf);
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
		do_tok(buf, "putActorAtObject", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W) );
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

	case 0x22:
	case 0xA2:
		do_tok(buf, "saveLoadGame", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x3D:
	case 0x7D:
	case 0xBD:
	case 0xFD:
		do_tok(buf, "setActorElevation", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x32:
	case 0xB2:
		do_tok(buf, "setCameraAt", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x54:
	case 0xD4:
		do_tok(buf, "setObjectName", ((opcode & 0x80) ? A1V : A1W) | A2ASCII);
		break;

	case 0x0B:
	case 0x4B:
	case 0x8B:
	case 0xCB:
		do_tok(buf, "setObjPreposition", ((opcode & 0x80) ? A1V : A1W) | A2B);
		break;
	case 0x29:
	case 0x69:
	case 0xA9:
	case 0xE9:
		//setOwnerOf
		do_tok(buf, "setOwnerOf", ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B));
		break;
		
	case 0x77:
	case 0xF7:
		// clearState01
		do_tok(buf, "clearState01", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x17:
	case 0x97:
		// clearState02
		do_tok(buf, "clearState02", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x67:
	case 0xE7:
		// clearState04
		do_tok(buf, "clearState04", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x47:
	case 0xC7:
		// clearState08
		do_tok(buf, "clearState08", ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x37:
	case 0xB7:
		//setState01
		do_tok(buf, "setState01", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x57:
	case 0xD7:
		//setState02
		do_tok(buf, "setState02", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x27:
	case 0xA7:
		//setState04
		do_tok(buf, "setState04", ((opcode & 0x80) ? A1V : A1W));
		break;
	case 0x07:
	case 0x87:
		//setState08
		do_tok(buf, "setState08", ((opcode & 0x80) ? A1V : A1W));
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

			strcpy(buf, "]);");

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
	case 0x1E:
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
		do_tok(buf, "walkActorToObject", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W));
		break;

	default:
		error("Unknown opcode %.2X", opcode);
	}
}

void next_line_V0(char *buf)
{
	byte opcode = get_byte();

	switch (opcode) {
	case 0x05:
	case 0x09:
	case 0x0A:
	case 0x19:
	case 0x23:
	case 0x2C:
	case 0x35:
	case 0x39:
	case 0x3B:
	case 0x45:
	case 0x49:
	case 0x59:
	case 0x63:
	case 0x65:
	case 0x6A:
	case 0x6C:
	case 0x79:
	case 0x7A:
	case 0x7B:
	case 0x80:
	case 0x82:
	case 0x85:
	case 0x89:
	case 0x8A:
	case 0x8D:
	case 0x96:
	case 0x99:
	case 0xA3:
	case 0xA6:
	case 0xAA:
	case 0xAC:
	case 0xB5:
	case 0xB9:
	case 0xBB:
	case 0xC5:
	case 0xC9:
	case 0xD8:
	case 0xD9:
	case 0xE3:
	case 0xE6:
	case 0xEA:
	case 0xEC:
	case 0xF5:
	case 0xF9:
	case 0xFA:
	case 0xFB:
		do_tok(buf, "stopCurrentScript", 0);
		break;

	case 0x67:
	case 0xE7:
		do_tok(buf, "getActorFacing", AVARSTORE | A1B | ((opcode & 0x80) ? A2V : A2B));
		break;

	case 0x31:
	case 0x71:
	case 0xB1:
	case 0xF1:
		do_tok(buf, "getBitVar", AVARSTORE | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x1D:
	case 0x3D:
	case 0x5D:
	case 0x7D:
	case 0x9D:
	case 0xBD:
	case 0xDD:
	case 0xFD:
		do_tok(buf, "setBitVar", A1B | ((opcode & 0x80) ? A2V : A2B) | ((opcode & 0x40) ? A3V : A3B));
		break;

	case 0x1B:
	case 0x5B:
	case 0x9B:
	case 0xDB:
		do_tok(buf, "getActorBitVar", AVARSTORE | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;

	case 0x0B:
	case 0x2B:
	case 0x4B:
	case 0x6B:
	case 0x8B:
	case 0xAB:
	case 0xCB:
	case 0xEB:
		do_tok(buf, "setActorBitVar", A1B | ((opcode & 0x80) ? A2V : A2B) | ((opcode & 0x40) ? A3V : A3B));
		break;

	case 0x58:
		do_tok(buf, "beginOverride", 0);
		get_byte();
		get_byte();
		get_byte();
		break;
	case 0x52:
	case 0xD2:
		do_tok(buf, "actorFollowCamera", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x95:
	case 0xD5:
		do_tok(buf, "actorFromPos",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) | AVARSTORE);
		break;
	case 0x15:
	case 0x55:
		do_tok(buf, "walkActorToActor",
					 ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) | A3B);
		break;											/* arg1=actor, arg2=actor */
	case 0x13:
		do_tok(buf, "lockCostume", A1B);
		break;
	case 0x4D:
		do_tok(buf, "lockRoom", A1B);
		break;
	case 0xCD:
		do_tok(buf, "unlockRoom", A1B);
		break;
	case 0x93:
		do_tok(buf, "unlockCostume", A1B);
		break;
	case 0x1A:
	case 0x9a:
		//move
	case 0x5A:
	case 0xDA:
		//add;
	case 0x3A:
	case 0xBA:
		//subtract
		do_varset_code(buf, opcode);
		break;

	case 0x11:
	case 0x51:
	case 0x91:
	case 0xD1:
		buf = do_tok(buf, "animateActor", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B) | A3B);
		break;
	case 0x10:
		do_tok(buf, "breakHere", 0);
		break;
	case 0x4A:
	case 0xCA:
		do_tok(buf, "loadRoom_c64", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x60:
	case 0xE0:
		//cursorCommand
		do_tok(buf, "cursorCommand", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x40:
		do_tok(buf, "cutscene", 0);
		break;
	case 0xC0:
		do_tok(buf, "endCutscene", 0);
		break;

	case 0x46:
	case 0xC6:
		// increment / decrement
		do_varset_code(buf, opcode);
		break;

	case 0x2A: {
		//delay
		int d = get_byte();
		d |= get_byte() << 8;
		d |= get_byte() << 16;
		d = 0xFFFFFF - d;
		sprintf(buf, "delay(%d)", d);
		break;
	}

	case 0x50:
	case 0x72:
	case 0xD0:
	case 0xF2:
		do_tok(buf, "nop", 0);
		break;
	case 0x14:
	case 0x94:
		// print
		buf += sprintf(buf, "print(");
		buf = get_var_or_byte(buf, (opcode & 0x80));
		buf += sprintf(buf, ",\"");
		do_decodeparsestring_v2(buf, opcode);
		strcat(buf, "\")");
		break;
	case 0x75:
		//printEgo
		buf += sprintf(buf, "printEgo(\"");
		do_decodeparsestring_v2(buf, opcode);
		strcat(buf, "\")");
		break;
	case 0x2E:
	case 0xAE:
		// print_c64
		buf += sprintf(buf, "print_c64(");
		buf = get_var_or_byte(buf, (opcode & 0x80));
		buf += sprintf(buf, ",\"");
		do_decodeparsestring_v2(buf, opcode);
		strcat(buf, "\")");
		break;
	case 0x0D:
		//printEgo_c64
		buf += sprintf(buf, "printEgo_c64(\"");
		do_decodeparsestring_v2(buf, opcode);
		strcat(buf, "\")");
		break;

	case 0x2D:
	case 0x6D:
	case 0xAD:
	case 0xED:
		do_tok(buf, "putActorInRoom", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0xE5:
		do_tok(buf, "loadRoomWithEgo", A1B | A2B);
		break;
	case 0x6E:
	case 0xEE:
		//dummy
		sprintf(buf, "dummy%.2X()", opcode);
		break;

	case 0x86:
		do_tok(buf, "bad", 0);
		break;
	case 0x56:
	case 0xD6:
		do_tok(buf, "getActorMoving", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x03:
	case 0x43:
	case 0x83:
	case 0xC3:
		do_tok(buf, "doSentence", A1B | A2B | A3B);
		break;
	case 0x07:
	case 0x87:
		do_tok(buf, "getActorRoom", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x47:
	case 0xC7:
		do_tok(buf, "getActorX", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x27:
	case 0xA7:
		do_tok(buf, "getActorY", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x26:
		do_tok(buf, "getClosestObjActor7", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x66:
		do_tok(buf, "getClosestObjActor25", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x06:
	case 0x34:
	case 0x74:
	case 0xB4:
	case 0xF4:
		do_tok(buf, "getDist",
					 AVARSTORE | ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
	case 0x90:
		do_tok(buf, "pickupObject", A1B);
		break;
	case 0x16:
		do_tok(buf, "getRandomNr", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x0f:
	case 0x4f:
		do_tok(buf, "clearState02", ((opcode & 0x40) ? 0 : A1B));
		break;
	case 0x37:
	case 0x77:
		do_tok(buf, "clearState04", ((opcode & 0x40) ? 0 : A1B));
		break;
	case 0x17:
	case 0x57:
		do_tok(buf, "clearState08", ((opcode & 0x40) ? 0 : A1B));
		break;
	case 0x8f:
	case 0xcf:
		do_tok(buf, "setState02", ((opcode & 0x40) ? 0 : A1B));
		break;
	case 0xb7:
	case 0xf7:
		do_tok(buf, "setState04", ((opcode & 0x40) ? 0 : A1B));
		break;
	case 0x97:
	case 0xd7:
		do_tok(buf, "setState08", ((opcode & 0x40) ? 0 : A1B));
		break;

	case 0x3f:
	case 0xff:
		//ifState02;
	case 0x1f:
	case 0x5f:
		//ifState04;
	case 0x2f:
	case 0x6f:
		//ifState08;
	case 0x7f:
	case 0xbf:
		//ifNotState02;
	case 0x9f:
	case 0xdf:
		//ifNotState04;
	case 0xaf:
	case 0xef:
		//ifNotState08;
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

	case 0x53:
		do_tok(buf, "lockSound", A1B);
		break;

	case 0xD3:
		do_tok(buf, "unlockSound", A1B);
		break;

	case 0x18:
		do_unconditional_jump(buf);
		break;

	case 0x70:
	case 0xF0:
		buf = do_tok(buf, "lights", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x25:
	case 0xA5:
		do_tok(buf, "loadRoom", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x24:
	case 0xA4:
		do_tok(buf, "unknown2", A1B);
		break;
	case 0x64:
	case 0xE4:
		do_if_active_object(buf, opcode);
		break;

	case 0x30:
	case 0xB0:
		do_tok(buf, "loadCostume", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x12:
	case 0x92:
		//panCameraTo
		do_tok(buf, "panCameraTo", ((opcode & 0x80) ? A1V : A1B));
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
	case 0x4E:
	case 0xCE:
	case 0x0E:
	case 0x8E:
		do_tok(buf, "putActorAtObject", ((opcode & 0x80) ? A1V : A1B) | A2B);
		break;
	case 0x0C:
	case 0x8C:
		do_tok(buf, "loadSound", A1B);
		break;
	case 0x98:
		do_tok(buf, "restart", 0);
		break;

	case 0x33:
		do_tok(buf, "lockScript", A1B);
		break;
	case 0xB3:
		do_tok(buf, "unlockScript", A1B);
		break;
	case 0x73:
	case 0xF3:
		do_tok(buf, "getObjectOwner", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x22:
	case 0xA2:
		do_tok(buf, "saveLoadGame", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x32:
	case 0xB2:
		do_tok(buf, "setCameraAt", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x54:
	case 0xD4:
		do_tok(buf, "setObjectName", A1B | A2ASCII);
		break;

	case 0x29:
	case 0x69:
	case 0xA9:
	case 0xE9:
		//setOwnerOf
		do_tok(buf, "setOwnerOf", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2B));
		break;
		
	case 0x02:
		do_tok(buf, "startMusic", ((opcode & 0x80) ? A1V : A1B));
		break;
	case 0x42:
	case 0xC2:
		//startScript
		do_tok(buf, "startScript", ((opcode & 0x80) ? A1V : A1B));
		break;
		
	case 0x1C:
	case 0x5C:
	case 0x9C:
	case 0xDC:
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

	case 0x4C:
	case 0xCC:
		do_tok(buf, "loadScript", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x1E:
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

	case 0x36:
	case 0xB6:
	case 0x76:
	case 0xF6:
		do_tok(buf, "walkActorToObject", ((opcode & 0x80) ? A1V : A1B) | A2B);
		break;

	default:
		error("Unknown opcode %.2X", opcode);
	}
}

void next_line_V345(char *buf)
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
		if (g_options.scriptVersion == 5) {
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
		if (g_options.scriptVersion == 5) {
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
		do_tok(buf, "faceActor", ((opcode & 0x80) ? A1V : A1B) | ((opcode & 0x40) ? A2V : A2W));
		break;

	case 0x0A:
	case 0x8A:
	case 0x2A:
	case 0xAA:
	case 0x4A:
	case 0xCA:
	case 0x6A:
	case 0xEA:
		do_tok(buf, "startScript", ((opcode & 0x80) ? A1V : A1B) | A2LIST);
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
		if (g_options.scriptVersion == 5) {
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
		do_unconditional_jump(buf);
		break;

	case 0x1D:
	case 0x9D:
		do_tok(buf, "classOfIs", ((opcode & 0x80) ? A1V : A1W) | A2LIST | ATO);
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
		do_tok(buf, "cutscene", A1LIST);
		break;

	case 0x42:
	case 0xC2:
		do_tok(buf, "chainScript", ((opcode & 0x80) ? A1V : A1B) | A2LIST);
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
		do_tok(buf, "getClosestObjActor", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
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
		if (g_options.IndyFlag)
			do_tok(buf, "waitForActor", ((opcode & 0x80) ? A1V : A1B));
		else
			do_tok(buf, "getActorScale", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0xAE:{
			if (g_options.IndyFlag)
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
					 ((opcode & 0x80) ? A1V : A1W) | ((opcode & 0x40) ? A2V : A2B) | A3LIST);
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
			if (!(opcode & 0x80) && (*g_scriptCurPos == 0xFE)) {
				strcpy(buf, "STOP)");
				g_scriptCurPos++;
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

	case 0xAC:
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
		do_actorops(buf, opcode);
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
		if (g_options.ZakFlag)
			do_tok(buf, "startMusic", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		else
			do_tok(buf, "startMusic", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0xCC:
		do_pseudoRoom(buf);
		break;

	case 0x33:
	case 0x73:
	case 0xB3:
	case 0xF3:
		if (g_options.scriptVersion == 5)
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
			sprintf(buf, "delay(%d)", d);
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

	case 0x58:{
			int d;
			d = get_byte();
			if (d != 0)
				sprintf(buf, "beginOverride");
			else
				sprintf(buf, "endOverride");
			break;
		}

	case 0x1C:
	case 0x9C:
		do_tok(buf, "startSound", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x4C:
		if (g_options.scriptVersion <= 3)
			do_tok(buf, "waitForSentence", 0);
		else
			do_tok(buf, "soundKludge", A1LIST);
		break;

	case 0x3C:
	case 0xBC:
		do_tok(buf, "stopSound", ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x98:
		do_tok(buf, "systemOps", A1B);
		break;

	case 0x7B:
	case 0xFB:
		do_tok(buf, "getActorWalkBox", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x43:
	case 0xC3:
		if (g_options.IndyFlag)
			do_tok(buf, "getActorX", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		else
			do_tok(buf, "getActorX", AVARSTORE | ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x23:
	case 0xA3:
		if (g_options.IndyFlag)
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
		do_tok(buf, "setClass", ((opcode & 0x80) ? A1V : A1W) | A2LIST);
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

			strcpy(buf, "]);");

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
	case 0xB2:
		do_tok(buf, "setCameraAt", ((opcode & 0x80) ? A1V : A1W));
		break;

	case 0x30:
	case 0xB0:
		if (g_options.scriptVersion == 3)
			do_tok(buf, "setBoxFlags", ((opcode & 0x80) ? A1V : A1B) | A2B);
		else
			do_matrix_ops(buf, opcode);
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
		if (g_options.scriptVersion == 5)
			do_tok(buf, "getAnimCounter", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		else
			do_tok(buf, "saveLoadGame", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
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
				do_tok(buf, "SaveVerbs", code);
				break;
			case 0x02:
				do_tok(buf, "RestoreVerbs", code);
				break;
			case 0x03:
				do_tok(buf, "DeleteVerbs", code);
				break;
			default:
				error("opcode 0xAB: Unhandled subop %d", opcode & 0x1F);
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

	case 0xA7:
		do_tok(buf, "saveLoadVars?", 0);
		break;

	case 0x67:
	case 0xE7:
		do_tok(buf, "getStringWidth?", AVARSTORE | ((opcode & 0x80) ? A1V : A1B));
		break;

	case 0x6B:
		do_tok(buf, "debug?", ((opcode & 0x80) ? A1V : A1W));
		break;

	default:
		if (g_options.haltOnError) {
			error("Unknown opcode %.2X", opcode);
		}
		sprintf(buf, "ERROR: Unknown opcode %.2X!", opcode);
	}
}
