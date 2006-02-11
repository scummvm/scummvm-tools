/* Loom_TG16_Extract - Extract data files from TG16 version of Loom
 * Copyright (C) 2004-2005  The ScummVM Team
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
 * $URL$ * $Id$
 *
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "util.h"

typedef int BOOL;
#define TRUE 1
#define FALSE 0

/* if defined, generates a set of .LFL files */
/* if not defined, dumps all resources to separate files */
#define	MAKE_LFLS


#ifdef _MSC_VER
	#define	vsnprintf _vsnprintf
#endif

void notice(const char *s, ...) {
	char buf[1024];
	va_list va;

	va_start(va, s);
	vsnprintf(buf, 1024, s, va);
	va_end(va);

	fprintf(stdout, "%s\n", buf);
}

unsigned char	read_cbyte (FILE *input, short *ctr)
{
	(*ctr) += 1;
	return readByte(input);
}
unsigned short	read_cword (FILE *input, short *ctr)
{
	(*ctr) += 2;
	return readUint16LE(input);
}

void	write_cbyte (FILE *output, uint8 val, short *ctr)
{
	writeByte(output, val);
	(*ctr) += 1;
}
void	write_cword (FILE *output, uint16 val, short *ctr)
{
	writeUint16LE(output, val);
	(*ctr) += 2;
}
void	write_clong (FILE *output, uint32 val, short *ctr)
{
	writeUint32LE(output, val);
	(*ctr) += 4;
}

typedef enum _res_type { RES_GLOBDATA = 0, RES_ROOM = 1, RES_SCRIPT = 2, RES_COSTUME = 3, RES_CHARSET = 4, RES_SOUND = 5, RES_UNKNOWN = 6} res_type;

typedef	enum _iso { ISO_USA, NUM_ISOS } t_iso;

t_iso ISO = NUM_ISOS;

typedef	struct	_resource
{
	unsigned long offset[NUM_ISOS];
	unsigned short length[NUM_ISOS];
	res_type type;
}	t_resource, *p_resource;

#define	NUM_ROOMS 100
t_resource res_rooms[NUM_ROOMS] = {
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x015000}, {0x3DC8}, RES_ROOM },	/* 1 */
	{ {0x037800}, {0x7C1D}, RES_ROOM },	/* 2 */
	{ {0x04C000}, {0x3BC4}, RES_ROOM },	/* 3 */
	{ {0x051800}, {0x7DA7}, RES_ROOM },	/* 4 */
	{ {0x05B800}, {0x827F}, RES_ROOM },	/* 5 */
	{ {0x065000}, {0x6EDE}, RES_ROOM },	/* 6 */
	{ {0x06D000}, {0x27E2}, RES_ROOM },	/* 7 */
	{ {0x080000}, {0x954D}, RES_ROOM },	/* 8 */
	{ {0x092800}, {0x67FB}, RES_ROOM },	/* 9 */
	{ {0x0A6800}, {0x6103}, RES_ROOM },	/* 10 */
	{ {0x0B4800}, {0x3E5C}, RES_ROOM },	/* 11 */
	{ {0x0BE800}, {0x1B1A}, RES_ROOM },	/* 12 */
	{ {0x0C4000}, {0x73FA}, RES_ROOM },	/* 13 */
	{ {0x0D1800}, {0x4092}, RES_ROOM },	/* 14 */
	{ {0x0D9800}, {0x594F}, RES_ROOM },	/* 15 */
	{ {0x0E2000}, {0x8C04}, RES_ROOM },	/* 16 */
	{ {0x0EB800}, {0x55CC}, RES_ROOM },	/* 17 */
	{ {0x0F4000}, {0x547E}, RES_ROOM },	/* 18 */
	{ {0x0FD800}, {0x679F}, RES_ROOM },	/* 19 */
	{ {0x109000}, {0x4491}, RES_ROOM },	/* 20 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 21 */
	{ {0x116800}, {0x5619}, RES_ROOM },	/* 22 */
	{ {0x11E000}, {0x86A5}, RES_ROOM },	/* 23 */
	{ {0x12D000}, {0xDDDB}, RES_ROOM },	/* 24 */
	{ {0x13F800}, {0x4C5F}, RES_ROOM },	/* 25 */
	{ {0x14B000}, {0x81D0}, RES_ROOM },	/* 26 */
	{ {0x155800}, {0x455D}, RES_ROOM },	/* 27 */
	{ {0x15B800}, {0xC836}, RES_ROOM },	/* 28 */
	{ {0x16F000}, {0x2002}, RES_ROOM },	/* 29 */
	{ {0x171800}, {0x7C16}, RES_ROOM },	/* 30 */
	{ {0x17B000}, {0x3572}, RES_ROOM },	/* 31 */
	{ {0x184000}, {0x2571}, RES_ROOM },	/* 32 */
	{ {0x187800}, {0x62B2}, RES_ROOM },	/* 33 */
	{ {0x197000}, {0xD702}, RES_ROOM },	/* 34 */
	{ {0x1AC000}, {0x5E16}, RES_ROOM },	/* 35 */
	{ {0x1BA000}, {0x7B89}, RES_ROOM },	/* 36 */
	{ {0x1C7000}, {0x4CA8}, RES_ROOM },	/* 37 */
	{ {0x1D2000}, {0x457D}, RES_ROOM },	/* 38 */
	{ {0x1D8800}, {0x2998}, RES_ROOM },	/* 39 */
	{ {0x1DB800}, {0x3FEC}, RES_ROOM },	/* 40 */
	{ {0x1E1800}, {0x6BE2}, RES_ROOM },	/* 41 */
	{ {0x1EF800}, {0x39D0}, RES_ROOM },	/* 42 */
	{ {0x1F5000}, {0x6753}, RES_ROOM },	/* 43 */
	{ {0x207000}, {0x3CC7}, RES_ROOM },	/* 44 */
	{ {0x20F800}, {0x39EA}, RES_ROOM },	/* 45 */
	{ {0x21C800}, {0x41C2}, RES_ROOM },	/* 46 */
	{ {0x22F000}, {0x6025}, RES_ROOM },	/* 47 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 48 */
	{ {0x23A800}, {0x5CD3}, RES_ROOM },	/* 49 */
	{ {0x242800}, {0x47B2}, RES_ROOM },	/* 50 */
	{ {0x247800}, {0x69A6}, RES_ROOM },	/* 51 */
	{ {0x250800}, {0x3F20}, RES_ROOM },	/* 52 */
	{ {0x256000}, {0x3D07}, RES_ROOM },	/* 53 */
	{ {0x25D000}, {0x2713}, RES_ROOM },	/* 54 */
	{ {0x260000}, {0x1533}, RES_ROOM },	/* 55 */
	{ {0x261800}, {0x1E21}, RES_ROOM },	/* 56 */
	{ {0x264000}, {0x2FF8}, RES_ROOM },	/* 57 */
	{ {0x268800}, {0x2C6C}, RES_ROOM },	/* 58 */
	{ {0x26B800}, {0x3AEC}, RES_ROOM },	/* 59 */
	{ {0x26F800}, {0x2139}, RES_ROOM },	/* 60 */
	{ {0x272800}, {0x0170}, RES_ROOM },	/* 61 */
	{ {0x276000}, {0x542A}, RES_ROOM },	/* 62 */
	{ {0x27C800}, {0x0FFB}, RES_ROOM },	/* 63 */
	{ {0x27E800}, {0x58FA}, RES_ROOM },	/* 64 */
	{ {0x284800}, {0x0656}, RES_ROOM },	/* 65 */
	{ {0x285800}, {0x7DA6}, RES_ROOM },	/* 66 */
	{ {0x291800}, {0x3091}, RES_ROOM },	/* 67 */
	{ {0x295000}, {0x2565}, RES_ROOM },	/* 68 */
	{ {0x298800}, {0x1F0C}, RES_ROOM },	/* 69 */
	{ {0x2A1800}, {0x4A6D}, RES_ROOM },	/* 70 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 71 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 72 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 73 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 74 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 75 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 76 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 77 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 78 */
	{ {0x2A8800}, {0x17BB}, RES_ROOM },	/* 79 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 80 */
	{ {0x2AA000}, {0x0D01}, RES_ROOM },	/* 81 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 82 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 83 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 84 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 85 */
	{ {0x2AE000}, {0x5591}, RES_ROOM },	/* 86 */
	{ {0x2B4800}, {0x22E9}, RES_ROOM },	/* 87 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
	{ {0x000000}, {0x0000}, RES_ROOM },	/* 0 */
};

#define	NUM_SCRIPTS	200
t_resource res_scripts[NUM_SCRIPTS] = {
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x019000}, {0x01F8}, RES_SCRIPT },	/* 1 */
	{ {0x019800}, {0x0030}, RES_SCRIPT },	/* 2 */
	{ {0x01A000}, {0x002F}, RES_SCRIPT },	/* 3 */
	{ {0x2A6800}, {0x0091}, RES_SCRIPT },	/* 4 */
	{ {0x01A800}, {0x023F}, RES_SCRIPT },	/* 5 */
	{ {0x01B000}, {0x01C9}, RES_SCRIPT },	/* 6 */
	{ {0x01B800}, {0x0058}, RES_SCRIPT },	/* 7 */
	{ {0x01C000}, {0x0064}, RES_SCRIPT },	/* 8 */
	{ {0x01C800}, {0x0080}, RES_SCRIPT },	/* 9 */
	{ {0x01D000}, {0x005B}, RES_SCRIPT },	/* 10 */
	{ {0x01D800}, {0x0072}, RES_SCRIPT },	/* 11 */
	{ {0x01E000}, {0x0055}, RES_SCRIPT },	/* 12 */
	{ {0x01E800}, {0x005D}, RES_SCRIPT },	/* 13 */
	{ {0x01F000}, {0x003C}, RES_SCRIPT },	/* 14 */
	{ {0x01F800}, {0x0025}, RES_SCRIPT },	/* 15 */
	{ {0x020000}, {0x0014}, RES_SCRIPT },	/* 16 */
	{ {0x020800}, {0x00E7}, RES_SCRIPT },	/* 17 */
	{ {0x021000}, {0x0257}, RES_SCRIPT },	/* 18 */
	{ {0x021800}, {0x0068}, RES_SCRIPT },	/* 19 */
	{ {0x022000}, {0x007D}, RES_SCRIPT },	/* 20 */
	{ {0x022800}, {0x003B}, RES_SCRIPT },	/* 21 */
	{ {0x023000}, {0x017E}, RES_SCRIPT },	/* 22 */
	{ {0x023800}, {0x00A8}, RES_SCRIPT },	/* 23 */
	{ {0x024000}, {0x0026}, RES_SCRIPT },	/* 24 */
	{ {0x024800}, {0x0028}, RES_SCRIPT },	/* 25 */
	{ {0x025000}, {0x00F2}, RES_SCRIPT },	/* 26 */
	{ {0x025800}, {0x00F8}, RES_SCRIPT },	/* 27 */
	{ {0x026000}, {0x002F}, RES_SCRIPT },	/* 28 */
	{ {0x026800}, {0x00E9}, RES_SCRIPT },	/* 29 */
	{ {0x027000}, {0x001E}, RES_SCRIPT },	/* 30 */
	{ {0x027800}, {0x0304}, RES_SCRIPT },	/* 31 */
	{ {0x028000}, {0x001E}, RES_SCRIPT },	/* 32 */
	{ {0x028800}, {0x000D}, RES_SCRIPT },	/* 33 */
	{ {0x029000}, {0x0028}, RES_SCRIPT },	/* 34 */
	{ {0x029800}, {0x008E}, RES_SCRIPT },	/* 35 */
	{ {0x02A000}, {0x00E5}, RES_SCRIPT },	/* 36 */
	{ {0x02A800}, {0x0015}, RES_SCRIPT },	/* 37 */
	{ {0x02B000}, {0x0025}, RES_SCRIPT },	/* 38 */
	{ {0x02B800}, {0x0060}, RES_SCRIPT },	/* 39 */
	{ {0x02C000}, {0x0046}, RES_SCRIPT },	/* 40 */
	{ {0x02C800}, {0x00A1}, RES_SCRIPT },	/* 41 */
	{ {0x02D000}, {0x00A9}, RES_SCRIPT },	/* 42 */
	{ {0x02D800}, {0x0039}, RES_SCRIPT },	/* 43 */
	{ {0x03F800}, {0x0596}, RES_SCRIPT },	/* 44 */
	{ {0x040000}, {0x00C7}, RES_SCRIPT },	/* 45 */
	{ {0x089800}, {0x13AC}, RES_SCRIPT },	/* 46 */
	{ {0x099000}, {0x0DCD}, RES_SCRIPT },	/* 47 */
	{ {0x27D800}, {0x00CF}, RES_SCRIPT },	/* 48 */
	{ {0x0B8800}, {0x02D3}, RES_SCRIPT },	/* 49 */
	{ {0x0AD000}, {0x0064}, RES_SCRIPT },	/* 50 */
	{ {0x0B9000}, {0x017F}, RES_SCRIPT },	/* 51 */
	{ {0x0B9000}, {0x017F}, RES_SCRIPT },	/* 52 */
	{ {0x0CB800}, {0x02B2}, RES_SCRIPT },	/* 53 */
	{ {0x0DF800}, {0x00C7}, RES_SCRIPT },	/* 54 */
	{ {0x0F9800}, {0x0052}, RES_SCRIPT },	/* 55 */
	{ {0x0F9800}, {0x0052}, RES_SCRIPT },	/* 56 */
	{ {0x10D800}, {0x0033}, RES_SCRIPT },	/* 57 */
	{ {0x10E000}, {0x0032}, RES_SCRIPT },	/* 58 */
	{ {0x0FA000}, {0x0034}, RES_SCRIPT },	/* 59 */
	{ {0x10E800}, {0x0035}, RES_SCRIPT },	/* 60 */
	{ {0x0FA800}, {0x0033}, RES_SCRIPT },	/* 61 */
	{ {0x0FB000}, {0x003F}, RES_SCRIPT },	/* 62 */
	{ {0x10F000}, {0x0029}, RES_SCRIPT },	/* 63 */
	{ {0x0FB800}, {0x006B}, RES_SCRIPT },	/* 64 */
	{ {0x104000}, {0x0314}, RES_SCRIPT },	/* 65 */
	{ {0x0F1000}, {0x0143}, RES_SCRIPT },	/* 66 */
	{ {0x11C000}, {0x010B}, RES_SCRIPT },	/* 67 */
	{ {0x263800}, {0x07AD}, RES_SCRIPT },	/* 68 */
	{ {0x144800}, {0x07A7}, RES_SCRIPT },	/* 69 */
	{ {0x13B000}, {0x0179}, RES_SCRIPT },	/* 70 */
	{ {0x0FC000}, {0x003F}, RES_SCRIPT },	/* 71 */
	{ {0x168800}, {0x007B}, RES_SCRIPT },	/* 72 */
	{ {0x13B800}, {0x004A}, RES_SCRIPT },	/* 73 */
	{ {0x169000}, {0x00A5}, RES_SCRIPT },	/* 74 */
	{ {0x169800}, {0x0435}, RES_SCRIPT },	/* 75 */
	{ {0x16A000}, {0x008D}, RES_SCRIPT },	/* 76 */
	{ {0x179800}, {0x004A}, RES_SCRIPT },	/* 77 */
	{ {0x285000}, {0x00C4}, RES_SCRIPT },	/* 78 */
	{ {0x186800}, {0x0091}, RES_SCRIPT },	/* 79 */
	{ {0x187000}, {0x004D}, RES_SCRIPT },	/* 80 */
	{ {0x17A000}, {0x0052}, RES_SCRIPT },	/* 81 */
	{ {0x17E800}, {0x00AA}, RES_SCRIPT },	/* 82 */
	{ {0x17F000}, {0x003E}, RES_SCRIPT },	/* 83 */
	{ {0x17F800}, {0x0041}, RES_SCRIPT },	/* 84 */
	{ {0x1A4800}, {0x05BF}, RES_SCRIPT },	/* 85 */
	{ {0x1CC000}, {0x0572}, RES_SCRIPT },	/* 86 */
	{ {0x1D6800}, {0x09DA}, RES_SCRIPT },	/* 87 */
	{ {0x1A5000}, {0x051D}, RES_SCRIPT },	/* 88 */
	{ {0x0B9800}, {0x0080}, RES_SCRIPT },	/* 89 */
	{ {0x1FB800}, {0x0786}, RES_SCRIPT },	/* 90 */
	{ {0x1FC000}, {0x0ECA}, RES_SCRIPT },	/* 91 */
	{ {0x1FD000}, {0x0031}, RES_SCRIPT },	/* 92 */
	{ {0x1FD800}, {0x061D}, RES_SCRIPT },	/* 93 */
	{ {0x104800}, {0x0144}, RES_SCRIPT },	/* 94 */
	{ {0x20B000}, {0x078B}, RES_SCRIPT },	/* 95 */
	{ {0x213800}, {0x00F3}, RES_SCRIPT },	/* 96 */
	{ {0x1A5800}, {0x004A}, RES_SCRIPT },	/* 97 */
	{ {0x1A6000}, {0x064D}, RES_SCRIPT },	/* 98 */
	{ {0x240800}, {0x0538}, RES_SCRIPT },	/* 99 */
	{ {0x235800}, {0x0A78}, RES_SCRIPT },	/* 100 */
	{ {0x09A000}, {0x08F2}, RES_SCRIPT },	/* 101 */
	{ {0x24E800}, {0x00DF}, RES_SCRIPT },	/* 102 */
	{ {0x24F000}, {0x0080}, RES_SCRIPT },	/* 103 */
	{ {0x24F800}, {0x06E6}, RES_SCRIPT },	/* 104 */
	{ {0x09B000}, {0x0225}, RES_SCRIPT },	/* 105 */
	{ {0x28D800}, {0x051E}, RES_SCRIPT },	/* 106 */
	{ {0x0AD800}, {0x00C9}, RES_SCRIPT },	/* 107 */
	{ {0x11C800}, {0x04DF}, RES_SCRIPT },	/* 108 */
	{ {0x16A800}, {0x03B4}, RES_SCRIPT },	/* 109 */
	{ {0x241000}, {0x0062}, RES_SCRIPT },	/* 110 */
	{ {0x2B3800}, {0x0100}, RES_SCRIPT },	/* 111 */
	{ {0x1A6800}, {0x008E}, RES_SCRIPT },	/* 112 */
	{ {0x040800}, {0x007B}, RES_SCRIPT },	/* 113 */
	{ {0x041000}, {0x008B}, RES_SCRIPT },	/* 114 */
	{ {0x28E000}, {0x002D}, RES_SCRIPT },	/* 115 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SCRIPT }	/* 0 */
};

#define	NUM_COSTUMES	200
t_resource res_costumes[NUM_COSTUMES] = {
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x02E000}, {0x53A5}, RES_COSTUME },	/* 1 */
	{ {0x033800}, {0x2148}, RES_COSTUME },	/* 2 */
	{ {0x036000}, {0x0B6E}, RES_COSTUME },	/* 3 */
	{ {0x29A800}, {0x597A}, RES_COSTUME },	/* 4 */
	{ {0x2A0800}, {0x01CC}, RES_COSTUME },	/* 5 */
	{ {0x25C000}, {0x0812}, RES_COSTUME },	/* 6 */
	{ {0x25A000}, {0x1875}, RES_COSTUME },	/* 7 */
	{ {0x042000}, {0x011A}, RES_COSTUME },	/* 8 */
	{ {0x050800}, {0x0332}, RES_COSTUME },	/* 9 */
	{ {0x050000}, {0x06BD}, RES_COSTUME },	/* 10 */
	{ {0x051000}, {0x0303}, RES_COSTUME },	/* 11 */
	{ {0x064000}, {0x0D55}, RES_COSTUME },	/* 12 */
	{ {0x059800}, {0x1090}, RES_COSTUME },	/* 13 */
	{ {0x05B000}, {0x03A2}, RES_COSTUME },	/* 14 */
	{ {0x0AE000}, {0x17DA}, RES_COSTUME },	/* 15 */
	{ {0x0AF800}, {0x1750}, RES_COSTUME },	/* 16 */
	{ {0x0C2800}, {0x1568}, RES_COSTUME },	/* 17 */
	{ {0x0B1000}, {0x1783}, RES_COSTUME },	/* 18 */
	{ {0x0C0800}, {0x1CA8}, RES_COSTUME },	/* 19 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 20 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 21 */
	{ {0x0BA000}, {0x0DD6}, RES_COSTUME },	/* 22 */
	{ {0x0BC000}, {0x083E}, RES_COSTUME },	/* 23 */
	{ {0x06C000}, {0x051D}, RES_COSTUME },	/* 24 */
	{ {0x06C800}, {0x0162}, RES_COSTUME },	/* 25 */
	{ {0x27B800}, {0x0765}, RES_COSTUME },	/* 26 */
	{ {0x08B000}, {0x0430}, RES_COSTUME },	/* 27 */
	{ {0x08B800}, {0x041E}, RES_COSTUME },	/* 28 */
	{ {0x08C000}, {0x0312}, RES_COSTUME },	/* 29 */
	{ {0x07C800}, {0x0A08}, RES_COSTUME },	/* 30 */
	{ {0x07D800}, {0x0A1D}, RES_COSTUME },	/* 31 */
	{ {0x07E800}, {0x0F46}, RES_COSTUME },	/* 32 */
	{ {0x07F800}, {0x00EA}, RES_COSTUME },	/* 33 */
	{ {0x08C800}, {0x572B}, RES_COSTUME },	/* 34 */
	{ {0x092000}, {0x0224}, RES_COSTUME },	/* 35 */
	{ {0x290800}, {0x03FB}, RES_COSTUME },	/* 36 */
	{ {0x09C000}, {0x076A}, RES_COSTUME },	/* 37 */
	{ {0x09C800}, {0x0B11}, RES_COSTUME },	/* 38 */
	{ {0x09D800}, {0x0DB1}, RES_COSTUME },	/* 39 */
	{ {0x09E800}, {0x0549}, RES_COSTUME },	/* 40 */
	{ {0x09F000}, {0x098F}, RES_COSTUME },	/* 41 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 42 */
	{ {0x0A0000}, {0x0672}, RES_COSTUME },	/* 43 */
	{ {0x0A0800}, {0x062F}, RES_COSTUME },	/* 44 */
	{ {0x070000}, {0x2A71}, RES_COSTUME },	/* 45 */
	{ {0x073000}, {0x25C7}, RES_COSTUME },	/* 46 */
	{ {0x075800}, {0x48F5}, RES_COSTUME },	/* 47 */
	{ {0x07A800}, {0x1D7C}, RES_COSTUME },	/* 48 */
	{ {0x0A1000}, {0x0940}, RES_COSTUME },	/* 49 */
	{ {0x0A2000}, {0x03AF}, RES_COSTUME },	/* 50 */
	{ {0x09B800}, {0x019A}, RES_COSTUME },	/* 51 */
	{ {0x2AC000}, {0x0FCD}, RES_COSTUME },	/* 52 */
	{ {0x0A2800}, {0x114D}, RES_COSTUME },	/* 53 */
	{ {0x0A4000}, {0x1F95}, RES_COSTUME },	/* 54 */
	{ {0x0CC800}, {0x0530}, RES_COSTUME },	/* 55 */
	{ {0x0CD000}, {0x027E}, RES_COSTUME },	/* 56 */
	{ {0x0CD800}, {0x02EF}, RES_COSTUME },	/* 57 */
	{ {0x0CE000}, {0x022E}, RES_COSTUME },	/* 58 */
	{ {0x0CE800}, {0x2FBF}, RES_COSTUME },	/* 59 */
	{ {0x0D6000}, {0x2419}, RES_COSTUME },	/* 60 */
	{ {0x0D8800}, {0x0D67}, RES_COSTUME },	/* 61 */
	{ {0x0E0000}, {0x106D}, RES_COSTUME },	/* 62 */
	{ {0x0E1800}, {0x00F3}, RES_COSTUME },	/* 63 */
	{ {0x127000}, {0x2196}, RES_COSTUME },	/* 64 */
	{ {0x13C800}, {0x23FF}, RES_COSTUME },	/* 65 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 66 */
	{ {0x13F000}, {0x0684}, RES_COSTUME },	/* 67 */
	{ {0x145000}, {0x1DE1}, RES_COSTUME },	/* 68 */
	{ {0x147000}, {0x010A}, RES_COSTUME },	/* 69 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 70 */
	{ {0x0FC800}, {0x0E90}, RES_COSTUME },	/* 71 */
	{ {0x10F800}, {0x0AC9}, RES_COSTUME },	/* 72 */
	{ {0x0F1800}, {0x2667}, RES_COSTUME },	/* 73 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 74 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 75 */
	{ {0x15A000}, {0x14C4}, RES_COSTUME },	/* 76 */
	{ {0x16B000}, {0x25C9}, RES_COSTUME },	/* 77 */
	{ {0x18E000}, {0x4D0D}, RES_COSTUME },	/* 78 */
	{ {0x1EB000}, {0x4760}, RES_COSTUME },	/* 79 */
	{ {0x0BB000}, {0x01C3}, RES_COSTUME },	/* 80 */
	{ {0x0BB800}, {0x00A1}, RES_COSTUME },	/* 81 */
	{ {0x0BE000}, {0x01C5}, RES_COSTUME },	/* 82 */
	{ {0x148000}, {0x1CAA}, RES_COSTUME },	/* 83 */
	{ {0x1E9800}, {0x1129}, RES_COSTUME },	/* 84 */
	{ {0x11D000}, {0x09BB}, RES_COSTUME },	/* 85 */
	{ {0x0EB000}, {0x01D3}, RES_COSTUME },	/* 86 */
	{ {0x042800}, {0x2ECA}, RES_COSTUME },	/* 87 */
	{ {0x110800}, {0x19DE}, RES_COSTUME },	/* 88 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 89 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 90 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 91 */
	{ {0x129800}, {0x2B6F}, RES_COSTUME },	/* 92 */
	{ {0x147800}, {0x00F9}, RES_COSTUME },	/* 93 */
	{ {0x1A7000}, {0x0255}, RES_COSTUME },	/* 94 */
	{ {0x126800}, {0x0327}, RES_COSTUME },	/* 95 */
	{ {0x16D800}, {0x0C15}, RES_COSTUME },	/* 96 */
	{ {0x1C4800}, {0x0489}, RES_COSTUME },	/* 97 */
	{ {0x17A800}, {0x05A3}, RES_COSTUME },	/* 98 */
	{ {0x180000}, {0x1DCC}, RES_COSTUME },	/* 99 */
	{ {0x182800}, {0x0636}, RES_COSTUME },	/* 100 */
	{ {0x183000}, {0x0719}, RES_COSTUME },	/* 101 */
	{ {0x214000}, {0x5207}, RES_COSTUME },	/* 102 */
	{ {0x1B2000}, {0x4713}, RES_COSTUME },	/* 103 */
	{ {0x1C2000}, {0x2004}, RES_COSTUME },	/* 104 */
	{ {0x1B6800}, {0x10A0}, RES_COSTUME },	/* 105 */
	{ {0x1B8000}, {0x0A21}, RES_COSTUME },	/* 106 */
	{ {0x1C6000}, {0x0D31}, RES_COSTUME },	/* 107 */
	{ {0x1C5000}, {0x059D}, RES_COSTUME },	/* 108 */
	{ {0x1C5800}, {0x0417}, RES_COSTUME },	/* 109 */
	{ {0x1CC800}, {0x453E}, RES_COSTUME },	/* 110 */
	{ {0x1E8800}, {0x0AEB}, RES_COSTUME },	/* 111 */
	{ {0x1A8800}, {0x0E7B}, RES_COSTUME },	/* 112 */
	{ {0x1FE000}, {0x2024}, RES_COSTUME },	/* 113 */
	{ {0x1D1000}, {0x0393}, RES_COSTUME },	/* 114 */
	{ {0x1D1800}, {0x03F2}, RES_COSTUME },	/* 115 */
	{ {0x200800}, {0x0EEF}, RES_COSTUME },	/* 116 */
	{ {0x297800}, {0x0885}, RES_COSTUME },	/* 117 */
	{ {0x1A8000}, {0x0430}, RES_COSTUME },	/* 118 */
	{ {0x1AA800}, {0x09C3}, RES_COSTUME },	/* 119 */
	{ {0x193000}, {0x3D45}, RES_COSTUME },	/* 120 */
	{ {0x1D7800}, {0x0940}, RES_COSTUME },	/* 121 */
	{ {0x222000}, {0x08D6}, RES_COSTUME },	/* 122 */
	{ {0x221000}, {0x0E93}, RES_COSTUME },	/* 123 */
	{ {0x238000}, {0x219E}, RES_COSTUME },	/* 124 */
	{ {0x28E800}, {0x19A3}, RES_COSTUME },	/* 125 */
	{ {0x1F4000}, {0x09E4}, RES_COSTUME },	/* 126 */
	{ {0x153800}, {0x0F9A}, RES_COSTUME },	/* 127 */
	{ {0x241800}, {0x09D2}, RES_COSTUME },	/* 128 */
	{ {0x112800}, {0x3220}, RES_COSTUME },	/* 129 */
	{ {0x223000}, {0xBA55}, RES_COSTUME },	/* 130 */
	{ {0x237000}, {0x07B4}, RES_COSTUME },	/* 131 */
	{ {0x201800}, {0x12EF}, RES_COSTUME },	/* 132 */
	{ {0x219800}, {0x1B71}, RES_COSTUME },	/* 133 */
	{ {0x20B800}, {0x3BAA}, RES_COSTUME },	/* 134 */
	{ {0x1B9000}, {0x050F}, RES_COSTUME },	/* 135 */
	{ {0x2A7000}, {0x00BF}, RES_COSTUME },	/* 136 */
	{ {0x14A000}, {0x03EE}, RES_COSTUME },	/* 137 */
	{ {0x2AD000}, {0x0DC9}, RES_COSTUME },	/* 138 */
	{ {0x047000}, {0x11A4}, RES_COSTUME },	/* 139 */
	{ {0x045800}, {0x17BD}, RES_COSTUME },	/* 140 */
	{ {0x041800}, {0x01CF}, RES_COSTUME },	/* 141 */
	{ {0x13C000}, {0x028F}, RES_COSTUME },	/* 142 */
	{ {0x06F800}, {0x01DD}, RES_COSTUME },	/* 143 */
	{ {0x2AB000}, {0x08FD}, RES_COSTUME },	/* 144 */
	{ {0x204000}, {0x269E}, RES_COSTUME },	/* 145 */
	{ {0x1A7800}, {0x05D8}, RES_COSTUME },	/* 146 */
	{ {0x254800}, {0x1291}, RES_COSTUME },	/* 147 */
	{ {0x273000}, {0x2866}, RES_COSTUME },	/* 148 */
	{ {0x236800}, {0x016A}, RES_COSTUME },	/* 149 */
	{ {0x048800}, {0x2DFF}, RES_COSTUME },	/* 150 */
	{ {0x182000}, {0x0687}, RES_COSTUME },	/* 151 */
	{ {0x1A9800}, {0x0C18}, RES_COSTUME },	/* 152 */
	{ {0x1DF800}, {0x10D1}, RES_COSTUME },	/* 153 */
	{ {0x105000}, {0x095E}, RES_COSTUME },	/* 154 */
	{ {0x106000}, {0x0155}, RES_COSTUME },	/* 155 */
	{ {0x1F3800}, {0x01AE}, RES_COSTUME },	/* 156 */
	{ {0x203000}, {0x086B}, RES_COSTUME },	/* 157 */
	{ {0x21B800}, {0x0940}, RES_COSTUME },	/* 158 */
	{ {0x106800}, {0x0752}, RES_COSTUME },	/* 159 */
	{ {0x107000}, {0x0840}, RES_COSTUME },	/* 160 */
	{ {0x237800}, {0x0696}, RES_COSTUME },	/* 161 */
	{ {0x116000}, {0x03EB}, RES_COSTUME },	/* 162 */
	{ {0x108000}, {0x0C15}, RES_COSTUME },	/* 163 */
	{ {0x037000}, {0x00EE}, RES_COSTUME },	/* 164 */
	{ {0x154800}, {0x09D0}, RES_COSTUME },	/* 165 */
	{ {0x12C800}, {0x00F3}, RES_COSTUME },	/* 166 */
	{ {0x0CC000}, {0x01C5}, RES_COSTUME },	/* 167 */
	{ {0x0BD000}, {0x09C3}, RES_COSTUME },	/* 168 */
	{ {0x0B2800}, {0x1040}, RES_COSTUME },	/* 169 */
	{ {0x0B4000}, {0x0208}, RES_COSTUME },	/* 170 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME },	/* 0 */
	{ {0x000000}, {0x0000}, RES_COSTUME }	/* 0 */
};

#define	NUM_SOUNDS	80
t_resource res_sounds[NUM_SOUNDS] = {
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND },	/* 0 */
	{ {0x000000}, {0x0000}, RES_SOUND }	/* 0 */
};

t_resource res_globdata =
	{ {0x012000}, {0x0FA7}, RES_GLOBDATA };
t_resource res_charset =
	{ {0x00659D}, {0x0484}, RES_CHARSET };

#define	NUM_UNKNOWNS	2
t_resource res_unknowns[NUM_UNKNOWNS] = {
	{ {0x013000}, {0x1A7F}, RES_UNKNOWN },	/* Unknown resource type, looks like a small Room resource */
	{ {0x2B4000}, {0x0155}, RES_COSTUME }	/* Duplicate of Costume 155 */
};

unsigned long r_offset (p_resource res)
{
	return res->offset[ISO];
}
unsigned short r_length (p_resource res)
{
	return res->length[ISO];
}

void	extract_resource (FILE *input, FILE *output, p_resource res)
{
	unsigned short j, off;
	signed short i, rlen;
	unsigned char junk, rtype, rid;

	if (res == NULL)
		error("extract_resource - no resource specified");
	if ((r_offset(res) == 0) && (r_length(res) == 0))
		return;	/* if offset/length are both 0, skip it */
	fseek(input,r_offset(res),SEEK_SET);

	switch (res->type)
	{
	case RES_CHARSET:
		rlen = r_length(res);
		writeUint16LE(output,(unsigned short)(rlen+4));
		writeUint16LE(output,0);
		for (i = 0; i < rlen; i++)
			writeByte(output,readByte(input));
		break;
	case RES_GLOBDATA:
		rlen = read_cword(input,&i);
		junk = read_cbyte(input,&i);
		rtype = read_cbyte(input,&i);
		rid = read_cbyte(input,&i);
		if (rlen != r_length(res))
			error("extract_resource(globdata) - length mismatch while extracting resource (was %04X, expected %04X)",rlen,r_length(res));
		if (rtype != 0x11)
			error("extract_resource(globdata) - resource tag is incorrect!");
		writeUint32LE(output,(unsigned short)(rlen + 1));
		writeUint16LE(output,'O0');	/* 0O - Object Index */
		for (i = 5; i < rlen; i++)
			writeByte(output,readByte(input));
		break;
#ifdef MAKE_LFLS
	case RES_ROOM:
		{
			i = 0;
			rlen = read_cword(input,&i);
			junk = read_cbyte(input,&i);
			rtype = read_cbyte(input,&i);
			rid = read_cbyte(input,&i);
/*
			notice("room res len %04X, junk %02X, type %02X, id %02X",rlen,junk,rtype,rid);
*/
			if (rlen != r_length(res))
				error("extract_resource(room) - length mismatch while extracting resource (was %04X, expected %04X)",rlen,r_length(res));
			if (rtype != 0x01)
				error("extract_resource(room) - resource tag is incorrect!");
			off = ftell(output);
			rlen = 0;
			write_clong(output,0,&rlen);
			write_cword(output,'OR',&rlen);	/* RO - Room */
			while (1)
			{
				unsigned short slen;
				unsigned char stype;
/* BM, HD, SL, NL, PA - current unknowns
				notice("reading local resource at offset %04X of %04X",i,len);
*/
				slen = read_cword(input,&i);
				if (slen == 0xFFFF)
					break;
				stype = read_cbyte(input,&i);
				slen -= 3;
				switch (stype)
				{
/*
				case 0x00:	break;
				case 0x01:	break;
				case 0x02:	break;
				case 0x03:	break;
				case 0x04:	break;
				case 0x05:	break;
				case 0x06:	break;
				case 0x07:	break;
				case 0x08:	break;	BM?
*/
				case 0x09:
					read_cword(input,&i);	slen -= 2;	/* skip first 2 bytes */
					write_clong(output,slen+6,&rlen);
					write_cword(output,'XB',&rlen);	/* BX - boxes */
					for (j = 0; j < slen; j++)
						write_cbyte(output,read_cbyte(input,&i),&rlen);
					break;
/*
				case 0x0A:
					break;
*/
				case 0x0B:
					write_clong(output,slen+6,&rlen);
					write_cword(output,'NE',&rlen);	/* EN - entrance script */
					for (j = 0; j < slen; j++)
						write_cbyte(output,read_cbyte(input,&i),&rlen);
					break;
				case 0x0C:
					write_clong(output,slen+6,&rlen);
					write_cword(output,'XE',&rlen);	/* EX - exit script */
					for (j = 0; j < slen; j++)
						write_cbyte(output,read_cbyte(input,&i),&rlen);
					break;
				case 0x0D:
					write_clong(output,slen+6,&rlen);
					write_cword(output,'IO',&rlen);	/* OI - object image */
					for (j = 0; j < slen; j++)
						write_cbyte(output,read_cbyte(input,&i),&rlen);
					break;
				case 0x0E:
					write_clong(output,slen+6,&rlen);
					write_cword(output,'CO',&rlen);	/* OC - object code */
					for (j = 0; j < slen; j++)
						write_cbyte(output,read_cbyte(input,&i),&rlen);
					break;
				case 0x0F:
					write_clong(output,slen+6,&rlen);
					write_cword(output,'CL',&rlen);	/* LC - local script count */
					for (j = 0; j < slen; j++)
						write_cbyte(output,read_cbyte(input,&i),&rlen);
					break;
				case 0x10:
					write_clong(output,slen+6,&rlen);
					write_cword(output,'SL',&rlen);	/* LS - local script */
					for (j = 0; j < slen; j++)
						write_cbyte(output,read_cbyte(input,&i),&rlen);
					break;
				default:
					fseek(input,slen,SEEK_CUR);
					warning("extract_resource(room) - unknown resource tag encountered: len %04X type %02X",slen,stype);
				}
			}
			fseek(output,off,SEEK_SET);
			writeUint32LE(output,rlen);
			fseek(output,0,SEEK_END);
		}
		break;
	case RES_SOUND:
		error("extract_resource(sound) - sound resources are not supported!");
		break;
	case RES_COSTUME:
		rlen = read_cword(input,&i);
		junk = read_cbyte(input,&i);
		rtype = read_cbyte(input,&i);
		rid = read_cbyte(input,&i);
		if (rlen != r_length(res))
			error("extract_resource(costume) - length mismatch while extracting resource (was %04X, expected %04X)",rlen,r_length(res));
		if (rtype != 0x03)
			error("extract_resource(costume) - resource tag is incorrect!");
		writeUint32LE(output,(unsigned short)(rlen + 1));
		writeUint16LE(output,'OC');	/* CO - Costume */
		for (i = 5; i < rlen; i++)
			writeByte(output,readByte(input));
		break;
	case RES_SCRIPT:
		rlen = read_cword(input,&i);
		junk = read_cbyte(input,&i);
		rtype = read_cbyte(input,&i);
		rid = read_cbyte(input,&i);
		if (rlen != r_length(res))
			error("extract_resource(script) - length mismatch while extracting resource (was %04X, expected %04X)",rlen,r_length(res));
		if (rtype != 0x02)
			error("extract_resource(script) - resource tag is incorrect!");
		writeUint32LE(output,(unsigned short)(rlen + 1));
		writeUint16LE(output,'CS');	/* SC - Script */
		for (i = 5; i < rlen; i++)
			writeByte(output,readByte(input));
		break;
	case RES_UNKNOWN:
#else
	case RES_ROOM:
	case RES_SOUND:
	case RES_COSTUME:
	case RES_SCRIPT:
	case RES_UNKNOWN:
		rlen = read_cword(input,&i);
		if (rlen != r_length(res))
			error("extract_resource - length mismatch while extracting resource (was %04X, expected %04X)",rlen,r_length(res));
		writeUint16LE(output,rlen);
		for (i = 2; i < rlen; i++)
			writeByte(output,readByte(input));
		break;
		break;
#endif
	default:
		warning("extract_resource - unknown resource type %d specified!",res->type);
	}
}

#ifdef	MAKE_LFLS
/* based on structure of Loom EGA LFL files */
p_resource lfl_01[] = { &res_rooms[1], &res_scripts[1], &res_scripts[2], &res_scripts[3], &res_scripts[5], &res_scripts[6], &res_scripts[7], &res_scripts[8], &res_scripts[9], &res_scripts[10], &res_scripts[11], &res_scripts[12], &res_scripts[13], &res_scripts[14], &res_scripts[15], &res_scripts[16], &res_scripts[17], &res_scripts[18], &res_scripts[19], &res_scripts[20], &res_scripts[21], &res_scripts[22], &res_scripts[23], &res_scripts[24], &res_scripts[25], &res_scripts[26], &res_scripts[27], &res_scripts[28], &res_scripts[29], &res_scripts[30], &res_scripts[31], &res_scripts[32], &res_scripts[33], &res_scripts[34], &res_scripts[35], &res_scripts[36], &res_scripts[37], &res_scripts[38], &res_scripts[39], &res_scripts[40], &res_scripts[41], &res_scripts[42], &res_scripts[43], &res_sounds[1], &res_sounds[2], &res_sounds[3], &res_sounds[4], &res_sounds[5], &res_sounds[6], &res_sounds[7], &res_sounds[8], &res_costumes[1], &res_costumes[2], &res_costumes[3], &res_costumes[164], NULL };
p_resource lfl_02[] = { &res_rooms[2], &res_scripts[44], &res_scripts[45], &res_scripts[113], &res_scripts[114], &res_costumes[141], &res_costumes[8], &res_costumes[87], &res_costumes[140], &res_costumes[139], &res_costumes[150], NULL };
p_resource lfl_03[] = { &res_rooms[3], &res_costumes[10], &res_costumes[9], &res_costumes[11], NULL };
p_resource lfl_04[] = { &res_rooms[4], &res_sounds[11], &res_costumes[13], &res_costumes[14], NULL };
p_resource lfl_05[] = { &res_rooms[5], &res_costumes[12], NULL };
p_resource lfl_06[] = { &res_rooms[6], &res_costumes[24], &res_costumes[25], NULL };
p_resource lfl_07[] = { &res_rooms[7], &res_costumes[143], &res_costumes[45], &res_costumes[46], &res_costumes[47], &res_costumes[48], &res_costumes[30], &res_costumes[31], &res_costumes[32], &res_costumes[33], NULL };
p_resource lfl_08[] = { &res_rooms[8], &res_scripts[46], &res_costumes[27], &res_costumes[28], &res_costumes[29], &res_costumes[34], &res_costumes[35], NULL };
p_resource lfl_09[] = { &res_rooms[9], &res_scripts[47], &res_scripts[101], &res_scripts[105], &res_costumes[51], &res_costumes[37], &res_costumes[38], &res_costumes[39], &res_costumes[40], &res_costumes[41], &res_costumes[43], &res_costumes[44], &res_costumes[49], &res_costumes[50], &res_costumes[53], &res_costumes[54], NULL };
p_resource lfl_10[] = { &res_rooms[10], &res_scripts[50], &res_scripts[107], &res_costumes[15], &res_costumes[16], &res_costumes[18], &res_costumes[169], &res_costumes[170], NULL };
p_resource lfl_11[] = { &res_rooms[11], &res_scripts[49], &res_scripts[51], &res_scripts[52], &res_scripts[89], &res_sounds[19], &res_costumes[22], &res_costumes[80], &res_costumes[81], &res_costumes[23], &res_costumes[168], &res_costumes[82], NULL };
p_resource lfl_12[] = { &res_rooms[12], &res_costumes[19], &res_costumes[17], NULL };
p_resource lfl_13[] = { &res_rooms[13], &res_scripts[53], &res_costumes[167], &res_costumes[55], &res_costumes[56], &res_costumes[57], &res_costumes[58], &res_costumes[59], NULL };
p_resource lfl_14[] = { &res_rooms[14], &res_costumes[60], &res_costumes[61], NULL };
p_resource lfl_15[] = { &res_rooms[15], &res_scripts[54], &res_costumes[62], &res_costumes[63], NULL };
p_resource lfl_16[] = { &res_rooms[16], &res_sounds[30], &res_costumes[86], NULL };
p_resource lfl_17[] = { &res_rooms[17], &res_scripts[66], &res_costumes[73], NULL };
p_resource lfl_18[] = { &res_rooms[18], &res_scripts[55], &res_scripts[56], &res_scripts[59], &res_scripts[61], &res_scripts[62], &res_scripts[64], &res_scripts[71], &res_costumes[71], NULL };
p_resource lfl_19[] = { &res_rooms[19], &res_scripts[65], &res_scripts[94], &res_sounds[17], &res_costumes[154], &res_costumes[159], &res_costumes[160], &res_costumes[163], NULL };
p_resource lfl_20[] = { &res_rooms[20], &res_scripts[57], &res_scripts[58], &res_scripts[60], &res_scripts[63], &res_costumes[72], &res_costumes[88], &res_costumes[129], &res_costumes[162], NULL };
p_resource lfl_22[] = { &res_rooms[22], &res_scripts[67], &res_scripts[108], &res_costumes[85], NULL };
p_resource lfl_23[] = { &res_rooms[23], &res_costumes[95], &res_costumes[64], &res_costumes[92], &res_costumes[166], NULL };
p_resource lfl_24[] = { &res_rooms[24], &res_scripts[70], &res_scripts[73], &res_costumes[142], &res_costumes[65], &res_costumes[67], NULL };
p_resource lfl_25[] = { &res_rooms[25], &res_scripts[69], &res_sounds[28], &res_sounds[29], &res_costumes[68], &res_costumes[69], &res_costumes[93], &res_costumes[83], &res_costumes[137], NULL };
p_resource lfl_26[] = { &res_rooms[26], &res_costumes[127], &res_costumes[165], NULL };
p_resource lfl_27[] = { &res_rooms[27], &res_costumes[76], NULL };
p_resource lfl_28[] = { &res_rooms[28], &res_scripts[72], &res_scripts[74], &res_scripts[75], &res_scripts[76], &res_scripts[109], &res_costumes[77], &res_costumes[96], NULL };
p_resource lfl_29[] = { &res_rooms[29], NULL };
p_resource lfl_30[] = { &res_rooms[30], &res_scripts[77], &res_scripts[81], &res_costumes[98], NULL };
p_resource lfl_31[] = { &res_rooms[31], &res_scripts[82], &res_scripts[83], &res_scripts[84], &res_costumes[99], &res_costumes[151], &res_costumes[100], &res_costumes[101], NULL };
p_resource lfl_32[] = { &res_rooms[32], &res_scripts[79], &res_scripts[80], NULL };
p_resource lfl_33[] = { &res_rooms[33], &res_costumes[78], &res_costumes[120], NULL };
p_resource lfl_34[] = { &res_rooms[34], &res_scripts[85], &res_scripts[88], &res_scripts[97], &res_scripts[98], &res_scripts[112], &res_costumes[94], &res_costumes[146], &res_costumes[118], &res_costumes[112], &res_costumes[152], &res_costumes[119], NULL };
p_resource lfl_35[] = { &res_rooms[35], &res_costumes[103], &res_costumes[105], &res_costumes[106], &res_costumes[135], NULL };
p_resource lfl_36[] = { &res_rooms[36], &res_sounds[27], &res_costumes[104], &res_costumes[97], &res_costumes[108], &res_costumes[109], &res_costumes[107], NULL };
p_resource lfl_37[] = { &res_rooms[37], &res_scripts[86], &res_costumes[110], &res_costumes[114], &res_costumes[115], NULL };
p_resource lfl_38[] = { &res_rooms[38], &res_scripts[87], &res_costumes[121], NULL };
p_resource lfl_39[] = { &res_rooms[39], NULL };
p_resource lfl_40[] = { &res_rooms[40], &res_costumes[153], NULL };
p_resource lfl_41[] = { &res_rooms[41], &res_sounds[12], &res_costumes[111], &res_costumes[84], &res_costumes[79], NULL };
p_resource lfl_42[] = { &res_rooms[42], &res_sounds[26], &res_costumes[156], &res_costumes[126], NULL };
p_resource lfl_43[] = { &res_rooms[43], &res_scripts[90], &res_scripts[91], &res_scripts[92], &res_scripts[93], &res_costumes[113], &res_costumes[116], &res_costumes[132], &res_costumes[157], &res_costumes[145], NULL };
p_resource lfl_44[] = { &res_rooms[44], &res_scripts[95], &res_costumes[134], NULL };
p_resource lfl_45[] = { &res_rooms[45], &res_scripts[96], &res_costumes[102], &res_costumes[133], &res_costumes[158], NULL };
p_resource lfl_46[] = { &res_rooms[46], &res_costumes[123], &res_costumes[122], &res_costumes[130], NULL };
p_resource lfl_47[] = { &res_rooms[47], &res_scripts[100], &res_costumes[149], &res_costumes[131], &res_costumes[161], &res_costumes[124], NULL };
p_resource lfl_49[] = { &res_rooms[49], &res_scripts[99], &res_scripts[110], &res_costumes[128], NULL };
p_resource lfl_50[] = { &res_rooms[50], NULL };
p_resource lfl_51[] = { &res_rooms[51], &res_scripts[102], &res_scripts[103], &res_scripts[104], &res_sounds[21], NULL };
p_resource lfl_52[] = { &res_rooms[52], &res_costumes[147], NULL };
p_resource lfl_53[] = { &res_rooms[53], &res_sounds[14], &res_sounds[15], &res_sounds[20], &res_sounds[25], &res_costumes[7], &res_costumes[6], NULL };
p_resource lfl_54[] = { &res_rooms[54], NULL };
p_resource lfl_55[] = { &res_rooms[55], NULL };
p_resource lfl_56[] = { &res_rooms[56], &res_scripts[68], NULL };
p_resource lfl_57[] = { &res_rooms[57], NULL };
p_resource lfl_58[] = { &res_rooms[58], NULL };
p_resource lfl_59[] = { &res_rooms[59], &res_sounds[31], NULL };
p_resource lfl_60[] = { &res_rooms[60], NULL };
p_resource lfl_61[] = { &res_rooms[61], &res_costumes[148], NULL };
p_resource lfl_62[] = { &res_rooms[62], &res_costumes[26], NULL };
p_resource lfl_63[] = { &res_rooms[63], &res_scripts[48], NULL };
p_resource lfl_64[] = { &res_rooms[64], NULL };
p_resource lfl_65[] = { &res_rooms[65], &res_scripts[78], NULL };
p_resource lfl_66[] = { &res_rooms[66], &res_scripts[106], &res_scripts[115], &res_costumes[125], &res_costumes[36], NULL };
p_resource lfl_67[] = { &res_rooms[67], NULL };
p_resource lfl_68[] = { &res_rooms[68], &res_costumes[117], NULL };
p_resource lfl_69[] = { &res_rooms[69], &res_sounds[16], &res_sounds[22], &res_sounds[32], &res_costumes[4], NULL };
p_resource lfl_70[] = { &res_rooms[70], &res_scripts[4], &res_sounds[9], &res_sounds[10], &res_sounds[13], &res_sounds[18], &res_sounds[23], &res_costumes[5], NULL };
p_resource lfl_79[] = { &res_rooms[79], NULL };
p_resource lfl_81[] = { &res_rooms[81], &res_costumes[144], &res_costumes[52], &res_costumes[138], NULL };
/*
p_resource lfl_82[] = { &res_rooms[82], &res_sounds[33], &res_sounds[34], &res_sounds[35], &res_sounds[36], &res_sounds[37], &res_sounds[38], &res_sounds[39], &res_sounds[40], &res_sounds[45], &res_sounds[46], &res_sounds[47], &res_sounds[48], &res_sounds[49], &res_sounds[50], &res_sounds[51], &res_sounds[52], &res_sounds[53], &res_sounds[54], &res_sounds[55], &res_sounds[41], &res_sounds[42], &res_sounds[44], &res_sounds[43], &res_sounds[56], NULL };
p_resource lfl_83[] = { &res_rooms[83], &res_sounds[57], &res_sounds[58], &res_sounds[59], &res_sounds[60], &res_sounds[61], &res_sounds[62], NULL };
p_resource lfl_84[] = { &res_rooms[84], &res_sounds[63], NULL };
p_resource lfl_85[] = { &res_rooms[85], &res_sounds[64], NULL };
*/
p_resource lfl_86[] = { &res_rooms[86], &res_scripts[111], &res_costumes[155], NULL };
p_resource lfl_87[] = { &res_rooms[87], NULL };

typedef	struct	_lfl
{
	int num;
	p_resource *entries;
}	t_lfl, *p_lfl;

t_lfl	lfls[] = {
	{  1, lfl_01 },
	{  2, lfl_02 },
	{  3, lfl_03 },
	{  4, lfl_04 },
	{  5, lfl_05 },
	{  6, lfl_06 },
	{  7, lfl_07 },
	{  8, lfl_08 },
	{  9, lfl_09 },
	{ 10, lfl_10 },
	{ 11, lfl_11 },
	{ 12, lfl_12 },
	{ 13, lfl_13 },
	{ 14, lfl_14 },
	{ 15, lfl_15 },
	{ 16, lfl_16 },
	{ 17, lfl_17 },
	{ 18, lfl_18 },
	{ 19, lfl_19 },
	{ 20, lfl_20 },
	{ 22, lfl_22 },
	{ 23, lfl_23 },
	{ 24, lfl_24 },
	{ 25, lfl_25 },
	{ 26, lfl_26 },
	{ 27, lfl_27 },
	{ 28, lfl_28 },
	{ 29, lfl_29 },
	{ 30, lfl_30 },
	{ 31, lfl_31 },
	{ 32, lfl_32 },
	{ 33, lfl_33 },
	{ 34, lfl_34 },
	{ 35, lfl_35 },
	{ 36, lfl_36 },
	{ 37, lfl_37 },
	{ 38, lfl_38 },
	{ 39, lfl_39 },
	{ 40, lfl_40 },
	{ 41, lfl_41 },
	{ 42, lfl_42 },
	{ 43, lfl_43 },
	{ 44, lfl_44 },
	{ 45, lfl_45 },
	{ 46, lfl_46 },
	{ 47, lfl_47 },
	{ 49, lfl_49 },
	{ 50, lfl_50 },
	{ 51, lfl_51 },
	{ 52, lfl_52 },
	{ 53, lfl_53 },
	{ 54, lfl_54 },
	{ 55, lfl_55 },
	{ 56, lfl_56 },
	{ 57, lfl_57 },
	{ 58, lfl_58 },
	{ 59, lfl_59 },
	{ 60, lfl_60 },
	{ 61, lfl_61 },
	{ 62, lfl_62 },
	{ 63, lfl_63 },
	{ 64, lfl_64 },
	{ 65, lfl_65 },
	{ 66, lfl_66 },
	{ 67, lfl_67 },
	{ 68, lfl_68 },
	{ 69, lfl_69 },
	{ 70, lfl_70 },
	{ 79, lfl_79 },
	{ 81, lfl_81 },
/*
	{ 82, lfl_82 },
	{ 83, lfl_83 },
	{ 84, lfl_84 },
	{ 85, lfl_85 },
*/
	{ 86, lfl_86 },
	{ 87, lfl_87 },
	{ -1, NULL }
};

struct	_index
{
	unsigned short	num_rooms;
	unsigned char	room_lfl[NUM_ROOMS];
	unsigned long	room_addr[NUM_ROOMS];

	unsigned short	num_costumes;
	unsigned char	costume_lfl[NUM_COSTUMES];
	unsigned long	costume_addr[NUM_COSTUMES];

	unsigned short	num_scripts;
	unsigned char	script_lfl[NUM_SCRIPTS];
	unsigned long	script_addr[NUM_SCRIPTS];
	unsigned short	num_sounds;
	unsigned char	sound_lfl[NUM_SOUNDS];
	unsigned long	sound_addr[NUM_SOUNDS];
}	lfl_index;
#else	/* !MAKE_LFLS */
void	dump_resource (FILE *input, char *fn_template, int num, p_resource res)
{
	char fname[256];
	FILE *output;
	sprintf(fname,fn_template,num);
	if (!(output = fopen(fname,"wb")))
		error("Error: unable to create %s!",fname);
	extract_resource(input,output,res);
	fclose(output);
}
#endif	/* MAKE_LFLS */

unsigned long	CRCtable[256];
void	InitCRC (void)
{
	const unsigned long poly = 0xEDB88320;
	int i, j;
	unsigned long n;
	for (i = 0; i < 256; i++)
	{
		n = i;
		for (j = 0; j < 8; j++)
			n = (n & 1) ? ((n >> 1) ^ poly) : (n >> 1);
		CRCtable[i] = n;
	}
}
unsigned long	ISO_CRC (FILE *file)
{
	unsigned long CRC = 0xFFFFFFFF;
	unsigned long i, len;
	fseek(file,0,SEEK_END);
	len = ftell(file);
	fseek(file,0,SEEK_SET);
	for (i = 0; i < len; i++)
		CRC = (CRC >> 8) ^ CRCtable[(CRC ^ readByte(file)) & 0xFF];
	return CRC ^ 0xFFFFFFFF;
}

int main (int argc, char **argv)
{
	FILE *input, *output;
	char fname[256];
	int i, j;
	unsigned long CRC;

	if (argc < 2)
	{
		printf("Syntax: %s <code_##.ISO>\n",argv[0]);
		return 1;
	}
	if (!(input = fopen(argv[1],"rb")))
		error("Error: unable to open file %s for input!",argv[1]);

	InitCRC();
	CRC = ISO_CRC(input);
	switch (CRC)
	{
	case 0x29EED3C5:
		ISO = ISO_USA;
		notice("ISO contents verified as Loom USA (track 2)");
		break;
	default:
		error("ISO contents not recognized!");
		break;
	}
#ifdef	MAKE_LFLS
	memset(&lfl_index,0xFF,sizeof(lfl_index));

	for (i = 0; lfls[i].num != -1; i++)
	{
		p_lfl lfl = &lfls[i];
		sprintf(fname,"%02i.LFL",lfl->num);
		if (!(output = fopen(fname,"wb")))
			error("Error: unable to create %s!",fname);
		notice("Creating %s...",fname);
		for (j = 0; lfl->entries[j] != NULL; j++)
		{
			p_resource entry = lfl->entries[j];
			switch (entry->type)
			{
			case RES_ROOM:
				lfl_index.room_lfl[entry - res_rooms] = lfl->num;
				lfl_index.room_addr[entry - res_rooms] = (unsigned short)ftell(output);
				break;
			case RES_COSTUME:
				lfl_index.costume_lfl[entry - res_costumes] = lfl->num;
				lfl_index.costume_addr[entry - res_costumes] = (unsigned short)ftell(output);
				break;
			case RES_SCRIPT:
				lfl_index.script_lfl[entry - res_scripts] = lfl->num;
				lfl_index.script_addr[entry - res_scripts] = (unsigned short)ftell(output);
				break;
			case RES_SOUND:
				lfl_index.sound_lfl[entry - res_sounds] = lfl->num;
				lfl_index.sound_addr[entry - res_sounds] = (unsigned short)ftell(output);
				break;
			default:
				notice("Unknown resource type %d detected in LFL index!",entry->type);
				break;
			}
			extract_resource(input,output,entry);
		}
		fclose(output);
	}
	if (!(output = fopen("00.LFL","wb")))
		error("Error: unable to create index file!");
	notice("Creating 00.LFL...");

	lfl_index.num_rooms = NUM_ROOMS;
	lfl_index.num_costumes = NUM_COSTUMES;
	lfl_index.num_scripts = NUM_SCRIPTS;
	lfl_index.num_sounds = NUM_SOUNDS;

	writeUint32LE(output,8+5*lfl_index.num_rooms);
	writeUint16LE(output,'R0');	/* 0R - room index */
	writeUint16LE(output,lfl_index.num_rooms);
	for (i = 0; i < lfl_index.num_rooms; i++)
	{
		writeByte(output,lfl_index.room_lfl[i]);
		writeUint32LE(output,lfl_index.room_addr[i]);
	}

	writeUint32LE(output,8+5*lfl_index.num_scripts);
	writeUint16LE(output,'S0');	/* 0S - script index */
	writeUint16LE(output,lfl_index.num_scripts);
	for (i = 0; i < lfl_index.num_scripts; i++)
	{
		writeByte(output,lfl_index.script_lfl[i]);
		writeUint32LE(output,lfl_index.script_addr[i]);
	}

	writeUint32LE(output,8+5*lfl_index.num_costumes);
	writeUint16LE(output,'C0');	/* 0C - costume index */
	writeUint16LE(output,lfl_index.num_costumes);
	for (i = 0; i < lfl_index.num_costumes; i++)
	{
		writeByte(output,lfl_index.costume_lfl[i]);
		writeUint32LE(output,lfl_index.costume_addr[i]);
	}

/*
	writeUint32LE(output,8+5*lfl_index.num_sounds);
	writeUint16LE(output,'N0');	0N - sounds index
	writeUint16LE(output,lfl_index.num_sounds);
	for (i = 0; i < lfl_index.num_sounds; i++)
	{
		writeByte(output,lfl_index.sound_lfl[i]);
		writeUint32LE(output,lfl_index.sound_addr[i]);
	}
*/

	extract_resource(input,output,&res_globdata);
	
	fclose(output);

	if (!(output = fopen("97.LFL","wb")))
		error("Error: unable to create charset file!");
	notice("Creating 97.LFL...");
	extract_resource(input,output,&res_charset);
	fclose(output);

	if (!(output = fopen("98.LFL","wb")))
		error("Error: unable to create charset file!");
	notice("Creating 98.LFL...");
	extract_resource(input,output,&res_charset);
	fclose(output);

	if (!(output = fopen("99.LFL","wb")))
		error("Error: unable to create charset file!");
	notice("Creating 99.LFL...");
	extract_resource(input,output,&res_charset);
	fclose(output);

#else	/* !MAKE_LFLS */
	dump_resource(input,"globdata.dmp",0,&res_globdata);
	dump_resource(input,"charset.dmp",0,&res_charset);
	for (i = 0; i < NUM_UNKNOWNS; i++)
		dump_resource(input,"unk-%d.dmp",i,&res_unknowns[i]);
	for (i = 0; i < NUM_ROOMS; i++)
		dump_resource(input,"room-%d.dmp",i,&res_rooms[i]);
	for (i = 0; i < NUM_COSTUMES; i++)
		dump_resource(input,"costume-%d.dmp",i,&res_costumes[i]);
	for (i = 0; i < NUM_SCRIPTS; i++)
		dump_resource(input,"script-%d.dmp",i,&res_scripts[i]);
	for (i = 0; i < NUM_SOUNDS; i++)
		dump_resource(input,"sound-%d.dmp",i,&res_sounds[i]);
#endif	/* MAKE_LFLS */
	notice("All done!");
	return 0;
}
