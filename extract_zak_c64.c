/* MM_C64_Extract - Extract data files from C64 version of Maniac Mansion
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $Header$
 *
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

typedef int BOOL;
#define TRUE 1
#define FALSE 0

#ifdef WIN32
	#define CDECL __cdecl
#else
	#define CDECL 
#endif

void CDECL debug (const char *Text, ...)
{
	va_list marker;
	va_start(marker,Text);
	vfprintf(stdout,Text,marker);
	fprintf(stdout,"\n");
	va_end(marker);
}

void CDECL error (const char *Text, ...)
{
	va_list marker;
	va_start(marker,Text);
	vfprintf(stderr,Text,marker);
	fprintf(stderr,"\n");
	va_end(marker);
	exit(1);
}

void CDECL _assert (BOOL condition, const char *Text, ...)
{
	va_list marker;
	if (condition)
		return;
	va_start(marker,Text);
	vfprintf(stderr,Text,marker);
	fprintf(stderr,"\n");
	va_end(marker);
	exit(1);
}

unsigned char	read_byte (FILE *input)
{
	unsigned char val;
	_assert(fread(&val,1,1,input) == 1,"read_byte - unexpected EOF");
	return val;
}
unsigned short	read_word (FILE *input)
{
	unsigned short val;
	_assert(fread(&val,2,1,input) == 1,"read_word - unexpected EOF");
	return val;
}
void	write_byte (FILE *output, unsigned char val)
{
	val ^= 0xFF;
	fwrite(&val,1,1,output);
}
void	write_word (FILE *output, unsigned short val)
{
	val ^= 0xFFFF;
	fwrite(&val,2,1,output);
}

unsigned char room_disks[59], room_tracks[59], room_sectors[59];

int main (int argc, char **argv)
{
	FILE *input1, *input2, *output;
	char fname[256];
	int i, j;
	unsigned short signature;

	if (argc < 3)
	{
		printf("Syntax: %s <disk1.d64> <disk2.d64>\n",argv[0]);
		return 1;
	}
	if (!(input1 = fopen(argv[1],"rb")))
		error("Error: unable to open file %s for input!",argv[1]);
	if (!(input2 = fopen(argv[2],"rb")))
		error("Error: unable to open file %s for input!",argv[2]);

	/* check signature */
	signature = read_word(input1);
	if (signature != 0x0A31)
		error("Error: signature not found in disk 1!\n");
	signature = read_word(input2);
	if (signature != 0x0132)
		error("Error: signature not found in disk 2!\n");

	if (!(output = fopen("00.LFL","wb")))
		error("Error: unable to create index file!");
	debug("Creating 00.LFL...");

	/* write signature */
	write_word(output, signature);

	/* copy object flags */
	for (i = 0; i < 775; i++)
		write_byte(output, read_byte(input1));

	/* copy room offsets */
	for (i = 0; i < 59; i++)
	{
		room_disks[i] = read_byte(input1);
		write_byte(output, room_disks[i]);
	}
	for (i = 0; i < 59; i++)
	{
		room_sectors[i] = read_byte(input1);
		room_tracks[i] = read_byte(input1);
		write_byte(output, room_sectors[i]);
		write_byte(output, room_tracks[i]);
	}

	for (i = 0; i < 38; i++)
		write_byte(output, read_byte(input1));
	for (i = 0; i < 38; i++)
		write_word(output, read_word(input1));

	for (i = 0; i < 155; i++)
		write_byte(output, read_byte(input1));
	for (i = 0; i < 155; i++)
		write_word(output, read_word(input1));

	for (i = 0; i < 127; i++)
		write_byte(output, read_byte(input1));
	for (i = 0; i < 127; i++)
		write_word(output, read_word(input1));
	fclose(output);

	for (i = 0; i < 59; i++)
	{
		const int SectorOffset[36] =
		{
			0,
			0, 21, 42, 63, 84, 105, 126, 147, 168, 189, 210, 231, 252, 273, 294, 315, 336,
			357, 376, 395, 414, 433, 452, 471,
			490, 508, 526, 544, 562, 580,
			598, 615, 632, 649, 666
		};
		const int ResourcesPerFile[59] =
		{
			 0, 29, 12, 14, 13,  4,  4, 10,  7,  4,
			14, 19,  5,  4,  7,  6, 11,  9,  4,  4,
			 1,  3,  3,  5,  1,  9,  4, 10, 13,  6,
			 7, 10,  2,  6,  1, 11,  2,  5,  7,  1,
			 7,  1,  4,  2,  8,  6,  6,  6,  4, 13,
			 3,  1,  2,  1,  2,  1, 10,  1,  1
		};
		FILE *input;
		if (room_disks[i] == '1')
			input = input1;
		else if (room_disks[i] == '2')
			input = input2;
		else	continue;

		sprintf(fname,"%02i.LFL", i);
		printf("Creating %s...\n",fname);
		output = fopen(fname, "wb");
		fseek(input, (SectorOffset[room_tracks[i]] + room_sectors[i]) * 256, SEEK_SET);
		for (j = 0; j < ResourcesPerFile[i]; j++)
		{
			unsigned short len = read_word(input);
			write_word(output, len);
			for (len -= 2; len > 0; len--)
				write_byte(output, read_byte(input));
		}
		rewind(input);
	}

	debug("All done!");
	return 0;
}
