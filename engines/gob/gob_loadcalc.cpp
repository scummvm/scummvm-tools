/* gob_loadcalc - Gobliiins (Gob1) Load Code Calculator
 * Copyright (C) 2010 The ScummVM project
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

#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>

void print_usage(void) {
	printf("Usage : ./gob-loadcalc [Level/Screen = 0 to 23] [Health = 0 to 10]\n");
}

const char base_codes[24][6] = {
	"GAGEA", /*  0 = Level  3 */
	"BULBE", /*  1 = Level  7 */
	"CANON", /*  2 = Level  4 */
	"TOTOD", /*  3 = Level  2 */
	"DRUID", /*  4 = Level  5 */
	"FOUDR", /*  5 = Level  6 */
	"GATEA", /*  6 = Level  9 */
	"HAHAH", /*  7 = Level  8 */
	"HIHIH", /*  8 = ???? - Unknown Screen with Watchdog Outside Wizard's House */
	"JONAS", /*  9 = Level 10 */
	"FLUTE", /* 10 = Level 11 */
	"DROIT", /* 11 = Level 12 */
	"BANJO", /* 12 = Level 13 */
	"CUBEN", /* 13 = Level 14 */
	"RALER", /* 14 = Level 15 */
	"RATOP", /* 15 = Level 16 */
	"GOBLI", /* 16 = Level 17 */
	"IIINS", /* 17 = Level 18 */
	"LEMEI", /* 18 = Level 19 */
	"LLEUR", /* 19 = Level 20 */
	"JEUDE", /* 20 = ???? - Unknown Screen Outside Castle with Catapult */
	"ROLED", /* 21 = Level 21 */
	"ETOUT", /* 22 = Level  1 */
	"LESTP"  /* 23 = ???? - Start Screen With Palette Scramble */
};

int main(int argc, char *argv[]) {
	int i, temp, level, health;

	char code[8];

	for (i = 0; i < 8; i++)
		code[i] = '\0';

	if (argc != 3) {
		print_usage();
		return EXIT_FAILURE;
	} else {
		level = atoi(argv[1]);
		if (level < 0 || level > 23) {
			print_usage();
			return EXIT_FAILURE;
		}
		health = atoi(argv[2]);
		if (health < 0 || health > 10) {
			print_usage();
			return EXIT_FAILURE;
		}
	}

	printf("Level: %d\n", level);
	printf("Health: %d\n", health);

	strncpy(code, base_codes[level], 5*sizeof(char));

	/* Used by Load to determine Level */
	code[5] = level + 'A';

	/* Adjust Code for Health */
	for (i = 0; i < 5; i++) {
		if (health >= 2) {
			code[i] += 2;
			health -= 2;
		} else if (health >= 1) {
			code[i] += 1;
			health -= 1;
		}
	}

	/* Calculate Checksum Character */
	temp = 0;
	for (i = 0; i < 6; i++)
		temp += code[i];
	temp %= 26;
	code[6] = 'A' + (char)temp;

	printf("Code :\"%s\"\n", code);

	return EXIT_SUCCESS;
}