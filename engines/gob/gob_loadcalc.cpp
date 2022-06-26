/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* Gobliiins (Gob1) Load Code Calculator */

#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>

void printUsage(void) {
	printf("Usage : ./gob-loadcalc [Level/Screen = 0 to 23] [Health = 0 to 10]\n");
}

const char baseCodes[24][6] = {
	"GAGEA", //  0 = Level  2
	"BULBE", //  1 = Level  6
	"CANON", //  2 = Level  3
	"TOTOD", //  3 = Level  1
	"DRUID", //  4 = Level  4
	"FOUDR", //  5 = Level  5
	"GATEA", //  6 = Level  8
	"HAHAH", //  7 = Level  7
	"HIHIH", //  8 = Level  9
	"JONAS", //  9 = Level 10
	"FLUTE", // 10 = Level 11
	"DROIT", // 11 = Level 12
	"BANJO", // 12 = Level 13
	"CUBEN", // 13 = Level 14
	"RALER", // 14 = Level 15
	"RATOP", // 15 = Level 16
	"GOBLI", // 16 = Level 17
	"IIINS", // 17 = Level 18
	"LEMEI", // 18 = Level 19
	"LLEUR", // 19 = Level 20
	"JEUDE", // 20 = Level 21
	"ROLED", // 21 = Level 22
	"ETOUT", // 22 = Level  0
	"LESTP"  // 23 = Level  0 - Bad Start Screen With Palette Scramble
};

int main(int argc, char *argv[]) {
	int i, temp, level, health;

	char code[8];

	for (i = 0; i < 8; i++)
		code[i] = '\0';

	if (argc != 3) {
		printUsage();
		return EXIT_FAILURE;
	} else {
		level = atoi(argv[1]);
		if (level < 0 || level > 23) {
			printUsage();
			return EXIT_FAILURE;
		}
		health = atoi(argv[2]);
		if (health < 0 || health > 10) {
			printUsage();
			return EXIT_FAILURE;
		}
	}

	printf("Level: %d\n", level);
	printf("Health: %d\n", health);

	// Convert Level to offset in baseCodes
	switch (level) {
		case 0:
			level = 22;
			break;
		case 1:
			level = 3;
			break;
		case 2:
			level = 0;
			break;
		case 6:
			level = 1;
			break;
		case 8:
			level = 6;
			break;
		case 4:
		case 5:
		case 7:
		case 23:
			// level unchanged
			break;
		default:
			level -= 1;
			break;
	}

	strncpy(code, baseCodes[level], 5*sizeof(char));

	// Used by Load to determine Level
	code[5] = level + 'A';

	// Adjust Code for Health
	for (i = 0; i < 5; i++) {
		if (health >= 2) {
			code[i] += 2;
			health -= 2;
		} else if (health >= 1) {
			code[i] += 1;
			health -= 1;
		}
	}

	// Calculate Checksum Character
	temp = 0;
	for (i = 0; i < 6; i++)
		temp += code[i];
	temp %= 26;
	code[6] = 'A' + (char)temp;

	printf("Code :\"%s\"\n", code);

	return EXIT_SUCCESS;
}
