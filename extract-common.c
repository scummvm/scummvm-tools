/* Scumm Tools
 * Copyright (C) 2003  The ScummVM Team
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

#include "extract.h"

FILE *input, *output_idx, *output_snd;

lameparams encparms = { minBitrDef, maxBitrDef, abrDef, vbrDef, algqualDef, vbrqualDef, 0 };
oggencparams oggparms = { -1, -1, -1, oggqualDef, 0 };

int oggmode = 0;


int getSampleRateFromVOCRate(int vocSR) {
	if (vocSR == 0xa5 || vocSR == 0xa6 || vocSR == 0x83) {
		return 11025;
	} else if (vocSR == 0xd2 || vocSR == 0xd3) {
		return 22050;
	} else {
		int sr = 1000000L / (256L - vocSR);
		warning("inexact sample rate used: %i (0x%x)", sr, vocSR);
		return sr;
	}
}

void process_mp3_parms(int argc, char *argv[], int i) {
	for(; i < argc; i++) {
		if (strcmp(argv[i], "--vbr") == 0) {
			encparms.vbr=1;
			encparms.abr=0;
		} else if (strcmp(argv[i], "--abr") == 0) {
			encparms.vbr=0;
			encparms.abr=1;
		} else if (strcmp(argv[i], "-b") == 0) {
			encparms.minBitr = atoi(argv[i + 1]);
			if ((encparms.minBitr % 8) != 0)
				encparms.minBitr -= encparms.minBitr % 8;
			if (encparms.minBitr >160)
				encparms.minBitr = 160;
			if (encparms.minBitr < 8)
				encparms.minBitr=8;
			i++;
		} else if (strcmp(argv[i], "-B") == 0) {
			encparms.maxBitr = atoi(argv[i + 1]);
			if ((encparms.maxBitr % 8) != 0)
				encparms.maxBitr -= encparms.maxBitr % 8;
			if (encparms.maxBitr > 160)
				encparms.maxBitr = 160;
			if (encparms.maxBitr < 8)
				encparms.maxBitr = 8;
			i++;
		} else if (strcmp(argv[i], "-V") == 0) {
			encparms.vbrqual = atoi(argv[i + 1]);
			if(encparms.vbrqual < 0)
				encparms.vbrqual = 0;
			if(encparms.vbrqual > 9)
				encparms.vbrqual = 9;
			i++;
		} else if (strcmp(argv[i], "-q") == 0) {
			encparms.algqual = atoi(argv[i + 1]);
			if (encparms.algqual < 0)
				encparms.algqual = 0;
			if (encparms.algqual > 9)
				encparms.algqual = 9;
			i++;
		} else if (strcmp(argv[i], "--silent") == 0) {
			encparms.silent = 1;
		} else if (strcmp(argv[i], "--help") == 0) {
			showhelp(argv[0]);
		} else if (argv[i][0] == '-') {
			showhelp(argv[0]);
		} else {
			break;
		}
	}
	if (i != (argc - 1)) {
		showhelp(argv[0]);
	}
}

void process_ogg_parms(int argc, char *argv[], int i) {
	for (; i < argc; i++) {
		if (strcmp(argv[i], "-b") == 0) {
			oggparms.nominalBitr = atoi(argv[i + 1]);
			i++;
		}
		else if (strcmp(argv[i], "-m") == 0) {
			oggparms.minBitr = atoi(argv[i + 1]);
			i++;
		}
		else if (strcmp(argv[i], "-M") == 0) {
			oggparms.maxBitr = atoi(argv[i + 1]);
			i++;
		}
		else if (strcmp(argv[i], "-q") == 0) {
			oggparms.quality = atoi(argv[i + 1]);
			i++;
		}
		else if (strcmp(argv[i], "--silent") == 0) {
			oggparms.silent = 1;
		}
		else if (strcmp(argv[i], "--help") == 0) {
			showhelp(argv[0]);
		}
		else if (argv[i][0] == '-') {
			showhelp(argv[0]);
		}
		else
			break;
	}
	if (i != argc - 1)
		showhelp(argv[0]);
}

