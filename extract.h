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

#ifndef EXTRACT_H
#define EXTRACT_H

#include "util.h"


/* These are the defaults parameters for the Lame invocation */
#define minBitrDef 24
#define maxBitrDef 64
#define abrDef 0
#define vbrDef 1
#define algqualDef 2
#define vbrqualDef 4

/* The default for oggenc invocation is to use the --quality option only */
#define oggqualDef 3

typedef struct  {
	uint32 minBitr;
	uint32 maxBitr; 
	uint32 abr;
	uint32 vbr;
	uint32 algqual;
	uint32 vbrqual;
	uint32 silent;
} lameparams;

typedef struct {
	int nominalBitr;
	int minBitr;
	int maxBitr;
	int quality;
	int silent;
} oggencparams;


#define TEMP_RAW	"tempfile.raw"
#define TEMP_MP3	"tempfile.mp3"
#define TEMP_OGG	"tempfile.ogg"

/*
 * Stuff which is in extract-common.c
 */
extern FILE *input, *output_idx, *output_snd;

extern lameparams encparms;
extern oggencparams oggparms;

extern int oggmode;

extern void process_mp3_parms(int argc, char *argv[], int i);
extern void process_ogg_parms(int argc, char *argv[], int i);

extern int getSampleRateFromVOCRate(int vocSR);


/*
 * Stuff which is in extract.c / simon2mp3.c
 */
extern void showhelp(char *exename);


#endif
