/* Scumm Tools
 * Copyright (C) 2004-2006  The ScummVM Team
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

#ifndef EXTRACT_H
#define EXTRACT_H

#include "util.h"

#if defined(__cplusplus)
extern "C" {
#endif

/* These are the defaults parameters for the Lame invocation */
#define minBitrDef 24
#define maxBitrDef 64
#define algqualDef 2
#define vbrqualDef 4

/* The default for oggenc invocation is to use the --quality option only */
#define oggqualDef 3

#define TEMP_WAV	"tempfile.wav"
#define TEMP_RAW	"tempfile.raw"
#define TEMP_MP3	"tempfile.mp3"
#define TEMP_OGG	"tempfile.ogg"
#define TEMP_FLAC	"tempfile.fla"

typedef enum { kMP3Mode, kVorbisMode, kFlacMode } CompressMode;

/*
 * Stuff which is in compress.c
 */

const extern char *tempEncoded;

extern int process_mp3_parms(int argc, char *argv[], int i);
extern int process_ogg_parms(int argc, char *argv[], int i);
extern int process_flac_parms(int argc, char *argv[], int i);

extern void extractAndEncodeVOC(const char *outName, FILE *input, CompressMode compMode);
extern void extractAndEncodeWAV(const char *outName, FILE *input, CompressMode compMode);

extern void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, CompressMode compmode);
extern void setRawAudioType(bool isLittleEndian, bool isStereo, bool isUnSigned, uint8 bitsPerSample);

#if defined(__cplusplus)
}
#endif

#endif
