/* Scumm Tools
 * Copyright (C) 2003-2006  The ScummVM Team
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
 * $Header$
 *
 */

#include "compress.h"

typedef struct  {
	uint32 minBitr;
	uint32 maxBitr; 
	bool abr;
	uint32 algqual;
	uint32 vbrqual;
	bool silent;
} lameparams;

typedef struct {
	int nominalBitr;
	int minBitr;
	int maxBitr;
	int quality;
	bool silent;
} oggencparams;

/* FIXME: This is an evil way to pass on the params to FLAC.
 It makes it near impossible to reliably pass default params to the
 encoder, which is why the ScummVM README has to tell the user to
 use this command:
   extract --best -b 1152 monster.sou
 If those are the best default options, then they should be *default*
 and the user shouldn't have to specify them.
*/
typedef struct {
	char * const* argv;
	int numArgs;
} flaccparams;

typedef struct {
    bool isLittleEndian, isStereo;
	uint8 bitsPerSample;
} rawtype;

lameparams encparms = { minBitrDef, maxBitrDef, false, algqualDef, vbrqualDef, 0 };
oggencparams oggparms = { -1, -1, -1, oggqualDef, 0 };
flaccparams flacparms;
rawtype	rawAudioType = { false, false, 8 };

const char *tempEncoded = TEMP_MP3;

void setRawAudioType(bool isLittleEndian, bool isStereo, uint8 bitsPerSample) {
	rawAudioType.isLittleEndian = isLittleEndian;
	rawAudioType.isStereo = isStereo;
	rawAudioType.bitsPerSample = bitsPerSample;
}

int getSampleRateFromVOCRate(int vocSR) {
	if (vocSR == 0xa5 || vocSR == 0xa6 || vocSR == 0x83) {
		return 11025;
	} else if (vocSR == 0xd2 || vocSR == 0xd3) {
		return 22050;
	} else {
		int sr = 1000000L / (256L - vocSR);
		warning("inexact sample rate used: %d (0x%x)", sr, vocSR);
		return sr;
	}
}

/* map frequency to a valid MP3 sample frequency
 *
 * Robert Hegemann 2000-07-01
 *
 * Copied from lame 3.96.1
 */
static int map2MP3Frequency(int freq)
{
    if (freq <=  8000) return  8000;
    if (freq <= 11025) return 11025;
    if (freq <= 12000) return 12000;
    if (freq <= 16000) return 16000;
    if (freq <= 22050) return 22050;
    if (freq <= 24000) return 24000;
    if (freq <= 32000) return 32000;
    if (freq <= 44100) return 44100;
    
    return 48000;
}

void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, CompressMode compmode) {
	char fbuf[2048];
	char *tmp = fbuf;
	int i;
	bool err = false;

	switch (compmode) {
	case kVorbisMode:
		tmp += sprintf(tmp, "oggenc ");
		if (rawInput) {
			tmp += sprintf(tmp, "--raw ");
			tmp += sprintf(tmp, "--raw-chan=%d ", (rawAudioType.isStereo ? 2 : 1));
			tmp += sprintf(tmp, "--raw-bits=%d ", rawAudioType.bitsPerSample);
			tmp += sprintf(tmp, "--raw-rate=%d ", rawSamplerate);
			tmp += sprintf(tmp, "--raw-endianness=%d ", (rawAudioType.isLittleEndian ? 0 : 1));
		}

		if (oggparms.nominalBitr != -1)
			tmp += sprintf(tmp, "--bitrate=%d ", oggparms.nominalBitr);
		if (oggparms.minBitr != -1)
			tmp += sprintf(tmp, "--min-bitrate=%d ", oggparms.minBitr);
		if (oggparms.maxBitr != -1)
			tmp += sprintf(tmp, "--max-bitrate=%d ", oggparms.maxBitr);
		if (oggparms.silent)
			tmp += sprintf(tmp, "--quiet ");
		tmp += sprintf(tmp, "--quality=%d ", oggparms.quality);
		tmp += sprintf(tmp, "--output=%s ", outname);
		tmp += sprintf(tmp, "%s ", inname);
		err = system(fbuf) != 0;
		break;

	case kMP3Mode:
		tmp += sprintf(tmp, "lame -t ");
		if (rawInput) {
			tmp += sprintf(tmp, "-r ");
			tmp += sprintf(tmp, "--bitwidth %d ", rawAudioType.bitsPerSample);
			if (rawAudioType.isLittleEndian)
				tmp += sprintf(tmp, "-x ");
			tmp += sprintf(tmp, (rawAudioType.isStereo ? "-m j " : "-m m "));
			tmp += sprintf(tmp, "-s %d ", rawSamplerate);
		}

		if (encparms.abr)
			tmp += sprintf(tmp, "--abr %d ", encparms.minBitr);
		else
			tmp += sprintf(tmp, "--vbr-new -b %d ", encparms.minBitr);

		/* Explicitly specify a target sample rate, to work around a bug (?)
		 * in newer lame versions (>= 3.95) which causes it to malfunction
		 * for odd sample rates when in VBR mode. See also bug #934026.
		 * We essentially duplicate the old behaviour of lame (found in e.g.
		 * version 3.93.1): we round the input sample rate up to the next
		 * higher valid MP3 sample rate, with a margin of 3%.
		 */
		if (rawSamplerate != -1)
			tmp += sprintf(tmp, "--resample %d ", map2MP3Frequency(97 * rawSamplerate / 100));

		if (encparms.silent)
			tmp += sprintf(tmp, " --silent ");
		tmp += sprintf(tmp, "-q %d ", encparms.algqual);
		tmp += sprintf(tmp, "-V %d ", encparms.vbrqual);
		tmp += sprintf(tmp, "-B %d ", encparms.maxBitr);
		tmp += sprintf(tmp, "%s %s ", inname, outname);
		err = system(fbuf) != 0;
		break;

	case kFlacMode:
		/* --lax is needed to allow 11kHz, we dont need place for meta-tags, and no seektable */
		/* -f is reqired to force override of unremoved temp file. See bug #1294648 */
		tmp += sprintf(tmp, "flac -f --lax --no-padding --no-seektable --no-ogg " );

		if (rawInput) {
			tmp += sprintf(tmp, "--force-raw-format --sign=unsigned ");
			tmp += sprintf(tmp, "--channels=%d ", (rawAudioType.isStereo ? 2 : 1));
			tmp += sprintf(tmp, "--bps=%d ", rawAudioType.bitsPerSample);
			tmp += sprintf(tmp, "--sample-rate=%d ", rawSamplerate);
			tmp += sprintf(tmp, "--endian=%s ", (rawAudioType.isLittleEndian ? "little" : "big"));
		}

		for (i = 0; i < flacparms.numArgs; i++) {
			/* Append optional encoder arguments */
			tmp += sprintf(tmp, "%s ", flacparms.argv[i]);
		}

		tmp += sprintf(tmp, "-o %s ", outname);
		tmp += sprintf(tmp, "%s ", inname);

		err = system(fbuf) != 0;
		break;
	}

	if (err) {
		printf("Got error from encoder. (check your parameters)\n");
		printf("Encoder Commandline: %s\n", fbuf );
		exit(-1);
	}
} 

void extractAndEncodeWAV(const char *outName, FILE *input, CompressMode compMode) {
	int length;
	FILE *f;
	char fbuf[2048];
	size_t size;

	fseek(input, -4, SEEK_CUR);
	length = readUint32LE(input);
	length += 8;
	fseek(input, -8, SEEK_CUR);

	/* Copy the WAV data to a temporary file */
	f = fopen(outName, "wb");
	while (length > 0) {
		size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : length, input);
		if (size <= 0)
			break;
		length -= size;
		fwrite(fbuf, 1, size, f);
	}
	fclose(f);

	/* Convert the WAV temp file to OGG/MP3 */
	encodeAudio(outName, false, -1, tempEncoded, compMode);
}

void extractAndEncodeVOC(const char *outName, FILE *input, CompressMode compMode) {
	FILE *f;
	int blocktype;
	int length;
	int sample_rate;
	int comp;
	char fbuf[2048];
	size_t size;
	int real_samplerate = -1;

	f = fopen(outName, "wb");

	while ((blocktype = fgetc(input))) {
		if (blocktype != 1) {
			/*
			   We only generate a warning, instead of erroring out, because
			   at least the monster.sou file of Full Throttle contains VOCs
			   with an invalid length field (value to small). So we encounter
			   the "block types" 0x80, 0x82 etc.. Not sure if there is another
			   (maybe even better) way to work around that... ?
			 */
			warning("Unsupported VOC block type: %02x", blocktype);
			break;
		}
	
		/* Sound Data */
		printf(" Sound Data\n");
		length = fgetc(input);
		length |= fgetc(input) << 8;
		length |= fgetc(input) << 16;
		length -= 2;
		sample_rate = fgetc(input);
		comp = fgetc(input);

		real_samplerate = getSampleRateFromVOCRate(sample_rate);

		printf(" - length = %d\n", length);
		printf(" - sample rate = %d (%02x)\n", real_samplerate, sample_rate);
		printf(" - compression = %s (%02x)\n",
			   (comp ==	   0 ? "8bits"   :
				(comp ==   1 ? "4bits"   :
				 (comp ==  2 ? "2.6bits" :
				  (comp == 3 ? "2bits"   :
								"Multi")))), comp);

		if (comp != 0)
			error("Cannot handle compressed VOC data");

		/* Copy the raw data to a temporary file */
		while (length > 0) {
			size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : (uint32)length, input);
			if (size <= 0)
				break;
			length -= size;
			fwrite(fbuf, 1, size, f);
		}
	}

	fclose(f);
	
	assert(real_samplerate != -1);

	/* Convert the raw temp file to OGG/MP3 */
	encodeAudio(outName, true, real_samplerate, tempEncoded, compMode);
}

int process_mp3_parms(int argc, char *argv[], int i) {
	for (; i < argc; i++) {
		if (strcmp(argv[i], "--vbr") == 0) {
			encparms.abr=0;
		} else if (strcmp(argv[i], "--abr") == 0) {
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
			return 0;
		} else if (argv[i][0] == '-') {
			return 0;
		} else {
			break;
		}
	}
	if (i != (argc - 1)) {
		return 0;
	}
	return 1;
}

int process_ogg_parms(int argc, char *argv[], int i) {
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
			return 0;
		}
		else if (argv[i][0] == '-') {
			return 0;
		}
		else
			break;
	}
	if (i != argc - 1)
		return 0;
	return 1;
}

int process_flac_parms(int argc, char *argv[], int i){
	flacparms.argv = &argv[i];
	flacparms.numArgs = argc - 1 - i;

	if (i >= argc)
		return 0;
	return 1;
}
