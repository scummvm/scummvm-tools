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
		warning("inexact sample rate used: %i (0x%x)", sr, vocSR);
		return sr;
	}
}

// todo: check rawAudioType for Flac encoding
void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, CompressMode compmode) {
	char fbuf[2048];
	char *tmp = fbuf;
	int i;
	bool err = false;

	switch (compmode) {
	case kVorbisMode:
		tmp += sprintf(tmp, "oggenc ");
		if (rawInput) {
			tmp += sprintf(tmp, "--raw --raw-chan=%d --raw-bits=%d ", (rawAudioType.isStereo ? 2 : 1), rawAudioType.bitsPerSample);
			tmp += sprintf(tmp, "--raw-rate=%i ", rawSamplerate);
			tmp += sprintf(tmp, "--raw-endianness=%d ", (rawAudioType.isLittleEndian ? 0 : 1));
		}

		if (oggparms.nominalBitr != -1)
			tmp += sprintf(tmp, "--bitrate=%i ", oggparms.nominalBitr);
		if (oggparms.minBitr != -1)
			tmp += sprintf(tmp, "--min-bitrate=%i ", oggparms.minBitr);
		if (oggparms.maxBitr != -1)
			tmp += sprintf(tmp, "--max-bitrate=%i ", oggparms.maxBitr);
		if (oggparms.silent)
			tmp += sprintf(tmp, "--quiet ");
		tmp += sprintf(tmp, "--quality=%i ", oggparms.quality);
		tmp += sprintf(tmp, "--output=%s ", outname);
		tmp += sprintf(tmp, "%s ", inname);
		err = system(fbuf) != 0;
		break;

	case kMP3Mode:
		tmp += sprintf(tmp, "lame -t -m m ");
		if (rawInput) {
			tmp += sprintf(tmp, "-r ");
			tmp += sprintf(tmp, "--bitwidth %d ", rawAudioType.bitsPerSample);
			if (rawAudioType.isLittleEndian)
				tmp += sprintf(tmp, "-x ");
			tmp += sprintf(tmp, "-s %d ", rawSamplerate);
		}

		if (encparms.abr)
			tmp += sprintf(tmp, "--abr %i ", encparms.minBitr);
		else
			tmp += sprintf(tmp, "--vbr-new -b %i ", encparms.minBitr);
		if (encparms.silent)
			tmp += sprintf(tmp, " --silent ");
		tmp += sprintf(tmp, "-q %i ", encparms.algqual);
		tmp += sprintf(tmp, "-V %i ", encparms.vbrqual);
		tmp += sprintf(tmp, "-B %i ", encparms.maxBitr);
		tmp += sprintf(tmp, "%s %s ", inname, outname);
		err = system(fbuf) != 0;
		break;

	case kFlacMode:
		/* --lax is needed to allow 11kHz, we dont need place for meta-tags, and no seektable */
		tmp += sprintf(tmp, "flac --lax --no-padding --no-seektable --no-ogg " );

		if (rawInput) {
			tmp += sprintf(tmp, "--force-raw-format --endian=little --sign=unsigned ");
			tmp += sprintf(tmp, "--bps=8 --channels=1 --sample-rate=%d ", rawSamplerate );
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

void process_mp3_parms(int argc, char *argv[], int i) {
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

void process_flac_parms(int argc, char *argv[], int i){
	flacparms.argv = &argv[i];
	flacparms.numArgs = argc - 1 - i;

	if (i >= argc)
		showhelp(argv[0]);
}

