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
	uint32 silent;
} lameparams;

typedef struct {
	int nominalBitr;
	int minBitr;
	int maxBitr;
	int quality;
	int silent;
} oggencparams;


FILE *input, *output_idx, *output_snd;

lameparams encparms = { minBitrDef, maxBitrDef, false, algqualDef, vbrqualDef, 0 };
oggencparams oggparms = { -1, -1, -1, oggqualDef, 0 };

bool oggmode = 0;


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

void encodeAudio(const char *inname, bool rawInput, int rawSamplerate, const char *outname, bool oggOutput) {
	char fbuf[2048];
	char *tmp = fbuf;

	if (oggOutput) {
		tmp += sprintf(tmp, "oggenc ");
		if (rawInput) {
			tmp += sprintf(tmp, "--raw --raw-chan=1 --raw-bits=8 ");
			tmp += sprintf(tmp, "--raw-rate=%i ", rawSamplerate);
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
		system(fbuf);
	} else {
		tmp += sprintf(tmp, "lame -t -m m ");
		if (rawInput) {
			tmp += sprintf(tmp, "-r --bitwidth 8 ");
			tmp += sprintf(tmp, "-s %d ", rawSamplerate);
		}

		if (encparms.abr == 1)
			tmp += sprintf(tmp, "--abr %i ", encparms.minBitr);
		else
			tmp += sprintf(tmp, "--vbr-new -b %i ", encparms.minBitr);
		if (encparms.silent == 1)
			tmp += sprintf(tmp, " --silent ");
		tmp += sprintf(tmp, "-q %i ", encparms.algqual);
		tmp += sprintf(tmp, "-V %i ", encparms.vbrqual);
		tmp += sprintf(tmp, "-B %i ", encparms.maxBitr);
		tmp += sprintf(tmp, "%s %s ", inname, outname);
		system(fbuf);
	}
} 

void get_wav(void) {
	int length;
	FILE *f;
	char fbuf[2048];
	size_t size;

	fseek(input, -4, SEEK_CUR);
	length = readUint32LE(input);
	length += 8;
	fseek(input, -8, SEEK_CUR);

	/* Copy the WAV data to a temporary file */
	f = fopen(TEMP_WAV, "wb");
	while (length > 0) {
		size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : length, input);
		if (size <= 0)
			break;
		length -= size;
		fwrite(fbuf, 1, size, f);
	}
	fclose(f);

	/* Convert the WAV temp file to OGG/MP3 */
	encodeAudio(TEMP_WAV, false, -1, oggmode ? TEMP_OGG : TEMP_MP3, oggmode);
}

void get_voc(void)
{
	int blocktype;

	blocktype = fgetc(input);
	switch (blocktype) {
	case 0x01:{
		int length = 0;
		int i;
		int sample_rate;
		int comp;
		FILE *f;
		char fbuf[2048];
		size_t size;
		int real_samplerate;

		/* Sound Data */
		printf(" Sound Data\n");
		for (i = 0; i < 3; i++)
			length = length | (fgetc(input) << (i * 8));
		length -= 2;
		printf(" - length = %d\n", length);
		sample_rate = fgetc(input);
		comp = fgetc(input);

		real_samplerate = getSampleRateFromVOCRate(sample_rate);

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
		f = fopen(TEMP_RAW, "wb");
		while (length > 0) {
			size = fread(fbuf, 1, length > sizeof(fbuf) ? sizeof(fbuf) : (uint32)length, input);
			if (size <= 0)
				break;
			length -= size;
			fwrite(fbuf, 1, size, f);
		}
		fclose(f);

		/* Convert the raw temp file to OGG/MP3 */
		encodeAudio(TEMP_RAW, true, real_samplerate, oggmode ? TEMP_OGG : TEMP_MP3, oggmode);
		break;
	}

	default:
		error("Unknown chunk: %02x", blocktype);
		break;
	}
}

void process_mp3_parms(int argc, char *argv[], int i) {
	for(; i < argc; i++) {
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

