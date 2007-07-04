/* compress_kyra_bun - compressor for kyra sound file packages
 * Copyright (C) 2006  The ScummVM Team
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

#include "compress.h"
#include "kyra_pak.h"

static void showhelp(const char *exename);
static void process(const char *infile, const char *output);

#define OUTPUT_MP3 ".VO3"
#define OUTPUT_OGG ".VOG"
#define OUTPUT_FLAC ".VOF"

#define TEMPFILE "TEMP.VOC"

const char *outputExt = 0;
static CompressMode gCompMode = kMP3Mode;

int main(int argc, char *argv[]) {
	if (argc < 3)
		showhelp(argv[0]);

	char inputFile[256];
	char outputFile[256];
	int i = 0;

	/* Compression mode */
	gCompMode = kMP3Mode;
	i = 1;

	if (strcmp(argv[1], "--mp3") == 0) {
		gCompMode = kMP3Mode;
		i++;
	}
	else if (strcmp(argv[1], "--vorbis") == 0) {
		gCompMode = kVorbisMode;
		i++;
	}
	else if (strcmp(argv[1], "--flac") == 0) {
		gCompMode = kFlacMode;
		i++;
	}

	switch (gCompMode) {
	case kMP3Mode:
		outputExt = OUTPUT_MP3;
		tempEncoded = TEMP_MP3;
		if (!process_mp3_parms(argc - 2, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		outputExt = OUTPUT_OGG;
		tempEncoded = TEMP_OGG;
		if (!process_ogg_parms(argc - 2, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		outputExt = OUTPUT_FLAC;
		tempEncoded = TEMP_FLAC;
		if (!process_flac_parms(argc - 2, argv, i))
			showhelp(argv[0]);
		break;
	}

	sprintf(inputFile, "%s/%s", argv[argc - 2], argv[argc - 3]);
	sprintf(outputFile, "%s/%s", argv[argc - 1], argv[argc - 3]);

	if (scumm_stricmp(inputFile, outputFile) == 0)
		error("infile and outfile are the same file");
	process(inputFile, outputFile);
	return 0;
}

static void showhelp(const char *exename) {
	printf("\nUsage: %s [params] <inputfile> <inputdir> <outputdir>\n", exename);

	printf("\nParams:\n");
	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
	printf(" --flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

	printf("\nMP3 mode params:\n");
	printf(" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf(" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf(" --vbr        LAME uses the VBR mode (default)\n");
	printf(" --abr        LAME uses the ABR mode\n");
	printf(" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf(" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf(" --silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf(" -b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf(" -m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf(" -M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf(" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf(" --silent     the output of oggenc is hidden (default:disabled)\n");

	printf("\nFlac mode params:\n");
 	printf(" --fast       FLAC uses compresion level 0\n");
 	printf(" --best       FLAC uses compresion level 8\n");
 	printf(" -<value>     specifies the value (0 - 8) of compresion (8=best)(default:%d)\n", flacCompressDef);
 	printf(" -b <value>   specifies a blocksize of <value> samples (default:%d)\n", flacBlocksizeDef);
	printf(" --verify     files are encoded and then decoded to check accuracy\n");
	printf(" --silent     the output of FLAC is hidden (default:disabled)\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");

	exit(2);
}

static bool hasSuffix(const char *str, const char *suf) {
	const int sufSize = strlen(suf);
	int off = strlen(str);
	if (off < sufSize)
		return false;
	off -= sufSize;
	printf("'%s'\n", &str[off]);
	return (scumm_stricmp(&str[off], suf) == 0);
}

static void process(const char *infile, const char *outfile) {
	PAKFile input, output;
	if (!input.loadFile(infile, false))
		return;
	if (!output.loadFile(0, false))
		return;

	PAKFile::cFileList *list = input.getFileList();
	char outputName[32];
	for (; list; list = list->next) {
		if (!hasSuffix(list->filename, ".VOC"))
			continue;

		if (list->data[26] != 1) {
			warning("broken VOC file '%s' skipping it...", list->filename);
			continue;
		}

		input.outputFileAs(list->filename, TEMPFILE);
		strncpy(outputName, list->filename, 32);

		FILE *tempFile = fopen(TEMPFILE, "rb");
		fseek(tempFile, 26, SEEK_CUR);
		extractAndEncodeVOC(TEMP_RAW, tempFile, gCompMode);
		fclose(tempFile);

		char *vocStart = strstr(outputName, ".VOC");
		for (unsigned int i = 0; i < strlen(outputExt); ++i)
			vocStart[i] = outputExt[i];
		output.addFile(outputName, tempEncoded);

		unlink(TEMPFILE);
		unlink(TEMP_RAW);
		unlink(tempEncoded);
	}

	if (output.getFileList()) {
		output.saveFile(outfile);
	} else {
		printf("file '%s' doesn't contain any .voc files\n", infile);
	}
}
