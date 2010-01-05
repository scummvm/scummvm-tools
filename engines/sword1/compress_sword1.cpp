/* compress_sword1 - Compress Broken Sword I sound clusters into MP3/Ogg Vorbis
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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "compress_sword1.h"

#include "common/endian.h"

#define TOTAL_TUNES 269

typedef struct {
	char fileName[8];
	bool missing; /* Some of the music files seem to have been removed from the game
	                 Try and look for them, but don't warn if they are missing. */
} MusicFile;

MusicFile musicNames[TOTAL_TUNES] = {
	{ "1M2", false },
	{ "1M3", false },
	{ "1M4", false },
	{ "1M6", false },
	{ "1M7", false },
	{ "1M8", false },
	{ "1M9", false },
	{ "1M10", false },
	{ "1M11", false },
	{ "1M12", false },
	{ "1M13", false },
	{ "1M14", false },
	{ "1M15", false },
	{ "1M16", false },
	{ "1M17", false },
	{ "1M18", false },
	{ "1M19", false },
	{ "1M20", false },
	{ "1M21", false },
	{ "1M22", false },
	{ "1M23", false },
	{ "1M24", false },
	{ "1M25", false },
	{ "1M26", false },
	{ "1M27", false },
	{ "1M28", false },
	{ "1M29", false },
	{ "1M28A", false },
	{ "1M30", false },
	{ "1M31", false },
	{ "1M32", false },
	{ "1M34", false },
	{ "1M35", false },
	{ "2M1", false },
	{ "2M2", false },
	{ "2M4", false },
	{ "2M5", false },
	{ "2M6", false },
	{ "2M7", false },
	{ "2M8", false },
	{ "2M9", false },
	{ "2M10", false },
	{ "2M11", false },
	{ "2M12", false },
	{ "2M13", false },
	{ "2M14", false },
	{ "2M15", false },
	{ "2M16", false },
	{ "2M17", false },
	{ "2M18", false },
	{ "2M19", false },
	{ "2M20", false },
	{ "2M21", false },
	{ "2M22", false },
	{ "2M23", false },
	{ "2M24", false },
	{ "2M25", false },
	{ "2M26", false },
	{ "2M27", false },
	{ "2M28", false },
	{ "2M29", false },
	{ "2M30", false },
	{ "2M31", false },
	{ "2M32", false },
	{ "2M33", false },
	{ "1M28", false },
	{ "2M24", false },
	{ "2M6", false },
	{ "1M25", false },
	{ "2M38", false },
	{ "2M39", false },
	{ "2M40", false },
	{ "3M1", false },
	{ "3M2", false },
	{ "3M3", false },
	{ "3M4", false },
	{ "1M28", false },
	{ "2M26", false },
	{ "3M7", false },
	{ "3M8", false },
	{ "3M9", true },
	{ "3M10", false },
	{ "2M13", false },
	{ "3M12", false },
	{ "3M13", false },
	{ "3M14", false },
	{ "2M9", false },
	{ "3M17", false },
	{ "3M18", false },
	{ "3M19", false },
	{ "3M20", false },
	{ "3M21", false },
	{ "3M22", false },
	{ "3M24", false },
	{ "3M26", false },
	{ "3M27", false },
	{ "2M26", false },
	{ "3M29", false },
	{ "3M30", false },
	{ "3M32", false },
	{ "3M33", false },
	{ "2M13", false },
	{ "4M3", false },
	{ "4M4", false },
	{ "4M5", false },
	{ "4M6", false },
	{ "4M8", false },
	{ "4M9", false },
	{ "4M10", false },
	{ "4M11", false },
	{ "4M12", false },
	{ "4M13", false },
	{ "4M14", false },
	{ "4M15", false },
	{ "4M17", false },
	{ "4M18", false },
	{ "4M19", false },
	{ "4M20", false },
	{ "4M21", false },
	{ "4M22", false },
	{ "4M24", false },
	{ "4M25", false },
	{ "4M28", false },
	{ "4M29", false },
	{ "4M31", false },
	{ "4M32", false },
	{ "5M1", false },
	{ "5M2", true },
	{ "5M3", false },
	{ "5M4", false },
	{ "5M5", false },
	{ "5M6", false },
	{ "5M8", false },
	{ "5M9", false },
	{ "5M10", false },
	{ "5M11", false },
	{ "5M12", false },
	{ "5M13", false },
	{ "5M14", false },
	{ "5M17", false },
	{ "5M22", true },
	{ "5M23", false },
	{ "5M24", false },
	{ "2M3", false },
	{ "6M1", false },
	{ "6M2", false },
	{ "6M3", false },
	{ "6M4", false },
	{ "6M5", false },
	{ "6M6", false },
	{ "6M7", false },
	{ "6M8", false },
	{ "6M12", true },
	{ "2M6", false },
	{ "5M1", false },
	{ "6M15", false },
	{ "7M1", false },
	{ "7M2", false },
	{ "7M4", false },
	{ "7M5", false },
	{ "7M6", false },
	{ "7M7", false },
	{ "7M8", false },
	{ "7M11", false },
	{ "7M14", false },
	{ "7M15", false },
	{ "5M18", false },
	{ "6M11", false },
	{ "7M17", false },
	{ "7M18", false },
	{ "7M19", false },
	{ "7M20", false },
	{ "7M21", false },
	{ "7M22", false },
	{ "7M23", false },
	{ "7M28", false },
	{ "7M30", false },
	{ "7M31", false },
	{ "7M32", false },
	{ "7M33", false },
	{ "7M34", false },
	{ "8M1", false },
	{ "8M2", false },
	{ "8M4", false },
	{ "8M7", false },
	{ "8M10", false },
	{ "8M11", false },
	{ "8M12", false },
	{ "8M13", false },
	{ "8M14", false },
	{ "8M15", false },
	{ "8M16", false },
	{ "8M18", false },
	{ "8M19", false },
	{ "8M20", false },
	{ "8M21", false },
	{ "8M22", false },
	{ "8M24", false },
	{ "8M26", false },
	{ "8M28", false },
	{ "8M29", false },
	{ "8M30", false },
	{ "8M31", false },
	{ "8M37", true },
	{ "8M38", false },
	{ "8M39", false },
	{ "8M40", false },
	{ "8M41", false },
	{ "9M1", false },
	{ "9M2", false },
	{ "9M3", false },
	{ "9M5", false },
	{ "9M6", false },
	{ "9M7", false },
	{ "9M8", false },
	{ "9M9", false },
	{ "9M10", false },
	{ "9M11", false },
	{ "9M13", false },
	{ "9M14", false },
	{ "9M15", false },
	{ "9M17", false },
	{ "9M18", false },
	{ "9M19", false },
	{ "9M20", false },
	{ "9M21", false },
	{ "9M22", false },
	{ "9M23", false },
	{ "9M24", false },
	{ "9M25", false },
	{ "10M1", false },
	{ "10M2", false },
	{ "10M3", false },
	{ "10M4", false },
	{ "11M1", false },
	{ "11M3", false },
	{ "11M4", false },
	{ "11M7", false },
	{ "11M8", false },
	{ "11M9", true },
	{ "12M1", false },
	{ "11M2", false },
	{ "SPM2", false },
	{ "SPM3", false },
	{ "SPM4", false },
	{ "SPM5", false },
	{ "SPM6", false },
	{ "SCM1", false },
	{ "SCM2", false },
	{ "SCM3", false },
	{ "SCM4", false },
	{ "SCM5", false },
	{ "SCM6", false },
	{ "SCM7", false },
	{ "SCM8", false },
	{ "SCM11", false },
	{ "RM3A", false },
	{ "RM3B", false },
	{ "SCM16", false },
	{ "SCM1B", false },
	{ "SPM6B", false },
	{ "MARQUET", false },
	{ "RM4", false },
	{ "RM5", false },
	{ "RM6", false },
	{ "RM7", false },
	{ "RM8", false },
	{ "RM3C", false },
	{ "RM3D", false }
};

int16 *CompressSword1::uncompressSpeech(Common::File &clu, uint32 idx, uint32 cSize, uint32 *returnSize) {
	uint32 resSize, srcPos;
	int16 *srcData, *dstData, *dstPos;
	uint32 headerPos = 0;
	int16 length, cnt;
	uint8 *fBuf = (uint8 *)malloc(cSize);
	clu.seek(idx, SEEK_SET);
	clu.read_throwsOnError(fBuf, cSize);

	while ((READ_BE_UINT32(fBuf + headerPos) != 'data') && (headerPos < 100))
		headerPos++;
	if (headerPos < 100) {
		resSize = READ_LE_UINT32(fBuf + headerPos + 4) >> 1;
		srcData = (int16 *)(fBuf + headerPos + 8);
		dstData = (int16 *)malloc(resSize * 2);
		srcPos = 0;
		dstPos = dstData;
		cSize = (cSize - (headerPos + 8)) / 2;
		while (srcPos < cSize) {
			length = (int16)READ_LE_UINT16(srcData + srcPos);
			srcPos++;
			if (length < 0) {
				length = -length;
				for (cnt = 0; cnt < (uint16)length; cnt++)
					*dstPos++ = srcData[srcPos];
				srcPos++;
			} else {
				memcpy(dstPos, srcData + srcPos, length * 2);
				dstPos += length;
				srcPos += length;
			}
		}
		free(fBuf);
		*returnSize = resSize * 2;
		if (_speechEndianness == UnknownEndian)
			guessEndianness(dstData, length);
		return dstData;
	} else {
		free(fBuf);
		error("Sound::uncompressSpeech(): DATA tag not found in wave header");
		*returnSize = 0;
		return NULL;
	}
}

void CompressSword1::guessEndianness(int16 *data, int16 length) {
	// Compute average of difference between two consecutive samples for both the given
	// data array and the byte swapped array.
	double bs_diff_sum = 0., diff_sum = 0.;
	if (length > 2000)
		length = 2000;

	int16 prev_bs_value = (int16)SWAP_16(*((uint16*)data));
	for (int16 i = 1 ; i < length ; ++i) {
		diff_sum += data[i] > data[i-1] ? data[i] - data[i-1] : data[i-1] - data[i];
		int16 bs_value = (int16)SWAP_16(*((uint16*)(data + i)));
		bs_diff_sum += bs_value > prev_bs_value ? bs_value - prev_bs_value : prev_bs_value - bs_value;
		prev_bs_value = bs_value;
	}
	// Set the little/big endian flags
	if (diff_sum < bs_diff_sum)
		_speechEndianness = LittleEndian;
	else
		_speechEndianness = BigEndian;
	setRawAudioType(_speechEndianness == LittleEndian, false, 16);
}

uint8 *CompressSword1::convertData(uint8 *rawData, uint32 rawSize, uint32 *resSize) {
	uint8 *resBuf;

	uint32 size;
	Common::File temp(TEMP_RAW, "wb");
	size = temp.write(rawData, rawSize);
	assert(size == rawSize);
	temp.close();
	encodeAudio(TEMP_RAW, true, 11025, _audioOuputFilename.c_str(), _format);
	temp.open(_audioOuputFilename, "rb");
	temp.seek(0, SEEK_END);
	*resSize = size = temp.pos();
	resBuf = (uint8*)malloc(size);
	temp.seek(0, SEEK_SET);
	temp.read_throwsOnError(resBuf, size);
	return resBuf;
}

void CompressSword1::convertClu(Common::File &clu, Common::File &cl3) {
	uint32 *cowHeader;
	uint32 numRooms;
	uint32 numSamples;
	uint32 cnt;
	uint32 *cl3Index, *sampleIndex;
	uint32 smpSize, mp3Size;
	uint8 *smpData, *mp3Data;

	uint32 headerSize = clu.readUint32LE();

	assert(!(headerSize & 3));
	cowHeader = (uint32*)malloc(headerSize);

	for (cnt = 0; cnt < (headerSize / 4) - 1; cnt++)
		cowHeader[cnt] = clu.readUint32LE();
	assert(!(cowHeader[0] & 3));
	numRooms = cowHeader[0] / 4;
	assert(cowHeader[numRooms] == 0);	/* This dword should be unused. */

	/* The samples are divided into rooms and samples. We don't care about the room indexes at all. */
	/* We simply copy them and go to the sample-index data. */
	cl3.writeUint32LE(headerSize);
	for (cnt = 0; cnt < numRooms; cnt++)
		cl3.writeUint32LE(cowHeader[cnt]);
	cl3.writeUint32LE(cowHeader[numRooms]);

	numSamples = (((headerSize / 4) - numRooms) / 2) - 1;
	for (cnt = 0; cnt < numSamples * 2; cnt++) {
		/* This is where we'll put the sample index data later. */
		cl3.writeUint32BE(0xdeadbeefL);
	}
	cl3Index = (uint32*)malloc(numSamples * 8);
	memset(cl3Index, 0, numSamples * 8);

	sampleIndex = cowHeader + numRooms + 1;
	/* This points to the sample index table. 8 bytes each (4 bytes size and then 4 bytes file index) */

	print("converting %d samples\n", numSamples);

	for (cnt = 0; cnt < numSamples; cnt++) {
		if (sampleIndex[cnt << 1] | sampleIndex[(cnt << 1) | 1]) {
			print("sample %5d: \n", cnt);
			smpData = (uint8*)uncompressSpeech(clu, sampleIndex[cnt << 1] + headerSize, sampleIndex[(cnt << 1) | 1], &smpSize);
			if ((!smpData) || (!smpSize))
				error("unable to handle speech sample %d!\n", cnt);

			mp3Data = convertData(smpData, smpSize, &mp3Size);
			cl3Index[cnt << 1] = cl3.pos();
			cl3Index[(cnt << 1) | 1] = mp3Size;
			cl3.write(mp3Data, mp3Size);

			free(smpData);
			free(mp3Data);
		} else {
			cl3Index[cnt << 1] = cl3Index[(cnt << 1) | 1] = 0;
			print("sample %5d: skipped\n", cnt);
		}
	}
	cl3.seek((numRooms + 2) * 4, SEEK_SET);	/* Now write the sample index into the CL3 file */
	for (cnt = 0; cnt < numSamples * 2; cnt++)
		cl3.writeUint32LE(cl3Index[cnt]);
	free(cl3Index);
	free(cowHeader);
}

void CompressSword1::compressSpeech(const Common::Filename *inpath, const Common::Filename *outpath) {
	Common::File clu, cl3;
	int i;
	char cluName[256], outName[256];

	if (_speechEndianness != UnknownEndian)
		setRawAudioType(_speechEndianness == LittleEndian, false, 16);

	for (i = 1; i <= 2; i++) {
		// Updates the progress bar, add music files if we compress those too
		updateProgress(i, 2 +(_compMusic? TOTAL_TUNES : 0));

		sprintf(cluName, "%s/SPEECH/SPEECH%d.CLU", inpath->getPath().c_str(), i);
		try {
			clu.open(cluName, "rb");
		} catch (Common::FileException &) {
			print("Unable to open \"SPEECH%d.CLU\".\n", i);
			print("Please copy the \"SPEECH.CLU\" from CD %d\nand rename it to \"SPEECH%d.CLU\".\n", i, i);
			continue;
		}

		switch (_format) {
		case AUDIO_MP3:
			sprintf(outName, "%s/SPEECH/SPEECH%d.%s", outpath->getPath().c_str(), i, "CL3");
			break;
		case AUDIO_VORBIS:
			sprintf(outName, "%s/SPEECH/SPEECH%d.%s", outpath->getPath().c_str(), i, "CLV");
			break;
		case AUDIO_FLAC:
			sprintf(outName, "%s/SPEECH/SPEECH%d.%s", outpath->getPath().c_str(), i, "CLF");
			break;
		default:
			error("Unknown encoding method");
		}

		cl3.open(outName, "wb");
		if (!cl3.isOpen()) {
			print("Unable to create file \"%s\".\n", outName);
			print("Please make sure you've got write permission in this directory.\n");
		} else {
			print("Converting CD %d...\n", i);
			convertClu(clu, cl3);
		}
	}
	unlink(TEMP_RAW);
	unlink(_audioOuputFilename.c_str());
}

void CompressSword1::compressMusic(const Common::Filename *inpath, const Common::Filename *outpath) {
	int i;
	char fNameIn[256], fNameOut[256];

	for (i = 0; i < TOTAL_TUNES; i++) {
		// Update the progress bar, we add 2 if we compress speech to, for those files
		updateProgress(i, TOTAL_TUNES +(_compSpeech? 2 : 0));

		if (!_macVersion)
			sprintf(fNameIn, "%s/MUSIC/%s.WAV", inpath->getPath().c_str(), musicNames[i].fileName);
		else
			sprintf(fNameIn, "%s/MUSIC/%s.AIF", inpath->getPath().c_str(), musicNames[i].fileName);
		try {
			Common::File inf(fNameIn, "rb");

			switch (_format) {
			case AUDIO_MP3:
				sprintf(fNameOut, "%s/MUSIC/%s.%s", outpath->getPath().c_str(), musicNames[i].fileName, "MP3");
				break;
			case AUDIO_VORBIS:
				sprintf(fNameOut, "%s/MUSIC/%s.%s", outpath->getPath().c_str(), musicNames[i].fileName, "OGG");
				break;
			case AUDIO_FLAC:
				sprintf(fNameOut, "%s/MUSIC/%s.%s", outpath->getPath().c_str(), musicNames[i].fileName, "FLA");
				break;
			default:
				error("Unknown encoding method");
			}

			print("encoding file (%3d/%d) %s -> %s\n", i + 1, TOTAL_TUNES, musicNames[i].fileName, fNameOut);
			if (!_macVersion)
				encodeAudio(fNameIn, false, -1, fNameOut, _format);
			else
				extractAndEncodeAIFF(fNameIn, fNameOut, _format);
		} catch (Common::FileException& err) {
			print(err.what());
		}
	}
}

void CompressSword1::checkFilesExist(bool checkSpeech, bool checkMusic, const Common::Filename *inpath) {
	int i;
	FILE *testFile;
	char fileName[256];
	bool speechFound = false, musicFound = false;

	if (checkSpeech) {
		for (i = 1; i <= 2; i++) {
			sprintf(fileName, "%s/SPEECH/SPEECH%d.CLU", inpath->getPath().c_str(), i);
			testFile = fopen(fileName, "rb");

			if (testFile){
				speechFound = true;
				fclose(testFile);
			}
		}

		if (!speechFound) {
			print("Unable to find speech files.\n");
			print("Please copy the SPEECH.CLU files from Broken Sword CD1 and CD2\n");
			print("into the \"SPEECH\" subdirectory and rename them to\n");
			print("SPEECH1.CLU and SPEECH2.CLU\n\n");
			print("If your OS is case-sensitive, make sure the filenames\n");
			print("and directorynames are all upper-case.\n\n");
		}
	}

	/* The PC Version uses LittleEndian speech files.
	 The Mac version can be either with little endian speech files or big endian speech files.
	 We detect if we have a PC or Mac version with the music files (WAV for PC and AIF for Mac).
	 If we have the Mac version or if we don't check the music files, an heuristic will be used
	 when first accessing the speech file to detect if it is little endian or big endian */
	 
	if (checkMusic) {
		for (i = 0; i < 20; i++) { /* Check the first 20 music files */
			// Check WAV file
			sprintf(fileName, "%s/MUSIC/%s.WAV", inpath->getPath().c_str(), musicNames[i].fileName);
			testFile = fopen(fileName, "rb");

			if (testFile) {
				musicFound = true;
				fclose(testFile);
				break;
			}
			
			// Check AIF file
			sprintf(fileName, "%s/MUSIC/%s.AIF", inpath->getPath().c_str(), musicNames[i].fileName);
			testFile = fopen(fileName, "rb");
			
			if (testFile) {
				musicFound = true;
				_macVersion = true;
				_speechEndianness = UnknownEndian;
				fclose(testFile);
				break;
			}
		}

		if (!musicFound) {
			print("Unable to find music files.\n");
			print("Please copy the music files from Broken Sword CD1 and CD2\n");
			print("into the \"MUSIC\" subdirectory.\n");
			print("If your OS is case-sensitive, make sure the filenames\n");
			print("and directorynames are all upper-case.\n");
		}
	} else {
		_speechEndianness = UnknownEndian;
	}

	if ((checkSpeech && (!speechFound)) || (checkMusic && (!musicFound))) {
		throw ToolException("Use --help for more information");
	}
}

InspectionMatch CompressSword1::inspectInput(const Common::Filename &filename) {
	// Wildcard matching as implemented in Tools is too restrictive (e.g. it doesn't
	// work with *.cl? or even *.cl*).
	// This is the reason why this function is reimplemented there.
	if (
		scumm_stricmp(filename.getExtension().c_str(), "clu") == 0 ||
		scumm_stricmp(filename.getExtension().c_str(), "clm") == 0
	)
		return IMATCH_PERFECT;
	return IMATCH_AWFUL;
}

CompressSword1::CompressSword1(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_compSpeech = true;
	_compMusic = true;
	_macVersion = false;
	_speechEndianness = LittleEndian;

	_supportsProgressBar = true;

	ToolInput input;
	input.format = "*.*";
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress the Broken Sword 1 data files.";
	_helptext = "\nUsage: " + getName() + " [mode] [mode params] [-o outputdir] [only] <inputdir>\n"
		"only can be either:\n"
		" --speech-only  only encode speech clusters\n"
		" --music-only   only encode music files\n";
}

void CompressSword1::parseExtraArguments() {
	if (!_arguments.empty() && _arguments.front() == "--speech-only") {
		_compMusic = false;
		_arguments.pop_front();
	}
	if (!_arguments.empty() && _arguments.front() == "--music-only") {
		_compSpeech = false;
		_arguments.pop_front();
	}
}

void CompressSword1::execute() {
	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename &outpath = _outputPath;

	switch (_format) {
	case AUDIO_MP3:
		_audioOuputFilename = TEMP_MP3;
		break;
	case AUDIO_VORBIS:
		_audioOuputFilename = TEMP_OGG;
		break;
	case AUDIO_FLAC:
		_audioOuputFilename = TEMP_FLAC;
		break;
	default:
		throw ToolException("Unknown audio format");
		break;
	}

	if (outpath.empty())
		// Extensions change between the in/out files, so we can use the same directory
		outpath = inpath;

	/* Do a quick check to see if we can open any files at all */
	checkFilesExist(_compSpeech, _compMusic, &inpath);

	if (_compSpeech)
		compressSpeech(&inpath, &outpath);
	if (_compMusic)
		compressMusic(&inpath, &outpath);
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressSword1 sword1(argv[0]);
	return sword1.run(argc, argv);
}
#endif

