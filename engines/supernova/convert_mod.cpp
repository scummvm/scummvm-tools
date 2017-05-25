#include <stdlib.h>
#include <stdio.h>
#include <common/endian.h>
#include <common/util.h>
#include "convert_mod.h"

ModReader::ModReader(const Common::Filename &fileName) {
	modFile.open(fileName, "rb");
	if (!modFile.isOpen()) {
		warning("Data file '%s' not found", fileName.getFullName().c_str());
		return;
	}

	modFile.read_throwsOnError(songName, 20);
	for (int i = 0 ; i < 31 ; ++i) {
		modFile.read_throwsOnError(instr[i].iname, 22);
		instr[i].length = modFile.readUint16BE();
		instr[i].finetune = (char)modFile.readChar();
		instr[i].volume = (char)modFile.readChar();
		instr[i].loopStart = modFile.readUint16BE();
		instr[i].loopLength = modFile.readUint16BE();
	}
	songLength = (char)modFile.readChar();
	ciaaSpeed = (char)modFile.readChar();
	modFile.read_throwsOnError(arrangement, 128);
	modFile.read_throwsOnError(mark, 4);

	char mark_str[0];
	strncpy(mark_str, mark, 4);
	mark_str[4] = 0;
	notice("Mark: '%s'", mark_str);

	uint32 l = modFile.size() - 1084;
	int nb_instr = 0;
	for (int i = 0 ; i < 31 ; ++i) {
		l -= (uint32)(instr[i].length) << 1;
		if (instr[i].length)
			++nb_instr;
	}

	modFile.seek(1084, SEEK_SET);
	patternNumber = l / 1024;
	for (int p = 0 ; p < patternNumber ; ++p) {
		for (int n = 0 ; n < 64 ; ++n) {
			for (int k = 0 ; k < 4 ; ++k) {
				note[p][n][k] = modFile.readSint32BE();
			}
		}
	}

	notice("\nName: %s\nInstruments: %d\nPatterns: %d\n", songName, nb_instr, (int)patternNumber);
}

ModReader::~ModReader() {
	if (modFile.isOpen())
		modFile.close();
}

bool ModReader::convertToMsn(const Common::Filename &fileName, int version) {
	if (!modFile.isOpen())
		return false;

	/* MSN-FORMAT */
	struct {
		uint16 seg;
		uint16 start;
		uint16 end;
		uint16 loopStart;
		uint16 loopEnd;
		char volume;
		char dummy[5];
	} instr2[22];
	int nbInstr2 = version == 1 ? 22 : 15;
	int32 note2[28][64][4];

	char convInstr[31];
	int i_new = 0;
	for (int i = 0 ; i < 31 ; ++i) {
		if (instr[i].length) {
			convInstr[i] = (char)i_new;
			++i_new;
		} else
			convInstr[i] = 31;
	}


	int pos = (version == 1 ? 484 : 372) + patternNumber * 1024;
	for (int i = 0; i < 31; ++i) {
		i_new = convInstr[i];
		if (i_new != 31) {
			if (instr[i].length > 30000)
				warning("\nInstrument too big");
			instr2[i_new].seg = pos >> 4;
			instr2[i_new].start = pos & 0x0F;
			instr2[i_new].end = (pos & 0x0F) + (instr[i].length << 1);
			instr2[i_new].loopStart = (pos & 0x0F) + (instr[i].loopStart << 1);
			instr2[i_new].loopEnd = instr2[i_new].loopStart + (instr[i].loopLength << 1);
			instr2[i_new].volume = instr[i].volume;
			notice("Nr: %d  Start: %d", i_new, pos);
			pos += instr[i].length << 1;
		}
	}

	const char convEff[16] = {0,1,2,3, 0,0,0,0, 0,0,4,0, 5,6,0,7};

	for (int p = 0; p < patternNumber; ++p) {
		for (int n = 0; n < 64; ++n) {
			for (int k = 0; k < 4; ++k) {
				int32* l = &(note2[p][n][k]);
				*l = note[p][n][k];
				int32 i = ((*l >> 12) & 0x0F) + ((*l >> 24) & 0xF0) - 1;
				i_new = 31;
				if (i != -1)
					i_new = convInstr[i];
				if (version == 1) {
					int32 e1 = (*l >> 8) & 0x0F;
					//int32 op = (*l & 255);
					int32 e = convEff[e1];
					*l &= 0x0fff00ff;
					*l |= (i_new << 11) | (e << 8);
				} else {
					if (i_new == 31)
						i_new = 15;
					int32 h = (*l >> 16) & 0x0fff;
					if (h)
						h = (0xe000 / h) - 256;
					*l &= 0x00000fff;
					*l |= (i_new << 12) | (h << 16);
				}
			}
		}
	}


	// Write file
	Common::File msnFile(fileName, "wb");
	if (!msnFile.isOpen()) {
		warning("Cannot create file '%s'", fileName.getFullName().c_str());
		return false;
	}

	for (int i = 0 ; i < nbInstr2 ; ++i) {
		msnFile.writeUint16LE(instr2[i].seg);
		msnFile.writeUint16LE(instr2[i].start);
		msnFile.writeUint16LE(instr2[i].end);
		msnFile.writeUint16LE(instr2[i].loopStart);
		msnFile.writeUint16LE(instr2[i].loopEnd);
		msnFile.writeChar(instr2[i].volume);
		msnFile.write(instr2[i].dummy, 5);
	}
	msnFile.writeUint16LE(*((uint16*)&songLength)); // writeSint16LE(songLength)
	msnFile.write(arrangement, 128);
	msnFile.writeUint16LE(*((uint16*)&patternNumber)); // writeSint16LE(patternNumber)
	for (int p = 0 ; p < patternNumber ; ++p) {
		for (int n = 0 ; n < 64 ; ++n) {
			for (int k = 0 ; k < 4 ; ++k) {
				msnFile.writeUint32LE(*((uint32*)(note2[p][n]+k))); // writeSInt32LE(note[p][n][k])
			}
		}
	}

	size_t nb;
	char buffer[4096];
	while ((nb = modFile.read_noThrow(buffer, 4096)) > 0)
		msnFile.write(buffer, nb);

	msnFile.close();

	return true;
}

MsnReader::MsnReader(const Common::Filename &fileName, int version) {
	nbInstr2 = version == 1 ? 22 : 15;

	msnFile.open(fileName, "rb");
	if (!msnFile.isOpen()) {
		warning("Data file '%s' not found", fileName.getFullName().c_str());
		return;
	}

	for (int i = 0 ; i < nbInstr2 ; ++i) {
		instr2[i].seg = msnFile.readUint16LE();
		instr2[i].start = msnFile.readUint16LE();
		instr2[i].end = msnFile.readUint16LE();
		instr2[i].loopStart = msnFile.readUint16LE();
		instr2[i].loopEnd = msnFile.readUint16LE();
		instr2[i].volume = msnFile.readChar();
		msnFile.read_throwsOnError(instr2[i].dummy, 5);
	}
	songLength = msnFile.readSint16LE();
	msnFile.read_throwsOnError(arrangement, 128);
	patternNumber = msnFile.readSint16LE();
	for (int p = 0 ; p < patternNumber ; ++p) {
		for (int n = 0 ; n < 64 ; ++n) {
			for (int k = 0 ; k < 4 ; ++k) {
				note2[p][n][k] = msnFile.readSint32LE();
			}
		}
	}
	notice("Song length: %d\nPatterns: %d\n", (int)songLength, (int)patternNumber);
}

MsnReader::~MsnReader() {
	if (msnFile.isOpen())
		msnFile.close();
}

bool MsnReader::convertToMod(const Common::Filename &fileName) {
	if (!msnFile.isOpen())
		return false;

	/* MOD format */
	struct {
		char iname[22];
		uint16 length;
		char finetune;
		char volume;
		uint16 loopStart;
		uint16 loopLength;
	} instr[31];
	int32 note[28][64][4];

	char convInstr[31];
	for (int i = 0 ; i < 31 ; ++i)
		convInstr[i] = 31;

	// We can't recover the position of 0 since it appears several times. So just assume it is the first one.
	const char invConvEff[8] = {0, 1, 2, 3, 10, 12, 13 ,15};

	// Can we recover the instruments mapping? I don't think so as part of the original instrument index is cleared.
	// Assume the mapping is 1<-> 1.
	for (int p = 0; p < patternNumber; ++p) {
		for (int n = 0; n < 64; ++n) {
			for (int k = 0; k < 4; ++k) {
				int32* l = &(note[p][n][k]);
				*l = note2[p][n][k];
				int32 i_new = 0;
				if (nbInstr2 == 22) { // version 1
					i_new = ((*l & 0xFF00) >> 11);
					int32 e = ((*l & 0x0700) >> 8);
					int32 e1 = invConvEff[e];
					*l &= 0x0fff00ff;
					*l |= (e1 << 8);
				} else { // version 2
					int32 h = (*l >> 16);
					i_new = ((*l & 0xFFFF) >> 12);
					*l &= 0x00000fff;
					if (h)
						h = 0xe000 / (h + 256);
					*l |= (h << 16);
					if (i_new == 15)
						i_new = 31;
				}

				if (i_new != 31) {
					// Get the part of the instrument that is present in the MSN file.
					int32 i = ((*l >> 24) & 0xF0) - 1;
					if (i < i_new) {
						// Write the missing par of the MOD instrument index
						*l |= ((i_new - i) << 12);
						i = i_new;
					}
					notice("MOD instrument: %d, MSN instrument: %d", (int)i, (int)i_new);
					convInstr[i] = i_new;
				}
			}
		}
	}

	int pos = (nbInstr2 == 22 ? 484 : 372) + patternNumber * 1024;
	for (int i = 0; i < 31; ++i) {
		// iname is not stored in the mod file. Just set it to 'instrument#'
		// finetune is not stored either. Assume 0.
		memset(instr[i].iname, 0, 22);
		sprintf(instr[i].iname, "instrument%d", i+1);
		instr[i].length = 0;
		instr[i].finetune = 0;
		instr[i].volume = 0;
		instr[i].loopStart = 0;
		instr[i].loopLength = 0;

		int i_new = convInstr[i];
		if (i_new != 31) {
			instr[i].length = ((instr2[i_new].end - (pos & 0x0F)) >> 1);
			instr[i].loopStart = ((instr2[i_new].loopStart - (pos & 0x0F)) >> 1);
			instr[i].loopLength = (( instr2[i_new].loopEnd - instr2[i_new].loopStart) >> 1);
			instr[i].volume = instr2[i].volume;
			pos += instr[i].length << 1;
		}
	}

	// The Restart byte for song looping cannot be recovered from MSN file.
	// Assume a value of 0.
	char ciaaSpeed = 0;

	// The mark cannot be recovered either.
	// It can be one of ID='M.K.', ID='4CHN',ID='6CHN',ID='8CHN', ID='4FLT',ID='8FLT'
	// Assume 'M.K.'
	const char mark[4] = { 'M', '.', 'K', '.' };

	// Write file
	Common::File modFile(fileName, "wb");
	if (!modFile.isOpen()) {
		warning("Cannot create file '%s'", fileName.getFullName().c_str());
		return false;
	}

	char songName[20];
	memset(songName, 0, 20);
	strncpy(songName, fileName.getFullName().c_str(), 19);
	modFile.write(songName, 20);

	for (int i = 0 ; i < 31 ; ++i) {
		modFile.write(instr[i].iname, 22);
		modFile.writeUint16BE(instr[i].length);
		modFile.writeChar(instr[i].finetune);
		modFile.writeChar(instr[i].volume);
		modFile.writeUint16BE(instr[i].loopStart);
		modFile.writeUint16BE(instr[i].loopLength);
	}
	modFile.writeChar((char)songLength);
	modFile.writeChar(ciaaSpeed);
	modFile.write(arrangement, 128);
	modFile.write(mark, 4);

	for (int p = 0 ; p < patternNumber ; ++p) {
		for (int n = 0 ; n < 64 ; ++n) {
			for (int k = 0 ; k < 4 ; ++k) {
				modFile.writeUint32BE(*((uint32*)(note[p][n]+k))); // writeSInt32BE(note[p][n][k])
			}
		}
	}

	size_t nb;
	char buffer[4096];
	while ((nb = msnFile.read_noThrow(buffer, 4096)) > 0)
		modFile.write(buffer, nb);

	modFile.close();

	return true;
}



int main(int argc, char *argv[]) {
	printf("What do you want to do?\n");
	printf("   1: convert HUMMEL1.MOD to MSN_DATA.052\n");
	printf("   2: convert SNTHEME2.MOD to MS2_DATA.056\n");
	printf("   3: convert MSN_DATA.052 to HUMMEL1.MOD\n");
	printf("   4: convert MS2_DATA.056 to SNTHEME2.MOD\n");

	int mode = -1;
	scanf("%d", &mode);
	if (mode < 1 || mode > 4)
		return -1;

	if (mode == 1) {
		ModReader reader("HUMMEL1.MOD");
		if (!reader.convertToMsn("MSN_DATA.052", 1))
			return 1;
	} else if (mode == 2) {
		ModReader reader("SNTHEME2.MOD");
		if (!reader.convertToMsn("MS2_DATA.056", 2))
			return 1;
	} else if (mode == 3) {
		MsnReader reader("MSN_DATA.052", 1);
		if (!reader.convertToMod("HUMMEL1.MOD"))
			return 1;
	} else if (mode == 4) {
		MsnReader reader("MS2_DATA.056", 2);
		if (!reader.convertToMod("SNTHEME2.MOD"))
			return 1;
	}

	return 0;
}
