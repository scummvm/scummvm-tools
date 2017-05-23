#include <stdlib.h>
#include <stdio.h>
#include <common/endian.h>
#include <common/util.h>
#include "convert_mod.h"

ModReader::ModReader(const Common::Filename& fileName) {
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

	uint32 l = modFile.size() - 1084;
	char i_new = 0;
	for (int i = 0 ; i < 31 ; ++i) {
		l -= (uint32)(instr[i].length) << 1;
		if (instr[i].length) {
			convInstr[i] = i_new;
			++i_new;
		} else
			convInstr[i] = 31;
	}

	modFile.seek(1084L, SEEK_SET);
	patternNumber = l / 1024;
	for (int p = 0 ; p < patternNumber ; ++p) {
		for (int n = 0 ; n < 64 ; ++n) {
			for (int k = 0 ; k < 4 ; ++k) {
				note[p][n][k] = modFile.readSint32BE();
			}
		}
	}

	notice("\nName: %s\nInstruments: %d\nPatterns: %d\n", songName, (int)i_new, patternNumber);
}

ModReader::~ModReader() {
	if (modFile.isOpen())
		modFile.close();
}

bool ModReader::convertToMsn(const Common::Filename& fileName, int version) {
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

	int pos = (version == 1 ? 484 : 372) + patternNumber * 1024;
	for (int i = 0; i < 31; ++i) {
		int i_new = convInstr[i];
		if (i_new != 31) {
			if (instr[i].length > 30000)
				warning("\nInstrument too big");
			instr2[i_new].seg = pos >> 4;
			instr2[i_new].start = pos & 15;
			instr2[i_new].end = (pos & 15) + (instr[i].length << 1);
			instr2[i_new].loopStart = (pos & 15) + (instr[i].loopStart << 1);
			instr2[i_new].loopEnd = (pos & 15) + (instr[i].loopStart << 1) + (instr[i].loopLength << 1);
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
				int32 i = ((*l >> 12) & 0x0FL) + ((*l >> 24) & 0xF0L) - 1;
				if (i == -1)
					i = 31;
				else
					i = convInstr[i];
				int32 h = (*l >> 16) & 0x0fff;
				if (h)
					h = (0xe000L / h) - 256;
				if (version == 1) {
					int32 e1 = (*l >> 8) & 15;
					//int32 op = (*l & 255);
					int32 e = convEff[e1];
					*l &= 0x0fff00ffL;
					*l |= (i << 11) | (e << 8);  // | (h << 16);
				} else {
					if (i == 31)
						i = 15;
					*l &= 0x00000fffL;
					*l |= (i << 12) | (h << 16);
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
	msnFile.writeUint16LE(songLength);
	msnFile.write(arrangement, 128);
	msnFile.writeUint16LE(patternNumber);
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


int main(int argc, char *argv[]) {
	printf("What do you want to do?\n");
	printf("   1: convert HUMMEL1.MOD to MSN_DATA.052\n");
	printf("   2: convert SNTHEME2.MOD to MS2_DATA.056\n");
//	printf("   3: convert MSN_DATA.052 to HUMMEL1.MOD\n");
//	printf("   4: convert MS2_DATA.056 to SNTHEME2.MOD\n");

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
		// TODO

	} else if (mode == 4) {
		// TODO

	}

	return 0;
}
