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

	uint32 l = modFile.size() - 1084;
	int nb_instr = 0;
	for (int i = 0 ; i < 31 ; ++i) {
		l -= (uint32)(instr[i].length) << 1;
		if (instr[i].length)
			++nb_instr;
	}

	patternNumber = l / 1024;
	for (int p = 0 ; p < patternNumber ; ++p) {
		for (int n = 0 ; n < 64 ; ++n) {
			for (int k = 0 ; k < 4 ; ++k) {
				note[p][n][k] = modFile.readSint32BE();
			}
		}
	}

	notice("\nName: %s\nSong length: %d\nInstruments: %d\nPatterns: %d\n", songName, (int)songLength, nb_instr, (int)patternNumber);
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

	// Initialize all instruments to 0. This is not really important but that way we get 0 instead
	// of garbage for the last 5 bytes (dummy) of each instruments and for the unused instruments.
	for (int i = 0 ; i < nbInstr2 ; ++i) {
		instr2[i].seg = 0;
		instr2[i].start = 0;
		instr2[i].end = 0;
		instr2[i].loopStart = 0;
		instr2[i].loopEnd = 0;
		instr2[i].volume = 0;
		memset(instr2[i].dummy, 0, 5);
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
			pos += instr[i].length << 1;
		}
	}

	// Table to convert MOD effect
	// Mod effect values:
	//   0: Arpeggio
	//   1: Slide up
	//   2: Slide down
	//   3: Slide to note
	//   4: Vibrato
	//   5: Continue 'Slide to note', but also do Volume slide
	//   6: Continue 'Vibrato', but also do Volume slide
	//   7: Tremolo
	//   8: (Set panning position) - often unused
	//   9: Set sample offset
	//  10: Volume slide
	//  11: Position Jump
	//  12: Set volume
	//  13: Pattern Break
	//  14: Misc (depends on the next 4 bits)
	//  15: Set speed
	//
	// MSN mapped values:
	//   1: Slide up
	//   2: Slide down
	//   3: Slide to note
	//   4: Volume slide
	//   5: Set volume
	//   6: Pattern Break
	//   7: Set speed
	//   0: Everything else
	const char convEff[16] = {0,1,2,3, 0,0,0,0, 0,0,4,0, 5,6,0,7};

	// For each note, the 4 bytes are:
	// 31 30 29 28 27 26 25 24 - 23 22 21 20 19 18 17 16 - 15 14 13 12 11 10 09 08 - 07 06 05 04 03 02 01 00
	//  h  h  h  h  g  g  g  g    f  f  f  f  e  e  e  e    d  d  d  d  c  c  c  c    b  b  b  b  a  a  a  a
	//
	// MOD:
	//  hhhh dddd        (8 bits) Sample index
	//  cccc             (4 bits) Effect type for this channel/division
	//  bbbb aaaa        (8 bits) Effect value
	//  gggg ffff eeee  (12 bits) Sample period
	//
	// MSN:
	//  hhhh             (4 bits) Cleared to 0
	//  dddd c           (5 bits) Sample index   | after mapping through convInstr
	//        ccc        (3 bits) Effect type    | after mapping through convEff
	//  bbbb aaaa        (8 bits) Effect value   | unmodified
	//  gggg ffff eeee  (12 bits) Sample period  | unmodified
	//
	// MS2:
	//  hhhh             (4 bits) Cleared to 0
	//  dddd             (4 bits) Sample index   | after mapping through convInstr
	//  cccc             (4 bits) Effect type    | unmodified
	//  bbbb aaaa        (8 bits) Effect value   | unmodified
	//  gggg ffff eeee  (12 bits) Sample period  | transformed (0xE000 / p) - 256


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
					*l &= 0x0FFF00FF;
					*l |= (i_new << 11) | (e << 8);
				} else {
					if (i_new == 31)
						i_new = 15;
					int32 h = (*l >> 16) & 0x0fff;
					if (h)
						h = (0xE000 / h) - 256;
					*l &= 0x00000FFF;
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
	int16 songLength2 = songLength;
	msnFile.writeUint16LE(*((uint16*)&songLength2)); // writeSint16LE(songLength)
	msnFile.write(arrangement, 128);
	msnFile.writeUint16LE(*((uint16*)&patternNumber)); // writeSint16LE(patternNumber)
	for (int p = 0 ; p < patternNumber ; ++p) {
		for (int n = 0 ; n < 64 ; ++n) {
			for (int k = 0 ; k < 4 ; ++k) {
				msnFile.writeUint32LE(*((uint32*)(note2[p][n]+k))); // writeSint32LE(note[p][n][k])
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

	// We can't recover some MOD effects since several of them are mapped to 0.
	// Assume the MSN effect of value 0 is Arpeggio (MOD effect of value 0).
	const char invConvEff[8] = {0, 1, 2, 3, 10, 12, 13 ,15};

	// Reminder from convertToMsn
	// 31 30 29 28 27 26 25 24 - 23 22 21 20 19 18 17 16 - 15 14 13 12 11 10 09 08 - 07 06 05 04 03 02 01 00
	//  h  h  h  h  g  g  g  g    f  f  f  f  e  e  e  e    d  d  d  d  c  c  c  c    b  b  b  b  a  a  a  a
	//
	// MSN:
	//  hhhh             (4 bits) Cleared to 0
	//  dddd c           (5 bits) Sample index   | after mapping through convInstr
	//        ccc        (3 bits) Effect type    | after mapping through convEff
	//  bbbb aaaa        (8 bits) Effect value   | unmodified
	//  gggg ffff eeee  (12 bits) Sample period  | unmodified
	//
	// MS2:
	//  hhhh             (4 bits) Cleared to 0
	//  dddd             (4 bits) Sample index   | after mapping through convInstr
	//  cccc             (4 bits) Effect type    | unmodified
	//  bbbb aaaa        (8 bits) Effect value   | unmodified
	//  gggg ffff eeee  (12 bits) Sample period  | transformed (0xE000 / p) - 256
	//
	// MOD:
	//  hhhh dddd        (8 bits) Sample index
	//  cccc             (4 bits) Effect type for this channel/division
	//  bbbb aaaa        (8 bits) Effect value
	//  gggg ffff eeee  (12 bits) Sample period

	// Can we recover the instruments mapping? I don't think so as part of the original instrument index is cleared.
	// And it doesn't really matter as long as we are consistent.
	// However we need to make sure 31 (or 15 in MS2) is mapped to 0 in MOD.
	// We just add 1 to all other values, and this means a 1 <-> 1 mapping for the instruments
	for (int p = 0; p < patternNumber; ++p) {
		for (int n = 0; n < 64; ++n) {
			for (int k = 0; k < 4; ++k) {
				int32* l = &(note[p][n][k]);
				*l = note2[p][n][k];
				int32 i = 0;
				if (nbInstr2 == 22) { // version 1
					i = ((*l & 0xF800) >> 11);
					int32 e = ((*l & 0x0700) >> 8);
					int32 e1 = invConvEff[e];
					*l &= 0x0FFF00FF;
					*l |= (e1 << 8);
				} else { // version 2
					int32 h = (*l >> 16);
					i = ((*l & 0xF000) >> 12);
					*l &= 0x00000FFF;
					if (h)
						h = 0xE000 / (h + 256);
					*l |= (h << 16);
					if (i == 15)
						i = 31;
				}

				// Add back index in note
				if (i != 31) {
					++i;
					*l |= ((i & 0x0F) << 12);
					*l |= ((i & 0xF0) << 24);
				}
			}
		}
	}

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

		if (i < nbInstr2) {
			instr[i].length = ((instr2[i].end - instr2[i].start) >> 1);
			instr[i].loopStart = ((instr2[i].loopStart - instr2[i].start) >> 1);
			instr[i].loopLength = (( instr2[i].loopEnd - instr2[i].loopStart) >> 1);
			instr[i].volume = instr2[i].volume;
		}
	}

	// The ciaaSpeed is kind of useless and not present in the MSN file.
	// Traditionally 0x78 in SoundTracker. Was used in NoiseTracker as a restart point.
	// ProTracker uses 0x7F. FastTracker uses it as a restart point, whereas ScreamTracker 3 uses 0x7F like ProTracker.
	// You can use this to roughly detect which tracker made a MOD, and detection gets more accurate for more obscure MOD types.
	char ciaaSpeed = 0x7F;

	// The mark cannot be recovered either. Since we have 4 channels and 31 instrument it can be either ID='M.K.' or ID='4CHN'.
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
				modFile.writeUint32BE(*((uint32*)(note[p][n]+k))); // writeSint32BE(note[p][n][k])
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
