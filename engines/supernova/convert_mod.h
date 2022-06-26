/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

#include <common/scummsys.h>
#include <common/file.h>


class ModReader {
public:
	ModReader(const Common::Filename&);
	~ModReader();

	bool convertToMsn(const Common::Filename& fileName, int version = 1);

private:
	Common::File modFile;

	char songName[20];         // liedname

	struct {
		char iname[22];
		uint16 length;          // laenge
		char finetune;
		char volume;            // lautst
		uint16 loopStart;       // wdh_start
		uint16 loopLength;      // wdh_laenge
	} instr[31];

	char songLength;            // liedlaenge
	char ciaaSpeed;
	char arrangement[128];
	char mark[4];               // markierung

	int16 patternNumber;        // patternzahl
	int32 note[28][64][4];
		// Each pattern is subdivided in 64 divisions, each one with 4 channels
};

class MsnReader {
public:
	MsnReader(const Common::Filename&, int version = 1);
	~MsnReader();

	bool convertToMod(const Common::Filename& fileName);

private:
	Common::File msnFile;

	struct {
		uint16 seg;
		uint16 start;
		uint16 end;
		uint16 loopStart;
		uint16 loopEnd;
		char volume;
		char dummy[5];
	} instr2[22];
	int nbInstr2; // 22 for version1, 15 for version 2

	int16 songLength;
	char arrangement[128];

	int16 patternNumber;
	int32 note2[28][64][4];
};