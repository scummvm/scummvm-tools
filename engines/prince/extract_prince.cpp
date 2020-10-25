/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "extract_prince.h"
#include "utils.h"
#include "common/endian.h"
#include "common/str.h"

struct FileData;

ExtractPrince::ExtractPrince(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "/";
	input.file = false;
	_inputPaths.push_back(input);

	_outputToDirectory = true;

	_shorthelp = "Used to extract The Prince and the Coward text data for translation.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] <inputdir>\n";
}

void ExtractPrince::execute() {
	const int kNumberOfLocations = 61;

	Common::Filename mainDir = _inputPaths[0].path;
	// We always need to setup default output path, since there is no obligation to specify it
	if (_outputPath.empty()) {
		_outputPath.setFullPath("./");
	}

	char pathBuffer[100];
	print("Unpacking The Prince and the Coward text data... ");

	std::string databankFullName = mainDir.getFullPath();
	sprintf(pathBuffer, "all/databank.ptc");
	databankFullName += pathBuffer;
	Common::Filename filename(databankFullName);
	// Trying to access the DATABANK files
	// Here we check if the File all/databank.ptc exists.
	// If so, we load it and extract the data from the databanks.
	//
	// If the databank.ptc file is not found, we try to access the
	// uncompressed files directly.
	if (Common::Filename(databankFullName).exists()) {
		_databank = new Databank(databankFullName);
		print("DATABANK.PTC loaded, processing... ");
		bool mobsDE = false;

		exportVariaTxt(_databank->loadFile("variatxt.dat"));
		exportInvTxt(_databank->loadFile("invtxt.dat"));
		exportTalkTxt(_databank->loadFile("talktxt.dat"));
		exportCredits(_databank->loadFile("credits.dat"));

		int index = _databank->getFileIndex("mob01.lst");

		if (index != -1) {
			// For DE game data
			mobsDE = true;
			_outputPath.setFullName("mob.txt");
			_fFiles.open(_outputPath, "w");
			if (!_fFiles.isOpen()) {
				error("Unable to create mob.txt");
			}
			_fFiles.print("mob.lst\nmob_name - exam text\n");
			int loc = 0;
			for (int j = 1; j <= kNumberOfLocations; j++) {
				_fFiles.print("%d.\n", j);
				// no databanks for loc 44 and 45
				if (j != 44 && j != 45) {
					exportMobs(_databank->loadFile(index + loc));
					loc++;
				}
			}
			print("mob.txt - done");
			print("All done!");
			_fFiles.close();
		}

		delete _databank;

		// For PL game data
		if (!mobsDE) {
			_outputPath.setFullName("mob.txt");
			_fFiles.open(_outputPath, "w");
			if (!_fFiles.isOpen()) {
				error("Unable to create mob.txt");
			}
			_fFiles.print("mob.lst\nmob_name - exam text\n");
			for (int loc = 1; loc <= kNumberOfLocations; loc++) {
				_fFiles.print("%d.\n", loc);
				// no databanks for loc 44 and 45
				if (loc != 44 && loc != 45) {
					databankFullName = mainDir.getFullPath();
					sprintf(pathBuffer, "%02d/databank.ptc", loc);
					databankFullName += pathBuffer;
					filename = Common::Filename(databankFullName);
					_databank = new Databank(databankFullName);
					if (!_databank->isOpen()) {
						_fFiles.close();
						error("Unable to open %02d/databank.ptc", loc);
					}
					exportMobs(_databank->loadFile("mob.lst"));

					delete _databank;
				}
			}
			print("mob.txt - done");
			print("All done!");
			_fFiles.close();
		}
	}
	if (Common::Filename(databankFullName).exists() == false) {
		print("Unable to load DATABANK.PTC. Trying to access the uncompressed data files...");

		// Since we found out that there's no databank.ptc file, we now
		// try to extract the data from the uncompressed datafiles.
		print("Processing variatxt.dat...");
		exportVariaTxt(loadFile(mainDir.getFullPath() + "variatxt.dat"));
		print("Processing invtxt.dat...");
		exportInvTxt(loadFile(mainDir.getFullPath() + "invtxt.dat"));
		print("Processing talktxt.dat...");
		exportTalkTxt(loadFile(mainDir.getFullPath() + "talktxt.dat"));
		print("Processing credits.dat...");
		exportCredits(loadFile(mainDir.getFullPath() + "credits.dat"));

		// Process the mob*.lst files. We have up to 61 files, so we process them recursively.
		_outputPath.setFullName("mob.txt");
		_fFiles.open(_outputPath, "w");
		_fFiles.print("mob.lst\nmob_name - exam text\n");

		int mobFileNumber;
		for (mobFileNumber = 1; mobFileNumber <= 61; ++mobFileNumber) {
			snprintf(pathBuffer, 100, "mob%02d.lst", mobFileNumber);
			if (Common::Filename(mainDir.getFullPath() + pathBuffer).exists() == true) {
				_fFiles.print("%d.\n", mobFileNumber);
				exportMobs(loadFile(mainDir.getFullPath() + pathBuffer));
				print("Processing mob%02d.lst", mobFileNumber);
			}
		}
		_fFiles.close();
		print("mob.txt - done");
		print("All done!");
	}
}

InspectionMatch ExtractPrince::inspectInput(const Common::Filename &filename) {
	if (!filename.directory())
		return IMATCH_AWFUL;

	// We expect either a "all/databank.ptc" file or a "variatxt.dat" file
	Common::Filename probe(filename.getPath() + "all/databank.ptc");
	if (probe.exists())
		return IMATCH_PERFECT;
	probe = filename;
	probe.setFullName("variatxt.dat");
	if (probe.exists())
		return IMATCH_PERFECT;

	return IMATCH_AWFUL;
}

// Uncompressed datafile loader
FileData ExtractPrince::loadFile(const std::string &fileName) {
	Common::File file;
	file.open(fileName, "rb");
	if (!file.isOpen()) {
		error("Unable to open datafile %s", fileName.c_str());
	}
	FileData fileData;
	fileData._size = file.size();
	fileData._fileTable = 0;

	fileData._fileTable = (byte *)malloc(fileData._size);
	file.read_throwsOnError(fileData._fileTable, fileData._size);

	return fileData;
}

void ExtractPrince::exportMobs(FileData fileData) {
	if (fileData._fileTable != 0) {
		const int kMobsStructSize = 32;
		const int kMobsTextOffsetsPos = 24;
		int streamPos = 0;

		while (READ_LE_UINT16(fileData._fileTable + streamPos) != 0xFFFF) {
			byte *mobTextOffset = fileData._fileTable + streamPos + kMobsTextOffsetsPos;

			uint32 nameOffset = READ_LE_UINT32(mobTextOffset);
			mobTextOffset += 4;
			uint32 examTextOffset = READ_LE_UINT32(mobTextOffset);

			std::string mobName, mobExamText;
			byte c;

			byte *namePointer = fileData._fileTable + nameOffset;
			mobName.clear();
			while ((c = *namePointer)) {
				c = correctPolishLetter(c);
				mobName += c;
				namePointer++;
			}

			_fFiles.print("%s - ", mobName.c_str());

			byte *examPointer = fileData._fileTable + examTextOffset;
			mobExamText.clear();
			c = *examPointer;
			if (c) {
				examPointer++;
				while ((c = *examPointer) != 255) {
					mobExamText.clear();
					do {
						c = correctPolishLetter(c);
						if (c == 10) {
							c = '|';
						}
						mobExamText += c;
						examPointer++;
					} while ((c = *examPointer));
					_fFiles.print("%s", mobExamText.c_str());
					examPointer++;
					if (*examPointer == 254) {
						_fFiles.print("*"); // show that there is pause before talking
						examPointer++;
					}
					if (*examPointer == 1) {
						_fFiles.print("+"); // show that there is next line of exam text
						examPointer++;
					}
				};
			}
			_fFiles.print("\n");

			streamPos += kMobsStructSize;
		}
		free(fileData._fileTable);
	}
}

void ExtractPrince::exportVariaTxt(FileData fileData) {
	if (fileData._fileTable != 0) {
		_outputPath.setFullName("variatxt.txt");
		_fFiles.open(_outputPath, "w");
		if (!_fFiles.isOpen()) {
			error("Unable to create variatxt.txt");
		}
		_fFiles.print("variatxt.dat\nstringId. string\n");
		const int kVariaTxtSize = 6000;
		for (int stringId = 0; stringId < kVariaTxtSize; stringId++) {
			uint32 stringOffset = READ_LE_UINT32(fileData._fileTable + stringId * 4);
			if (stringOffset > fileData._size) {
				assert(false);
			}
			std::string variaTxtString;
			byte c;
			byte *txtPointer = fileData._fileTable + stringOffset;
			variaTxtString.clear();
			txtPointer++;
			while ((c = *txtPointer)) {
				c = correctPolishLetter(c);
				if (c == 10) {
					c = '|';
				}
				variaTxtString += c;
				txtPointer++;
			}
			if (variaTxtString.size()) {
				_fFiles.print("%d. %s\n", stringId, variaTxtString.c_str());
			}
		}
		free(fileData._fileTable);
		_fFiles.close();
		print("variatxt.txt - done");
	}
}

void ExtractPrince::exportInvTxt(FileData fileData) {
	if (fileData._fileTable != 0) {
		std::string itemName, itemExamText;
		const int kItems = 100;
		_outputPath.setFullName("invtxt.txt");
		_fFiles.open(_outputPath, "w");
		if (!_fFiles.isOpen()) {
			error("Unable to create invtxt.txt");
		}
		_fFiles.print("invtxt.dat\nitemNr. name - exam text\n");
		for (int itemNr = 0; itemNr < kItems; itemNr++) {
			int txtOffset = READ_LE_UINT32(fileData._fileTable + itemNr * 8);
			int examTxtOffset = READ_LE_UINT32(fileData._fileTable + itemNr * 8 + 4);

			byte c;
			byte *nameTxt = fileData._fileTable + txtOffset;
			itemName.clear();
			while ((c = *nameTxt)) {
				c = correctPolishLetter(c);
				itemName += c;
				nameTxt++;
			}

			byte *examTxt = fileData._fileTable + examTxtOffset;
			itemExamText.clear();
			while ((c = *examTxt)) {
				c = correctPolishLetter(c);
				if (c == 10) {
					c = '|';
				}
				itemExamText += c;
				examTxt++;
			}

			if (itemName.size() != 2) {
				_fFiles.print("%d. %s - %s\n", itemNr, itemName.c_str(), itemExamText.c_str());
			}
		}
		free(fileData._fileTable);
		_fFiles.close();
		print("invtxt.txt - done");
	}
}

void ExtractPrince::exportCredits(FileData fileData) {
	if (fileData._fileTable != 0) {
		_outputPath.setFullName("credits.txt");
		_fFiles.open(_outputPath, "w");
		if (!_fFiles.isOpen()) {
			error("Unable to create credits.txt");
		}
		_fFiles.print("credits.dat\n");
		byte c;
		byte lastC = 10;
		byte *creditsTxt = fileData._fileTable;
		byte *end = fileData._fileTable + fileData._size;
		while (creditsTxt != end) {
			c = *creditsTxt;
			if (c == 10) {
				_fFiles.print("@\n");
			}
			if (c != 13 && c != 10) {
				c = correctPolishLetter(c);
				_fFiles.print("%c", c);
			}
			if (lastC != 10 && c == 13) {
				_fFiles.print("\n");
			}
			lastC = c;
			creditsTxt++;
		}
		free(fileData._fileTable);
		_fFiles.close();
		print("credits.txt - done");
	}
}

void ExtractPrince::exportTalkTxt(FileData fileData) {
	if (fileData._fileTable != 0) {
		_outputPath.setFullName("talktxt.txt");
		_fFiles.open(_outputPath, "w");
		if (!_fFiles.isOpen()) {
			error("Unable to create talktxt.txt");
		}
		_fFiles.print("talktxt.dat\n");

		byte *setStringOffsets = fileData._fileTable;
		const int kSetStringValues = 2000;
		int setStringOffsetsArray[kSetStringValues];
		int setStringIdArray[kSetStringValues];
		for (int i = 0; i < kSetStringValues; i++) {
			setStringOffsetsArray[i] = READ_LE_UINT32(setStringOffsets);
			setStringIdArray[i] = 0;
			setStringOffsets += 4;
		}

		int id = 1;
		int diff = 0;
		const int kTextOffset = 8000;
		byte *talkTxt = fileData._fileTable + kTextOffset;
		byte *endOfTalkTxt = fileData._fileTable + fileData._size;
		while (talkTxt < endOfTalkTxt) {
			diff = talkTxt - fileData._fileTable;
			for (int i = 0; i < kSetStringValues; i++) {
				if (setStringOffsetsArray[i] == diff) {
					setStringIdArray[i] = id;
				}
			}
			if (*talkTxt == 0xFF) {
				talkTxt = talkTxtWithDialog(talkTxt);
			} else {
				talkTxt = talkTxtNoDialog(talkTxt);
			}
			id++;
		}
		free(fileData._fileTable);
		_fFiles.close();
		print("talktxt.txt - done");

		// Additional id data for texts
		_outputPath.setFullName("talktxt_ids.txt");
		_fFiles.open(_outputPath, "w");
		if (!_fFiles.isOpen()) {
			error("Unable to create talktxt_ids.txt");
		}
		_fFiles.print("talktxt_ids\n");
		for (int i = 0; i < kSetStringValues; i++) {
			_fFiles.print("%d\n", setStringIdArray[i]);
		}
		_fFiles.close();
		print("talktxt_ids.txt - done");
	}
}

byte *ExtractPrince::talkTxtWithDialog(byte *talkTxt) {
	byte *mainString;
	byte *dialogBoxAddr[32];
	byte *dialogOptAddr[32];
	byte c;
	std::string lineString;

	byte *stringCurrOff = talkTxt;
	stringCurrOff++;
	int32 adressOfFirstSequence = (int)READ_LE_UINT16(stringCurrOff);
	mainString = talkTxt + adressOfFirstSequence;
	stringCurrOff += 2;

	for (int i = 0; i < 32; i++) {
		dialogBoxAddr[i] = 0;
		dialogOptAddr[i] = 0;
	}

	int16 off;
	byte *line = 0;

	int dialogBox = 0;
	while ((off = (int)READ_LE_UINT16(stringCurrOff)) != -1) {
		stringCurrOff += 2;
		if (off) {
			line = talkTxt + off;
		}
		dialogBoxAddr[dialogBox] = line;
		dialogBox++;
	}
	stringCurrOff += 2;

	int dialogOpt = 0;
	while ((off = (int)READ_LE_UINT16(stringCurrOff)) != -1) {
		stringCurrOff += 2;
		if (off) {
			line = talkTxt + off;
		}
		dialogOptAddr[dialogOpt] = line;
		dialogOpt++;
	}

	_fFiles.print("@DIALOGBOX_LINES:\n");
	while ((c = *mainString) != 255) {
		if (printSpecialDialogData(c)) {
			mainString++;
			_fFiles.print("%d\n", *mainString);
		}
		mainString++;
	}
	_fFiles.print("#END\n");

	for (int i = 0; i < dialogBox; i++) {
		_fFiles.print("@DIALOG_BOX %d\n", i);
		while ((c = *dialogBoxAddr[i]) != 255) {
			_fFiles.print("$%d\n", *dialogBoxAddr[i]);
			dialogBoxAddr[i]++;
			lineString.clear();
			while ((c = *dialogBoxAddr[i])) {
				c = correctPolishLetter(c);
				if (c == 10) {
					c = '|';
				}
				dialogBoxAddr[i]++;
				lineString += c;
			}
			dialogBoxAddr[i]++;
			_fFiles.print("%s\n", lineString.c_str());
		}
		_fFiles.print("#END\n");
	}

	for (int i = 0; i < dialogOpt; i++) {
		_fFiles.print("@DIALOG_OPT %d\n", i);
		byte lastC = 0;
		while ((c = *dialogOptAddr[i]) != 255) {
			// WALKAROUND: fix for unnecessery '0' after PAUSE
			// and double #HERO
			if ((lastC != 254 || c != 0) && (lastC != 1 || c != 1)) {
				if (printSpecialDialogData(c)) {
					dialogOptAddr[i]++;
					_fFiles.print("%d\n", *dialogOptAddr[i]);
				}
			}
			lastC = c;
			dialogOptAddr[i]++;
		}
		_fFiles.print("#END\n");
	}
	_fFiles.print("#ENDEND\n");
	talkTxt = dialogOptAddr[dialogOpt - 1];
	talkTxt++;
	return talkTxt;
}

byte *ExtractPrince::talkTxtNoDialog(byte *talkTxt) {
	byte c;
	_fFiles.print("@NORMAL_LINES:\n");
	while ((c = *talkTxt) != 255) {
		if (printSpecialDialogData(c)) {
			talkTxt++;
			_fFiles.print("%d\n", *talkTxt);
		}
		talkTxt++;
	}
	_fFiles.print("#END\n");
	talkTxt++;
	return talkTxt;
}

// Returns 'true' if next char is a value for 'enable option',
// 'disable option', 'show dialog box", 'flag code' or 'exit code'
bool ExtractPrince::printSpecialDialogData(byte c) {
	switch (c) {
	case 0:
		_fFiles.print("\n");
		return false;
	case 1:
		_fFiles.print("#HERO\n");
		return false;
	case 4:
		_fFiles.print("#OTHER\n");
		return false;
	case 5:
		_fFiles.print("#OTHER2\n");
		return false;
	case 10:
		_fFiles.print("|");
		return false;
	case 254:
		_fFiles.print("#PAUSE\n");
		return false;
	case 240:
		_fFiles.print("#ENABLE ");
		return true;
	case 241:
		_fFiles.print("#DISABLE ");
		return true;
	case 242:
		_fFiles.print("#BOX ");
		return true;
	case 243:
		_fFiles.print("#EXIT ");
		return true;
	case 244:
		_fFiles.print("#FLAG ");
		return true;
	default:
		c = correctPolishLetter(c);
		_fFiles.print("%c", c);
		return false;
	}
}

// Conversion from Mazovia to Windows-1250
char ExtractPrince::correctPolishLetter(char c) {
	switch (c) {
	case '\x86':
		return '\xB9';
	case '\x92':
		return '\xB3';
	case '\x9E':
		return '\x9C';
	case '\x8D':
		return '\xE6';
	case '\xA4':
		return '\xF1';
	case '\xA6':
		return '\x9F';
	case '\x91':
		return '\xEA';
	case '\xA2':
		return '\xF3';
	case '\xA7':
		return '\xBF';
	case '\x8F':
		return '\xA5';
	case '\x9C':
		return '\xA3';
	case '\x98':
		return '\x8C';
	case '\x95':
		return '\xC6';
	case '\xA5':
		return '\xD1';
	case '\xA0':
		return '\x8F';
	case '\x90':
		return '\xCA';
	case '\xA3':
		return '\xD3';
	case '\xA1':
		return '\xAF';
	default:
		return c;
	}
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractPrince prince(argv[0]);
	return prince.run(argc, argv);
}
#endif
