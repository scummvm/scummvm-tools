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

#include <string.h>
#include "pack_prince.h"
#include "common\endian.h"

PackPrince::PackPrince(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "/";
	_inputPaths.push_back(input);

	_outputToDirectory = true;

	_shorthelp = "Used to repackage The Prince and the Coward text data for translation.";
	_helptext = "\nUsage: " + getName() + " [-o /path/to/output/dir/] /path/to/inputfiles\n";
}

void PackPrince::execute() {
	Common::Filename mainDir = _inputPaths[0].path;
	// We always need to setup default output path, since there is no obligation to specify it
	if (_outputPath.empty()) {
		_outputPath.setFullPath("./");
	}

	_outputPath.setFullName("prince_translation.dat");
	_fFiles.open(_outputPath, "wb");
	if (!_fFiles.isOpen()) {
		error("Unable to create prince_translation.dat");
	}

	// Export names of files
	_fFiles.print("variatxt_translate.dat\n");
	_fFiles.print("invtxt_translate.dat\n");
	_fFiles.print("talktxt_translate.dat\n");
	_fFiles.print("mob_translate.dat\n");
	_fFiles.print("credits_translate.dat\n");


	// Export files infomation
	int posOfFilesInformation = _fFiles.pos();
	FileEntry filesInfo[5];
	int fileNr = 0;
	for (int i = 0; i < 5; i++) {
		_fFiles.writeUint32LE(0); // place for files offsets
		_fFiles.writeUint32LE(0); // and size of files
	}

	printf("Packing The Prince and the Coward text data... \n");

	filesInfo[fileNr]._offset = _fFiles.pos();
	mainDir.setFullName("variatxt.txt");
	_databank.open(mainDir, "rb");
	if (!_databank.isOpen()) {
		error("Unable to open variatxt.txt");
	}
	packVariaTxt();
	filesInfo[fileNr]._size = _fFiles.pos() - filesInfo[fileNr]._offset;
	fileNr++;
	_databank.close();

	filesInfo[fileNr]._offset = _fFiles.pos();
	mainDir.setFullName("invtxt.txt");
	_databank.open(mainDir, "rb");
	if (!_databank.isOpen()) {
		error("Unable to open invtxt.txt");
	}
	packInvTxt();
	filesInfo[fileNr]._size = _fFiles.pos() - filesInfo[fileNr]._offset;
	fileNr++;
	_databank.close();

	filesInfo[fileNr]._offset = _fFiles.pos();
	mainDir.setFullName("talktxt.txt");
	_databank.open(mainDir, "rb");
	if (!_databank.isOpen()) {
		error("Unable to open talktxt.txt");
	}
	packTalkTxt();
	filesInfo[fileNr]._size = _fFiles.pos() - filesInfo[fileNr]._offset;
	fileNr++;
	_databank.close();

	filesInfo[fileNr]._offset = _fFiles.pos();
	mainDir.setFullName("mob.txt");
	_databank.open(mainDir, "rb");
	if (!_databank.isOpen()) {
		error("Unable to open mob.txt");
	}
	packMobs();
	filesInfo[fileNr]._size = _fFiles.pos() - filesInfo[fileNr]._offset;
	fileNr++;
	_databank.close();

	filesInfo[fileNr]._offset = _fFiles.pos();
	mainDir.setFullName("credits.txt");
	_databank.open(mainDir, "rb");
	if (!_databank.isOpen()) {
		error("Unable to open credits.txt");
	}
	packCredits();
	filesInfo[fileNr]._size = _fFiles.pos() - filesInfo[fileNr]._offset;
	fileNr++;
	_databank.close();

	_fFiles.seek(posOfFilesInformation, SEEK_SET);
	for (int i = 0; i < fileNr; i++) {
		_fFiles.writeUint32LE(filesInfo[i]._offset);
		_fFiles.writeUint32LE(filesInfo[i]._size);
	}
	_fFiles.close();
}

// TODO
InspectionMatch PackPrince::inspectInput(const Common::Filename &filename) {
	/*
	std::string file = filename.getFullName();
	if (scumm_stricmp(file.c_str(), "databank.ptc") == 0) {
		return IMATCH_PERFECT;
	}
	return IMATCH_AWFUL;
	*/
	return IMATCH_PERFECT;
}

void PackPrince::packVariaTxt() {
	// File size
	uint32 fileSize = _databank.size();

	// Header test
	byte c;
	std::string name, examTxt;
	while ((c = _databank.readByte()) != '\r') {
		name += c;
	}
	if (name.compare("variatxt.dat")) {
		error("Wrong header in variatxt.txt");
	}
	
	// Skip comment until first number
	while ((c = _databank.readByte()) != '\r');
	_databank.readByte(); // skip '\n'
	
	// Loading to array:
	const int variaTxtSize = 6000;
	Common::Array<VariaTxt> variaTxtList; // list of all txt
	VariaTxt newVariaTxt; // temp varia txt
	newVariaTxt._txt = "";
	for (int i = 0; i < variaTxtSize; i++) {
		variaTxtList.push_back(newVariaTxt);
	}

	while (1) {
		// Id:
		int id = 0;
		while ((c = _databank.readByte()) != '.') {
			id *= 10;
			id += c - 48;
		}
		_databank.readByte(); // skip space

		// Text:
		newVariaTxt._txt.clear();
		while ((c = _databank.readByte()) != '\r') {
			c = correctPolishLetter(c); // temporary
			if (c == '|') {
				c = 10;
			}
			newVariaTxt._txt += c;
		}

		// Set VariaTxt in array
		variaTxtList[id]._txt = newVariaTxt._txt;

		// Skip '\n' and test eof
		_databank.readByte(); 
		if (_databank.pos() == fileSize) {
			break;
		}
	}
	
	int textOffset = variaTxtSize * 4;
	// Offset counting and packing:
	for (int i = 0; i < variaTxtSize; i++) {
		if (variaTxtList[i]._txt.size()) {
			_fFiles.writeUint32LE(textOffset);
			textOffset += variaTxtList[i]._txt.size() + 3;
		} else {
			_fFiles.writeUint32LE(0);
		}
	}
	for (uint i = 0; i < variaTxtList.size(); i++) {
		if (variaTxtList[i]._txt.size()) {
			_fFiles.writeByte(1);
			for (uint j = 0; j < variaTxtList[i]._txt.size(); j++) {
				_fFiles.writeByte(variaTxtList[i]._txt[j]);
			}
			_fFiles.writeByte(0);
			_fFiles.writeByte(255);
		}
	}
}

void PackPrince::packInvTxt() {
	// File size
	uint32 fileSize = _databank.size();

	// Header test
	byte c;
	std::string name, examTxt;
	while ((c = _databank.readByte()) != '\r') {
		name += c;
	}
	if (name.compare("invtxt.dat")) {
		error("Wrong header in invtxt.txt");
	}
	
	// Skip comments until first inventory item nr
	while ((c = _databank.readByte()) != '0');
	
	// Skip first dot and space
	_databank.readByte();
	_databank.readByte();

	// Loading to an array:
	const int kItems = 100;
	Common::Array<InvTxt> invTxtList; // list of all invTxt
	InvTxt newInvTxt; // temp invTxt
	for (int i = 0; i < kItems; i++) {
		invTxtList.push_back(newInvTxt);
	}

	int id = 0;
	while (1) {
		newInvTxt._name.clear();
		newInvTxt._examTxt.clear();
		// Name:
		while ((c = _databank.readByte()) != '-') {
			c = correctPolishLetter(c); // temporary
			newInvTxt._name += c;
		}
		newInvTxt._name.pop_back(); // remove first space
		_databank.readByte(); // skip second space

		// Exam text:
		while ((c = _databank.readByte()) != '\r') {
			c = correctPolishLetter(c); // temporary
			if (c == '|') {
				c = 10;
			}
			newInvTxt._examTxt += c;
		}

		// Add new item to list
		invTxtList[id] = newInvTxt;

		// Skip '\n' and test eof
		_databank.readByte(); 
		if (_databank.pos() == fileSize) {
			break;
		} else {
			while ((c = _databank.readByte()) != ' '); // skip item nr, space and dot
		}
		id++;
	}
	
	// Offset counting and packing:
	int nameOffset = kItems * 4 * 2;
	for (uint i = 0; i < kItems; i++) {
		if (invTxtList[i]._name.size()) {
			int examTextOffset = nameOffset + invTxtList[i]._name.size() + 1;
			_fFiles.writeUint32LE(nameOffset);
			_fFiles.writeUint32LE(examTextOffset);
			nameOffset += invTxtList[i]._name.size() + invTxtList[i]._examTxt.size() + 2;
		} else {
			_fFiles.writeUint32LE(0);
			_fFiles.writeUint32LE(0);
		}
	}
	for (uint32 i = 0; i < invTxtList.size(); i++) {
		if (invTxtList[i]._name.size()) {
			// Names
			for (uint j = 0; j < invTxtList[i]._name.size(); j++) {
				_fFiles.writeByte(invTxtList[i]._name[j]);
			}
			_fFiles.writeByte(0);
			// Exam texts
			for (uint j = 0; j < invTxtList[i]._examTxt.size(); j++) {
				_fFiles.writeByte(invTxtList[i]._examTxt[j]);
			}
			_fFiles.writeByte(0);
		}
	}
}

void PackPrince::packCredits() {
	// File size
	uint32 fileSize = _databank.size();

	// Header test
	byte c;
	std::string line;
	while ((c = _databank.readByte()) != '\r') {
		line += c;
	}
	if (line.compare("credits.dat")) {
		error("Wrong header in credits.txt");
	}
	_databank.readByte(); // skip '\n'

	while (1) {
		c = _databank.readByte();
		if (c != 13 && c != 10) {
			if (c == '@') {
				_fFiles.writeByte(13);
				_fFiles.writeByte(10);
			} else {
				c = correctPolishLetter(c);
				_fFiles.writeByte(c);
			}
		}
		if (_databank.pos() == fileSize) {
			break;
		}
	}
}

void PackPrince::packTalkTxt() {
	// IDs array for first 2000 offsets calculation
	const int kSetStringValues = 2000;
	int setStringIdArray[kSetStringValues];
	Common::Filename mainDir = _inputPaths[0].path;
	Common::File idsFile;
	mainDir.setFullName("talktxt_ids.txt");
	idsFile.open(mainDir, "rb");
	if (!idsFile.isOpen()) {
		error("Unable to open talktxt_ids.txt");
	}

	// Header test
	byte c;
	std::string line;
	while ((c = idsFile.readByte()) != '\r') {
		line += c;
	}
	if (line.compare("talktxt_ids")) {
		error("Wrong header in talktxt_ids.txt");
	}
	idsFile.readByte(); // skip '\n'

	// IDs loading
	for (int i = 0; i < kSetStringValues; i++) {
		int value = 0;
		while ((c = idsFile.readByte()) != '\r') {
			value *= 10;
			value += c - 48;
		}
		idsFile.readByte(); // skip '\n'
		setStringIdArray[i] = value;
	}
	idsFile.close();

	// Main file
	// File size
	uint32 fileSize = _databank.size();

	// Header test
	line.clear();
	while ((c = _databank.readByte()) != '\r') {
		line += c;
	}
	if (line.compare("talktxt.dat")) {
		error("Wrong header in talktxt.txt");
	}
	_databank.readByte(); // skip '\n'

	// Start pos of talkTxt file for later offset setting
	int startTalkTxtPos = _fFiles.pos();
	
	// TODO - temp, to put main offsets here
	for (int i = 0; i < kSetStringValues; i++) {
		_fFiles.writeUint32LE(0);
	}

	int id = 1;
	int setStringOffsetsArray[kSetStringValues];
	for (int i = 0; i < kSetStringValues; i++) {
		setStringOffsetsArray[i] = 0;
	}

	while (1) {
		line.clear();
		while ((c = _databank.readByte()) != '\r') {
			line += c;
		}
		_databank.readByte(); // skip '\n'

		for (int i = 0; i < kSetStringValues; i++) {
			if (id == setStringIdArray[i]) {
				setStringOffsetsArray[i] = _fFiles.pos() - startTalkTxtPos;
			}
		}

		if (!line.compare("@DIALOGBOX_LINES:")) {
			talkTxtWithDialog();
		} else if (!line.compare("@NORMAL_LINES:")) {
			talkTxtNoDialog();
		}

		id++;

		if (_databank.pos() == fileSize) {
			break;
		}
	}
	// End of talkTxt file
	int endTalkTxtPos = _fFiles.pos();

	// Back to start of talkTxt file for offsets setting
	_fFiles.seek(startTalkTxtPos, SEEK_SET);
	for (int i = 0; i < kSetStringValues; i++) {
		_fFiles.writeUint32LE(setStringOffsetsArray[i]);
	}

	// Back to the end of talkTxt file
	_fFiles.seek(endTalkTxtPos, SEEK_SET);
}

void PackPrince::talkTxtWithDialog() {
	byte c;
	std::string line;
	Common::Array<TalkBeforeBox> beforeDialogBoxArray;
	Common::Array<Common::Array<TalkBeforeBox>> allDialogBoxesArray;
	Common::Array<Common::Array<TalkBeforeBox>> allDialogOptionsArray;

	// Intro talk before dialog box:
	TalkBeforeBox tempTalkBeforeBox;
	while (1) {
		// Special dialog data
		line.clear();
		while ((c = _databank.readByte()) != '\r') {
			line += c;
		}
		_databank.readByte(); // skip '\n'

		if (!line.compare("#HERO")) {
			tempTalkBeforeBox._dialogData = 1;
		} else if (!line.compare("#OTHER")) {
			tempTalkBeforeBox._dialogData = 4;
		} else if (!line.compare("#OTHER2")) {
			tempTalkBeforeBox._dialogData = 5;
		} else if (!line.compare("#PAUSE")) {
			tempTalkBeforeBox._dialogData = 254;
			tempTalkBeforeBox._txt.clear();
			beforeDialogBoxArray.push_back(tempTalkBeforeBox);
			continue;
		} else if (!line.compare("#BOX 0")) {
			while (_databank.readByte() != '\n'); // skip #END
			break;
		}

		// Line of text
		tempTalkBeforeBox._txt.clear();
		while ((c = _databank.readByte()) != '\r') {
			c = correctPolishLetter(c); // temporary
			if (c == '|') {
				c = 10;
			}
			tempTalkBeforeBox._txt += c;
		}
		c = 0;
		tempTalkBeforeBox._txt += c;
		_databank.readByte(); // skip '\n'

		beforeDialogBoxArray.push_back(tempTalkBeforeBox);
	}

	// All dialog boxes:
	int dbNr = 0;
	Common::Array<TalkBeforeBox> tempDialogBoxArray;
	TalkBeforeBox tempDialogBoxLine;

	while (1) {
		// Check if @DIALOG_BOX
		line.clear();
		while ((c = _databank.readByte()) != ' ') {
			line += c;
		}
		while (_databank.readByte() != '\n'); // skip " %d\r\n"
		if (line.compare("@DIALOG_BOX")) {
			break;
		}

		tempDialogBoxArray.clear();
		allDialogBoxesArray.push_back(tempDialogBoxArray);
	
		// Lines of dialog box
		while (1) {
			// Skip $, check if #END
			c = _databank.readByte();
			if (c == '#') {
				while (_databank.readByte() != '\n'); // skip "END\r\n"
				break;
			}

			// Text number
			int textNr = 0;
			while ((c = _databank.readByte()) != '\r') {
				textNr *= 10;
				textNr += c - 48;
			}
			tempDialogBoxLine._dialogData = textNr;
			_databank.readByte(); // skip '\n'

			// Line of text
			tempDialogBoxLine._txt.clear();
			while ((c = _databank.readByte()) != '\r') {
				c = correctPolishLetter(c); // temporary
				if (c == '|') {
					c = 10;
				}
				tempDialogBoxLine._txt += c;
			}
			c = 0;
			tempDialogBoxLine._txt += c;
			_databank.readByte(); // skip '\n'

			allDialogBoxesArray[dbNr].push_back(tempDialogBoxLine);
		}

		dbNr++;
	}

	// All dialog options:
	int dbOptNr = 0;
	Common::Array<TalkBeforeBox> tempDialogOptionsArray;
	TalkBeforeBox tempDialogOptionsLine;

	while (1) {
		tempDialogOptionsArray.clear();
		allDialogOptionsArray.push_back(tempDialogOptionsArray);
	
		// Lines of dialog opt
		while (1) {
			// Special dialog data
			line.clear();
			while ((c = _databank.readByte()) != '\r' && c != ' ') {
				line += c;
			}
			// Check if #END
			if (!line.compare("#END")) {
				_databank.readByte(); // skip '\n'
				break;
			}
			if (!line.compare("#HERO")) {
				tempDialogOptionsLine._dialogData = 1;
			} else if (!line.compare("#OTHER")) {
				tempDialogOptionsLine._dialogData = 4;
			} else if (!line.compare("#OTHER2")) {
				tempDialogOptionsLine._dialogData = 5;
			} else if (!line.compare("#PAUSE")) {
				tempDialogOptionsLine._dialogData = 254;
				tempDialogOptionsLine._txt.clear();
				_databank.readByte(); // skip '\n'
				allDialogOptionsArray[dbOptNr].push_back(tempDialogOptionsLine);
				continue;
			} else {
				if (!line.compare("#ENABLE")) { // 0 - 15
					tempDialogOptionsLine._dialogData = 240;
					uint8 value = 0;
					while ((c = _databank.readByte()) != '\r') {
						value *= 10;
						value += c - 48;
					}
					tempDialogOptionsLine._txt.clear();
					tempDialogOptionsLine._txt += value;
				} else {
					if (!line.compare("#DISABLE")) { // 1
						tempDialogOptionsLine._dialogData = 241;
					} else if (!line.compare("#BOX")) { // 0, 1, 2, 3, 4
						tempDialogOptionsLine._dialogData = 242;
					} else if (!line.compare("#EXIT")) { // 0, 1, 2, 3, 4
						tempDialogOptionsLine._dialogData = 243;
					} else if (!line.compare("#FLAG")) { // 1
						tempDialogOptionsLine._dialogData = 244;
					}
					tempDialogOptionsLine._txt.clear();
					tempDialogOptionsLine._txt += _databank.readByte() - 48;
					_databank.readByte(); // skip '\r'
				}
				_databank.readByte(); // skip '\n'
				allDialogOptionsArray[dbOptNr].push_back(tempDialogOptionsLine);
				continue;
			}

			_databank.readByte(); // skip '\n'

			// Line of text
			tempDialogOptionsLine._txt.clear();
			while ((c = _databank.readByte()) != '\r') {
				c = correctPolishLetter(c); // temporary
				if (c == '|') {
					c = 10;
				}
				tempDialogOptionsLine._txt += c;
			}
			c = 0;
			tempDialogOptionsLine._txt += c;
			_databank.readByte(); // skip '\n'
			allDialogOptionsArray[dbOptNr].push_back(tempDialogOptionsLine);
		}
		dbOptNr++;
		// Check if #ENDEND, skip @DIALOG_OPT %d
		line.clear();
		while ((c = _databank.readByte()) != '\r') {
			line += c;
		}
		_databank.readByte(); // skip '\n'
		if (!line.compare("#ENDEND")) {
			break;
		}
	}

	// Offset counting and packing:
	_fFiles.writeByte(255); // show that this is dialog box
	
	// Offset of init dialog texts
	int offset = 3 + allDialogBoxesArray.size() * 2 + 2 + allDialogOptionsArray.size() * 2 + 2;
	_fFiles.writeUint16LE(offset); 
	for (uint i = 0; i < beforeDialogBoxArray.size(); i++) {
		offset += beforeDialogBoxArray[i]._txt.size() + 1; // data and text
	}
	offset += 3; // always BOX 0 and 255 at the end
	
	// Dialog boxes texts offsets:
	for (uint i = 0; i < allDialogBoxesArray.size(); i++) {
		_fFiles.writeUint16LE(offset);
		for (uint j = 0; j < allDialogBoxesArray[i].size(); j++) {
			offset += allDialogBoxesArray[i][j]._txt.size() + 1; // data and text
		}
		offset++; // 255 at the end
	}
	_fFiles.writeByte(255);
	_fFiles.writeByte(255);
	// Dialog opts texts offsets:
	for (uint i = 0; i < allDialogOptionsArray.size(); i++) {
		_fFiles.writeUint16LE(offset);
		for (uint j = 0; j < allDialogOptionsArray[i].size(); j++) {
			offset += allDialogOptionsArray[i][j]._txt.size() + 1;
		}
		offset++; // 255 at the end
	}
	_fFiles.writeByte(255);
	_fFiles.writeByte(255);

	// Init texts:
	for (uint i = 0; i < beforeDialogBoxArray.size(); i++) {
		_fFiles.writeByte(beforeDialogBoxArray[i]._dialogData);
		for (uint j = 0; j < beforeDialogBoxArray[i]._txt.size(); j++) {
			_fFiles.writeByte(beforeDialogBoxArray[i]._txt[j]);
		}
	}
	_fFiles.writeByte(242);
	_fFiles.writeByte(0);
	_fFiles.writeByte(255);

	// Dialog boxes:
	for (uint i = 0; i < allDialogBoxesArray.size(); i++) {
		for (uint j = 0; j < allDialogBoxesArray[i].size(); j++) {
			_fFiles.writeByte(allDialogBoxesArray[i][j]._dialogData);
			for (uint k = 0; k < allDialogBoxesArray[i][j]._txt.size(); k++) {
				_fFiles.writeByte(allDialogBoxesArray[i][j]._txt[k]);
			}
		}
		_fFiles.writeByte(255);
	}

	// Dialog opts:
	for (uint i = 0; i < allDialogOptionsArray.size(); i++) {
		for (uint j = 0; j < allDialogOptionsArray[i].size(); j++) {
			_fFiles.writeByte(allDialogOptionsArray[i][j]._dialogData);
			for (uint k = 0; k < allDialogOptionsArray[i][j]._txt.size(); k++) {
				_fFiles.writeByte(allDialogOptionsArray[i][j]._txt[k]);
			}
		}
		_fFiles.writeByte(255);
	}
}

void PackPrince::talkTxtNoDialog() {
	byte c;
	std::string line;
	Common::Array<TalkBeforeBox> normalLinesArray;
	TalkBeforeBox tempNormalLine;
	while (1) {
		// Special dialog data
		line.clear();
		while ((c = _databank.readByte()) != '\r') {
			line += c;
		}
		_databank.readByte(); // skip '\n'

		if (!line.compare("#HERO")) {
			tempNormalLine._dialogData = 1;
		} else if (!line.compare("#OTHER")) {
			tempNormalLine._dialogData = 4;
		} else if (!line.compare("#OTHER2")) {
			tempNormalLine._dialogData = 5;
		} else if (!line.compare("#PAUSE")) {
			tempNormalLine._dialogData = 254;
			tempNormalLine._txt.clear();
			normalLinesArray.push_back(tempNormalLine);
			continue;
		} else if (!line.compare("#END")) {
			break;
		}

		// Line of text
		tempNormalLine._txt.clear();
		while ((c = _databank.readByte()) != '\r') {
			c = correctPolishLetter(c); // temporary
			if (c == '|') {
				c = 10;
			}
			tempNormalLine._txt += c;
		}
		c = 0;
		tempNormalLine._txt += c;
		_databank.readByte(); // skip '\n'

		normalLinesArray.push_back(tempNormalLine);
	}

	// Offset counting and packing:
	for (uint i = 0; i < normalLinesArray.size(); i++) {
		_fFiles.writeByte(normalLinesArray[i]._dialogData);
		for (uint j = 0; j < normalLinesArray[i]._txt.size(); j++) {
			_fFiles.writeByte(normalLinesArray[i]._txt[j]);
		}
	}
	_fFiles.writeByte(255);
}

void PackPrince::packMobs() {
	// File size
	uint32 fileSize = _databank.size();

	// Header test
	byte c;
	std::string name, examTxt;
	while ((c = _databank.readByte()) != '\r') {
		name += c;
	}
	if (name.compare("mob.lst")) {
		error("Wrong header in mob.txt");
	}
	_databank.readByte(); // skip '\n'

	// Skip comment until first location nr
	while ((c = _databank.readByte()) != '\n');

	// Loading to an array:
	Common::Array<Common::Array<Mob>> allLocations; // array of locations of all Mobs
	Common::Array<Mob> tempLocation; // temp array of Mobs in one location
	Mob newMob; // temp Mob
	const int kLocations = 61; // max nr of location
	int nr = 0; // number of current location

	for (int i = 0; i < kLocations; i++) {
		allLocations.push_back(tempLocation);
	}

	while (1) {
		newMob._name.clear();
		newMob._examTxt.clear();

		// Test if end of location - next number
		if ((c = _databank.readByte()) > 47 && c < 58) {
			// Set location nr:
			nr = c - 48;
			while ((c = _databank.readByte()) != '.') {
				nr *= 10;
				nr += c - 48;
			}
			nr--;
			_databank.readByte(); // skip '\r'
			_databank.readByte(); // skip '\n'
		} else {
			c = correctPolishLetter(c); // temporary
			newMob._name += c; // first letter of name
		}

		// Test for eof
		if (_databank.pos() == fileSize) {
			break;
		}

		// No mobs in this location
		if ((c = _databank.readByte()) > 47 && c < 58) {
			_databank.seek(-1, SEEK_CUR);
			continue;
		} else {
			c = correctPolishLetter(c); // temporary
			newMob._name += c;
		}

		// Name:
		while ((c = _databank.readByte()) != '-') {
			c = correctPolishLetter(c); // temporary
			newMob._name += c;
		}
		newMob._name.pop_back(); // remove first space
		_databank.readByte(); // skip second space

		// Exam text:
		while ((c = _databank.readByte()) != '\r') {
			c = correctPolishLetter(c); // temporary
			if (c == '|') {
				c = 10;
			}
			if (c == '*') {
				c = 254;
			}
			if (c == '+') {
				c = 0;
				newMob._examTxt += c;
				c = 1;
			}
			newMob._examTxt += c;
		}

		// Skip '\n'
		_databank.readByte();

		// Add new item to list
		allLocations[nr].push_back(newMob);
	}
	// Offset counting and packing:
	int locationOffset = allLocations.size() * 2;
	for (uint i = 0; i < allLocations.size(); i++) {
		if (allLocations[i].size()) {
			_fFiles.writeUint16LE(locationOffset);
			for (uint j = 0; j < allLocations[i].size(); j++) {
				locationOffset += allLocations[i][j]._name.size() + allLocations[i][j]._examTxt.size() + 5;
			}
		} else {
			_fFiles.writeUint16LE(0);
		}
	}
	for (uint i = 0; i < allLocations.size(); i++) {
		for (uint j = 0; j < allLocations[i].size(); j++) {
			_fFiles.writeByte(1);
			for (uint k = 0; k < allLocations[i][j]._name.size(); k++) {
				_fFiles.writeByte(allLocations[i][j]._name[k]);
			}
			_fFiles.writeByte(0);
			_fFiles.writeByte(1);
			for (uint k = 0; k < allLocations[i][j]._examTxt.size(); k++) {
				_fFiles.writeByte(allLocations[i][j]._examTxt[k]);			
			}
			_fFiles.writeByte(0);
			_fFiles.writeByte(255);
		}
	}
}

// Conversion from Windows-1250 to Mazovia
// temporary for not complete translation
char PackPrince::correctPolishLetter(char c) {
	switch (c) {
	case '\xB9':
		return '\x86';
	case '\xB3':
		return '\x92';
	case '\x9C':
		return '\x9E';
	case '\xE6':
		return '\x8D';
	case '\xF1':
		return '\xA4';
	case '\x9F':
		return '\xA6';
	case '\xEA':
		return '\x91';
	case '\xF3':
		return '\xA2';
	case '\xBF':	
		return '\xA7';
	case '\xA5':
		return '\x8F';
	case '\xA3':
		return '\x9C';
	case '\x8C':
		return '\x98';
	case '\xC6':
		return '\x95';
	case '\xD1':	
		return '\xA5';
	case '\x8F':
		return '\xA0';
	case '\xCA':	
		return '\x90';
	case '\xD3':
		return '\xA3';
	case '\xAF':	
		return '\xA1';
	default:
		return c;
	}
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractCge cge(argv[0]);
	return cge.run(argc, argv);
}
#endif
