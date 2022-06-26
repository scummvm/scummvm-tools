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

#ifndef EXTRACT_PRINCE_H
#define EXTRACT_PRINCE_H

#include "tool.h"
#include "utils.h"
#include "common/array.h"

class Databank;

class ExtractPrince : public Tool {
public:
	ExtractPrince(const std::string &name = "extract_prince");

	virtual void execute();

	virtual InspectionMatch inspectInput(const Common::Filename &filename);

protected:
	FileData loadFile(const std::string &fileName);
	char correctPolishLetter(char c);

	void exportMobs(FileData fileData);
	void exportVariaTxt(FileData fileData);
	void exportInvTxt(FileData fileData);
	void exportCredits(FileData fileData);
	void exportTalkTxt(FileData fileData);
	byte *talkTxtWithDialog(byte *talkTxt);
	byte *talkTxtNoDialog(byte *talkTxt);
	bool printSpecialDialogData(byte c);

	Common::File _fFiles;

	Databank *_databank;
};

#endif
