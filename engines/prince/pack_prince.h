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

#ifndef PACK_PRINCE_H
#define PACK_PRINCE_H

#include "tool.h"
#include "common\array.h"

class PackPrince : public Tool {
public:
	PackPrince(const std::string &name = "pack_prince");
	
	virtual void execute();
	
	virtual InspectionMatch inspectInput(const Common::Filename &filename);
	
protected:
	struct FileEntry {
		uint32 _offset;
		uint32 _size;
	};

	struct InvTxt {
		std::string _name;
		std::string _examTxt;
	};

	struct VariaTxt {
		std::string _txt;
	};

	struct Mob {
		std::string _name;
		std::string _examTxt;
	};

	struct TalkBeforeBox {
		int _dialogData;
		std::string _txt;
	};

	void packMobs();
	void packVariaTxt();
	void packInvTxt();
	void packCredits();
	void packTalkTxt();
	char correctPolishLetter(char c);
	void talkTxtWithDialog();
	void talkTxtNoDialog();

	Common::File _databank, _fFiles;
};

#endif
