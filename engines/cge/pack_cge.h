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

#ifndef PACK_CGE_H
#define PACK_CGE_H

#include "tool.h"
#include "cge_structs.h"

#define MAX_FILES 6000

class PackCge : public Tool {
public:
	PackCge(const std::string &name = "pack_cge");
	
	virtual void execute();
	
	virtual InspectionMatch inspectInput(const Common::Filename &filename);
	
protected:
	void writeData(Common::File &f, byte *buff, int size);
	void pack();
	void pack2();

	Common::Filename inPath;
	Common::File _volCat, _volDat, _fIn;
	int _leaSize, _fileCount;
	char _files[MAX_FILES][kBtKeySize];
};

#endif
