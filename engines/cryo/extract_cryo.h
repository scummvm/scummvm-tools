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

#ifndef EXTRACT_CRYO_H
#define EXTRACT_CRYO_H

#include "tool.h"
#include "common/array.h"

struct DATEntry {
	char	filename[16];
	uint32	size;
	uint32	offset;
	char	flag;
};

#define MPCIterator Common::Array<DATEntry *>::iterator

class ExtractCryo : public Tool {
public:
	ExtractCryo(const std::string &name = "extract_cryo");

	virtual void execute();

	virtual InspectionMatch inspectInput(const Common::Filename &filename);

protected:
	bool openDAT(Common::Filename &filename);

	Common::File _datFile;

	Common::Array<DATEntry *> _dir;
};

#endif
