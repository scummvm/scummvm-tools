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

#ifndef EXTRACT_ASYLUM_H
#define EXTRACT_ASYLUM_H

#include "tool.h"

#include "common/array.h"
#include "common/file.h"

typedef int ResourceId;

struct ResourceEntry {
	uint8  *data;
	uint32  size;
	uint32  offset;
};

class ExtractAsylum : public Tool {
public:
	ExtractAsylum(const std::string &name = "extract_asylum");
	virtual InspectionMatch inspectInput(const Common::Filename &filename);
	virtual void parseExtraArguments();
	virtual void execute();

protected:
	uint _packId;
	int _resInd;
	Common::File _packFile;
	Common::Array<ResourceEntry> _resources;

	void initPack(const Common::Filename &filename);
	void dumpResource(int resInd);
};

#endif // EXTRACT_ASYLUM_H
