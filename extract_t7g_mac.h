/* extract_t7g_mac - Extractor for the Mac version of The 7th Guest
 * Copyright (C) 2009 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */

#ifndef EXTRACT_T7G_MAC_H
#define EXTRACT_T7G_MAC_H

#include "tool.h"

class ExtractT7GMac : public Tool {
public:
	ExtractT7GMac(const std::string &name = "extract_t7g_mac");

	virtual void execute();

protected:
	std::string readString(File &infile);
	void dumpResource(File &infile, std::string name);
	void handleReferenceList(File &infile, uint32 offsetRefList, uint16 numRes, uint32 offsetResNames);
};

#endif

