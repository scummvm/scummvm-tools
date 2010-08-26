/* extract_fascination_cd - a tool for extracting .stk archives from a mode1/2048 Fascination CD image
 * Copyright (C) 2010  The ScummVM Team
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
 * $URL: $
 * $Id: $
 *
 */

#ifndef EXTRACT_FASCINATION_CD_H
#define EXTRACT_FASCINATION_CD_H

#include "compress.h"

class ExtractFascinationCD : public Tool {
public:
	ExtractFascinationCD(const std::string &name = "extract_fascination_cd");

	virtual void execute();
};

#endif

