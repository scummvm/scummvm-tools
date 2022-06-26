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

/* Extract data files from TG16 version of Loom */

#ifndef EXTRACT_LOOM_TG16_H
#define EXTRACT_LOOM_TG16_H

#include "compress.h"

struct t_resource;
typedef t_resource * p_resource;

class ExtractLoomTG16 : public Tool {
public:
	ExtractLoomTG16(const std::string &name = "extract_loom_tg16");

	virtual void execute();

protected:

	void extract_resource(Common::File &input, Common::File &output, p_resource res);
};

#endif

