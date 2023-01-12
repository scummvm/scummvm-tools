/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
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

#ifndef TWINE_SHARED_H
#define TWINE_SHARED_H

#include <stdio.h>
#include <string.h>

#include "common/array.h"
#include "common/util.h"
#include "common/memstream.h"

static const int initialLevel = 3;
static const int indentWidth = 2;

struct ScriptContext {
	ScriptContext(const uint8* data, int size) : stream(data, size) {}
	Common::MemoryReadStream stream;
	Common::Array<int> offsets;
	int level = initialLevel;
	int comportmentId = 0;
	bool comportment = false;
};

typedef void ScriptFunc(ScriptContext &ctx);

struct ScriptFunction {
	const char *name;
	ScriptFunc *function;
};

#define MAPFUNC(name, func) \
	{ name, func }

#endif