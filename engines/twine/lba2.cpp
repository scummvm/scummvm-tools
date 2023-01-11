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

#include "engines/twine/lba2.h"
#include "common/memstream.h"

// static int decompileLBA2MoveScript(int actor, const uint8 *moveScript, int16 moveScriptSize) {
// 	return 0;
// }

// static int decompileLBA2LifeScript(int actor, const uint8 *moveScript, int16 moveScriptSize) {
// 	return 0;
// }

int decompileLBA2(const uint8 *data, int size) {
	Common::MemoryReadStream stream(data, size);
	if (stream.err()) {
		return 1;
	}
	return 0;
}
