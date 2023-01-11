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

#ifndef TWINE_HQR_H
#define TWINE_HQR_H

#include "common/stream.h"
#include "common/file.h"

namespace TwinE {

class TwinEEngine;

/**
 * High Quality Resource
 *
 * https://web.archive.org/web/20181218233826/http://lbafileinfo.kazekr.net/index.php?title=High_quality_resource
 */
namespace HQR {


/**
 * Get a HQR entry pointer
 * @param ptr pointer to save the entry
 * @param filename HQR file name
 * @param index entry index to extract
 * @return entry real size
 */
int32 getEntry(uint8 *ptr, const Common::Filename &filename, int32 index);

/**
 * Get a HQR entry pointer
 * @param filename HQR file name
 * @param index entry index to extract
 * @return entry real size
 */
int32 entrySize(const Common::Filename &filename, int32 index);

/**
 * Get a HQR total number of entries
 * @param filename HQR file name
 * @return total number of entries
 */
int32 numEntries(const Common::Filename &filename);

/**
 * Get a HQR entry pointer with memory allocation
 * @param ptr pointer to save the entry. This pointer is automatically freed and therefore must be initialized
 * to @c nullptr on the first run.
 * @param filename HQR file name
 * @param index entry index to extract
 * @return entry real size
 */
int32 getAllocEntry(uint8 **ptr, const Common::Filename &filename, int32 index);

/**
 * Get a HQR entry pointer
 * @param ptr pointer to save the entry
 * @param filename HQR file name
 * @param index entry index to extract
 * @return entry real size
 */
int32 getVoxEntry(uint8 *ptr, const Common::Filename &filename, int32 index, int32 hiddenIndex);
/**
 * Get a HQR entry pointer with memory allocation
 * @param ptr pointer to save the entry. This pointer is automatically freed and therefore must be initialized
 * to @c nullptr on the first run.
 * @param filename HQR file name
 * @param index entry index to extract
 * @return entry real size
 */
int32 getAllocVoxEntry(uint8 **ptr, const Common::Filename &filename, int32 index, int32 hiddenIndex);

Common::SeekableReadStream *makeReadStream(const Common::Filename &filename, int index);

} // namespace HQR

} // namespace TwinE

#endif
