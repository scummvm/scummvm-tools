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

#ifndef SOUND_WAVE_H
#define SOUND_WAVE_H

#include "common/scummsys.h"

namespace Common {
	class File;
}

namespace Audio {

class AudioStream;

/**
 * Try to load a WAVE from the given seekable stream. Returns true if
 * successful. In that case, the stream's seek position will be set to the
 * start of the audio data, and size, rate and flags contain information
 * necessary for playback. Currently this function only supports uncompressed
 * raw PCM data as well as IMA ADPCM.
 */
extern bool loadWAVFromStream(Common::File &stream, int &size, int &rate, byte &flags, uint16 *wavType = 0, int *blockAlign = 0);

/**
 * Try to load a WAVE from the given seekable stream and create an AudioStream
 * from that data. Currently this function only supports uncompressed raw PCM
 * data as well as IMA ADPCM.
 *
 * This function uses loadWAVFromStream() internally.
 */
AudioStream *makeWAVStream(Common::File &stream);

} // End of namespace Audio

#endif
