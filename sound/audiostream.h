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

#ifndef SOUND_AUDIOSTREAM_H
#define SOUND_AUDIOSTREAM_H

#include "common/scummsys.h"

namespace Audio {

class Mixer {
public:
	enum {
		/** unsigned samples (default: signed) */
		FLAG_UNSIGNED = 1 << 0,

		/** sound is 16 bits wide (default: 8bit) */
		FLAG_16BITS = 1 << 1,

		/** sample is little endian (default: big endian) */
		FLAG_LITTLE_ENDIAN = 1 << 2,

		/** sound is in stereo (default: mono) */
		FLAG_STEREO = 1 << 3,

		/** reverse the left and right stereo channel */
		FLAG_REVERSE_STEREO = 1 << 4,

		/** sound buffer is freed automagically at the end of playing */
		FLAG_AUTOFREE = 1 << 5,

		/** loop the audio */
		FLAG_LOOP = 1 << 6
	};
};

/**
 * Generic input stream for the resampling code.
 */
class AudioStream {
public:
	virtual ~AudioStream() {}

	/**
	 * Fill the given buffer with up to numSamples samples.
	 * Returns the actual number of samples read, or -1 if
	 * a critical error occured (note: you *must* check if
	 * this value is less than what you requested, this can
	 * happen when the stream is fully used up).
	 *
	 * Data has to be in native endianess, 16 bit per sample, signed.
	 * For stereo stream, buffer will be filled with interleaved
	 * left and right channel samples, starting with a left sample.
	 * Furthermore, the samples in the left and right are summed up.
	 * So if you request 4 samples from a stereo stream, you will get
	 * a total of two left channel and two right channel samples.
	 */
	virtual int readBuffer(int16 *buffer, const int numSamples) = 0;

	/** Is this a stereo stream? */
	virtual bool isStereo() const = 0;

	/**
	 * End of data reached? If this returns true, it means that at this
	 * time there is no data available in the stream. However there may be
	 * more data in the future.
	 * This is used by e.g. a rate converter to decide whether to keep on
	 * converting data or stop.
	 */
	virtual bool endOfData() const = 0;

	/**
	 * End of stream reached? If this returns true, it means that all data
	 * in this stream is used up and no additional data will appear in it
	 * in the future.
	 * This is used by the mixer to decide whether a given stream shall be
	 * removed from the list of active streams (and thus be destroyed).
	 * By default this maps to endOfData()
	 */
	virtual bool endOfStream() const { return endOfData(); }

	/** Sample rate of the stream. */
	virtual int getRate() const = 0;
};

AudioStream *makeLinearInputStream(int rate, byte flags, const byte *ptr, uint32 len, uint32 loopOffset, uint32 loopLen);

/**
 * An audio stream to which additional data can be appended on-the-fly.
 * Used by SMUSH, iMuseDigital, and the Kyrandia 3 VQA player.
 */
class AppendableAudioStream : public Audio::AudioStream {
public:
	virtual void append(const byte *data, uint32 len) = 0;
	virtual void finish() = 0;
};

AppendableAudioStream *makeAppendableAudioStream(int rate, byte _flags, uint32 len);


// This used to be an inline template function, but
// buggy template function handling in MSVC6 forced
// us to go with the macro approach. So far this is
// the only template function that MSVC6 seemed to
// compile incorrectly. Knock on wood.
#define READ_ENDIAN_SAMPLE(is16Bit, isUnsigned, ptr, isLE) \
	((is16Bit ? (isLE ? READ_LE_UINT16(ptr) : READ_BE_UINT16(ptr)) : (*ptr << 8)) ^ (isUnsigned ? 0x8000 : 0))


} // End of namespace Audio

#endif
