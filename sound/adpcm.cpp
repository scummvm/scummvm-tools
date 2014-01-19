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

#include "sound/adpcm.h"
#include "common/endian.h"
#include "common/util.h"

#include <assert.h>
#include <string.h>

namespace Audio {

// TODO: Switch from a SeekableReadStream to a plain ReadStream. This requires
// some internal refactoring but is definitely possible and will increase the
// flexibility of this code.
class ADPCMInputStream : public AudioStream {
private:
	Common::File *_stream;
	uint32 _endpos;
	int _channels;
	typesADPCM _type;
	uint32 _blockAlign;
	uint32 _blockPos;
	int _blockLen;
	int _rate;

	struct ADPCMChannelStatus {
		byte predictor;
		int16 delta;
		int16 coeff1;
		int16 coeff2;
		int16 sample1;
		int16 sample2;
	};

	struct adpcmStatus {
		// IMA
		int32 last;
		int32 stepIndex;

		// MS ADPCM
		ADPCMChannelStatus ch[2];
	} _status;

	int16 stepAdjust(byte);
	int16 decodeOKI(byte);
	int16 decodeMSIMA(byte);
	int16 decodeMS(ADPCMChannelStatus *c, byte);

public:
	ADPCMInputStream(Common::File *stream, uint32 size, typesADPCM type, int rate, int channels = 2, uint32 blockAlign = 0);
	~ADPCMInputStream() {}

	int readBuffer(int16 *buffer, const int numSamples);
	int readBufferOKI(int16 *buffer, const int numSamples);
	int readBufferMSIMA1(int16 *buffer, const int numSamples);
	int readBufferMSIMA2(int16 *buffer, const int numSamples);
	int readBufferMS(int channels, int16 *buffer, const int numSamples);

	bool endOfData() const { return (_stream->eos() || _stream->pos() >= (int)_endpos); }
	bool isStereo() const	{ return false; }
	int getRate() const	{ return _rate; }
};

// Routines to convert 12 bit linear samples to the
// Dialogic or Oki ADPCM coding format aka VOX.
// See also <http://www.comptek.ru/telephony/tnotes/tt1-13.html>
//
// In addition, also MS IMA ADPCM is supported. See
//   <http://wiki.multimedia.cx/index.php?title=Microsoft_IMA_ADPCM>.

ADPCMInputStream::ADPCMInputStream(Common::File *stream, uint32 size, typesADPCM type, int rate, int channels, uint32 blockAlign)
	: _stream(stream), _channels(channels), _type(type), _blockAlign(blockAlign), _rate(rate) {

	_status.last = 0;
	_status.stepIndex = 0;
	memset(_status.ch, 0, sizeof(_status.ch));
	_endpos = stream->pos() + size;
	_blockPos = _blockLen = 0;

	if (type == kADPCMMSIma && blockAlign == 0)
		error("ADPCMInputStream(): blockAlign isn't specifiled for MS IMA ADPCM");
	if (type == kADPCMMS && blockAlign == 0)
		error("ADPCMInputStream(): blockAlign isn't specifiled for MS ADPCM");
}

int ADPCMInputStream::readBuffer(int16 *buffer, const int numSamples) {
	switch (_type) {
	case kADPCMOki:
		return readBufferOKI(buffer, numSamples);
		break;
	case kADPCMMSIma:
		if (_channels == 1)
			return readBufferMSIMA1(buffer, numSamples);
		else
			return readBufferMSIMA2(buffer, numSamples);
		break;
	case kADPCMMS:
		return readBufferMS(_channels, buffer, numSamples);
		break;
	default:
		error("Unsupported ADPCM encoding");
		break;
	}
	return 0;
}

int ADPCMInputStream::readBufferOKI(int16 *buffer, const int numSamples) {
	int samples;
	byte data;

	assert(numSamples % 2 == 0);

	for (samples = 0; samples < numSamples && !_stream->eos() && _stream->pos() < (int)_endpos; samples += 2) {
		data = _stream->readByte();
		WRITE_LE_UINT16(buffer + samples,     decodeOKI((data >> 4) & 0x0f));
		WRITE_LE_UINT16(buffer + samples + 1, decodeOKI(data & 0x0f));
	}
	return samples;
}


int ADPCMInputStream::readBufferMSIMA1(int16 *buffer, const int numSamples) {
	int samples;
	byte data;

	assert(numSamples % 2 == 0);

	samples = 0;

	while (samples < numSamples && !_stream->eos() && _stream->pos() < (int)_endpos) {
		if (_blockPos == _blockAlign) {
			// read block header
			_status.last = _stream->readSint16LE();
			_status.stepIndex = _stream->readSint16LE();
			_blockPos = 4;
		}

		for (; samples < numSamples && _blockPos < _blockAlign && !_stream->eos() && _stream->pos() < (int)_endpos; samples += 2) {
			data = _stream->readByte();
			_blockPos++;
			WRITE_LE_UINT16(buffer + samples,     decodeMSIMA(data & 0x0f));
			WRITE_LE_UINT16(buffer + samples + 1, decodeMSIMA((data >> 4) & 0x0f));
		}
	}
	return samples;
}


// Microsoft as usual tries to implement it differently. This method
// is used for stereo data.
int ADPCMInputStream::readBufferMSIMA2(int16 *buffer, const int numSamples) {
	int samples;
	uint32 data;
	int nibble;

	for (samples = 0; samples < numSamples && !_stream->eos() && _stream->pos() < (int)_endpos;) {
		for (int channel = 0; channel < 2; channel++) {
			data = _stream->readUint32LE();

			for (nibble = 0; nibble < 8; nibble++) {
				byte k = ((data & 0xf0000000) >> 28);
				WRITE_LE_UINT16(buffer + samples + channel + nibble * 2, decodeMSIMA(k));
				data <<= 4;
			}
		}
		samples += 16;
	}
	return samples;
}

static const int MSADPCMAdaptCoeff1[] = {
	256, 512, 0, 192, 240, 460, 392
};

static const int MSADPCMAdaptCoeff2[] = {
	0, -256, 0, 64, 0, -208, -232
};

int ADPCMInputStream::readBufferMS(int channels, int16 *buffer, const int numSamples) {
	int samples;
	byte data;
	int stereo = channels - 1; // We use it in index

	samples = 0;

	while (samples < numSamples && !_stream->eos() && _stream->pos() < (int)_endpos) {
		if (_blockPos == _blockAlign) {
			// read block header
			_status.ch[0].predictor = CLIP(_stream->readByte(), (byte)0, (byte)6);
			_status.ch[0].coeff1 = MSADPCMAdaptCoeff1[_status.ch[0].predictor];
			_status.ch[0].coeff2 = MSADPCMAdaptCoeff2[_status.ch[0].predictor];
			if (stereo) {
				_status.ch[1].predictor = CLIP(_stream->readByte(), (byte)0, (byte)6);
				_status.ch[1].coeff1 = MSADPCMAdaptCoeff1[_status.ch[1].predictor];
				_status.ch[1].coeff2 = MSADPCMAdaptCoeff2[_status.ch[1].predictor];
			}

			_status.ch[0].delta = _stream->readSint16LE();
			if (stereo)
				_status.ch[1].delta = _stream->readSint16LE();

			buffer[samples++] = _status.ch[0].sample1 = _stream->readSint16LE();
			if (stereo)
				buffer[samples++] = _status.ch[1].sample1 = _stream->readSint16LE();

			buffer[samples++] = _status.ch[0].sample2 = _stream->readSint16LE();
			if (stereo)
				buffer[samples++] = _status.ch[1].sample2 = _stream->readSint16LE();

			_blockPos = channels * 7;
		}


		for (; samples < numSamples && _blockPos < _blockAlign && !_stream->eos() && _stream->pos() < (int)_endpos; samples += 2) {
			data = _stream->readByte();
			_blockPos++;
			WRITE_LE_UINT16(buffer + samples,     decodeMS(&_status.ch[0], (data >> 4) & 0x0f));
			WRITE_LE_UINT16(buffer + samples + 1, decodeMS(&_status.ch[stereo], data & 0x0f));
		}
	}

	return samples;
}


// adjust the step for use on the next sample.
int16 ADPCMInputStream::stepAdjust(byte code) {
	static const int16 adjusts[] = {-1, -1, -1, -1, 2, 4, 6, 8};

	return adjusts[code & 0x07];
}

static const int16 okiStepSize[49] = {
	  16,   17,   19,   21,   23,   25,   28,   31,
	  34,   37,   41,   45,   50,   55,   60,   66,
	  73,   80,   88,   97,  107,  118,  130,  143,
	 157,  173,  190,  209,  230,  253,  279,  307,
	 337,  371,  408,  449,  494,  544,  598,  658,
	 724,  796,  876,  963, 1060, 1166, 1282, 1411,
	1552
};

// Decode Linear to ADPCM
int16 ADPCMInputStream::decodeOKI(byte code) {
	int16 diff, E, samp;

	E = (2 * (code & 0x7) + 1) * okiStepSize[_status.stepIndex] / 8;
	diff = (code & 0x08) ? -E : E;
	samp = _status.last + diff;

    // Clip the values to +/- 2^11 (supposed to be 12 bits)
	if (samp > 2048)
		samp = 2048;
	if (samp < -2048)
		samp = -2048;

	_status.last = samp;
	_status.stepIndex += stepAdjust(code);
	if (_status.stepIndex < 0)
		_status.stepIndex = 0;
	if (_status.stepIndex > ARRAYSIZE(okiStepSize) - 1)
		_status.stepIndex = ARRAYSIZE(okiStepSize) - 1;

	// * 16 effectively converts 12-bit input to 16-bit output
	return samp * 16;
}


static const uint16 imaStepTable[89] = {
		7,	  8,	9,	 10,   11,	 12,   13,	 14,
	   16,	 17,   19,	 21,   23,	 25,   28,	 31,
	   34,	 37,   41,	 45,   50,	 55,   60,	 66,
	   73,	 80,   88,	 97,  107,	118,  130,	143,
	  157,	173,  190,	209,  230,	253,  279,	307,
	  337,	371,  408,	449,  494,	544,  598,	658,
	  724,	796,  876,	963, 1060, 1166, 1282, 1411,
	 1552, 1707, 1878, 2066, 2272, 2499, 2749, 3024,
	 3327, 3660, 4026, 4428, 4871, 5358, 5894, 6484,
	 7132, 7845, 8630, 9493,10442,11487,12635,13899,
	15289,16818,18500,20350,22385,24623,27086,29794,
	32767
};

int16 ADPCMInputStream::decodeMSIMA(byte code) {
	int32 diff, E, samp;

	E = (2 * (code & 0x7) + 1) * imaStepTable[_status.stepIndex] / 8;
	diff = (code & 0x08) ? -E : E;
	samp = _status.last + diff;

	if (samp < -0x8000)
		samp = -0x8000;
	else if (samp > 0x7fff)
		samp = 0x7fff;

	_status.last = samp;

	_status.stepIndex += stepAdjust(code);
	if (_status.stepIndex < 0)
		_status.stepIndex = 0;
	if (_status.stepIndex > ARRAYSIZE(imaStepTable) - 1)
		_status.stepIndex = ARRAYSIZE(imaStepTable) - 1;

	return samp;
}

static const int MSADPCMAdaptationTable[] = {
	230, 230, 230, 230, 307, 409, 512, 614,
	768, 614, 512, 409, 307, 230, 230, 230
};


int16 ADPCMInputStream::decodeMS(ADPCMChannelStatus *c, byte code) {
	int32 predictor;

	predictor = (((c->sample1) * (c->coeff1)) + ((c->sample2) * (c->coeff2))) / 256;
	predictor += (signed)((code & 0x08) ? (code - 0x10) : (code)) * c->delta;

	if (predictor < -0x8000)
		predictor = -0x8000;
	else if (predictor > 0x7fff)
		predictor = 0x7fff;

	c->sample2 = c->sample1;
	c->sample1 = predictor;
	c->delta = (MSADPCMAdaptationTable[(int)code] * c->delta) >> 8;

	if (c->delta < 16)
		c->delta = 16;

	return (int16)predictor;
}

AudioStream *makeADPCMStream(Common::File *stream, uint32 size, typesADPCM type, int rate, int channels, uint32 blockAlign) {
	return new ADPCMInputStream(stream, size, type, rate, channels, blockAlign);
}

} // End of namespace Audio
