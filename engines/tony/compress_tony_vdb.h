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

#ifndef COMPRESS_TONY_VDB_H
#define COMPRESS_TONY_VDB_H

#include "compress_tony.h"
#include "compress.h"

struct VoiceHeader {
	int _offset;
	int _code;
	int _parts;
};

class CompressTonyVDB : public CompressionTool {
public:
	CompressTonyVDB(const std::string &name = "compress_tony_vdb");

	virtual void execute();

protected:
	Common::File _input_vdb, _output_enc;
	ADPCMStatus _status;
	uint32 _sampleSize;
	uint32 _rate;
	byte *_inBuffer;
	int16 *_outBuffer;
	uint32 _uncompressedSize;

	int16 decodeIMA(byte code, int channel);
	bool convertTonyADPCMSample();
};

#endif
