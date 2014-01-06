/* compress_sci - resource.aud/resource.sfx compressor
 * Copyright (C) 2009 The ScummVM Team
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

#ifndef COMPRESS_SCI_H
#define COMPRESS_SCI_H

#include "compress.h"
#include <map>

enum SciResourceDataType {
	kSciResourceDataTypeUnknown	= 0,
	kSciResourceDataTypeWAVE	= 1,
	kSciResourceDataTypeSOL		= 2,
	kSciResourceTypeTypeSync	= 3,
	kSciResourceDataTypeRaw		= 4
};

class CompressSci : public CompressionTool {
public:
	CompressSci(const std::string &name = "compress_sci");

	virtual void execute();

protected:
	SciResourceDataType detectData(byte *header, bool compressMode);
	void compressData(SciResourceDataType dataType);
	uint parseRawAudioMap();

	Common::File _input, _output;
	int _inputOffset;
	int _inputEndOffset;
	int _inputSize;
	int _outputOffset;
	bool _rawAudio;
	std::map<uint32,uint32> _rawAudioMap;
};

#endif
