/* compress_scumm_bun - compressor for bundle files
 * Copyright (C) 2004-2006  The ScummVM Team
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
 * $URL$
 * $Id$
 *
 */


#ifndef COMPRESS_SCUMM_BUN_H
#define COMPRESS_SCUMM_BUN_H

#include "compress.h"

class CompressScummBun : public CompressionTool {
public:
	CompressScummBun(const std::string &name = "compress_scumm_bun");
	
	virtual bool inspectInput(const Filename &filename);

	virtual void execute();


	struct BundleAudioTable {
		char filename[24];
		int size;
		int offset;
	};

protected:

	File _waveTmpFile;
	int32 _waveDataSize;
	BundleAudioTable *_bundleTable;
	BundleAudioTable _cbundleTable[10000]; // difficult to calculate
	int32 _cbundleCurIndex;

	int32 compDecode(byte *src, byte *dst);
	int32 decompressCodec(int32 codec, byte *comp_input, byte *comp_output, int32 input_size);
	void encodeWaveWithFlac(char *filename);
	void encodeWaveWithOgg(char *filename);
	void encodeWaveWithLame(char *filename);
	void writeWaveHeader(int s_size, int rate, int chan);
	void writeToTempWave(char *fileName, byte *output_data, unsigned int size);
	byte *decompressBundleSound(int index, File  &input, int32 &finalSize);
	byte *convertTo16bit(byte *ptr, int inputSize, int &outputSize, int bits, int freq, int channels);
	void countMapElements(byte *ptr, int &numRegions, int &numJumps, int &numSyncs, int &numMarkers);
	void writeRegions(byte *ptr, int bits, int freq, int channels, const char *dir, char *filename, File &output);
	void recalcRegions(int32 &value, int bits, int freq, int channels);
	void writeToRMAPFile(byte *ptr, File &output, char *filename, int &offsetData, int &bits, int &freq, int &channels);

};

#endif


