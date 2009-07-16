/* compress_scumm_san - compressor for smush san files
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


#ifndef COMPRESS_SCUMM_SAN_H
#define COMPRESS_SCUMM_SAN_H

#include "compress.h"


#define COMPRESS_SCUMM_SAN_MAX_TRACKS 150

class CompressScummSan : public CompressionTool {
public:
	CompressScummSan(const std::string &name = "compress_scumm_san");
	
	virtual bool inspectInput(const Filename &filename);

	virtual void execute();

	struct FrameInfo {
		int32 frameSize;
		int32 offsetOutput;
		int32 fobjDecompressedSize;
		int32 fobjCompressedSize;
		int32 lessIACTSize;
		int32 lessPSADSize;
	};

	struct AudioTrackInfo {
		int animFrame;
		int trackId;
		int bits;
		bool stereo;
		int freq;
		bool used;
		File file;
		int waveDataSize;
		int *volumes;
		int *pans;
		int *sizes;
		int nbframes;
		int countFrames;
		int lastFrame;
		int32 sdatSize;
	};

protected:
	byte _IACToutput[0x1000];
	int _IACTpos;
	File _waveTmpFile;
	int32 _waveDataSize;
	AudioTrackInfo _audioTracks[COMPRESS_SCUMM_SAN_MAX_TRACKS];

	void encodeSanWaveWithOgg(char *filename);
	void encodeSanWaveWithLame(char *filename);
	void writeWaveHeader(int s_size);
	void writeToTempWaveFile(char *fileName, byte *output_data, unsigned int size);
	void decompressComiIACT(char *fileName, byte *output_data, byte *d_src, int bsize);
	void handleComiIACT(File &input, int size, const char *outputDir, const char *inputFilename);
	AudioTrackInfo *allocAudioTrack(int trackId, int frame);
	AudioTrackInfo *findAudioTrack(int trackId);
	void flushTracks(int frame);
	void prepareForMixing(const char *outputDir, const char *inputFilename);
	void mixing(const char *outputDir, const char *inputFilename, int frames, int fps);
	void handleMapChunk(AudioTrackInfo *audioTrack, File &input);
	int32 handleSaudChunk(AudioTrackInfo *audioTrack, File &input);
	void handleAudioTrack(int index, int trackId, int frame, int nbframes, File &input, const char *outputDir,
		const char *inputFilename, int &size, int volume, int pan, bool iact);
	void handleDigIACT(File &input, int size, const char *outputDir, const char *inputFilename,int flags, int track_flags, int frame);
	void handlePSAD(File &input, int size, const char *outputDir, const char *inputFilename, int frame);
};

#endif


