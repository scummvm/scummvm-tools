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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>
#include <stdio.h>

#include "compress_scumm_san.h"
#include "common/endian.h"

void CompressScummSan::encodeSanWaveWithOgg(const std::string &filename) {
	std::string fbuf = filename + ".raw";
	Common::Filename fbuf2(filename.c_str());

	fbuf2.setExtension(".ogg");
	encodeAudio(fbuf.c_str(), true, 22050, fbuf2.getFullPath().c_str(), AUDIO_VORBIS);
}

void CompressScummSan::encodeSanWaveWithLame(const std::string &filename) {
	std::string fbuf = filename + ".raw";
	Common::Filename fbuf2(filename.c_str());

	fbuf2.setExtension(".mp3");
	encodeAudio(fbuf.c_str(), true, 22050, fbuf2.getFullPath().c_str(), AUDIO_MP3);
}

void CompressScummSan::writeToTempWaveFile(const std::string &fileName, byte *output_data, unsigned int size) {
	if (!_waveTmpFile.isOpen()) {
		_waveTmpFile.open(fileName, "wb");
		if (!_waveTmpFile.isOpen()) {
			error("error writing temp wave file");
		}
		_waveDataSize = 0;
	}
	for (unsigned int j = 0; j < size - 1; j += 2) {
		byte tmp = output_data[j + 0];
		output_data[j + 0] = output_data[j + 1];
		output_data[j + 1] = tmp;
	}

	_waveTmpFile.write(output_data, size);
	_waveDataSize += size;
}

void CompressScummSan::decompressComiIACT(const std::string &fileName, byte *output_data, byte *d_src, int bsize) {
	byte value;

	while (bsize > 0) {
		if (_IACTpos >= 2) {
			int32 len = READ_BE_UINT16(_IACToutput) + 2;
			len -= _IACTpos;
			if (len > bsize) {
				memcpy(_IACToutput + _IACTpos, d_src, bsize);
				_IACTpos += bsize;
				bsize = 0;
			} else {
				memcpy(_IACToutput + _IACTpos, d_src, len);
				byte *dst = output_data;
				byte *d_src2 = _IACToutput;
				d_src2 += 2;
				int32 count = 1024;
				byte variable1 = *d_src2++;
				byte variable2 = variable1 / 16;
				variable1 &= 0x0f;
				do {
					value = *(d_src2++);
					if (value == 0x80) {
						*dst++ = *d_src2++;
						*dst++ = *d_src2++;
					} else {
						int16 val = (int8)value << variable2;
						*dst++ = val >> 8;
						*dst++ = (byte)(val);
					}
					value = *(d_src2++);
					if (value == 0x80) {
						*dst++ = *d_src2++;
						*dst++ = *d_src2++;
					} else {
						int16 val = (int8)value << variable1;
						*dst++ = val >> 8;
						*dst++ = (byte)(val);
					}
				} while (--count);
				writeToTempWaveFile(fileName, output_data, 0x1000);
				bsize -= len;
				d_src += len;
				_IACTpos = 0;
			}
		} else {
			if (bsize > 1 && _IACTpos == 0) {
				*(_IACToutput + 0) = *d_src++;
				_IACTpos = 1;
				bsize--;
			}
			*(_IACToutput + _IACTpos) = *d_src++;
			_IACTpos++;
			bsize--;
		}
	}
}

void CompressScummSan::handleComiIACT(Common::File &input, int size, const std::string &outputDir, const std::string &inputFilename) {
	input.seek(10, SEEK_CUR);
	int bsize = size - 18;
	byte output_data[0x1000];
	byte *src = (byte *)malloc(bsize);
	input.read_throwsOnError(src, bsize);

	const std::string tmpPath = outputDir + inputFilename + ".raw";
	decompressComiIACT(tmpPath, output_data, src, bsize);

	free(src);
}

CompressScummSan::AudioTrackInfo *CompressScummSan::allocAudioTrack(int trackId, int frame) {
	for (int l = 0; l < COMPRESS_SCUMM_SAN_MAX_TRACKS; l++) {
		if ((_audioTracks[l].animFrame != frame) && (_audioTracks[l].trackId != trackId) && (!_audioTracks[l].used))
			return &_audioTracks[l];
	}
	return NULL;
}

CompressScummSan::AudioTrackInfo *CompressScummSan::findAudioTrack(int trackId) {
	for (int l = 0; l < COMPRESS_SCUMM_SAN_MAX_TRACKS; l++) {
		if (_audioTracks[l].trackId == trackId && _audioTracks[l].used && _audioTracks[l].file.isOpen())
			return &_audioTracks[l];
	}
	return NULL;
}

void CompressScummSan::flushTracks(int frame) {
	for (int l = 0; l < COMPRESS_SCUMM_SAN_MAX_TRACKS; l++) {
		if (_audioTracks[l].used && _audioTracks[l].file.isOpen() && (frame - _audioTracks[l].lastFrame) > 1) {
			_audioTracks[l].file.close();
		}
	}
}

void CompressScummSan::prepareForMixing(const std::string &outputDir, const std::string &inputFilename) {
	char filename[200];

	print("Decompressing tracks files...\n");
	for (int l = 0; l < COMPRESS_SCUMM_SAN_MAX_TRACKS; l++) {
		if (_audioTracks[l].used) {
			_audioTracks[l].file.close();

			sprintf(filename, "%s%s_%04d_%03d.tmp", outputDir.c_str(), inputFilename.c_str(), _audioTracks[l].animFrame, _audioTracks[l].trackId);
			_audioTracks[l].file.open(filename, "rb");
			_audioTracks[l].file.seek(0, SEEK_END);
			int fileSize = _audioTracks[l].file.pos();
			_audioTracks[l].file.seek(0, SEEK_SET);
			byte *audioBuf = (byte *)malloc(fileSize);
			_audioTracks[l].file.read_throwsOnError(audioBuf, fileSize);
			_audioTracks[l].file.close();

			int outputSize = fileSize;
			if (_audioTracks[l].bits == 8)
				outputSize *= 2;
			if (_audioTracks[l].bits == 12)
				outputSize = (outputSize / 3) * 4;
			if (!_audioTracks[l].stereo)
				outputSize *= 2;
			if (_audioTracks[l].freq == 11025)
				outputSize *= 2;

			byte *outputBuf = (byte *)malloc(outputSize);
			if (_audioTracks[l].bits == 8) {
				byte *buf = outputBuf;
				byte *src = audioBuf;
				for (int i = 0; i < fileSize; i++) {
					uint16 val = (*src++ - 0x80) << 8;
					*buf++ = (byte)val;
					*buf++ = (byte)(val >> 8);
					if (_audioTracks[l].freq == 11025) {
						*buf++ = (byte)val;
						*buf++ = (byte)(val >> 8);
					}
					if (!_audioTracks[l].stereo) {
						*buf++ = (byte)val;
						*buf++ = (byte)(val >> 8);
						if (_audioTracks[l].freq == 11025) {
							*buf++ = (byte)val;
							*buf++ = (byte)(val >> 8);
						}
					}
				}
			}
			if (_audioTracks[l].bits == 12) {
				int loop_size = fileSize / 3;
				byte *decoded = outputBuf;
				byte *source = audioBuf;
				uint32 value;

				while (loop_size--) {
					byte v1 =  *source++;
					byte v2 =  *source++;
					byte v3 =  *source++;
					value = ((((v2 & 0x0f) << 8) | v1) << 4) - 0x8000;
					*decoded++ = (byte)(value & 0xff);
					*decoded++ = (byte)((value >> 8) & 0xff);
					if (_audioTracks[l].freq == 11025) {
						*decoded++ = (byte)(value & 0xff);
						*decoded++ = (byte)((value >> 8) & 0xff);
					}
					if (!_audioTracks[l].stereo) {
						*decoded++ = (byte)(value & 0xff);
						*decoded++ = (byte)((value >> 8) & 0xff);
						if (_audioTracks[l].freq == 11025) {
							*decoded++ = (byte)(value & 0xff);
							*decoded++ = (byte)((value >> 8) & 0xff);
						}
					}
					value = ((((v2 & 0xf0) << 4) | v3) << 4) - 0x8000;
					*decoded++ = (byte)(value & 0xff);
					*decoded++ = (byte)((value >> 8) & 0xff);
					if (_audioTracks[l].freq == 11025) {
						*decoded++ = (byte)(value & 0xff);
						*decoded++ = (byte)((value >> 8) & 0xff);
					}
					if (!_audioTracks[l].stereo) {
						*decoded++ = (byte)(value & 0xff);
						*decoded++ = (byte)((value >> 8) & 0xff);
						if (_audioTracks[l].freq == 11025) {
							*decoded++ = (byte)(value & 0xff);
							*decoded++ = (byte)((value >> 8) & 0xff);
						}
					}
				}
			}

			free(audioBuf);
			_audioTracks[l].file.open(filename, "wb");
			_audioTracks[l].file.write(outputBuf, outputSize);
			free(outputBuf);
		}
	}
}

#define ST_SAMPLE_MAX 0x7fffL
#define ST_SAMPLE_MIN (-ST_SAMPLE_MAX - 1L)

static inline void clampedAdd(int16& a, int b) {
	register int val;
	val = a + b;

	if (val > ST_SAMPLE_MAX)
		val = ST_SAMPLE_MAX;
	else if (val < ST_SAMPLE_MIN)
		val = ST_SAMPLE_MIN;

	a = val;
}
void CompressScummSan::mixing(const std::string &outputDir, const std::string &inputFilename, int frames, int fps) {
	int l, r, z;

	const std::string wavPath = outputDir + inputFilename + ".raw";
	Common::File &wavFile(_waveTmpFile);

	wavFile.open(wavPath.c_str(), "wb+");

	int frameAudioSize = 0;
	if (fps == 12) {
		frameAudioSize = 7352;
	} else if (fps == 10) {
		frameAudioSize = 8802;
	} else {
		error("Unsupported fps value %d", fps);
	}

	print("Creating silent wav file...\n");
	for (l = 0; l < frameAudioSize * frames; l++) {
		wavFile.writeByte(0);
	}

	print("Mixing tracks into wav file...\n");
	for (l = 0; l < COMPRESS_SCUMM_SAN_MAX_TRACKS; l++) {
		if (_audioTracks[l].used) {
			char filename[200];
			sprintf(filename, "%s%s_%04d_%03d.tmp", outputDir.c_str(), inputFilename.c_str(), _audioTracks[l].animFrame, _audioTracks[l].trackId);
			_audioTracks[l].file.open(filename, "rb");
			const uint32 fileSize = _audioTracks[l].file.size();
			byte *tmpBuf = (byte *)malloc(fileSize);
			_audioTracks[l].file.read_throwsOnError(tmpBuf, fileSize);
			_audioTracks[l].file.close();
			Common::removeFile(filename);

			byte *wavBuf = (byte *)malloc(fileSize);
			memset(wavBuf, 0, fileSize);
			wavFile.seek(frameAudioSize * _audioTracks[l].animFrame, SEEK_SET);
			try {
				wavFile.read_throwsOnError(wavBuf, fileSize);
			} catch (...) {
				// ... pass through
				wavFile.clearErr();
			}

			int offset = 0;
			for (z = 0; z < _audioTracks[l].countFrames; z++) {
				int length = _audioTracks[l].sizes[z];
				if (length == 0) {
					warning("zero length audio frame");
					break;
				}
				if (_audioTracks[l].sdatSize != 0 && (offset + length) > _audioTracks[l].sdatSize) {
					length = _audioTracks[l].sdatSize - offset;
				}
				int volume = _audioTracks[l].volumes[z];
				for (r = 0; r < length; r += 4) {
					int16 wavSampleL = (int16)READ_LE_UINT16(wavBuf + offset + r + 0);
					int16 wavSampleR = (int16)READ_LE_UINT16(wavBuf + offset + r + 2);
					int32 tmpSampleL = (int16)READ_LE_UINT16(tmpBuf + offset + r + 0);
					int32 tmpSampleR = (int16)READ_LE_UINT16(tmpBuf + offset + r + 2);
					tmpSampleL = (tmpSampleL * volume) / 255;
					tmpSampleR = (tmpSampleR * volume) / 255;
					clampedAdd(wavSampleL, tmpSampleL);
					clampedAdd(wavSampleR, tmpSampleR);
					WRITE_LE_UINT16(wavBuf + offset + r + 0, wavSampleL);
					WRITE_LE_UINT16(wavBuf + offset + r + 2, wavSampleR);
				}
				offset += length;
			}
			wavFile.seek(frameAudioSize * _audioTracks[l].animFrame, SEEK_SET);
			wavFile.write(wavBuf, fileSize);

			free(wavBuf);
			free(tmpBuf);
		}
	}

	_waveDataSize = frames * frameAudioSize;
}

void CompressScummSan::handleMapChunk(AudioTrackInfo *audioTrack, Common::File &input) {
	uint32 tag;
	int32 size;
	tag = input.readUint32BE();
	size = input.readUint32BE();
	assert(tag == 'iMUS');
	tag = input.readUint32BE();
	size = input.readUint32BE();
	assert(tag == 'MAP ');
	tag = input.readUint32BE();
	size = input.readUint32BE();
	assert(tag == 'FRMT');
	input.seek(8, SEEK_CUR);
	audioTrack->bits = input.readUint32BE();
	audioTrack->freq = input.readUint32BE();
	int chan = input.readUint32BE();
	if (chan == 2)
		audioTrack->stereo = true;
	tag = input.readUint32BE();
	size = input.readUint32BE();
	if (tag == 'TEXT') {
		input.seek(size, SEEK_CUR);
		tag = input.readUint32BE();
		size = input.readUint32BE();
		if (tag == 'TEXT') {
			input.seek(size, SEEK_CUR);
			tag = input.readUint32BE();
			size = input.readUint32BE();
		}
	}
	assert(tag == 'REGN');
	input.seek(8, SEEK_CUR);
	tag = input.readUint32BE();
	size = input.readUint32BE();
	if (tag == 'TEXT') {
		input.seek(size, SEEK_CUR);
		tag = input.readUint32BE();
		size = input.readUint32BE();
		if (tag == 'REGN') {
			input.seek(8, SEEK_CUR);
			tag = input.readUint32BE();
			size = input.readUint32BE();
		}
	}
	if (tag == 'STOP') {
		input.seek(4, SEEK_CUR);
		tag = input.readUint32BE();
		size = input.readUint32BE();
	}
	assert(tag == 'DATA');
}

int32 CompressScummSan::handleSaudChunk(AudioTrackInfo *audioTrack, Common::File &input) {
	uint32 tag;
	int32 size;
	tag = input.readUint32BE();
	size = input.readUint32BE();
	assert(tag == 'SAUD');
	tag = input.readUint32BE();
	size = input.readUint32BE();
	assert(tag == 'STRK');
	input.seek(size, SEEK_CUR);
	tag = input.readUint32BE();
	size = input.readUint32BE();
	assert(tag == 'SDAT');
	return size;
}

void CompressScummSan::handleAudioTrack(int index, int trackId, int frame, int nbframes, Common::File &input, const std::string &outputDir,
					  const std::string &inputFilename, int &size, int volume, int pan, bool iact) {
	AudioTrackInfo *audioTrack = NULL;
	if (index == 0) {
		audioTrack = allocAudioTrack(trackId, frame);
		assert(audioTrack);
		audioTrack->animFrame = frame;
		audioTrack->trackId = trackId;
		audioTrack->used = true;
		audioTrack->nbframes = nbframes;
		audioTrack->volumes = (int *)malloc(nbframes * sizeof(int));
		audioTrack->pans = (int *)malloc(nbframes * sizeof(int));
		audioTrack->sizes = (int *)malloc(nbframes * sizeof(int));
		memset(audioTrack->sizes, 0, nbframes * sizeof(int));
		if (iact) {
			int pos = input.pos();
			handleMapChunk(audioTrack, input);
			size -= (input.pos() - pos) + 18;
		} else {
			audioTrack->bits = 8;
			audioTrack->stereo = false;
			audioTrack->freq = 22050;
			int pos = input.pos();
			audioTrack->sdatSize = handleSaudChunk(audioTrack, input);
			audioTrack->sdatSize *= 4;
			size -= (input.pos() - pos) + 10;
			audioTrack->lastFrame = frame;
		}
		char tmpPath[1024];
		sprintf(tmpPath, "%s%s_%04d_%03d.tmp", outputDir.c_str(), inputFilename.c_str(), frame, trackId);
		audioTrack->file.open(tmpPath, "wb");
		if (!audioTrack->file.isOpen()) {
			error("error writing temp file");
		}
	} else {
		if (!iact)
			flushTracks(frame);
		audioTrack = findAudioTrack(trackId);
		assert(audioTrack);
		if (iact)
			size -= 18;
		else {
			size -= 10;
			audioTrack->lastFrame = frame;
		}
	}
	byte *buffer = (byte *)malloc(size);
	input.read_throwsOnError(buffer, size);
	audioTrack->file.write(buffer, size);
	free(buffer);
	audioTrack->volumes[index] = volume;
	audioTrack->pans[index] = pan;
	audioTrack->sizes[index] = size;
	if (audioTrack->bits == 8)
		audioTrack->sizes[index] *= 2;
	if (audioTrack->bits == 12)
		audioTrack->sizes[index] = (audioTrack->sizes[index] / 3) * 4;
	if (!audioTrack->stereo)
		audioTrack->sizes[index] *= 2;
	if (audioTrack->freq == 11025)
		audioTrack->sizes[index] *= 2;
	audioTrack->countFrames++;

	// FIXME. This doesn't work with Russian FT
	if ((index + 1) >= nbframes) {
		audioTrack->file.close();
	}
}

void CompressScummSan::handleDigIACT(Common::File &input, int size, const std::string &outputDir, const std::string &inputFilename,int flags, int track_flags, int frame) {
	int track = input.readUint16LE();
	int index = input.readUint16LE();
	int nbframes = input.readUint16LE();
	/*int data_size = */ input.readUint32LE();
	int volume = 127;
	int trackId = track;
	int pan = 0;

	if (track_flags == 1) {
		trackId = track + 100;
	} else if (track_flags == 2) {
		trackId = track + 200;
	} else if (track_flags == 3) {
		trackId = track + 300;
	} else if ((track_flags >= 100) && (track_flags <= 163)) {
		trackId = track + 400;
		volume = track_flags * 2 - 200;
	} else if ((track_flags >= 200) && (track_flags <= 263)) {
		trackId = track + 500;
		volume = track_flags * 2 - 400;
	} else if ((track_flags >= 300) && (track_flags <= 363)) {
		trackId = track + 600;
		volume = track_flags * 2 - 600;
	} else {
		error("handleDigIACT() Bad track_flags: %d", track_flags);
	}

	handleAudioTrack(index, trackId, frame, nbframes, input, outputDir, inputFilename, size, volume, pan, true);
}

void CompressScummSan::handlePSAD(Common::File &input, int size, const std::string &outputDir, const std::string &inputFilename, int frame) {
	int trackId = input.readUint16LE();
	int index = input.readUint16LE();
	int nbframes = input.readUint16LE();
	/*int flags = */ input.readUint16LE();
	int volume = input.readByte();
	int pan = input.readByte();

	handleAudioTrack(index, trackId, frame, nbframes, input, outputDir, inputFilename, size, volume, pan, false);
}

CompressScummSan::CompressScummSan(const std::string &name) : CompressionTool(name, TOOLTYPE_COMPRESSION) {
	_IACTpos = 0;

	_supportedFormats = AudioFormat(AUDIO_MP3 | AUDIO_VORBIS);
	_supportsProgressBar = true;

	ToolInput input;
	input.format = "*.san";
	_inputPaths.push_back(input);

	_shorthelp = "Used to compress .san files found in the later SCUMM games.";
	// TODO: Feature set seems more limited than what kCompressionAudioHelp contains
	_helptext = "\nUsage: " + getName() + " [mode] [mode-params] [-o outpufile = inputfile.san] <inputfile>\n";
}

void CompressScummSan::execute() {
	if (_format == AUDIO_FLAC)
		error("Only ogg vorbis and MP3 are supported for this tool.");

	Common::Filename inpath(_inputPaths[0].path);
	Common::Filename outpath(_outputPath);

	// We default to the current directory.
	// TODO: We shouldn't have to do this, an empty output path *should* work
	// fine. However, it currently doesn't, because we insert an extra slash
	// between the outpath and some filename, which would cause paths like
	//  "/foo.san" be formed -- and we wouldn't want to write into the root dir
	// by accident, right?
	if (outpath.empty()) {
		outpath.setFullPath("./");	// FIXME: Crude hack. Will this work on Windows?
	}

	// Use the same filename as for the input file, and ensure the extension is right.
	outpath.setFullName(inpath.getName());
	outpath.setExtension(".san");

	// Don't use the input file name for output by some weird accident.
	// (This check won't catch all cases of this, but it's better than nothing.)
	assert(inpath.getFullPath() != outpath.getFullPath());

	Common::File input(inpath, "rb");
	Common::File output(outpath, "wb");

	Common::Filename flupath(inpath);
	flupath.setExtension(".flu");

	Common::File flu_in;

	try {
		flu_in.open(flupath, "rb");
	} catch (...) {
		// pass
	}

	Common::File flu_out;
	if (flu_in.isOpen()) {
		flupath = outpath;
		flupath.setExtension(".flu");
		flu_out.open(flupath, "wb");
	}

	int32 l, size;

	output.writeUint32BE(input.readUint32BE()); // ANIM
	int32 animChunkSize = input.readUint32BE(); // ANIM size
	output.writeUint32BE(animChunkSize);

	output.writeUint32BE(input.readUint32BE()); // AHDR
	size = input.readUint32BE();
	output.writeUint32BE(size); // AHDR size
	output.writeUint16BE(input.readUint16BE()); // version
	int32 nbframes = input.readUint16LE(); // number frames
	output.writeUint16LE(nbframes);
	output.writeUint16BE(input.readUint16BE()); // unk
	for (l = 0; l < size - 6; l++) {
		output.writeByte(input.readByte()); // 0x300 palette + some bytes
	}

	FrameInfo *frameInfo = (FrameInfo *)malloc(sizeof(FrameInfo) * nbframes);

	for (l = 0; l < COMPRESS_SCUMM_SAN_MAX_TRACKS; l++) {
		_audioTracks[l].animFrame = -1;
		_audioTracks[l].trackId = 0;
		_audioTracks[l].bits = 0;
		_audioTracks[l].stereo = 0;
		_audioTracks[l].freq = 0;
		_audioTracks[l].used = 0;
		_audioTracks[l].waveDataSize = 0;
		_audioTracks[l].volumes = 0;
		_audioTracks[l].pans = 0;
		_audioTracks[l].sizes = 0;
		_audioTracks[l].nbframes = 0;
		_audioTracks[l].countFrames = 0;
		_audioTracks[l].lastFrame = 0;
		_audioTracks[l].sdatSize = 0;
	}

	bool tracksCompress = false;
	int fps = 0;
	uint32 inputSize = input.size();

	print("Frames: %d\n", nbframes);

	for (l = 0; l < nbframes; l++) {
		// Compression takes place in this loops, which takes the most time by far
		updateProgress(l, nbframes);

		print("frame: %d\n", l);
		bool first_fobj = true;
		uint32 tag = input.readUint32BE(); // chunk tag
		assert(tag == 'FRME');
		output.writeUint32BE(tag); // FRME
		int32 frameSize = input.readUint32BE(); // FRME size
		frameInfo[l].frameSize = frameSize;
		frameInfo[l].offsetOutput = output.pos();
		frameInfo[l].fobjDecompressedSize = 0;
		frameInfo[l].fobjCompressedSize = 0;
		frameInfo[l].lessIACTSize = 0;
		frameInfo[l].lessPSADSize = 0;
		output.writeUint32BE(frameSize);
		for (;;) {
			try {
				tag = input.readUint32BE(); // chunk tag
			} catch (...) {
				break;
			}

			if (input.eos())
				break;
			if (tag == 'FRME') {
				input.seek(-4, SEEK_CUR);
				break;
			} else if ((tag == 'FOBJ') && (first_fobj)) {
				size = input.readUint32BE(); // FOBJ size
				if ((size & 1) != 0)
					size++;
				first_fobj = false;
				unsigned long outputSize = size + (size / 9) + 100;
				byte *zlibInputBuffer = (byte *)malloc(size);
				byte *zlibOutputBuffer = (byte *)malloc(outputSize);
				for (int k = 0; k < size; k++) {
					*(zlibInputBuffer + k) = input.readByte(); // FOBJ datas
				}
				int result = compress2(zlibOutputBuffer, &outputSize, zlibInputBuffer, size, 9);
				if (result != Z_OK) {
					error("compression error");
				}
				if ((outputSize & 1) != 0)
					outputSize++;
				frameInfo[l].fobjDecompressedSize = size;
				frameInfo[l].fobjCompressedSize = outputSize;
				output.writeUint32BE('ZFOB');
				output.writeUint32BE(outputSize + 4);
				output.writeUint32BE(size);
				for (unsigned int k = 0; k < outputSize; k++) {
					output.writeByte(*(zlibOutputBuffer + k)); // compressed FOBJ datas
				}
				free(zlibInputBuffer);
				free(zlibOutputBuffer);
				continue;
			} else if ((tag == 'IACT') && (!flu_in.isOpen())) {
				size = input.readUint32BE(); // chunk size
				int code = input.readUint16LE();
				int flags = input.readUint16LE();
				int unk = input.readUint16LE();
				int track_flags = input.readUint16LE();
				if ((code == 8) && (track_flags == 0) && (unk == 0) && (flags == 46)) {
					handleComiIACT(input, size, outpath.getPath(), inpath.getFullName());
				} else if ((code == 8) && (track_flags != 0) && (unk == 0) && (flags == 46)) {
					handleDigIACT(input, size, outpath.getPath(), inpath.getFullName(), flags, track_flags, l);
					tracksCompress = true;
					fps = 12;
				} else {
					input.seek(-12, SEEK_CUR);
					goto skip;
				}

				if ((size & 1) != 0) {
					input.seek(1, SEEK_CUR);
					size++;
				}
				frameInfo[l].lessIACTSize += size + 8;
				continue;
			} else if ((tag == 'PSAD') && (!flu_in.isOpen())) {
				size = input.readUint32BE(); // chunk size
				handlePSAD(input, size, outpath.getPath(), inpath.getFullName(), l);
				if ((size & 1) != 0) {
					input.seek(1, SEEK_CUR);
					size++;
				}
				frameInfo[l].lessPSADSize += size + 8;
				tracksCompress = true;
				fps = 10;
			} else {
skip:
				size = input.readUint32BE(); // chunk size

				// Some files have garbage at the end
				if ((uint32)(size + input.pos()) > inputSize) {
					print("Skipping rest of the file (%d bytes)\n", inputSize - input.pos());
					break;
				}

				output.writeUint32BE(tag);
				output.writeUint32BE(size);
				if ((size & 1) != 0)
					size++;
				for (int k = 0; k < size; k++) {
					output.writeByte(input.readByte()); // chunk datas
				}
			}
		}
	}

	if (tracksCompress) {
		prepareForMixing(outpath.getPath(), inpath.getFullName());
		assert(fps);
		mixing(outpath.getPath(), inpath.getFullName(), nbframes, fps);
	}

	if (_waveTmpFile.isOpen()) {
		std::string tmpPath = outpath.getPath() + inpath.getFullName();
		setRawAudioType(true, true, 16); // LE, stereo, 16-bit

		if (_format == AUDIO_VORBIS)
			encodeSanWaveWithOgg(tmpPath);
		else
			encodeSanWaveWithLame(tmpPath);
		tmpPath += ".raw";
		Common::removeFile(tmpPath.c_str());
	}

	input.close();

	print("Fixing frames header...\n");
	int32 sumDiff = 0;
	for (l = 0; l < nbframes; l++) {
		int32 diff = 0;
		if (frameInfo[l].fobjCompressedSize != 0) {
			diff += frameInfo[l].fobjDecompressedSize - (frameInfo[l].fobjCompressedSize + 4);
		}
		if (frameInfo[l].lessIACTSize != 0) {
			diff += frameInfo[l].lessIACTSize;
		}
		if (frameInfo[l].lessPSADSize != 0) {
			diff += frameInfo[l].lessPSADSize;
		}
		output.seek(frameInfo[l].offsetOutput, SEEK_SET);
		sumDiff += diff;
		if (diff != 0)
			output.writeUint32BE(frameInfo[l].frameSize - diff);
	}
	print("done.\n");

	print("Fixing anim header...\n");
	output.seek(4, SEEK_SET);
	output.writeUint32BE(animChunkSize - sumDiff);
	print("done.\n");

	if (flu_in.isOpen()) {
		print("Fixing flu offsets...\n");
		int fsize = flu_in.size();
		for (int k = 0; k < fsize; k++) {
			flu_out.writeByte(flu_in.readByte());
		}
		flu_out.seek(0x324, SEEK_SET);
		for (l = 0; l < nbframes; l++) {
			flu_out.writeUint32LE(frameInfo[l].offsetOutput - 4);
		}
		print("done.\n");
	}

	free(frameInfo);

	print("compression done.\n");
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	CompressScummSan scummsan(argv[0]);
	return scummsan.run(argc, argv);
}
#endif

