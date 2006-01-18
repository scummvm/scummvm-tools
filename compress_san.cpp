/* compress_san - compressor for smush san files
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
 * $Header$
 *
 */

#include "compress.h"
#include "zlib.h"

inline uint16 READ_LE_UINT16(const void *ptr) {
	const byte *b = (const byte *)ptr;
	return (b[1] << 8) + b[0];
}

inline void WRITE_LE_UINT16(void *ptr, uint16 value) {
	byte *b = (byte *)ptr;
	b[0] = (byte)(value >> 0);
	b[1] = (byte)(value >> 8);
}

const char *tag2str(uint32 tag) {
	static char str[5];
	str[0] = (char)(tag >> 24);
	str[1] = (char)(tag >> 16);
	str[2] = (char)(tag >> 8);
	str[3] = (char)tag;
	str[4] = '\0';
	return str;
}

void showhelp(char *exename) {
	printf("\nUsage: %s <inputfile> <inputdir> <outputdir> [--ogg] [encoder params]\n", exename);
	printf("\nMP3 mode params:\n");
	printf(" -b <rate>    <rate> is the target bitrate(ABR)/minimal bitrate(VBR) (default:%d)\n", minBitrDef);
	printf(" -B <rate>    <rate> is the maximum VBR/ABR bitrate (default:%d)\n", maxBitrDef);
	printf(" --vbr        LAME uses the VBR mode (default)\n");
	printf(" --abr        LAME uses the ABR mode\n");
	printf(" -V <value>   specifies the value (0 - 9) of VBR quality (0=best) (default:%d)\n", vbrqualDef);
	printf(" -q <value>   specifies the MPEG algorithm quality (0-9; 0=best) (default:%d)\n", algqualDef);
	printf(" --silent     the output of LAME is hidden (default:disabled)\n");

	printf("\nVorbis mode params:\n");
	printf(" -b <rate>    <rate> is the nominal bitrate (default:unset)\n");
	printf(" -m <rate>    <rate> is the minimum bitrate (default:unset)\n");
	printf(" -M <rate>    <rate> is the maximum bitrate (default:unset)\n");
	printf(" -q <value>   specifies the value (0 - 10) of VBR quality (10=best) (default:%d)\n", oggqualDef);
	printf(" --silent     the output of oggenc is hidden (default:disabled)\n");
	exit(2);
}

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
	FILE *file;
	int waveDataSize;
	int *volumes;
	int *pans;
	int *sizes;
	int nbframes;
	int countFrames;
	int32 sdatSize;
};

#define MAX_TRACKS 150

static byte _IACToutput[0x1000];
static int _IACTpos = 0;
static FILE *_waveTmpFile;
static int32 _waveDataSize;
static AudioTrackInfo _audioTracks[MAX_TRACKS];
static bool _oggMode = false; // mp3 default

void encodeWaveWithOgg(char *filename) {
	char fbuf[2048];
	char fbuf2[2048];
	sprintf(fbuf, "\"%s\".wav", filename);
	sprintf(fbuf2, "\"%s\".ogg", filename);
	encodeAudio(fbuf, false, -1, fbuf2, kVorbisMode);
}

void encodeWaveWithLame(char *filename) {
	char fbuf[2048];
	char fbuf2[2048];

	sprintf(fbuf, "\"%s\".wav", filename);
	sprintf(fbuf2, "\"%s\".mp3", filename);
	encodeAudio(fbuf, false, -1, fbuf2, kMP3Mode);
}

void writeWaveHeader(int s_size) {
	int rate = 22050;
	int bits = 16;
	int chan = 2;
	byte wav[44];
	memset (wav, 0,	44);
	wav[0] = 'R';
	wav[1] = 'I';
	wav[2] = 'F';
	wav[3] = 'F';
	wav[4] = (s_size + 36) & 0xff;
	wav[5] = ((s_size +	36)	>> 8) &	0xff;
	wav[6] = ((s_size +	36)	>> 16) & 0xff;
	wav[7] = ((s_size +	36)	>> 24) & 0xff;
	wav[8] = 'W';
	wav[9] = 'A';
	wav[10]	= 'V';
	wav[11]	= 'E';
	wav[12]	= 'f';
	wav[13]	= 'm';
	wav[14]	= 't';
	wav[15]	= 0x20;
	wav[16]	= 16;
	wav[20]	= 1;
	wav[22]	= chan;
	wav[24]	= rate & 0xff;
	wav[25]	= (rate	>> 8) &	0xff;
	wav[26]	= (rate	>> 16) & 0xff;
	wav[27]	= (rate	>> 24) & 0xff;
	wav[28]	= (rate	* chan * (bits / 8)) & 0xff;
	wav[29]	= ((rate * chan	* (bits	/ 8))>>	8) & 0xff;
	wav[30]	= ((rate * chan	* (bits	/ 8)) >> 16) & 0xff;
	wav[31]	= ((rate * chan	* (bits	/ 8)) >> 24) & 0xff;
	wav[32]	= (chan	* (bits	/ 8)) &	0xff;
	wav[33]	= ((chan * (bits / 8)) >> 8) & 0xff;
	wav[34]	= bits;
	wav[36]	= 'd';
	wav[37]	= 'a';
	wav[38]	= 't';
	wav[39]	= 'a';
	wav[40]	= s_size & 0xff;
	wav[41]	= (s_size >> 8)	& 0xff;
	wav[42]	= (s_size >> 16) & 0xff;
	wav[43]	= (s_size >> 24) & 0xff;

	fseek(_waveTmpFile, 0, SEEK_SET);
	if (fwrite(wav, 1, 44, _waveTmpFile) != 44) {
		printf("error write temp wave file");
		exit(1);
	}
	fclose(_waveTmpFile);
	_waveTmpFile = NULL;
}
void writeToTempWave(char *fileName, byte *output_data, unsigned int size) {
	if (!_waveTmpFile) {
		_waveTmpFile = fopen(fileName, "wb");
		if (!_waveTmpFile) {
			printf("error write temp wave file");
			exit(1);
		}
		byte wav[44];
		memset(wav, 0, 44);
		if (fwrite(output_data, 1, 44, _waveTmpFile) != 44) {
			printf("error write temp wave file");
			exit(1);
		}
		_waveDataSize = 0;
	}
	for (unsigned int j = 0; j < size - 1; j += 2) {
		byte tmp = output_data[j + 0];
		output_data[j + 0] = output_data[j + 1];
		output_data[j + 1] = tmp;
	}

	if (fwrite(output_data, 1, size, _waveTmpFile) != size) {
		printf("error write temp wave file");
		exit(1);
	}
	_waveDataSize += size;
}

void decompressComiIACT(char *fileName, byte *output_data, byte *d_src, int bsize) {
	byte value;

	while (bsize > 0) {
		if (_IACTpos >= 2) {
			int32 len = *(uint16 *)(_IACToutput);
			len = TO_BE_16(len) + 2;
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
				writeToTempWave(fileName, output_data, 0x1000);
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

void handleComiIACT(FILE *input, int size, char *outputDir, char *inputFilename, char *tmpPath) {
	fseek(input, 10, SEEK_CUR);
	int bsize = size - 18;
	byte output_data[0x1000];
	byte *src = (byte *)malloc(bsize);
	fread(src, bsize, 1, input);

	sprintf(tmpPath, "%s/%s.wav", outputDir, inputFilename);
	decompressComiIACT(tmpPath, output_data, src, bsize);

	free(src);
}

AudioTrackInfo *allocAudioTrack(int trackId, int frame) {
	for (int l = 0; l < MAX_TRACKS; l++) {
		if ((_audioTracks[l].animFrame != frame) && (_audioTracks[l].trackId != trackId) && (!_audioTracks[l].used))
			return &_audioTracks[l];
	}
	return NULL;
}

AudioTrackInfo *findAudioTrack(int trackId) {
	for (int l = 0; l < MAX_TRACKS; l++) {
		if ((_audioTracks[l].trackId == trackId) && (_audioTracks[l].used) && (_audioTracks[l].file))
			return &_audioTracks[l];
	}
	return NULL;
}

void prepareForMixing(char *outputDir, char *inputFilename) {
	char filename[200];

	printf("Decompresing tracks files...\n");
	for (int l = 0; l < MAX_TRACKS; l++) {
		if (_audioTracks[l].used) {
			if (_audioTracks[l].file)
				fclose(_audioTracks[l].file);
			sprintf(filename, "%s/%s_%04d_%03d.tmp", outputDir, inputFilename, _audioTracks[l].animFrame, _audioTracks[l].trackId);
			_audioTracks[l].file = fopen(filename, "rb");
			assert(_audioTracks[l].file);
			fseek(_audioTracks[l].file, 0, SEEK_END);
			int fileSize = ftell(_audioTracks[l].file);
			fseek(_audioTracks[l].file, 0, SEEK_SET);
			byte *audioBuf = (byte *)malloc(fileSize);
			fread(audioBuf, fileSize, 1, _audioTracks[l].file);
			fclose(_audioTracks[l].file);
			_audioTracks[l].file = NULL;

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
			_audioTracks[l].file = fopen(filename, "wb");
			assert(_audioTracks[l].file);
			fwrite(outputBuf, outputSize, 1, _audioTracks[l].file);
			fclose(_audioTracks[l].file);
			_audioTracks[l].file = NULL;
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
void mixing(char *outputDir, char *inputFilename, int frames, int fps) {
	char wavPath[200];
	char filename[200];
	int l, r, z;

	sprintf(wavPath, "%s/%s.wav", outputDir, inputFilename);
	FILE *wavFile = fopen(wavPath, "wb+");
	assert(wavFile);

	int frameAudioSize;
	if (fps == 12) {
		frameAudioSize = 7352;
	} else if (fps == 10) {
		frameAudioSize = 8802;
	} else {
		error("Unsupported fps value %d", fps);
	}

	printf("Creating silent wav file...\n");
	for (l = 0; l < 44 + (frameAudioSize * frames); l++) {
		fputc(0, wavFile);
	}

	printf("Mixing tracks into wav file...\n");
	for (l = 0; l < MAX_TRACKS; l++) {
		if (_audioTracks[l].used) {
			sprintf(filename, "%s/%s_%04d_%03d.tmp", outputDir, inputFilename, _audioTracks[l].animFrame, _audioTracks[l].trackId);
			_audioTracks[l].file = fopen(filename, "rb");
			assert(_audioTracks[l].file);
			fseek(_audioTracks[l].file, 0, SEEK_END);
			int fileSize = ftell(_audioTracks[l].file);
			fseek(_audioTracks[l].file, 0, SEEK_SET);
			byte *tmpBuf = (byte *)malloc(fileSize);
			fread(tmpBuf, fileSize, 1, _audioTracks[l].file);
			fclose(_audioTracks[l].file);
			unlink(filename);

			byte *wavBuf = (byte *)malloc(fileSize);
			fseek(wavFile, 44 + (frameAudioSize * _audioTracks[l].animFrame), SEEK_SET);
			fread(wavBuf, fileSize, 1, wavFile);

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
			fseek(wavFile, 44 + (frameAudioSize * _audioTracks[l].animFrame), SEEK_SET);
			fwrite(wavBuf, fileSize, 1, wavFile);

			free(wavBuf);
			free(tmpBuf);
		}
	}

	_waveTmpFile = wavFile;
	_waveDataSize = frames * frameAudioSize;
}

void handleMapChunk(AudioTrackInfo *audioTrack, FILE *input) {
	uint32 tag;
	int32 size;
	tag = readUint32BE(input);
	size = readUint32BE(input);
	assert(tag == 'iMUS');
	tag = readUint32BE(input);
	size = readUint32BE(input);
	assert(tag == 'MAP ');
	tag = readUint32BE(input);
	size = readUint32BE(input);
	assert(tag == 'FRMT');
	fseek(input, 8, SEEK_CUR);
	audioTrack->bits = readUint32BE(input);
	audioTrack->freq = readUint32BE(input);
	int chan = readUint32BE(input);
	if (chan == 2)
		audioTrack->stereo = true;
	tag = readUint32BE(input);
	size = readUint32BE(input);
	if (tag == 'TEXT') {
		fseek(input, size, SEEK_CUR);
		tag = readUint32BE(input);
		size = readUint32BE(input);
		if (tag == 'TEXT') {
			fseek(input, size, SEEK_CUR);
			tag = readUint32BE(input);
			size = readUint32BE(input);
		}
	}
	assert(tag == 'REGN');
	fseek(input, 8, SEEK_CUR);
	tag = readUint32BE(input);
	size = readUint32BE(input);
	if (tag == 'TEXT') {
		fseek(input, size, SEEK_CUR);
		tag = readUint32BE(input);
		size = readUint32BE(input);
		if (tag == 'REGN') {
			fseek(input, 8, SEEK_CUR);
			tag = readUint32BE(input);
			size = readUint32BE(input);
		}
	}
	if (tag == 'STOP') {
		fseek(input, 4, SEEK_CUR);
		tag = readUint32BE(input);
		size = readUint32BE(input);
	}
	assert(tag == 'DATA');
}

int32 handleSaudChunk(AudioTrackInfo *audioTrack, FILE *input) {
	uint32 tag;
	int32 size;
	tag = readUint32BE(input);
	size = readUint32BE(input);
	assert(tag == 'SAUD');
	tag = readUint32BE(input);
	size = readUint32BE(input);
	assert(tag == 'STRK');
	fseek(input, size, SEEK_CUR);
	tag = readUint32BE(input);
	size = readUint32BE(input);
	assert(tag == 'SDAT');
	return size;
}

void handleAudioTrack(int index, int trackId, int frame, int nbframes, FILE *input, char *outputDir,
					  char *inputFilename, char *tmpPath, int &size, int volume, int pan, bool iact) {
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
			int pos = ftell(input);
			handleMapChunk(audioTrack, input);
			size -= (ftell(input) - pos) + 18;
		} else {
			audioTrack->bits = 8;
			audioTrack->stereo = false;
			audioTrack->freq = 22050;
			int pos = ftell(input);
			audioTrack->sdatSize = handleSaudChunk(audioTrack, input);
			audioTrack->sdatSize *= 4;
			size -= (ftell(input) - pos) + 10;
		}
		sprintf(tmpPath, "%s/%s_%04d_%03d.tmp", outputDir, inputFilename, frame, trackId);
		audioTrack->file = fopen(tmpPath, "wb");
		if (!audioTrack->file) {
			printf("error write temp file");
			exit(1);
		}
	} else {
		audioTrack = findAudioTrack(trackId);
		assert(audioTrack);
		if (iact)
			size -= 18;
		else
			size -= 10;
	}
	byte *buffer = (byte *)malloc(size);
	fread(buffer, size, 1, input);
	fwrite(buffer, size, 1, audioTrack->file);
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
	if ((index + 1) == nbframes) {
		fclose(audioTrack->file);
		audioTrack->file = NULL;
	}
}

void handleDigIACT(FILE *input, int size, char *outputDir, char *inputFilename, char *tmpPath, int flags, int track_flags, int frame) {
	int track = readUint16LE(input);
	int index = readUint16LE(input);
	int nbframes = readUint16LE(input);
	/*int data_size = */ readUint32LE(input);
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
		printf("handleDigIACT() Bad track_flags: %d\n", track_flags);
		exit(1);
	}

	handleAudioTrack(index, trackId, frame, nbframes, input, outputDir, inputFilename, tmpPath, size, volume, pan, true);
}

void handlePSAD(FILE *input, int size, char *outputDir, char *inputFilename, char *tmpPath, int frame) {
	int trackId = readUint16LE(input);
	int index = readUint16LE(input);
	int nbframes = readUint16LE(input);
	/*int flags = */ readUint16LE(input);
	int volume = readByte(input);
	int pan = readByte(input);

	handleAudioTrack(index, trackId, frame, nbframes, input, outputDir, inputFilename, tmpPath, size, volume, pan, false);
}

int main(int argc, char *argv[]) {
	if (argc < 4)
		showhelp(argv[0]);

	char inputDir[200];
	char outputDir[200];
	char inputFilename[200];
	char tmpPath[200];

	strcpy(inputFilename, argv[1]);
	strcpy(inputDir, argv[2]);
	strcpy(outputDir, argv[3]);

	if (argc > 4) {
		int i = 4;

		if (strcmp(argv[i], "--ogg") == 0) {
			_oggMode = true;
			i++;
		}

		if (argc > i) {
			// HACK: The functions in compress.c expect the last
			// argument to be a filename. As we don't expect one,
			// we simply add a dummy argument to the list.
			char **args = (char **)malloc((argc + 1) * sizeof(char *));
			char dummyName[] = "dummy";
			int j;

			for (j = 0; j < argc; j++)
				args[j] = argv[j];
			args[j] = dummyName;
		
			int result;

			if (_oggMode)
				result = process_ogg_parms(argc + 1, args, i);
			else
				result = process_mp3_parms(argc + 1, args, i);

			if (!result)
				showhelp(argv[0]);

			free(args);
		}
	}

	char *index = strrchr(inputFilename, '.');
	if (index != NULL) {
		*index = 0;
	}

	sprintf(tmpPath, "%s/%s.san", inputDir, inputFilename);

	FILE *input = fopen(tmpPath, "rb");
	if (!input) {
		printf("Cannot open file: %s\n", tmpPath);
		exit(-1);
	}

	sprintf(tmpPath, "%s/%s.san", outputDir, inputFilename);

	FILE *output = fopen(tmpPath, "wb");
	if (!output) {
		printf("Cannot open file: %s\n", tmpPath);
		exit(-1);
	}

	sprintf(tmpPath, "%s/%s.flu", inputDir, inputFilename);

	FILE *flu_in = NULL;
	FILE *flu_out = NULL;
	flu_in = fopen(tmpPath, "rb");

	if (flu_in) {
		sprintf(tmpPath, "%s/%s.flu", outputDir, inputFilename);
		flu_out = fopen(tmpPath, "wb");
		if (!flu_out) {
			printf("Cannot open file: %s\n", tmpPath);
			exit(-1);
		}
	}

	uint32 tag;
	int32 l, size;

	writeUint32BE(output, readUint32BE(input)); // ANIM
	int32 animChunkSize = readUint32BE(input); // ANIM size
	writeUint32BE(output, animChunkSize);

	writeUint32BE(output, readUint32BE(input)); // AHDR
	size = readUint32BE(input);
	writeUint32BE(output, size); // AHDR size
	writeUint16BE(output, readUint16BE(input)); // version
	int32 nbframes = readUint16LE(input); // number frames
	writeUint16LE(output, nbframes);
	writeUint16BE(output, readUint16BE(input)); // unk
	for (l = 0; l < size - 6; l++) {
		writeByte(output, readByte(input)); // 0x300 palette + some bytes
	}

	FrameInfo *frameInfo = (FrameInfo *)malloc(sizeof(FrameInfo) * nbframes);

	memset(_audioTracks, 0, sizeof(AudioTrackInfo) * MAX_TRACKS);
	for (l = 0; l < MAX_TRACKS; l++) {
		_audioTracks[l].animFrame = -1;
	}

	bool tracksCompress = false;
	int fps = 0;

	for (l = 0; l < nbframes; l++) {
		printf("frame: %d\n", l);
		bool first_fobj = true;
		tag = readUint32BE(input); // chunk tag
		assert(tag == 'FRME');
		writeUint32BE(output, tag); // FRME
		int32 frameSize = readUint32BE(input); // FRME size
		frameInfo[l].frameSize = frameSize;
		frameInfo[l].offsetOutput = ftell(output);
		frameInfo[l].fobjDecompressedSize = 0;
		frameInfo[l].fobjCompressedSize = 0;
		frameInfo[l].lessIACTSize = 0;
		frameInfo[l].lessPSADSize = 0;
		writeUint32BE(output, frameSize);
		for (;;) {
			tag = readUint32BE(input); // chunk tag
			if (feof(input))
				break;
			if (tag == 'FRME') {
				fseek(input, -4, SEEK_CUR);
				break;
			} else if ((tag == 'FOBJ') && (first_fobj)) {
				size = readUint32BE(input); // FOBJ size
				if ((size & 1) != 0)
					size++;
				first_fobj = false;
				unsigned long outputSize = size + (size / 9) + 100;
				byte *zlibInputBuffer = (byte *)malloc(size);
				byte *zlibOutputBuffer = (byte *)malloc(outputSize);
				for (int k = 0; k < size; k++) {
					*(zlibInputBuffer + k) = readByte(input); // FOBJ datas
				}
				int result = compress2(zlibOutputBuffer, &outputSize, zlibInputBuffer, size, 9);
				if (result != Z_OK) {
					error("compression error");
				}
				if ((outputSize & 1) != 0)
					outputSize++;
				frameInfo[l].fobjDecompressedSize = size;
				frameInfo[l].fobjCompressedSize = outputSize;
				writeUint32BE(output, 'ZFOB');
				writeUint32BE(output, outputSize + 4);
				writeUint32BE(output, size);
				for (unsigned int k = 0; k < outputSize; k++) {
					writeByte(output, *(zlibOutputBuffer + k)); // compressed FOBJ datas
				}
				free(zlibInputBuffer);
				free(zlibOutputBuffer);
				continue;
			} else if ((tag == 'IACT') && (!flu_in)) {
				size = readUint32BE(input); // chunk size
				int code = readUint16LE(input);
				int flags = readUint16LE(input);
				int unk = readUint16LE(input);
				int track_flags = readUint16LE(input);
				if ((code == 8) && (track_flags == 0) && (unk == 0) && (flags == 46)) {
					handleComiIACT(input, size, outputDir, inputFilename, tmpPath);
				} else if ((code == 8) && (track_flags != 0) && (unk == 0) && (flags == 46)) {
					handleDigIACT(input, size, outputDir, inputFilename, tmpPath, flags, track_flags, l);
					tracksCompress = true;
					fps = 12;
				} else {
					fseek(input, -12, SEEK_CUR);
					goto skip;
				}

				if ((size & 1) != 0) {
					fseek(input, 1, SEEK_CUR);
					size++;
				}
				frameInfo[l].lessIACTSize += size + 8;
				continue;
			} else if ((tag == 'PSAD') && (!flu_in)) {
				size = readUint32BE(input); // chunk size
				handlePSAD(input, size, outputDir, inputFilename, tmpPath, l);
				if ((size & 1) != 0) {
					fseek(input, 1, SEEK_CUR);
					size++;
				}
				frameInfo[l].lessPSADSize += size + 8;
				tracksCompress = true;
				fps = 10;
			} else {
skip:
				size = readUint32BE(input); // chunk size
				writeUint32BE(output, tag);
				writeUint32BE(output, size);
				if ((size & 1) != 0)
					size++;
				for (int k = 0; k < size; k++) {
					writeByte(output, readByte(input)); // chunk datas
				}
			}
		}
	}

	if (tracksCompress) {
		prepareForMixing(outputDir, inputFilename);
		assert(fps);
		mixing(outputDir, inputFilename, nbframes, fps);
	}

	if (_waveTmpFile) {
		writeWaveHeader(_waveDataSize);
		sprintf(tmpPath, "%s/%s", outputDir, inputFilename);
		if (_oggMode)
			encodeWaveWithOgg(tmpPath);
		else
			encodeWaveWithLame(tmpPath);
		sprintf(tmpPath, "%s/%s.wav", outputDir, inputFilename);
		unlink(tmpPath);
	}

	fclose(input);

	printf("Fixing frames header...\n");
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
		fseek(output, frameInfo[l].offsetOutput, SEEK_SET);
		sumDiff += diff;
		if (diff != 0)
			writeUint32BE(output, frameInfo[l].frameSize - diff);
	}
	printf("done.\n");

	printf("Fixing anim header...\n");
	fseek(output, 4, SEEK_SET);
	writeUint32BE(output, animChunkSize - sumDiff);
	printf("done.\n");

	if (flu_in) {
		printf("Fixing flu offsets...\n");
		fseek(flu_in, 0, SEEK_END);
		int fsize = ftell(flu_in);
		fseek(flu_in, 0, SEEK_SET);
		for (int k = 0; k < fsize; k++) {
			writeByte(flu_out, readByte(flu_in));
		}
		fseek(flu_out, 0x324, SEEK_SET);
		for (l = 0; l < nbframes; l++) {
			writeUint32LE(flu_out, frameInfo[l].offsetOutput - 4);
		}
		fclose(flu_in);
		fclose(flu_out);
		printf("done.\n");
	}

	free(frameInfo);
	
	fclose(output);

	printf("compression done.\n");
		
	return 0;
}
