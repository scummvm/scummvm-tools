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

#include "engines/twine/extract_twine_convert.h"

#include "common/endian.h"
#include "common/file.h"
#include "common/memstream.h"
#include "common/util.h"
#include "engines/twine/parser/body.h"
#include "sound/voc.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef USE_PNG
#include <png.h>
#endif

namespace {

static const int kScreenWidth = 640;
static const int kScreenHeight = 480;

template<typename T>
static inline T extractBits(T value, uint8 offset, uint8 numBits) {
	return (((T(1) << numBits) - 1) & (value >> offset));
}

static bool isCreativeVoc(const uint8 *data, int32 size) {
	return size >= 20 && !memcmp(data, "Creative Voice File", 19);
}

static bool isRiffWav(const uint8 *data, int32 size) {
	if (size < 12)
		return false;
	if (!memcmp(data, "RIFF", 4) && !memcmp(data + 8, "WAVE", 4))
		return true;
	return data[1] == 'I' && data[2] == 'F' && data[3] == 'F' && !memcmp(data + 8, "WAVE", 4);
}

static bool looksLikeSpriteHeader(const uint8 *data, int32 size) {
	if (size < 12)
		return false;

	const uint32 offset1 = READ_LE_UINT32(data);
	const uint32 offset2 = READ_LE_UINT32(data + 4);
	if (offset1 < 8 || offset2 <= offset1 || offset2 > (uint32)size)
		return false;

	const uint8 width = data[offset1];
	const uint8 height = data[offset1 + 1];
	return width > 0 && height > 0;
}

static bool looksLikeBrickSprite(const uint8 *data, int32 size) {
	if (size < 4)
		return false;

	const uint8 width = data[0];
	const uint8 height = data[1];
	if (width == 0 || height == 0 || width > 64 || height > 64)
		return false;

	Common::MemoryReadStream stream(data, size);
	stream.readByte();
	stream.readByte();
	stream.readByte();
	stream.readByte();
	const uint8 numRuns = stream.readByte();
	if (numRuns == 0 || stream.err())
		return false;

	return true;
}

struct DecodedImage {
	int width;
	int height;
	uint8 *pixels;

	DecodedImage() : width(0), height(0), pixels(nullptr) {}

	~DecodedImage() {
		free(pixels);
	}
};

static bool decodeSpriteAt(Common::SeekableReadStream &stream, int32 size, uint32 offset, DecodedImage &image) {
	if (!stream.seek(offset))
		return false;

	const int width = stream.readByte();
	const int height = stream.readByte();
	if (width <= 0 || height <= 0)
		return false;

	stream.readByte();
	stream.readByte();

	image.width = width;
	image.height = height;
	image.pixels = (uint8 *)calloc(width * height, 1);
	if (!image.pixels)
		return false;

	for (int y = 0; y < height; ++y) {
		if (stream.pos() >= (uint32)size)
			return false;

		const uint8 numRuns = stream.readByte();
		int x = 0;
		for (uint8 run = 0; run < numRuns; ++run) {
			if (stream.pos() >= (uint32)size)
				return false;

			const uint8 runSpec = stream.readByte();
			const uint8 runLength = extractBits(runSpec, 0, 6) + 1;
			const uint8 type = extractBits(runSpec, 6, 2);
			if (x + runLength > width)
				return false;

			if (type == 1) {
				for (uint8 i = 0; i < runLength; ++i) {
					if (stream.pos() >= (uint32)size)
						return false;
					image.pixels[y * width + x++] = stream.readByte();
				}
			} else if (type != 0) {
				if (stream.pos() >= (uint32)size)
					return false;
				const uint8 color = stream.readByte();
				memset(image.pixels + y * width + x, color, runLength);
				x += runLength;
			} else {
				x += runLength;
			}
		}
	}

	return !stream.err();
}

static bool decodeSpriteEntry(const uint8 *data, int32 size, DecodedImage &image) {
	Common::MemoryReadStream stream(data, size);

	if (looksLikeSpriteHeader(data, size)) {
		const uint32 offset1 = stream.readUint32LE();
		return decodeSpriteAt(stream, size, offset1, image);
	}

	if (looksLikeBrickSprite(data, size))
		return decodeSpriteAt(stream, size, 0, image);

	return false;
}

#ifdef USE_PNG
static bool writeIndexedPng(const char *outPath, const DecodedImage &image, const TwinePalette &palette) {
	FILE *fout = fopen(outPath, "wb");
	if (!fout) {
		fprintf(stderr, "Unable to open %s for writing\n", outPath);
		return false;
	}

	png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
	if (!png) {
		fclose(fout);
		return false;
	}

	png_infop info = png_create_info_struct(png);
	if (!info) {
		png_destroy_write_struct(&png, nullptr);
		fclose(fout);
		return false;
	}

	if (setjmp(png_jmpbuf(png))) {
		png_destroy_write_struct(&png, &info);
		fclose(fout);
		return false;
	}

	png_init_io(png, fout);
	png_set_IHDR(png, info, image.width, image.height, 8, PNG_COLOR_TYPE_PALETTE,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

	png_color pngPalette[256];
	memset(pngPalette, 0, sizeof(pngPalette));
	for (int i = 0; i < 256; ++i) {
		pngPalette[i].red = palette.data[i * 3 + 0];
		pngPalette[i].green = palette.data[i * 3 + 1];
		pngPalette[i].blue = palette.data[i * 3 + 2];
	}
	png_set_PLTE(png, info, pngPalette, 256);

	png_byte trans = 0;
	png_set_tRNS(png, info, &trans, 1, nullptr);

	png_write_info(png, info);

	png_bytep *rowPointers = (png_bytep *)malloc(sizeof(png_bytep) * image.height);
	if (!rowPointers) {
		png_destroy_write_struct(&png, &info);
		fclose(fout);
		return false;
	}

	for (int y = 0; y < image.height; ++y)
		rowPointers[y] = image.pixels + y * image.width;

	png_write_image(png, rowPointers);
	png_write_end(png, nullptr);

	free(rowPointers);
	png_destroy_write_struct(&png, &info);
	fclose(fout);
	return true;
}
#endif

static bool writePcmWav(FILE *fout, const void *samples, uint32 dataSize, uint32 sampleRate,
		uint16 bitsPerSample, uint16 numChannels) {
	const uint16 blockAlign = numChannels * bitsPerSample / 8;
	const uint32 byteRate = sampleRate * blockAlign;
	const uint16 audioFormat = 1;
	const uint32 riffSize = 36 + dataSize;

	fwrite("RIFF", 1, 4, fout);
	fwrite(&riffSize, 4, 1, fout);
	fwrite("WAVE", 1, 4, fout);
	fwrite("fmt ", 1, 4, fout);

	const uint32 fmtChunkSize = 16;
	fwrite(&fmtChunkSize, 4, 1, fout);
	fwrite(&audioFormat, 2, 1, fout);
	fwrite(&numChannels, 2, 1, fout);
	fwrite(&sampleRate, 4, 1, fout);
	fwrite(&byteRate, 4, 1, fout);
	fwrite(&blockAlign, 2, 1, fout);
	fwrite(&bitsPerSample, 2, 1, fout);

	fwrite("data", 1, 4, fout);
	fwrite(&dataSize, 4, 1, fout);
	fwrite(samples, 1, dataSize, fout);
	return !ferror(fout);
}

static const int16 kImaStepTable[89] = {
	7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41, 45, 50, 55, 60, 66,
	73, 80, 88, 97, 107, 118, 130, 143, 157, 173, 190, 209, 230, 253, 279, 307, 337, 371, 408,
	449, 494, 544, 598, 658, 724, 796, 876, 963, 1060, 1166, 1282, 1411, 1552, 1707, 1878,
	2066, 2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358, 5894, 6484, 7132,
	7845, 8630, 9493, 10442, 11487, 12635, 13899, 15289, 16818, 18500, 20350, 22385,
	24623, 27086, 29794, 32767
};

static const int kImaIndexTable[16] = {
	-1, -1, -1, -1, 2, 4, 6, 8, -1, -1, -1, -1, 2, 4, 6, 8
};

static int16 *decodeImaAdpcm(const uint8 *adpcmData, int32 adpcmSize, int32 blockAlign, int32 *outSamples) {
	if (blockAlign < 8 || adpcmSize < blockAlign) {
		*outSamples = 0;
		return nullptr;
	}

	const int32 samplesPerBlock = (blockAlign - 4) * 2 + 1;
	const int32 numBlocks = adpcmSize / blockAlign;
	const int32 remainder = adpcmSize % blockAlign;
	int32 totalSamples = numBlocks * samplesPerBlock;
	if (remainder >= 8)
		totalSamples += (remainder - 4) * 2 + 1;

	int16 *pcm = (int16 *)malloc(sizeof(int16) * totalSamples);
	if (!pcm) {
		*outSamples = 0;
		return nullptr;
	}

	int32 outIdx = 0;
	const uint8 *src = adpcmData;
	int32 bytesLeft = adpcmSize;

	while (bytesLeft >= 8) {
		const int32 blockSize = bytesLeft >= blockAlign ? blockAlign : bytesLeft;
		int16 predictor = (int16)(src[0] | (src[1] << 8));
		int stepIndex = src[2];
		if (stepIndex < 0)
			stepIndex = 0;
		if (stepIndex > 88)
			stepIndex = 88;

		pcm[outIdx++] = predictor;
		for (int32 i = 4; i < blockSize && outIdx < totalSamples; ++i) {
			for (int nibbleIdx = 0; nibbleIdx < 2 && outIdx < totalSamples; ++nibbleIdx) {
				const int nibble = (nibbleIdx == 0) ? (src[i] & 0x0F) : ((src[i] >> 4) & 0x0F);
				const int step = kImaStepTable[stepIndex];
				int diff = step >> 3;
				if (nibble & 1)
					diff += step >> 2;
				if (nibble & 2)
					diff += step >> 1;
				if (nibble & 4)
					diff += step;
				if (nibble & 8)
					diff = -diff;

				int sample = (int)predictor + diff;
				if (sample > 32767)
					sample = 32767;
				if (sample < -32768)
					sample = -32768;
				predictor = (int16)sample;
				pcm[outIdx++] = predictor;

				stepIndex += kImaIndexTable[nibble];
				if (stepIndex < 0)
					stepIndex = 0;
				if (stepIndex > 88)
					stepIndex = 88;
			}
		}

		src += blockSize;
		bytesLeft -= blockSize;
	}

	*outSamples = outIdx;
	return pcm;
}

struct WavPayload {
	const uint8 *data;
	int32 size;
	uint32 sampleRate;
	uint16 channels;
	uint16 bitsPerSample;
	uint16 formatTag;
	int32 blockAlign;
};

static bool parseWavPayload(const uint8 *buffer, int32 sizeBytes, WavPayload &payload) {
	payload.data = buffer;
	payload.size = sizeBytes;
	payload.sampleRate = 11025;
	payload.channels = 1;
	payload.bitsPerSample = 8;
	payload.formatTag = 1;
	payload.blockAlign = 1;

	if (sizeBytes < 12)
		return false;

	if (!isRiffWav(buffer, sizeBytes))
		return false;

	int32 pos = 12;
	while (pos + 8 <= sizeBytes) {
		const int32 chunkSize = (int32)READ_LE_UINT32(buffer + pos + 4);
		if (!memcmp(buffer + pos, "fmt ", 4) && chunkSize >= 16 && pos + 8 + 16 <= sizeBytes) {
			const uint8 *fmt = buffer + pos + 8;
			payload.formatTag = READ_LE_UINT16(fmt);
			payload.channels = READ_LE_UINT16(fmt + 2);
			payload.sampleRate = READ_LE_UINT32(fmt + 4);
			payload.blockAlign = (int32)READ_LE_UINT16(fmt + 12);
			payload.bitsPerSample = READ_LE_UINT16(fmt + 14);
		}

		if (!memcmp(buffer + pos, "data", 4) && chunkSize > 0 && pos + 8 + chunkSize <= sizeBytes) {
			payload.data = buffer + pos + 8;
			payload.size = chunkSize;
			return true;
		}

		pos += 8 + ((chunkSize + 1) & ~1);
	}

	return false;
}

static bool isXmidi(const uint8 *data, int32 size) {
	if (size < 12)
		return false;
	if (!memcmp(data, "FORM", 4) && size >= 8 && !memcmp(data + 8, "XDIR", 4))
		return true;
	if (!memcmp(data, "XMID", 4))
		return true;
	return false;
}

static bool looksLikeAnim(const uint8 *data, int32 size) {
	if (size < 8)
		return false;
	const uint16 numKeyframes = READ_LE_UINT16(data);
	const uint16 numBones = READ_LE_UINT16(data + 2);
	if (numKeyframes == 0 || numKeyframes > 5000 || numBones == 0 || numBones > 256)
		return false;
	const uint32 minSize = 8u + numKeyframes * (8u + numBones * 8u);
	return (uint32)size >= minSize && (uint32)size <= minSize + 256u;
}

static bool writeRawFile(const char *outPath, const uint8 *data, int32 size) {
	FILE *fout = fopen(outPath, "wb");
	if (!fout)
		return false;
	fwrite(data, 1, size, fout);
	const bool ok = !ferror(fout);
	fclose(fout);
	return ok;
}

static bool writePaletteGpl(const char *outPath, const uint8 *data, int32 entryIndex) {
	FILE *fout = fopen(outPath, "wb");
	if (!fout) {
		fprintf(stderr, "Unable to open %s for writing\n", outPath);
		return false;
	}

	fprintf(fout, "GIMP Palette\n");
	fprintf(fout, "Name: LBA Palette %d\n", entryIndex);
	fprintf(fout, "#\n");

	for (int i = 0; i < 256; ++i) {
		fprintf(fout, "%3d %3d %3d  Index %d\n",
			data[i * 3 + 0], data[i * 3 + 1], data[i * 3 + 2], i);
	}

	const bool ok = !ferror(fout);
	fclose(fout);
	return ok;
}

#ifdef USE_PNG
static bool writePaletteStripPng(const char *outPath, const uint8 *data) {
	FILE *fout = fopen(outPath, "wb");
	if (!fout) {
		fprintf(stderr, "Unable to open %s for writing\n", outPath);
		return false;
	}

	png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
	if (!png) {
		fclose(fout);
		return false;
	}

	png_infop info = png_create_info_struct(png);
	if (!info) {
		png_destroy_write_struct(&png, nullptr);
		fclose(fout);
		return false;
	}

	if (setjmp(png_jmpbuf(png))) {
		png_destroy_write_struct(&png, &info);
		fclose(fout);
		return false;
	}

	png_init_io(png, fout);
	png_set_IHDR(png, info, 256, 1, 8, PNG_COLOR_TYPE_RGB,
		PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
	png_write_info(png, info);

	uint8 row[256 * 3];
	for (int i = 0; i < 256; ++i) {
		row[i * 3 + 0] = data[i * 3 + 0];
		row[i * 3 + 1] = data[i * 3 + 1];
		row[i * 3 + 2] = data[i * 3 + 2];
	}

	png_bytep rowPointer = row;
	png_write_image(png, &rowPointer);
	png_write_end(png, nullptr);

	png_destroy_write_struct(&png, &info);
	fclose(fout);
	return true;
}
#endif

static bool convertPaletteEntry(const uint8 *data, int32 size, int32 entryIndex, const char *outPath) {
	if (size != 256 * 3)
		return false;

	char gplPath[1100];
	char pngPath[1100];
	snprintf(gplPath, sizeof(gplPath), "%s.gpl", outPath);
	snprintf(pngPath, sizeof(pngPath), "%s.png", outPath);

	if (!writePaletteGpl(gplPath, data, entryIndex))
		return false;

#ifndef USE_PNG
	fprintf(stderr, "PNG support was not enabled at build time, skipping %s\n", pngPath);
	printf("Converted entry %d -> %s\n", entryIndex, gplPath);
	return true;
#else
	if (!writePaletteStripPng(pngPath, data))
		return false;
	printf("Converted entry %d -> %s (+ %s)\n", entryIndex, gplPath, pngPath);
	return true;
#endif
}

} // namespace

TwineEntryType twineDetectEntryType(const uint8 *data, int32 size, bool preferLba1) {
	if (!data || size <= 0)
		return TWINE_ENTRY_RAW;

	if (size == 256 * 3)
		return TWINE_ENTRY_PALETTE;

	if (size == 131884)
		return TWINE_ENTRY_SHADING_PALETTE;

	if (isCreativeVoc(data, size))
		return TWINE_ENTRY_VOC;

	if (isRiffWav(data, size))
		return TWINE_ENTRY_WAV;

	if (isXmidi(data, size))
		return TWINE_ENTRY_XMIDI;

	if (twineLooksLikeSjisFont(data, size))
		return TWINE_ENTRY_FONT_SJIS;

	if (twineLooksLikeMsDosFont(data, size))
		return TWINE_ENTRY_FONT_MSDOS;

	if (size == kScreenWidth * kScreenHeight)
		return TWINE_ENTRY_SCREEN;

	if (size == 256 * 256)
		return TWINE_ENTRY_TEXTURE;

	if (looksLikeAnim(data, size))
		return TWINE_ENTRY_ANIM;

	if (twineLooksLikeLbaFont(data, size))
		return TWINE_ENTRY_FONT;

	TwinE::BodyData body;
	bool isLba1 = false;
	if (twineTryParseBody(data, size, preferLba1, body, isLba1))
		return TWINE_ENTRY_BODY;

	if (looksLikeSpriteHeader(data, size) || looksLikeBrickSprite(data, size))
		return TWINE_ENTRY_SPRITE;

	return TWINE_ENTRY_RAW;
}

bool twineResolvePalette(const TwineConvertContext &ctx, int32 entryIndex, TwinePalette &out) {
	if (ctx.archivePalettes && ctx.fixedPaletteIndex >= 0 && ctx.fixedPaletteIndex < ctx.numEntries &&
		ctx.archivePalettes[ctx.fixedPaletteIndex].loaded) {
		out = ctx.archivePalettes[ctx.fixedPaletteIndex];
		return true;
	}

	if (ctx.archivePalettes && entryIndex + 1 < ctx.numEntries &&
		ctx.archivePalettes[entryIndex + 1].loaded) {
		out = ctx.archivePalettes[entryIndex + 1];
		return true;
	}

	if (ctx.archivePalettes && ctx.numEntries > 0 && ctx.archivePalettes[0].loaded) {
		out = ctx.archivePalettes[0];
		return true;
	}

	if (ctx.externalPalette && ctx.externalPalette->loaded) {
		out = *ctx.externalPalette;
		return true;
	}

	return false;
}

static bool convertEntryToPng(const uint8 *data, int32 size, TwineEntryType type, const TwinePalette &palette, const char *outPath) {
#ifndef USE_PNG
	(void)data;
	(void)size;
	(void)type;
	(void)palette;
	(void)outPath;
	fprintf(stderr, "PNG support was not enabled at build time\n");
	return false;
#else
	if (!palette.loaded) {
		fprintf(stderr, "No palette available for %s\n", outPath);
		return false;
	}

	DecodedImage image;
	if (type == TWINE_ENTRY_SCREEN) {
		image.width = kScreenWidth;
		image.height = kScreenHeight;
		image.pixels = (uint8 *)malloc(kScreenWidth * kScreenHeight);
		if (!image.pixels)
			return false;
		memcpy(image.pixels, data, kScreenWidth * kScreenHeight);
	} else if (type == TWINE_ENTRY_SPRITE || type == TWINE_ENTRY_TEXTURE) {
		if (type == TWINE_ENTRY_TEXTURE) {
			image.width = 256;
			image.height = 256;
			image.pixels = (uint8 *)malloc(256 * 256);
			if (!image.pixels)
				return false;
			memcpy(image.pixels, data, 256 * 256);
		} else if (!decodeSpriteEntry(data, size, image)) {
			return false;
		}
	} else {
		return false;
	}

	const bool ok = writeIndexedPng(outPath, image, palette);
	return ok;
#endif
}

static bool convertEntryToWav(const uint8 *data, int32 size, const char *outPath) {
	FILE *fout = fopen(outPath, "wb");
	if (!fout) {
		fprintf(stderr, "Unable to open %s for writing\n", outPath);
		return false;
	}

	bool ok = false;

	if (isCreativeVoc(data, size)) {
		char tempName[1024];
		snprintf(tempName, sizeof(tempName), "%s.voctmp", outPath);
		Common::File tempOut;
		tempOut.open(tempName, "wb");
		if (!tempOut.isOpen()) {
			fclose(fout);
			return false;
		}
		tempOut.write(data, size);
		tempOut.close();

		Common::File tempIn;
		tempIn.open(tempName, "rb");
		int pcmSize = 0;
		int rate = 0;
		byte *pcm = Audio::loadVOCFromStream(tempIn, pcmSize, rate);
		tempIn.close();
		remove(tempName);

		if (pcm && pcmSize > 0)
			ok = writePcmWav(fout, pcm, pcmSize, rate, 8, 1);
		free(pcm);
	} else if (isRiffWav(data, size)) {
		WavPayload payload;
		if (!parseWavPayload(data, size, payload)) {
			fclose(fout);
			return false;
		}

		if (payload.formatTag == 1) {
			ok = writePcmWav(fout, payload.data, payload.size, payload.sampleRate,
				payload.bitsPerSample, payload.channels);
		} else if (payload.formatTag == 17) {
			int32 numSamples = 0;
			int16 *pcm = decodeImaAdpcm(payload.data, payload.size, payload.blockAlign, &numSamples);
			if (pcm && numSamples > 0) {
				ok = writePcmWav(fout, pcm, numSamples * sizeof(int16), payload.sampleRate, 16, payload.channels);
				free(pcm);
			}
		} else {
			fprintf(stderr, "Unsupported WAVE compression format %u in %s\n", payload.formatTag, outPath);
		}
	}

	fclose(fout);
	if (!ok)
		remove(outPath);
	return ok;
}

const char *twineConvertedExtension(TwineEntryType type) {
	switch (type) {
	case TWINE_ENTRY_SCREEN:
	case TWINE_ENTRY_TEXTURE:
	case TWINE_ENTRY_SPRITE:
	case TWINE_ENTRY_FONT:
	case TWINE_ENTRY_FONT_SJIS:
	case TWINE_ENTRY_FONT_MSDOS:
		return "png";
	case TWINE_ENTRY_BODY:
		return "obj";
	case TWINE_ENTRY_VOC:
	case TWINE_ENTRY_WAV:
		return "wav";
	case TWINE_ENTRY_XMIDI:
		return "xmi";
	case TWINE_ENTRY_ANIM:
		return "anim";
	case TWINE_ENTRY_PALETTE:
		return "gpl";
	case TWINE_ENTRY_SHADING_PALETTE:
		return "osp";
	default:
		return "bin";
	}
}

bool twineConvertEntry(const uint8 *data, int32 size, int32 entryIndex, TwineEntryType type,
		const TwineConvertContext &ctx, const char *outPath) {
	char convertedPath[1100];
	snprintf(convertedPath, sizeof(convertedPath), "%s.%s", outPath, twineConvertedExtension(type));

	switch (type) {
	case TWINE_ENTRY_PALETTE:
		if (!convertPaletteEntry(data, size, entryIndex, outPath))
			return false;
		return true;
	case TWINE_ENTRY_SCREEN:
	case TWINE_ENTRY_TEXTURE:
	case TWINE_ENTRY_SPRITE: {
		TwinePalette palette;
		if (!twineResolvePalette(ctx, entryIndex, palette)) {
			fprintf(stderr, "No palette available for image entry %d\n", entryIndex);
			return false;
		}
		if (!convertEntryToPng(data, size, type, palette, convertedPath))
			return false;
		printf("Converted entry %d -> %s\n", entryIndex, convertedPath);
		return true;
	}
	case TWINE_ENTRY_FONT:
	case TWINE_ENTRY_FONT_SJIS:
	case TWINE_ENTRY_FONT_MSDOS:
		if (!twineConvertFontToPng(data, size, type, outPath))
			return false;
		printf("Converted entry %d -> %s.png\n", entryIndex, outPath);
		return true;
	case TWINE_ENTRY_BODY: {
		TwinePalette palette;
		if (!twineResolvePalette(ctx, entryIndex, palette))
			palette = TwinePalette();
		if (!twineConvertBodyToObj(data, size, ctx.preferLba1, palette, outPath))
			return false;
		printf("Converted entry %d -> %s.obj (+ .mtl)\n", entryIndex, outPath);
		return true;
	}
	case TWINE_ENTRY_VOC:
	case TWINE_ENTRY_WAV:
		if (!convertEntryToWav(data, size, convertedPath))
			return false;
		printf("Converted entry %d -> %s\n", entryIndex, convertedPath);
		return true;
	case TWINE_ENTRY_XMIDI:
	case TWINE_ENTRY_ANIM:
	case TWINE_ENTRY_SHADING_PALETTE:
		if (!writeRawFile(convertedPath, data, size))
			return false;
		printf("Extracted entry %d -> %s\n", entryIndex, convertedPath);
		return true;
	default:
		snprintf(convertedPath, sizeof(convertedPath), "%s.bin", outPath);
		if (!writeRawFile(convertedPath, data, size))
			return false;
		printf("Extracting entry %d -> %s (unrecognized format)\n", entryIndex, convertedPath);
		return true;
	}
}
