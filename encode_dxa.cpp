/* encode_dxa - compressor for dxa files
 * Copyright (c) 2006 The ScummVM Team
 * Copyright (c) 2006 Benjamin Haisch
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

#include "compress.h"
#include "util.h"

#include <png.h>
#include <zlib.h>

const uint32 typeDEXA = 0x41584544;
const uint32 typeFRAM = 0x4d415246;
const uint32 typeWAVE = 0x45564157;
const uint32 typeCMAP = 0x50414D43;
const uint32 typeNULL = 0x4C4C554E;

#define	 BUFFER_LEN	1024

//#define USE_ZMBV

#ifdef USE_ZMBV
#include "zmbv.h"
#endif

static CompressMode gCompMode = kMP3Mode;

class DxaEncoder {
private:
	FILE *_dxa;
	int _width, _height, _framerate, _framecount;
	uint8 *_prevframe, *_prevpalette;

#ifdef USE_ZMBV
	VideoCodec *_codec;

	byte *_codecBuf;
	int _codecBufSize;
#endif

public:
	DxaEncoder(char *filename, int width, int height, int fps);
	~DxaEncoder();
	void writeHeader();
	void writeNULL();
	void writeFrame(uint8 *frame, uint8 *palette);
};

DxaEncoder::DxaEncoder(char *filename, int width, int height, int framerate) {
	_dxa = fopen(filename, "wb");
	_width = width;
	_height = height;
	_framerate = framerate;
	_framecount = 0;
	_prevframe = new uint8[_width * _height];
	_prevpalette = new uint8[768];

	writeHeader();

#ifdef USE_ZMBV
	_codec = new VideoCodec();
	_codec->SetupCompress(width, height);
	_codecBufSize = _codec->NeededSize(width, height, ZMBV_FORMAT_8BPP);
	_codecBuf = (byte *)malloc(_codecBufSize);
#endif
}

DxaEncoder::~DxaEncoder() {
	fseek(_dxa, 0, SEEK_SET);

	writeHeader();

	fclose(_dxa);

	delete[] _prevframe;
	delete[] _prevpalette;
}

void DxaEncoder::writeHeader() {
	//DEXA
	uint8 version = 0;

	writeUint32LE(_dxa, typeDEXA);
	writeByte(_dxa, version);

	writeUint16BE(_dxa, _framecount);
	writeUint32BE(_dxa, _framerate);
	writeUint16BE(_dxa, _width);
	writeUint16BE(_dxa, _height);
}

void DxaEncoder::writeNULL() {
	//NULL
	writeUint32LE(_dxa, typeNULL);
}

void DxaEncoder::writeFrame(uint8 *frame, uint8 *palette) {
	if (_framecount == 0 || memcmp(_prevpalette, palette, 768)) {
		writeUint32LE(_dxa, typeCMAP);
		fwrite(palette, 768, 1, _dxa);

		memcpy(_prevpalette, palette, 768);
	} else {
		writeNULL();
	}

#ifdef USE_ZMBV
	uint8 cpalette[1024];

	for (int i = 0; i < 256; i++) {
		cpalette[i * 4 + 0] = palette[i * 3 + 0];
		cpalette[i * 4 + 1] = palette[i * 3 + 1];
		cpalette[i * 4 + 2] = palette[i * 3 + 2];
		cpalette[i * 4 + 3] = 0;
	}

	_codec->PrepareCompressFrame(0, ZMBV_FORMAT_8BPP, (char *)cpalette, _codecBuf, _codecBufSize);
#endif

	if (_framecount == 0 || memcmp(_prevframe, frame, _width * _height)) {
		//FRAM
		uint8 compType;

		writeUint32LE(_dxa, typeFRAM);

		if (_framecount == 0)
			compType = 2;
		else
			compType = 3;

#ifdef USE_ZMBV
		compType = 10;
#endif
		switch (compType) {
		case 2:
			{
				uLong outsize = _width * _height;
				uint8 *outbuf = new uint8[outsize];

				compress2(outbuf, &outsize, frame, _width * _height, 9);

				writeByte(_dxa, compType);

				writeUint32BE(_dxa, outsize);

				fwrite(outbuf, outsize, 1, _dxa);

				delete[] outbuf;

				break;
			}
		case 3:
			{
				uLong outsize1 = _width * _height;
				uLong outsize2 = outsize1;
				uLong outsize;
				uint8 *outbuf;
				uint8 *outbuf1 = new uint8[outsize1];
				uint8 *outbuf2 = new uint8[outsize2];
				uint8 *xorbuf = new uint8[_width * _height];

				for (int i = 0; i < _width * _height; i++)
					xorbuf[i] = _prevframe[i] ^ frame[i];

				compress2(outbuf1, &outsize1, xorbuf, _width * _height, 9);
				compress2(outbuf2, &outsize2, frame, _width * _height, 9);

				if (outsize1 < outsize2) {
					compType = 3;
					outsize = outsize1;
					outbuf = outbuf1;
				} else {
					compType = 2;
					outsize = outsize2;
					outbuf = outbuf2;
				}

				writeByte(_dxa, compType);

				writeUint32BE(_dxa, outsize);

				fwrite(outbuf, outsize, 1, _dxa);

				delete[] outbuf1;
				delete[] outbuf2;
				delete[] xorbuf;

				break;
			}
#ifdef USE_ZMBV
		case 10:
			{
				int outsize;
				void *ptr;

				for (int i = 0; i < _height; i++) {
					ptr = frame + i * _width;
					_codec->CompressLines(1, &ptr);
				}
				outsize = _codec->FinishCompressFrame();

				writeByte(_dxa, compType);
				writeUint32BE(_dxa, outsize);
				fwrite(_codecBuf, outsize, 1, _dxa);
				break;
			}
#endif
		}

		memcpy(_prevframe, frame, _width * _height);

	} else {
		writeNULL();
	}

	_framecount++;
}

int read_png_file(char* filename, unsigned char *&image, unsigned char *&palette, int &width, int &height) {
	png_byte header[8];

	png_byte color_type;
	png_byte bit_depth;

	png_structp png_ptr;
	png_infop info_ptr;
	int number_of_passes;
	png_bytep *row_pointers;

	FILE *fp = fopen(filename, "rb");
	if (!fp) {
		printf("read_png_file: Can't open file: %s\n", filename);
		exit(-1);
	}
	fread(header, 1, 8, fp);
	if (png_sig_cmp(header, 0, 8))
		return 1;

	png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);

	if (!png_ptr)
		return 1;

	info_ptr = png_create_info_struct(png_ptr);
	if (!info_ptr)
		return 1;

	if (setjmp(png_jmpbuf(png_ptr)))
		return 1;

	png_init_io(png_ptr, fp);
	png_set_sig_bytes(png_ptr, 8);

	png_read_info(png_ptr, info_ptr);

	width = info_ptr->width;
	height = info_ptr->height;
	color_type = info_ptr->color_type;
	bit_depth = info_ptr->bit_depth;

	if (color_type != PNG_COLOR_TYPE_PALETTE) {
		palette = NULL;
		return 2;
	}

	number_of_passes = png_set_interlace_handling(png_ptr);
	png_read_update_info(png_ptr, info_ptr);

	// read file
	if (setjmp(png_jmpbuf(png_ptr)))
		return 1;

	row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
	for (int y=0; y<height; y++)
		row_pointers[y] = (png_byte*) malloc(info_ptr->rowbytes);

	png_read_image(png_ptr, row_pointers);

	if (image) {
		image = new unsigned char[width * height];
		for (int y=0; y<height; y++)
			memcpy(&image[y*width], row_pointers[y], info_ptr->rowbytes);
	}

	for (int y=0; y<height; y++)
		free(row_pointers[y]);
	free(row_pointers);

	if (palette) {
		png_colorp pngpalette;
		int num_palette;

		png_get_PLTE(png_ptr, info_ptr, &pngpalette, &num_palette);

		palette = new unsigned char[768];

		memcpy(palette, pngpalette, 768);

		free(pngpalette);
	}

	fclose(fp);

	return 0;
}

void readSmackerInfo(char *filename, int &width, int &height, int &framerate, int &frames) {
	FILE *smk = fopen(filename, "rb");
	if (!smk) {
		printf("readSmackerInfo: Can't open file: %s\n", filename);
		exit(-1);
	}

	uint32 flags;

	// Skip the signature. We could use it to verify that it's a SMK file,
	// but if it wasn't, how did we ever extract the PNG frames from it?

	readUint32LE(smk);

	width = readUint32LE(smk);
	height = readUint32LE(smk);
	frames = readUint32LE(smk);
	framerate = readUint32LE(smk);
	flags = readUint32LE(smk);

	// If the Y-interlaced or Y-doubled flag is set, the RAD Video Tools
	// will have scaled the frames to twice their original height.

	if ((flags & 0x02) || (flags & 0x04))
		height *= 2;

	fclose(smk);
}

void convertWAV(char *wavName, char *prefix) {
	const char *ext;
	char outName[256];

	switch (gCompMode) {
	case kMP3Mode:
		ext = "mp3"; break;
	case kVorbisMode:
		ext = "ogg"; break;
	case kFlacMode:
		ext = "fla"; break;
	default:
		error("Unknown compression mode");
	}

	printf("Encoding audio...");
	fflush(stdout);

	sprintf(outName, "%s.%s", prefix, ext);
	encodeAudio(wavName, false, -1, outName, gCompMode);
}

void showhelp(char *exename) {
	printf("\nUsage: %s <inputfile> \n", exename);

	printf("\nParams:\n");
	printf(" --mp3        encode to MP3 format (default)\n");
	printf(" --vorbis     encode to Vorbis format\n");
	printf(" --flac       encode to Flac format\n");
	printf("(If one of these is specified, it must be the first parameter.)\n");

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

	printf("\nFlac mode params:\n");
	printf(" [params]     optional arguments passed directly to the encoder\n");
	printf("              recommended is: --best -b 1152\n");

	printf("\n --help     this help message\n");

	printf("\n\nIf a parameter is not given the default value is used\n");
	printf("If using VBR mode for MP3 -b and -B must be multiples of 8; the maximum is 160!\n");
	printf("Use the `mac' option instead of a filename if converting simon2mac sounds\n");
	exit(2);
}

int main(int argc, char *argv[]) {
	if (argc < 2)
		showhelp(argv[0]);

	char strbuf[512];
	int width, height, framerate, frames;
       
	/* compression mode */
	gCompMode = kMP3Mode;
	int i = 1;
	if (!strcmp(argv[1], "--mp3")) {
		gCompMode = kMP3Mode;
		i++;
	} else if (!strcmp(argv[1], "--vorbis")) {
		gCompMode = kVorbisMode;
		i++;
	} else if (!strcmp(argv[1], "--flac")) {
		gCompMode = kFlacMode;
		i++;
	}

	switch (gCompMode) {
	case kMP3Mode:
		if (!process_mp3_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kVorbisMode:
		if (!process_ogg_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	case kFlacMode:
		if (!process_flac_parms(argc, argv, i))
			showhelp(argv[0]);
		break;
	}

	i = argc - 1;
	char *prefix = argv[i++];

	// check if the wav file exists.
	sprintf(strbuf, "%s.wav", prefix);
	struct stat statinfo;
	if (!stat(strbuf, &statinfo)) {
		convertWAV(strbuf, prefix);
	}

	// read some data from the Smacker file.
	sprintf(strbuf, "%s.smk", prefix);
	readSmackerInfo(strbuf, width, height, framerate, frames);

	printf("Width = %d, Height = %d, Framerate = %d, Frames = %d\n",
		   width, height, framerate, frames);

	// create the encoder object
	sprintf(strbuf, "%s.dxa", prefix);
	DxaEncoder dxe(strbuf, width, height, framerate);

	// No sound block
	dxe.writeNULL();

	uint8 *image, *palette;
	int framenum = 0;

	printf("Encoding video...");
	fflush(stdout);

	for (int f = 0; f < frames; f++) {
		if (frames > 999)
			sprintf(strbuf, "%s%04d.png", prefix, framenum);
		else if (frames > 99)
			sprintf(strbuf, "%s%03d.png", prefix, framenum);
		else if (frames > 9)
			sprintf(strbuf, "%s%02d.png", prefix, framenum);
		else
			sprintf(strbuf, "%s%d.png", prefix, framenum);

		int r = read_png_file(strbuf, image, palette, width, height);

		if (!palette) {
			printf("Error: 8-bit 256-color image expected!\n");
			exit(0);
		}

		if (!r) {
			dxe.writeFrame(image, palette);
		}

		if (image) delete[] image;
		if (palette) delete[] palette;

		if (r)
			break;
                
		framenum++;

		if (framenum % 20 == 0) {
			printf("\rEncoding video...%d%% (%d of %d)", 100 * framenum / frames, framenum, frames);
			fflush(stdout);
		}
	}
        
	printf("\rEncoding video...100%% (%d of %d)\n", frames, frames);
        
	return 0;
}


