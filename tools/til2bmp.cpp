#include <fstream>
#include <iostream>
#include <string>
#include <zlib.h>
#include <cassert>
#include <sys/types.h>
#include <sstream>
#include <stdint.h>
#include <cstdio>
#include <cstring>
#include "common/endian.h"

/*
This tool converts EMI-TILEs into BMP-files, and supports both the format used in the Windows
Demo, as well as the PS2-format, it's worth to note that Windows uses 32-bit Bitmaps, while
PS2 uses 16-bit bitmaps, this tool currently converts both to 32-bit BMPs, for convenience,
since it's much easier to swap RGB to BGR that way. I might add down-converting back to 16-bit
when I get the time. The upconverting-function MAY be off by a bit, but as far as I could understand,
the PS2-format uses 16-bit, with alpha-bit first, then 5 bits per channel.

The current implementation also has a few limitation w.r.t. if there should be TILEs that aren't 640x480.

Also, I _THINK_ that it should work on Big-Endian-systems now, but I haven't gotten around to testing that yet.

Usage:
til2bmp <filename> 

somaen.
*/


struct BMPHeader{
	uint32_t size;
	uint32_t reserved;
	uint32_t offset;
	uint32_t headerSize;
	int32_t width;
	int32_t height;
	uint32_t nplanesbpp;
	uint32_t compress_type;
	uint32_t bmp_bytesz;
	int32_t hres;
	int32_t vres;
	uint32_t ncolors;
	uint32_t nimpcolors;
};

Bytef *decompress(Bytef *in, int size, uint32_t &outsize);

class LucasBitMap{
public:
	char *_data;
	inline uint32_t size(){	return _height*_width*_bpp; }
	uint32_t _width, _height, _bpp;
	~LucasBitMap(){ delete[] _data; }
	LucasBitMap() : _data(0), _width(0), _height(0), _bpp(4){}
	LucasBitMap(char* data, uint32_t width, uint32_t height,uint32_t bpp=4, bool copy=true);
	void MakeNewData(){ 
		printf("Size: %d Bpp: %d Width: %d Height: %d\n",size(),_bpp,_width,_height);
		_data = new char[size()]; 
	
	}
	void BGR2RGB();
	void MoreBits();
	void LessBits();
	void WriteBMP(const char* name);
};

void LucasBitMap::LessBits(){
	printf("Not implemented\n");
}

void LucasBitMap::MoreBits(){
	if(_bpp==2){
		char* newData = new char[_width*_height*4];
		char* to = newData;
		char red=0,green=0,blue=0;
		for(unsigned int i=0;i<_height;i++){
			for(unsigned int j=0;j<_width;j++){
				char byte2 = _data[i*_width*_bpp+j*2];
				char byte1 = _data[i*_width*_bpp+j*2+1];
			// Probably Alpha, then 555.
			// Red
				red = (byte1>>2)&31;
				red = red << 3 | red >> 2;
			// Green
				char green1 = (byte1&3);
				char green2 = (((byte2)>>5)&7);
				char green3 = green1 << 3 | green2;
				green = green3 << 3 | green3 >> 2 ;
			// Blue
				blue = (byte2)&31; 
				blue = blue << 3 | blue >> 2;
			// Some more magic to stretch the values 
				*to = red;
				to++;
				*to = green;
				to++;
				*to = blue;
				to++;
				*to = 0;
				to++;
			}
		}
		delete _data;
		_data = newData;
		_bpp=4;
	}
}

LucasBitMap::LucasBitMap(char* data, uint32_t width, uint32_t height,uint32_t bpp, bool copy)  : _data(data), _width(width), _height(height), _bpp(bpp){
	if(data==0)
		MakeNewData();
	else if(copy){
		MakeNewData();
		memcpy(_data, data, size());
	}
}

void LucasBitMap::WriteBMP(const char* name){
	std::fstream file(name, std::fstream::out | std::fstream::binary);
	BMPHeader header;
	unsigned short bm = TO_LE_16(19778);
	file.write((char*)&bm, 2);
	header.size = TO_LE_32(size()+54);
	header.reserved = TO_LE_32(0);
	header.width = TO_LE_32(_width);
	header.height = TO_LE_32(_height);
	header.offset = TO_LE_32(54);
	header.headerSize = TO_LE_32(40);
if(_bpp==4)
	header.nplanesbpp = TO_LE_32(2097153);
else if(_bpp==2)
	header.nplanesbpp = TO_LE_32(1048577);
	header.compress_type = TO_LE_32(0);
	header.bmp_bytesz = TO_LE_32(0);
	header.hres = TO_LE_32(2835);
	header.vres = TO_LE_32(2835);
	header.ncolors = TO_LE_32(0);
	header.nimpcolors = TO_LE_32(0);
	file.write((char *)&header, sizeof(BMPHeader));
	file.write(_data, size());
	file.close();
}

void LucasBitMap::BGR2RGB(){
	if(_bpp==4){
		uint32_t end = size();
		for(uint32_t i = 0; i < end; i += 4){
			char temp = _data[i+2];
			_data[i+2] = _data[i];
			_data[i] = temp;
		}
	}else if(_bpp==2){
		MoreBits();	
	}
}

char* GetLine(int lineNum, LucasBitMap* bit){
	int bpp = bit->_bpp;
	return bit->_data + (lineNum*(bit->_width*bpp));
}

char* GetLine(int lineNum, char* data, unsigned int width,int bpp){
	return data + (lineNum*(width*bpp));
}

// Expects 5 LucasBitmaps, and returns a LucasBitmap untiled.
LucasBitMap* MakeFullPicture(LucasBitMap** bits){
	LucasBitMap* fullImage = new LucasBitMap(0,640,480,bits[0]->_bpp);
	
	const int tWidth = 256*bits[0]->_bpp; // All of them should have the same bpp. (32)
	int bpp = bits[0]->_bpp;
	
	char* target = fullImage->_data;
	for(int i=0;i<256;i++){
		/* This can be modified to actually use the last 32 lines.
		 * We simply put the lower half on line 223 and down to line 32, 
		 * then skip the last 32.
		 * While the upper half is put on line 479 and down to line 224.
		 */
		if(i<224){ // Skip blank space
			target=GetLine(223-i,fullImage);
			
			memcpy(target,GetLine(i,bits[3]),tWidth);
			target += bits[3]->_width*bpp;
		
			memcpy(target,GetLine(i,bits[4]),tWidth);
			target += bits[4]->_width*bpp;
			
			memcpy(target,GetLine(i,bits[2])+128*bpp,128*bpp);
			target += bpp*bits[2]->_width/2;
		}
		
		// Top half of course
		
		target = GetLine(479-i,fullImage);
		
		memcpy(target,GetLine(i,bits[0]),tWidth);
		target += bits[0]->_width*bpp;
		
		memcpy(target,GetLine(i,bits[1]),tWidth);
		target += bits[1]->_width*bpp;
		
		memcpy(target,GetLine(i,bits[2]),128*bpp);
		target += bpp*bits[2]->_width/2;
		
	}
	fullImage->BGR2RGB();
	
	return fullImage;
}

void ProcessFile(const char *_data, uint32_t size, std::string name){
	std::stringstream til;
	uint32_t outsize = 0, bpp = 4;
	Bytef *data = decompress((Bytef *)_data, size, outsize);
	if(!data)
		return;
	til.write((const char *)data, outsize);
	delete[] data;
	uint32_t id, bmoffset, rects, b, c, numImages;

	til.read((char *)&id, 4);
	FROM_LE_32(id);
	til.read((char *)&bmoffset, 4);
	FROM_LE_32(bmoffset);
	til.read((char *)&rects, 4);
	FROM_LE_32(rects);
	til.read((char *)&b, 4);
	FROM_LE_32(b);
	til.read((char *)&c, 4);
	FROM_LE_32(c);

// We want to actually read numImages and bpp
	til.seekg(bmoffset+16,std::ios::beg);
	til.read((char*)&numImages, 4);
	FROM_LE_32(numImages);

	if(numImages < 5){
		printf("This tile has less than 5 tiles, I don't know how to parse it\n");
	}

	til.seekg(16,std::ios::cur);
	til.read((char*) &bpp, 4);
	FROM_LE_32(bpp);
	bpp = bpp/8;

	printf("Detected %d bpp\n",bpp*8);

	til.seekg(bmoffset+128, std::ios::beg);

	uint32_t width = 0, height = 0;
	
	LucasBitMap **allTheData = new LucasBitMap*[5];
	
	for (uint32_t i = 0; i < 5; ++i) {
		til.read((char *)&width, 4);
		FROM_LE_32(width);
		til.read((char *)&height, 4);
		FROM_LE_32(height);
		uint32_t size = width*height*bpp;
		char *data = new char[size];
		char *outnamet = new char[64];
		sprintf(outnamet,"%d.bmp",i);
		til.read(data, size);
		
		allTheData[i] = new LucasBitMap(data, width, height,bpp,false);
//		allTheData[i]->WriteBMP(outnamet);	
	}
	LucasBitMap* bit = MakeFullPicture(allTheData);
	bit->WriteBMP(name.c_str());
	
	for (uint32_t i = 0; i < 5; ++i) {
		delete allTheData[i];
	}

	delete bit;
	delete[] allTheData;
}


Bytef *decompress(Bytef *in, int size, uint32_t &outsize){
	const uint32_t block = 8*1024*1024; 
	Bytef *dest = new Bytef[block]; // 8 MiB ought to be enough.
	
	int success = 0;
	z_stream_s zStream;
	
	zStream.next_in = Z_NULL;
	zStream.avail_in = 0;
	zStream.zalloc = Z_NULL;
	zStream.zfree = Z_NULL;
	zStream.opaque = Z_NULL;
	
	success = inflateInit2(&zStream, 16+MAX_WBITS);
	if(success!=Z_OK){
		std::cout << "ZLIB failed to initialize\n";
		return 0;
	}
	zStream.avail_in = size;
	zStream.next_in = in;
	zStream.avail_out = block;
	zStream.next_out = dest;
	
	success = inflate(&zStream, Z_NO_FLUSH);
	
	outsize = zStream.total_out;
	
	if(success != Z_STREAM_END) {
		std::cout << "ERROR: decompressed size bigger than 8 MiB\n";
		return 0;
	}
	return dest;
}

int main(int argc, char **argv){
	if (argc < 2) {
		std::cout << "No Argument" << std::endl;
		std::cout << "Usage: til2bmp <filename> [PS2]" << std::endl;
		return 0;
	}
	// Cheap fix to support PS2
	bool ps2=false;
	if(argc>2)
		ps2 = true;
	
	std::fstream file(argv[1], std::fstream::in|std::fstream::binary);
	if (!file.is_open()) {
		std::cout << "Could not open file" << std::endl;
		return 0;
	}
	
	std::string outname = argv[1];
	outname += ".bmp";
	
	file.seekg(0, std::ios::end);
	int end = (int)file.tellg();
	file.seekg(0, std::ios::beg);
	char *data = new char[end];
	file.read(data, end);
	file.close();
	ProcessFile(data, end, outname);
	
	delete[] data;
}
