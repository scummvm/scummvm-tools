#include <fstream>
#include <iostream>
#include <string>
#include <zlib.h>
#include <cassert>
#include <sys/types.h>
#include <sstream>
#include <stdint.h>


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
	char *data;
	inline uint32_t size(){	return height*width*bpp; }
	uint32_t width, height, bpp;
	~LucasBitMap(){ delete[] data; }
	LucasBitMap() : data(0), width(0), height(0), bpp(4){}
	LucasBitMap(char* data, uint32_t width, uint32_t height,uint32_t bpp=4, bool copy=true);
	void MakeNewData(){ data = new char[size()]; }
	void BGR2RGB();
	void WriteBMP(const char* name);
};

LucasBitMap::LucasBitMap(char* data, uint32_t width, uint32_t height,uint32_t bpp, bool copy)  : width(width), height(height), bpp(bpp), data(data){
	if(data==0)
		MakeNewData();
	else if(copy){
		MakeNewData();
		memcpy(this->data, data, size());
	}
}

void LucasBitMap::WriteBMP(const char* name){
	std::fstream file(name, std::fstream::out | std::fstream::binary);
	BMPHeader header;
	unsigned short bm = 19778;
	file.write((char *)&bm, 2);
	header.size = size()+54;
	header.reserved = 0;
	header.width = width;
	header.height = height;
	header.offset = 54;
	header.headerSize = 40;
	header.nplanesbpp = 2097153;
	header.compress_type = 0;
	header.bmp_bytesz = 0;
	header.hres = 2835;
	header.vres = 2835;
	header.ncolors = 0;
	header.nimpcolors = 0;
	file.write((char *)&header, sizeof(BMPHeader));
	file.write(data, size());
	file.close();
}

void LucasBitMap::BGR2RGB(){
	uint32_t end = size();
	for(uint32_t i = 0; i < end; i += 4){
		char temp = data[i+2];
		data[i+2] = data[i];
		data[i] = temp;
	}
}

char* GetLine(int lineNum, LucasBitMap* bit){
	return bit->data + (lineNum*(bit->width*4));
}

char* GetLine(int lineNum, char* data, unsigned int width){
	return data + (lineNum*(width*4));
}

// Expects 5 LucasBitmaps, and returns a LucasBitmap untiled.
LucasBitMap* MakeFullPicture(LucasBitMap** bits){
	LucasBitMap* fullImage = new LucasBitMap(0,640,480);
	
	char* target = fullImage->data;
	for(int i=0;i<256;i++){
		/* This can be modified to actually use the last 32 lines.
		 * We simply put the lower half on line 223 and down to line 32, 
		 * then skip the last 32.
		 * While the upper half is put on line 479 and down to line 224.
		 */
		if(i<224){ // Skip blank space
			target=GetLine(223-i,fullImage);
			
			memcpy(target,GetLine(i,bits[3]),256*4);
			target += bits[3]->width*4;
			
			memcpy(target,GetLine(i,bits[4]),256*4);
			target += bits[4]->width*4;
			
			memcpy(target,GetLine(i,bits[2])+128*4,128*4);
			target += 4*bits[2]->width/2;
		}
		
		// Top half of course
		
		target = GetLine(479-i,fullImage);
		
		memcpy(target,GetLine(i,bits[0]),256*4);
		target += bits[0]->width*4;
		
		memcpy(target,GetLine(i,bits[1]),256*4);
		target += bits[1]->width*4;
		
		memcpy(target,GetLine(i,bits[2]),128*4);
		target += 4*bits[2]->width/2;		  
	}
	
	fullImage->BGR2RGB();
	
	return fullImage;
}

void ProcessFile(const char *_data, uint32_t size, std::string name){
	std::stringstream til;
	uint32_t outsize = 0;
	Bytef *data = decompress((Bytef *)_data, size, outsize);
	if(!data)
		return;
	til.write((const char *)data, outsize);
	delete[] data;
	uint32_t id, bmoffset, rects, b, c;
	
	til.read((char *)&id, 4);
	til.read((char *)&bmoffset, 4);
	til.read((char *)&rects, 4);
	til.read((char *)&b, 4);
	til.read((char *)&c, 4);
	
	til.seekg(bmoffset+128, std::ios::beg);
	
	uint32_t width = 0, height = 0;
	
	LucasBitMap **allTheData = new LucasBitMap*[5];
	
	for (uint32_t i = 0; i < 5; ++i) {
		til.read((char *)&width, 4);
		til.read((char *)&height, 4);
		uint32_t size = width*height*4;
		char *data = new char[size];
		
		til.read(data, size);
		
		allTheData[i] = new LucasBitMap(data, width, height,4,false);
		
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
		return 0;
	}
	
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
