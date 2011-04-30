#include <fstream>
#include <iostream>
#include <string>
#include <zlib.h>
#include <cassert>
#include <sstream>

typedef unsigned int uint;

struct BMPHeader
{
	//unsigned short bm;
	unsigned int size;
	unsigned int reserved;
	unsigned int offset;
	unsigned int headerSize;
	int width;
	int height;
	unsigned int nplanesbpp;
	unsigned int compress_type;
	unsigned int bmp_bytesz;
	int hres;
	int vres;
	unsigned int ncolors;
	unsigned int nimpcolors;
};

Bytef *decompress(Bytef *in, int size, int &outsize);

class LucasBitMap
{
private:
	inline uint size(){	return height*width*bpp; }
public:
	char *data;
	unsigned int width, height, bpp;
	~LucasBitMap();
	LucasBitMap() : data(0), width(0), height(0), bpp(4){}
	LucasBitMap(char* data, uint width, uint height,uint bpp=4);
	void MakeNewData();
	void AdjustHeight(int newHeight);
	void BGR2RGB();
	void UpsideDown();
	void AddToRightOfThis(LucasBitMap* bitmap);
	void AddBelowThis(LucasBitMap* bitmap);
	LucasBitMap* GetSubImage(int start, int end);
	void WriteBMP(std::string name);
};

LucasBitMap::~LucasBitMap()
{
	delete[] data;
}

LucasBitMap::LucasBitMap(char* data, uint width, uint height,uint bpp)
{
	this->width = width;
	this->height = height;
	this->bpp = bpp;
	MakeNewData();
	memcpy(this->data, data, size());
}


void LucasBitMap::MakeNewData() // If it needs deletion, please do that before
{
	data = new char[size()];
}

void LucasBitMap::WriteBMP(std::string name)
{
	std::cout << "WriteBMP("<<name<<")\n";
	std::fstream file(name.c_str(), std::fstream::out | std::fstream::binary);
	BMPHeader header;
	int size = width*height*4;
	//header.bm = 19778;
	unsigned short bm = 19778;
	file.write((char *)&bm, 2);
	header.size = size+54;
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
	file.write(data, size);
	file.close();
}

void LucasBitMap::AdjustHeight(int newHeight)
{
	UpsideDown();
	height = newHeight;
	char *newData = new char[size()];
	memcpy(newData, data, size());
	delete[] data;
	data = newData;
	UpsideDown();
}

void LucasBitMap::BGR2RGB()
{
	int end = size();
	for(int i = 0; i < end; i += 4)
	{
		char temp = data[i+2];
		data[i+2] = data[i];
		data[i] = temp;
	}
}

char* GetLine(int lineNum, char* data, unsigned int width)
{
	return data + (lineNum*(width*4));
}

void LucasBitMap::UpsideDown()
{
	unsigned lineLength = width*4;
	char* fixedData = new char[size()];
	for(uint i = 0; i < height; i++)
	{
		char* to = GetLine(i,fixedData, width);
		char* from = GetLine(height-i-1,data, width);
		memcpy(to,from,lineLength);
	}
	memcpy(data, fixedData, size());
	delete[] fixedData;
}

void LucasBitMap::AddToRightOfThis(LucasBitMap* bitmap)
{
	if(height != bitmap->height)
		std::cout << "WARNING, can't combine with differing heights\n";
	int newSize = size() + bitmap->size();
	char* dest = new char[newSize];
	int newWidth = bitmap->width+width;
	for(uint i = 0; i < height; i++)
	{
		char *part1 = GetLine(i,data,width);
		char *part2 = GetLine(i,bitmap->data,bitmap->width);
		char *to = GetLine(i, dest, newWidth);
		memcpy(to,part1,width*4);
		memcpy(to+width*4,part2,bitmap->width*4);
	}
	delete[] data;
	width+=bitmap->width;
	MakeNewData();
	memcpy(data,dest,newSize);
	delete[] dest;
}

void LucasBitMap::AddBelowThis(LucasBitMap* bitmap)
{
	if(width != bitmap->width)
		std::cout << "WARNING, can't combine with differing widths\n";
	int newSize = size() + bitmap->size();
	char* dest = new char[newSize];

	for(uint i = 0; i < bitmap->height; i++)
	{
		char* from = GetLine(i,bitmap->data,width);
		char* to = GetLine(i,dest,width);
		memcpy(to,from,width*4);
	}	

	char *lowerFrom = dest + bitmap->size();
	for(uint i=0; i < height; i++)
	{
		char *from = GetLine(i,data,width);
		char *to = GetLine(i,lowerFrom,width);
		memcpy(to,from,width*4);
	}		
	delete[] data;
	height+=bitmap->height;
	MakeNewData();
	memcpy(data,dest,newSize);
	delete[] dest;
}

LucasBitMap* LucasBitMap::GetSubImage(int start, int end)
{
	LucasBitMap *dest = new LucasBitMap();
	dest->width = end-start;
	dest->height = height;
	dest->MakeNewData();
	for(uint i = 0; i < height; i++)
	{
		char* from = GetLine(i,data,width);
		char* to = GetLine(i,dest->data,dest->width);
		memcpy(to,from+start*4,dest->width*4);
	}
	return dest;
}


void MakeTheBiggerPicture(LucasBitMap** bits, std::string name)
{
	// Get the parts that belong in the rightmost 128 pixels of the screen
	// They are in tile 3, half each.
	LucasBitMap* extra1 = bits[2]->GetSubImage(0,128);
	LucasBitMap* extra2 = bits[2]->GetSubImage(128,256);

	bits[0]->AddToRightOfThis(bits[1]);
	bits[0]->AddToRightOfThis(extra1);
	bits[3]->AddToRightOfThis(bits[4]);


	bits[3]->AddToRightOfThis(extra2);
	bits[3]->AdjustHeight(224);

	delete bits[1];
	delete bits[2];
	delete bits[4];
	delete extra1;
	delete extra2;


	bits[0]->AddBelowThis(bits[3]);
	delete bits[3];

	bits[0]->WriteBMP(name.c_str());
	delete bits[0];

}

void ProcessFile(const char *_data, int size, std::string name)
{
	std::stringstream til;
	int outsize = 0;
	Bytef *data = decompress((Bytef *)_data, size, outsize);
	til.write((const char *)data, outsize);
	delete[] data;
	int id, bmoffset, rects, b, c;

	til.read((char *)&id, 4);
	til.read((char *)&bmoffset, 4);
	til.read((char *)&rects, 4);
	til.read((char *)&b, 4);
	til.read((char *)&c, 4);
	
	til.seekg(bmoffset+128, std::ios::beg);
	
	int width = 0, height = 0;
	
	LucasBitMap **allTheData = new LucasBitMap*[5];
	int *sizes = new int[5];
	for (int i = 0; i < 5; ++i) {
		til.read((char *)&width, 4);
		til.read((char *)&height, 4);
		unsigned int size = width*height*4;
		char *data = new char[size];
		
		til.read(data, size);
		
		allTheData[i] = new LucasBitMap(data, width, height);
		allTheData[i]->BGR2RGB();
		allTheData[i]->UpsideDown();

		sizes[i] = size;
		delete[] data;
	}
	MakeTheBiggerPicture(allTheData, name);
	delete[] sizes;
	delete[] allTheData;
}


Bytef *decompress(Bytef *in, int size, int &outsize)
{
	const unsigned int block = 8192*1024; 
	int success = 0;
	z_stream_s zStream;
	zStream.next_in = Z_NULL;
	zStream.avail_in = 0;
	zStream.zalloc = Z_NULL;
	zStream.zfree = Z_NULL;
	zStream.opaque = Z_NULL;


	success = inflateInit2(&zStream, 16+MAX_WBITS);

	Bytef *dest = new Bytef[block]; // Please don't tell me we have larger compression-ratios than this;

	zStream.avail_in = size;
	zStream.next_in = in;
			
	zStream.avail_out = block;
	zStream.next_out = dest;
				
	success = inflate(&zStream, Z_NO_FLUSH);

	outsize = zStream.total_out;
					
				
	if(success != Z_STREAM_END) {
		std::cout << "Oops, more than 8 MiB needed\n";
		return 0;
	}
	return dest;
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		std::cout << "No Argument" << std::endl;
		return 0;
	}
	std::fstream file(argv[1], std::fstream::in|std::fstream::binary);
	if (!file.is_open()) {
		std::cout << "Could not open file" << std::endl;
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
