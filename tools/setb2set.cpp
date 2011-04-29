/*
* TODO: Put GPL-license here
* Thanks to klusark, for his work.
* This version written by Einar Johan T. S√m√en
*/

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>

using namespace std;

typedef int uint32;

enum SectorType{
	camera,
	chernobyl,
	special,
	walk
};

enum SectorVisibility{
	visible
};

enum LightType{
	omni,
	direct
};

struct Section{
	const char* buf;
	float GetFloat()
	{
		float retVal = *(float*) buf;
		buf+=4;
		return retVal;
	}
	int GetInt()
	{
		int retVal = *(int*) buf;
		buf+=4;
		return retVal;
	}
	virtual uint32 load() = 0;
	virtual string MakeVeryLongString() = 0;
};

struct Sector : public Section
{
	string name;
	int ID; // byte;
	SectorType type;
	SectorVisibility visibility;
	float height;
	int numVertices; // byte;
	float* vertices; // 3 * numVertices.
	virtual uint32 load()
	{
		numVertices = GetInt();
		vertices = new float[3*numVertices];
		for(int i=0; i < numVertices; i++)
		{
			vertices[0+3*i]=GetFloat();
			vertices[1+3*i]=GetFloat();
			vertices[2+3*i]=GetFloat();
		}
		int nameLength = GetInt();
		name = buf;
		buf += nameLength;
		ID = GetInt();
		//visible = *buf;
		buf+=1;
		//type = *buf;
		buf+=4;
		int skip = (*(int*)buf); //64;
		buf+= (skip+1)*4;
		height = GetFloat();
		
		return 12*numVertices + nameLength + skip*4 + 25;
	}
	virtual string MakeVeryLongString()
	{
		cout << "SECTOR-MAKESTRING";
		return "";
	}
};

struct Setup : public Section
{
	string name;
	string background;
	string zbuffer;
	float* position;
	float* interest;
	float roll;
	float fov;
	float nclip;
	float fclip;
	virtual uint32 load()
	{
		cout << "Running setup-load";
		name = buf; // Parse a string really
		buf += 128;

		// Skip an unknown number
		buf += 4;

		char title[128];
		
		char in = 0;
		int i = 0;
		do {
			in = *buf;
			++i;
			++buf;
		}while(in);

		position = new float[3];
		
		position[0] = GetFloat();
		position[1] = GetFloat();
		position[2] = GetFloat();
		
		interest = new float[3];

		interest[0] = GetFloat();
		interest[1] = GetFloat();
		interest[2] = GetFloat();
		
		roll = GetFloat();
		fov  = GetFloat();
		nclip = GetFloat();
		fclip = GetFloat();
		//	cout << MakeVeryLongString();

		return 172+i;
	}

	virtual string MakeVeryLongString()
	{
		cout << "SETUP-MAKESTRING";
		stringstream ss;
		ss << "\tname\t" << name << "\n";
		// background
		// zbuffer
		ss << "\tposition\t" << position[0] << "\t" << position[1] << "\t" << position[2] << "\n";
		ss << "\tinterest\t" << interest[0] << "\t" << interest[1] << "\t" << interest[2] << "\n";
		ss << "\troll\t" << roll << "\n";
		ss << "\fov\t" << fov << "\n";
		ss << "\tnclip\t" << nclip << "\n";
		ss << "\tfclip\t" << fclip << "\n";

		return ss.str();
	}
};

struct Light : public Section
{
	string name;
	LightType type;
	float* position;
	float* direction;
	float intensity;
	float umbraangla;
	float penumbraangle;
	int* color; // Byte
	virtual uint32 load()
	{
		return 100; // Skipme
	}
	virtual string MakeVeryLongString()
	{
		cout << "LIGHT-MAKESTRING";
		return "";
	}
};

struct Set{
	string setName;
	uint32 numSetups;
	uint32 numColormaps;
	uint32 numLights;
	uint32 numSectors;
	vector<Section*> setups;
	vector<string> colormaps;
	vector<Section*> lights;
	vector<Section*> sectors;
	virtual string MakeVeryLongString()
	{
		stringstream ss;
		// colormaps
		ss << "section: colormaps\n"; // we don't have any.
		// setups
		ss << "section: setups\n";
		vector<Section*>::iterator it;
		cout << "Size of setups: " << setups.size();
		for(it = setups.begin();it!=setups.end();++it)
		{
			cout << "HELLO";
			ss << (*it)->MakeVeryLongString();
		}
		// lights
		cout << "Size of lights: " << lights.size();
		ss << "section: lights\n";
		for(it = lights.begin();it!=lights.end();it++)
		{
			ss << (*it)->MakeVeryLongString();
		}
		// sectors
		cout << "Size of sectors: " << sectors.size();
		ss << "section: sectors\n";
		for(it = sectors.begin();it!=sectors.end();it++)
		{
			cout << "HELLO";
			ss << (*it)->MakeVeryLongString();
		}
		return ss.str();
	}
	Set(const char* buf)
	{
		numSetups = *(int*)buf;
		buf += 4;
		setups.reserve(numSetups);
		cout << "We should have " << numSetups << "setups\n";
		for(int i=0;i<numSetups;i++)
		{
			cout << "Adding the " << i << "th Setup\n";
			setups[i]=new Setup();
			setups[i]->buf = buf;
			buf += setups[i]->load();
		}
		// Ignore all lights, since we don't know how to handle them
		numLights = *(int*)buf;
		buf+=4;

		buf+= numLights*100;

		numSectors = *(int*)buf;
		buf+=4;
		sectors.reserve(numSectors);
		cout << "Numsectors: " << numSectors << endl;
		for(int i=0;i<numSectors;i++)
		{
			sectors[i]=new Sector();
			sectors[i]->buf = buf;
			buf+= sectors[i]->load();
		}
	}
};

int main(int argc, char** argv){
	char* buf;
	FILE* f;
	
	f = fopen(argv[1],"rb");
	fseek(f,0,SEEK_END);
	uint32 size = ftell(f);
	buf = new char[size];
	fseek(f,0,SEEK_SET);
	
	int result = fread(buf,1,size,f);
	assert(result==size);
	
	Set* ourSet = new Set(buf);
	cout << ourSet->MakeVeryLongString();
}
