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

enum SectorType {
	NoneType = 0,
	WalkType = 0x1000,
	FunnelType = 0x1100,
	CameraType = 0x2000,
	SpecialType = 0x4000,
	HotType = 0x8000
};


enum LightType{
	omni,
	direct
};

class Data {
public:
	Data(const char *data);
	float GetFloat();
	int GetInt();
	bool GetBool();
	string GetString(int length);
	string GetNullTerminatedString();
	void Skip(int val);
private:
	const char *buf;
};

Data::Data(const char *data)
{
	buf = data;
}

float Data::GetFloat()
{
	float retVal = *(float *) buf;
	buf += 4;
	return retVal;
}

int Data::GetInt()
{
	int retVal = *(int *) buf;
	buf += 4;
	return retVal;
}

bool Data::GetBool()
{
	bool retVal = *(bool *) buf;
	buf += 1;
	return retVal;
}

string Data::GetString(int length)
{
	//kind of a hack
	string s = string(buf);
	buf += length;
	return s;
}

string Data::GetNullTerminatedString()
{
	string s = string(buf);
	buf += s.length()+1;
	return s;
}

void Data::Skip(int val)
{
	buf += val;
}

struct Section {
public:
	Section(Data *data);
	//virtual uint32 load() = 0;
	virtual string ToString() = 0;
protected:
	Data *data;
};

Section::Section(Data *data)
{
	this->data = data;
}

class Sector : public Section
{
public:
	Sector(Data *data);

	virtual string ToString();
private:
	string name;
	int ID; // byte;
	SectorType type;
	float height;
	int numVertices; // byte;
	float* vertices; // 3 * numVertices.
	bool visible;
};

Sector::Sector(Data *data) : Section(data)
{
	numVertices = data->GetInt();
	vertices = new float[3*numVertices];
	for(int i=0; i < numVertices; i++)
	{
		vertices[0+3*i] = data->GetFloat();
		vertices[1+3*i] = data->GetFloat();
		vertices[2+3*i] = data->GetFloat();
	}
	int nameLength = data->GetInt();

	name = data->GetString(nameLength);
	ID = data->GetInt();
	visible = data->GetBool();
	type = (SectorType)data->GetInt();
	int skip = data->GetInt();
	data->Skip(skip*4);
	height = data->GetFloat();
}

string Sector::ToString()
{
	stringstream ss;
	ss.precision(6);
	ss.setf(ios::fixed,ios::floatfield);
	ss << "\tsector\t" << name << endl;
	ss << "\tID\t" << ID << endl;
	ss << "\ttype\t";
	switch (type) {
	case WalkType:
		ss << "walk";
		break;
	case FunnelType:
		ss << "funnel";
		break;
	case CameraType:
		ss << "camera";
		break;
	case SpecialType:
		ss << "special";
		break;
	case HotType:
		ss << "hot";
		break;
	};
	ss << endl;
	ss << "\tdefault visibility\t";
	if (visible)
		ss << "visible";
	else
		ss << "invisible";
	ss << endl;
	ss << "\theight\t" << height << endl;
	ss << "\tnumvertices\t" << numVertices << endl;
	ss << "\tvertices:\t\t";
	for (int i = 0; i < numVertices*3; i+=3) {
		if (i != 0)
			ss << "\t\t\t\t";
		ss << vertices[i] << "\t" << vertices[i+1] << "\t" << vertices[i+2] << endl;
	}

	return ss.str();
}

class Setup : public Section
{
public:
	Setup(Data *data);

	virtual string ToString();
private:
	string name;
	string tile;
	string background;
	string zbuffer;
	float* position;
	float* interest;
	float roll;
	float fov;
	float nclip;
	float fclip;
};

Setup::Setup(Data *data) : Section(data)
{
	name = data->GetString(128); // Parse a string really

	// Skip an unknown number
	int unknown = data->GetInt();


	tile = data->GetNullTerminatedString();

	position = new float[3];

	position[0] = data->GetFloat();
	position[1] = data->GetFloat();
	position[2] = data->GetFloat();

	interest = new float[3];

	interest[0] = data->GetFloat();
	interest[1] = data->GetFloat();
	interest[2] = data->GetFloat();

	roll = data->GetFloat();
	fov  = data->GetFloat();
	nclip = data->GetFloat();
	fclip = data->GetFloat();
}

string Setup::ToString()
{
	stringstream ss;
	ss.precision(6);
	ss.setf(ios::fixed,ios::floatfield);
	ss << "\tname\t" << name << endl;
	// background
	// zbuffer
	ss << "\tposition\t" << position[0] << "\t" << position[1] << "\t" << position[2] << endl;
	ss << "\tinterest\t" << interest[0] << "\t" << interest[1] << "\t" << interest[2] << endl;
	ss << "\troll\t" << roll << endl;
	ss << "\tfov\t" << fov << endl;
	ss << "\tnclip\t" << nclip << endl;
	ss << "\tfclip\t" << fclip << endl;

	return ss.str();
}

class Light : public Section
{
public:
	Light(Data *data);
	virtual string ToString();

private:
	string name;
	LightType type;
	float* position;
	float* direction;
	float intensity;
	float umbraangla;
	float penumbraangle;
	int* color; // Byte

};

Light::Light(Data *data) : Section(data)
{
	data->Skip(100);
}

string Light::ToString()
{
	return "";
}

class Set {
public:
	virtual string ToString();
	Set(Data *data);
private:
	string setName;
	uint32 numSetups;
	uint32 numColormaps;
	uint32 numLights;
	uint32 numSectors;
	vector<Section *> setups;
	vector<string> colormaps;
	vector<Section *> lights;
	vector<Section *> sectors;
};

Set::Set(Data *data)
{
	numSetups = data->GetInt();
	setups.reserve(numSetups);
	for(uint32 i = 0; i < numSetups; i++) {
		setups.push_back(new Setup(data));
	}

	numLights = data->GetInt();
	lights.reserve(numLights);
	for(uint32 i = 0; i < numLights; i++) {
		lights.push_back(new Light(data));
	}

	numSectors = data->GetInt();
	sectors.reserve(numSectors);
	for(uint32 i = 0; i < numSectors; i++) {
		sectors.push_back(new Sector(data));
	}
}
string Set::ToString()
{
	stringstream ss;
	// colormaps
	ss << "section: colormaps" << endl; // we don't have any.
	// setups
	ss << "section: setups" << endl;
	vector<Section*>::iterator it;
	ss << "\tnumsetups " << setups.size() << endl;
	for(it = setups.begin(); it != setups.end(); ++it) {
		ss << (*it)->ToString() << endl << endl;
	}

	// lights
	ss << "section: lights" << endl;
	ss << "\tnumlights 0" << endl;
	for(it = lights.begin();it!=lights.end();it++) {
		ss << (*it)->ToString() << endl << endl;
	}
	// sectors
	ss << "section: sectors\n";
	for(it = sectors.begin(); it != sectors.end();it++) {
		ss << (*it)->ToString() << endl << endl;
	}
	return ss.str();
}
int main(int argc, char** argv){
	if (argc < 2)
		return 0;
	char* buf;
	FILE* f;

	f = fopen(argv[1],"rb");
	fseek(f,0,SEEK_END);
	uint32 size = ftell(f);
	buf = new char[size];
	fseek(f,0,SEEK_SET);

	int result = fread(buf,1,size,f);
	assert(result==size);
	Data *data = new Data(buf);
	Set* ourSet = new Set(data);
	delete data;
	delete buf;
	cout << ourSet->ToString();
}
