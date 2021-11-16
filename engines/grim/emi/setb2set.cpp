/* ResidualVM - A 3D game interpreter
 *
 * ResidualVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <vector>
#include <sstream>
#include "engines/grim/lab.h"

using namespace std;

enum SectorType {
	NoneType = 0,
	WalkType = 0x1000,
	FunnelType = 0x1100,
	CameraType = 0x2000,
	SpecialType = 0x4000,
	HotType = 0x8000
};


enum LightType {
	OmniType = 1,
	SpotType = 2,
	DirectType = 3,
	AmbientType = 4
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

Data::Data(const char *data) {
	buf = data;
}

float Data::GetFloat() {
	float retVal = *(float *) buf;
	buf += 4;
	return retVal;
}

int Data::GetInt() {
	int retVal = *(int *) buf;
	buf += 4;
	return retVal;
}

bool Data::GetBool() {
	bool retVal = *(bool *) buf;
	buf += 1;
	return retVal;
}

string Data::GetString(int length) {
	//kind of a hack
	string s = string(buf);
	buf += length;
	return s;
}

string Data::GetNullTerminatedString() {
	string s = string(buf);
	buf += s.length() + 1;
	return s;
}

void Data::Skip(int val) {
	buf += val;
}

struct Section {
public:
	Section(Data *data);
	virtual ~Section() {};
	//virtual uint32 load() = 0;
	virtual string ToString() = 0;
protected:
	Data *section_data;
};

Section::Section(Data *data) {
	this->section_data = data;
}

class Sector : public Section {
public:
	Sector(Data *data);

	virtual string ToString();
private:
	string name;
	int ID; // byte;
	SectorType type;
	float height;
	int numVertices; // byte;
	float *vertices; // 3 * numVertices.
	float normal[3];
	bool visible;
	int numSortPlanes;
	int *sortPlanes;
};

Sector::Sector(Data *data) : Section(data) {
	numVertices = data->GetInt();
	vertices = new float[3 * numVertices];
	for (int i = 0; i < numVertices; i++) {
		vertices[0 + 3 * i] = data->GetFloat();
		vertices[1 + 3 * i] = data->GetFloat();
		vertices[2 + 3 * i] = data->GetFloat();
	}
	int nameLength = data->GetInt();

	name = data->GetString(nameLength);
	ID = data->GetInt();
	visible = data->GetBool();
	type = (SectorType)data->GetInt();
	numSortPlanes = data->GetInt();
	sortPlanes = new int[numSortPlanes];
	for (int i = 0; i < numSortPlanes; ++i)
		sortPlanes[i] = data->GetInt();
	height = data->GetFloat();

	float cross1[3], cross2[3];
	cross1[0] = vertices[3] - vertices[0];
	cross1[1] = vertices[4] - vertices[1];
	cross1[2] = vertices[5] - vertices[2];

	int x = 3 * (numVertices - 1);
	cross2[0] = vertices[x + 0] - vertices[0];
	cross2[1] = vertices[x + 1] - vertices[1];
	cross2[2] = vertices[x + 2] - vertices[2];

	float &nx = normal[0];
	float &ny = normal[1];
	float &nz = normal[2];
	nx = cross1[1] * cross2[2] - cross2[1] * cross1[2];
	ny = cross1[0] * cross2[2] - cross2[0] * cross1[2];
	nz = cross1[0] * cross2[1] - cross2[0] * cross1[1];

	float norm = nx * nx + ny * ny + nz * nz;
	norm = ::sqrt(norm);
	nx /= norm;
	ny /= norm;
	nz /= norm;
}

string Sector::ToString() {
	stringstream ss;
	ss.precision(6);
	ss.setf(ios::fixed, ios::floatfield);
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
	case NoneType:
		ss << "unknown";
		break;
	};
	ss << endl;
	ss << "\tdefault visibility\t";
	if (visible) {
		ss << "visible";
	} else {
		ss << "invisible";
	}
	ss << endl;
	ss << "\theight\t" << height << endl;
	ss << "\tnumvertices\t" << numVertices << endl;
	ss << "\tsortplanes\t" << numSortPlanes << "\t";
	for (int i = 0; i < numSortPlanes; ++i) {
		if (i != 0)
			ss << ",";
		ss << sortPlanes[i];
	}
	ss << endl;
	ss << "\tnormal\t\t\t" << normal[0] << "\t" << normal[1] << "\t" << normal[2] << endl;
	ss << "\tvertices:\t\t";
	for (int i = 0; i < numVertices * 3; i += 3) {
		if (i != 0) {
			ss << "\t\t\t\t";
		}
		ss << vertices[i] << "\t" << vertices[i + 1] << "\t" << vertices[i + 2] << endl;
	}

	return ss.str();
}

class Setup : public Section {
public:
	Setup(Data *data);

	virtual string ToString();
private:
	string name;
	string tile;
	string background;
	string zbuffer;
	float *position;
	float *rotationQuat;
	float fov;
	float nclip;
	float fclip;
};

Setup::Setup(Data *data) : Section(data) {
	name = data->GetString(128); // Parse a string really

	// Skip an unknown number
	data->GetInt();

	tile = data->GetNullTerminatedString();

	position = new float[3];

	position[0] = data->GetFloat();
	position[1] = data->GetFloat();
	position[2] = data->GetFloat();

	rotationQuat = new float[4];

	rotationQuat[0] = data->GetFloat();
	rotationQuat[1] = data->GetFloat();
	rotationQuat[2] = data->GetFloat();
	rotationQuat[3] = data->GetFloat();

	fov  = data->GetFloat();
	nclip = data->GetFloat();
	fclip = data->GetFloat();
}

string Setup::ToString() {
	stringstream ss;
	ss.precision(6);
	ss.setf(ios::fixed, ios::floatfield);
	ss << "\tname\t" << name << endl;
	// background
	// zbuffer
	ss << "\tposition\t" << position[0] << "\t" << position[1] << "\t" << position[2] << endl;
	ss << "\trotationQuat\tX: " << rotationQuat[0] << "\tY: " << rotationQuat[1] << "\tZ: " << rotationQuat[2] << "\tW: " << rotationQuat[3] << "\t" << endl;
	ss << "\tfov\t" << fov << endl;
	ss << "\tnclip\t" << nclip << endl;
	ss << "\tfclip\t" << fclip << endl;

	return ss.str();
}

class Light : public Section {
public:
	Light(Data *data);
	virtual string ToString();

private:
	string name;
	LightType type;
	float *position;
	float *direction;
	float intensity;
	int *color; // Byte
	float umbraangle;
	float penumbraangle;
	float focusdistance;
	float spreaddistance;

};

Light::Light(Data *data) : Section(data) {
	name = data->GetString(32);	// 0x00

	position = new float[3];	// 0x20
	position[0] = data->GetFloat(); // X
	position[1] = data->GetFloat(); // Y
	position[2] = data->GetFloat(); // Z

	direction = new float[3];	// 0x2C
	direction[0] = data->GetFloat(); // X
	direction[1] = data->GetFloat(); // Y
	direction[2] = data->GetFloat(); // Z

	intensity = data->GetFloat();	// 0x38

	// Need to check the light type
	type = (LightType)data->GetInt(); // 0x3C

	data->GetFloat();	// 0x40 // Unknown, definitely float
	int j = data->GetInt(); 	// 0x44
	if (j != 0) {
		cout << "Warning j != 0!" << endl;
	}

	// Light color
	color = new int[3];
	color[0] = data->GetInt(); // R // 0x48
	color[1] = data->GetInt(); // G // 0x4C
	color[2] = data->GetInt(); // B // 0x50

	// Not 100% on these names
	focusdistance = data->GetFloat();	// 0x54
	spreaddistance = data->GetFloat();	// 0x58
	umbraangle = data->GetFloat();		// 0x5C // In radians
	penumbraangle = data->GetFloat();	// 0x60 // In radians
}

string Light::ToString() {
	stringstream ss;
	ss.precision(6);
	ss.setf(ios::fixed, ios::floatfield);
	ss << "\tlight\t" << name << endl;
	ss << "\ttype\t";
	switch (type) {
	case OmniType:
		ss << "omni";
		break;
	case SpotType:
		ss << "spot";
		break;
	case DirectType:
		ss << "direct";
		break;
	case AmbientType:
		ss << "ambient";
		break;
	default:
		ss << "unknown: " << type;
		break;
	}
	ss << endl;
	ss << "\tposition\t" << position[0] << "\t" << position[1] << "\t" << position[2] << endl;
	ss << "\tdirection\t" << direction[0] << "\t" << direction[1] << "\t" << direction[2] << endl;
	ss << "\tintensity\t" << intensity << endl;
	ss << "\tcolor\t" << color[0] << " " << color[1] << " " << color[2] << endl;
	ss << "\tumbraangle\t" << umbraangle << endl;
	ss << "\tpenumbraangle\t" << penumbraangle << endl;
	return ss.str();
}

class Set {
public:
	virtual string ToString();
	Set(Data *data);
	virtual ~Set() {}
private:
	string setName;
	uint32 numSetups;
	uint32 numLights;
	uint32 numSectors;
	vector<Section *> setups;
	vector<string> colormaps;
	vector<Section *> lights;
	vector<Section *> sectors;
};

Set::Set(Data *data) {
	numSetups = data->GetInt();
	setups.reserve(numSetups);
	for (uint32 i = 0; i < numSetups; i++) {
		setups.push_back(new Setup(data));
	}

	numLights = data->GetInt();
	lights.reserve(numLights);
	for (uint32 i = 0; i < numLights; i++) {
		lights.push_back(new Light(data));
	}

	numSectors = data->GetInt();
	sectors.reserve(numSectors);
	for (uint32 i = 0; i < numSectors; i++) {
		sectors.push_back(new Sector(data));
	}
}
string Set::ToString() {
	stringstream ss;
	// colormaps
	ss << "section: colormaps" << endl; // we don't have any.
	// setups
	ss << "section: setups" << endl;
	ss << "\tnumsetups " << setups.size() << endl;
	for (vector<Section *>::iterator it = setups.begin(); it != setups.end(); ++it) {
		ss << (*it)->ToString() << endl << endl;
	}
	// lights
	ss << "section: lights" << endl;
	ss << "\tnumlights " << lights.size() << endl;
	for (vector<Section *>::iterator it = lights.begin(); it != lights.end(); it++) {
		ss << (*it)->ToString() << endl << endl;
	}
	// sectors
	ss << "section: sectors\n";
	ss << "\tnumsectors " << sectors.size() << endl;
	for (vector<Section *>::iterator it = sectors.begin(); it != sectors.end(); it++) {
		ss << (*it)->ToString() << endl << endl;
	}
	return ss.str();
}

int main(int argc, char **argv) {
	if (argc < 2) {
		return 0;
	}
	Lab *lab = NULL;
	std::string filename;
	int length = 0;

	if (argc > 2) {
		lab = new Lab(argv[1]);
		filename = argv[2];
	} else {
		filename = argv[1];
	}

	std::istream *file = getFile(filename, lab, length);

	if (!file) {
		std::cout << "Could not open file" << std::endl;
		return 0;
	}

	char *buf = new char[length];
	file->read(buf, length);
	delete file;

	Data *data = new Data(buf);
	Set *ourSet = new Set(data);
	delete data;
	delete[] buf;
	cout << ourSet->ToString();
}
