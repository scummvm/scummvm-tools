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

#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <cstdio>
#include <ctime>
#include <cstring> // for lab
#include <cstdlib>
#include <GL/glfw.h>
#include "filetools.h"
#include "model.h"
#include "engines/grim/lab.h"

using namespace std;

/*
 * Model-viewer for EMI, usage:
 * renderModel [labName] [mesh-name] [skel-name]
 *
 * If no labName is specified, then all files will be searched for
 * in the current working directory. (and no skeleton will be loaded)
 *
 * This is quite possibly not endian-safe yet, and requires GLFW for
 * rendering. (And is thus not built by default)
 */

bool Bone::hasChild(Bone *node) {
	if (!_child) {
		return false;
	}
	if (_child == node) {
		return true;
	} else {
		return _child->hasSibling(node);
	}
}

bool Bone::hasSibling(Bone *node) {
	if (!_sibling) {
		return false;
	}
	if (_sibling == node) {
		return true;
	} else {
		return _sibling->hasSibling(node);
	}
}

void Bone::addChild(Bone *node) {
	if (!_child) {
		_child = node;
		_child->addParent(this);
	} else {
		_child->addSibling(node);
	}
}

void Bone::addSibling(Bone *node) {
	if (!_sibling) {
		_sibling = node;
		_sibling->addParent(this);
	} else {
		_sibling->addSibling(node);
	}
}

void Bone::addParent(Bone *node) {
	_parent = node;
}

void Bone::addVertex(int num, float wgt) {
	_verts.push_back(num);
	_verts.push_back(wgt);
}

void Material::bindTexture(int index) {
	glBindTexture(GL_TEXTURE_2D, _texIDs[index]);
	glEnable(GL_TEXTURE_2D);
}

void Material::loadTexture(string filename) {
	string s = filename.substr(filename.length() - 3);
	if (s == "sur") {
		loadSURTexture(filename);
	} else {
		loadTGATexture(filename);
	}
}

void Material::loadSURTexture(string filename) {
	istream *file = getFile(filename, _lab);

	_texIDs = new uint[1]; // TODO: Handle the rest of the sur-items.

	string data;
	getline(*file, data);
	getline(*file, data);
	getline(*file, data);
	*file >> data;
	cout << "Only reading first surface-TGA for " << filename << endl;
	*file >> data;

	string test = data;//.substr(5);
	test[3] = '\\';
	delete file;
	loadTGATexture(test);
}

// TODO: This needs a bit more specific handling for indexes
void Material::loadTGATexture(string filename) {
	std::istream *file = getFile(filename, _lab);

	if (!((fstream *)file)->is_open()) {
		std::cout << "Unable to open file " << filename << std::endl;
		return;
	}

	_texIDs = new uint[1];
	glGenTextures(1, _texIDs);

	file->seekg(2, std::ios::cur);
	char type = readByte(*file);
	assert(type == 2);


	int width, height = 0;
	int bpp;
	file->seekg(9, std::ios::cur);

	width = readShort(*file);
	height = readShort(*file);
	bpp = readByte(*file);
	file->seekg(1, std::ios::cur);
	if (bpp != 24) {
		cout << bpp << " BPP " << endl;
	}
	int len = width * height * bpp / 8;
	char *newData = new char[len];
	char *target = newData + len - width * bpp / 8;
	for (int i = 0; i < height; i++) {
		file->read(target, width * (bpp / 8));
		target -= width * (bpp / 8);
	}
	glBindTexture(GL_TEXTURE_2D, _texIDs[0]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_BGR, GL_UNSIGNED_BYTE, newData);
	delete newData;
}

void MeshFace::loadFace(std::istream *file) {
	_flags = readInt(*file);
	_hasTexture = readInt(*file);
	if (_hasTexture > 1) {
		cout << "We have this many textures: " <<  _hasTexture << endl;
	}
	if (_hasTexture) {
		_texID = readInt(*file);
	}
	_faceLength = readInt(*file);
	short x = 0, y = 0, z = 0;
	_indexes = new Vector3<int>[_faceLength];

	for (int i = 0; i < _faceLength; i += 3) {
		file->read((char *)&x, 2);
		file->read((char *)&y, 2);
		file->read((char *)&z, 2);
		_indexes[i].setVal(x, y, z);
	}
}

void MeshFace::render() {
	if (_hasTexture) {
		_parent->setTex(_texID);
	}
	glDrawElements(GL_TRIANGLES, _faceLength * 3, GL_UNSIGNED_INT, _indexes);
}


void Mesh::loadMesh(string filename) {
	std::istream *file = getFile(filename, _lab);

	int strLength = 0;

	std::string nameString = readString(*file);

	_sphereData = readVector4d(*file);
	std::cout << "# Spheredata: " << _sphereData->toString() << std::endl;
	_boxData = readVector3d(*file);
	_boxData2 = readVector3d(*file);
	std::cout << "# Boxdata: " << _boxData->toString()
			  << _boxData2->toString() << std::endl;

	_numTexSets = readInt(*file);
	_setType = readInt(*file);
	std::cout << "# NumTexSets: " << _numTexSets << " setType: " << _setType << std::endl;
	_numTextures = readInt(*file);

	_texNames = new string[_numTextures];

	for (int i = 0; i < _numTextures; i++) {
		_texNames[i] = readString(*file);
		// Every texname seems to be followed by 4 0-bytes (Ref mk1.mesh,
		// this is intentional)
		file->seekg(4, std::ios::cur);
	}

	// 4 unknown bytes - usually with value 19
	file->seekg(4, std::ios::cur);

	_numVertices = readInt(*file);
	std::cout << "#File has " << _numVertices << " Vertices" << std::endl;

	float x = 0, y = 0;
	int r = 0, g = 0, b = 0, a = 0;
	// Vertices
	_vertices = readVector3d(*file, _numVertices);
	_normals = readVector3d(*file, _numVertices);
	_colorMap = new Colormap[_numVertices];
	for (int i = 0; i < _numVertices; ++i) {
		_colorMap[i].r = readByte(*file) ;
		_colorMap[i].g = readByte(*file) ;
		_colorMap[i].b = readByte(*file) ;
		_colorMap[i].a = readByte(*file) ;
	}
	_texVerts = readVector2d(*file, _numVertices);

	// Faces

	file->read((char *) &_numFaces, 4);
	_faces = new MeshFace[_numFaces];
	int faceLength = 0;
	for (int j = 0; j < _numFaces; j++) {
		_faces[j].setParent(this);
		_faces[j].loadFace(file);
	}

	int hasBones = 0;
	file->read((char *)&hasBones, 4);

	if (hasBones == 1) {
		_numBones = readInt(*file);
		_bones = new Bone[_numBones];
		for (int i = 0; i < _numBones; i++) {
			_bones[i].setName(readString(*file));
			_boneMap[_bones[i].getName()] = _bones + i;
		}

		int numBoneData = readInt(*file);
		int unknownVal = 0;
		int boneDatanum;
		float boneDataWgt;
		int vertex = 0;
		for (int i = 0; i < numBoneData; i++) {
			unknownVal = readInt(*file);
			boneDatanum = readInt(*file);
			boneDataWgt = readFloat(*file);
			if (unknownVal) {
				vertex++;
			}
			_bones[boneDatanum].addVertex(vertex, boneDataWgt);
		}
	}
	delete file;
}

void Mesh::loadSkeleton(std::string filename) {
	std::istream *file = getFile(filename, _lab);

	int numBones = readInt(*file);

	string boneName;
	string parentName;
	Bone *bone;
	Bone *parent;
	float angle = 0.0f;
	// Bones are listed in the same order as in the meshb.
	Vector3d *vec = 0;
	for (int i = 0; i < numBones; i++) {
		boneName = readCString(*file, 32);
		parentName = readCString(*file, 32);

		bone = _boneMap[boneName];


		/*  for (int j = 0; j < _numBones; j++) {
		        if (_bones[j].getName() == boneName) {
		            bone = _bones + j;
		            break;
		        }
		    }*/
		if (parentName != "no_parent") {
			/*  for (int j = 0; j < _numBones; j++) {
			        if (_bones[j].getName() == parentName) {
			            parent = _bones + j;
			            break;
			        }
			    }*/
			parent = _boneMap[parentName];

			assert(parent != 0);
			assert(bone != 0);

			if (!parent->hasChild(bone)) {
				parent->addChild(bone);
			}
		}
		bone->setPos(readVector3d(*file));
		bone->setRot(readVector3d(*file));
		bone->setAngle(readFloat(*file));
	}
}

void Mesh::loadAnimation(std::string filename) {
	std::istream *file = getFile(filename, _lab);

	_anim = new Animation();
	_anim->_name = readString(*file);


	_anim->_timelen = readFloat(*file);
	_anim->_numBones = readInt(*file);
	std::cout << "animName: " << _anim->_name << " duration: " << _anim->_timelen << " bones: " << _anim->_numBones << std::endl;
	float time = 0.0f;

	KeyframeList *keyList;
	Keyframe *key;
	Bone *bone;

	for (int i = 0; i < _anim->_numBones; i++) {
		std::string boneName = readString(*file);
		int operation = readInt(*file);
		int unknown1 = readInt(*file);
		int unknown2 = readInt(*file);
		int numKeyframes = readInt(*file);

		bone = _boneMap[boneName];
		keyList = new KeyframeList(numKeyframes, operation);

		std::cout << "Bone: " << boneName << " Operation: " << operation << " Unknown1: " << unknown1 <<
				  " Unknown2: " << unknown2 << " numKeyframes: " << numKeyframes << std::endl;

		if (operation == 3) { // Translation
			for (int i = 0; i < numKeyframes; i++) {
				key = keyList->_frames + i;
				key->_time = readFloat(*file);
				key->_vec3d = readVector3d(*file);
			}
		} else if (operation == 4) { // Rotation
			for (int i = 0; i < numKeyframes; i++) {
				key = keyList->_frames + i;
				key->_time = readFloat(*file);
				key->_vec4d = readVector4d(*file);
			}
		}

	}
}

void Mesh::prepare() {
	_mats = new Material[_numTextures];
	for (int i = 0; i < _numTextures; i++) {
		_mats[i].setLab(_lab);
		_mats[i].loadTexture(_texNames[i]);
	}
}

void Mesh::render() {
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);

	glVertexPointer(3, GL_FLOAT, 0, _vertices);
	glTexCoordPointer(2, GL_FLOAT, 0, _texVerts);
	glNormalPointer(GL_FLOAT, 0, _normals);
	glColorPointer(4, GL_UNSIGNED_BYTE, 0, _colorMap);

	for (int i = 0; i < _numFaces; i++) {
		_faces[i].render();
	}

	glDisableClientState(GL_VERTEX_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
	glDisableClientState(GL_COLOR_ARRAY);
}

void renderInit() {
	glClearColor(0.2f, 0.2f, 0.2f, 0.0);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glLoadIdentity();
}

Mesh m;

void renderLoop() {
	float rot = 0.5f;
	while (true) {
		renderInit();
		glTranslatef(0.0, -0.9f, 0.0f);
		glRotatef(rot, 0.0f, 0.1f, 0.0f);
		rot += 0.1f;
		m.render();
		glFlush();
		glfwSwapBuffers();
		usleep(5000);
	}
}

int main(int argc, char **argv) {
	if (argc > 2) {
		cout << "Using LAB!" << endl;
		Lab *lab = new Lab(string(argv[1]));
		m.setLab(lab);
		m.loadMesh(argv[2]);
	} else {
		m.loadMesh(argv[1]);
	}
	if (argc > 3) {
		m.loadSkeleton(argv[3]);
	}
	if (argc > 4) {
		m.loadAnimation(argv[4]);
	}

	glfwInit();
	glfwOpenWindow(1024, 768, 8, 8, 8, 8, 8, 8, GLFW_WINDOW);
	m.prepare();
	glEnable(GL_DEPTH_TEST);
	renderLoop();

	return 0;
}
