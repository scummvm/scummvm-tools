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

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#ifndef MODEL_H
#define MODEL_H

#include <string>
#include <stdint.h>
#include <map>
#include <vector>

struct Colormap {
	unsigned char r, g, b, a;
};

class Mesh;
class Lab;

class Material {
	uint32_t *_texIDs;
	uint32_t _numTextures;
	Lab *_lab;
	void loadTGATexture(std::string filename);
	void loadSURTexture(std::string filename);
public:
	Material() { _lab = 0; }
	void setLab(Lab *lab) { _lab = lab; }
	void loadTexture(std::string filename);
	void bindTexture(int index = 0);
};

class MeshFace {
	Vector3<int> *_indexes;
	uint32_t _faceLength;
	uint32_t _numFaces;
	uint32_t _hasTexture;
	uint32_t _texID;
	uint32_t _flags;
	Mesh *_parent;
public:
	MeshFace() : _numFaces(0), _hasTexture(0), _texID(0), _flags(0) { }
	void loadFace(std::istream *file);
	void setParent(Mesh *m) { _parent = m; }
	void render();
};

class Keyframe {
public:
	float _time;
	Vector3d *_vec3d;
	Vector4d *_vec4d;
};

class KeyframeList {
public:
	Keyframe *_frames;
	int _numFrames;
	float _time;
	int _operation;
	KeyframeList(int num, int op) : _numFrames(num), _operation(op) {
		_frames = new Keyframe[_numFrames];
	}
};

class Bone {
	std::string _name;
	Bone *_parent;
	Bone *_child;
	Bone *_sibling;
	Vector3d *_pos;
	Vector3d *_rot;
	float _angle;
	std::vector<int> _verts;
	std::vector<float> _wgts;
	KeyframeList *_keyframes;
public:
	Bone() : _parent(0), _child(0), _sibling(0), _angle(0.0f), _keyframes(0) {}
	void addParent(Bone *node);
	void addChild(Bone *node);
	void addSibling(Bone *node);
	bool hasSibling(Bone *node);
	bool hasChild(Bone *node);
	void setName(std::string name) { _name = name; }
	void setPos(Vector3d *pos) { _pos = pos; }
	void setRot(Vector3d *rot) { _rot = rot; }
	void setAngle(float angle) { _angle = angle; }
	void addVertex(int num, float wgt);
	void setKeyFrames(KeyframeList* keyframes) { _keyframes = keyframes; }

	std::string getName() { return _name; }
};

class Animation {
public:
	std::string _name;
	float _timelen;
	int _numBones;
	int _numKeyFrames;
	std::vector<Bone *> _bones;
};

class Mesh {
	int _numVertices;
	Vector3d *_vertices;
	Vector3d *_normals;
	Colormap *_colorMap;
	Vector2d *_texVerts;

	uint32_t _numFaces;
	MeshFace *_faces;
	uint32_t _numTextures;
	std::string *_texNames;
	Material *_mats;

	Bone *_bones;
	int _numBones;
	std::map<std::string, Bone *> _boneMap;

	Animation *_anim;

	Lab *_lab;

	// Stuff I dont know how to use:
	Vector4d *_sphereData;
	Vector3d *_boxData;
	Vector3d *_boxData2;
	int _numTexSets;
	int _setType;

public:
	Mesh() { _lab = 0; }
	void setLab(Lab *lab) { _lab = lab; }
	void setTex(int index) { _mats[index].bindTexture(); }
	void loadMesh(std::string fileName);
	void loadSkeleton(std::string filename);
	void loadAnimation(std::string filename);
	void prepare();
	void render();
};

#endif
