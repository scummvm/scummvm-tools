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

#include <fstream>
#include <string>
#include <iostream>
#include "filetools.h"
#include "tools/lab.h"

int main(int argc, char **argv) {
	if (argc < 2) {
		std::cout << "Error: filename not specified" << std::endl;
		return 0;
	}

	Lab *lab = NULL;
	std::string filename;

	if (argc > 2) {
		lab = new Lab(argv[1]);
		filename = argv[2];
	} else {
		filename = argv[1];
	}

	std::istream *file = getFile(filename, lab);

	if (!file) {
		std::cout << "Unable to open file " << filename << std::endl;
		return 0;
	}
	int strLength = 0;

	std::string nameString = readString(*file);

	Vector4d *vec4d;
	Vector3d *vec3d;

	vec4d = readVector4d(*file);
	std::cout << "# Spheredata: " << vec4d->toString() << std::endl;
	delete vec4d;
	vec3d = readVector3d(*file);
	std::cout << "# Boxdata: " << vec3d->toString();
	delete vec3d;
	vec3d = readVector3d(*file);
	std::cout << vec3d->toString() << std::endl;
	delete vec3d;

	int numTexSets = readInt(*file);
	int setType = readInt(*file);
	std::cout << "# NumTexSets: " << numTexSets << " setType: " << setType << std::endl;
	int numTextures = readInt(*file);

	std::string *texNames = new std::string[numTextures];
	for (int i = 0; i < numTextures; i++) {
		texNames[i] = readString(*file);
		// Every texname seems to be followed by 4 0-bytes (Ref mk1.mesh,
		// this is intentional)
		readInt(*file);
	}
	for (int i = 0; i < numTextures; i++) {
		std::cout << "# TexName " << texNames[i] << std::endl;
	}
	// 4 unknown bytes - usually with value 19
	readInt(*file);

	// Should create an empty mtl
	std::cout << "mtllib quit.mtl" << std::endl << "o Arrow" << std::endl;

	int numVertices = readInt(*file);
	std::cout << "#File has " << numVertices << " Vertices" << std::endl;

	float x = 0, y = 0;
	int r = 0, g = 0, b = 0, a = 0;
	// Vertices
	for (int i = 0; i < numVertices; ++i) {
		vec3d = readVector3d(*file);
		std::cout << "v " << vec3d->x << " " << vec3d->y << " " << vec3d->z << std::endl;
		delete vec3d;
	}
	// Vertex-normals
	for (int i = 0; i < numVertices; ++i) {
		vec3d = readVector3d(*file);
		std::cout << "vn " << vec3d->x << " " << vec3d->y << " " << vec3d->z << std::endl;
		delete vec3d;
	}
	// Color map-data, dunno how to interpret them right now.
	for (int i = 0; i < numVertices; ++i) {
		r = readByte(*file);
		g = readByte(*file);
		b = readByte(*file);
		a = readByte(*file);
		std::cout << "# R: " << r << " G: " << g << " B: " << b << " A: " << a << std::endl;
	}
	// Texture-vertices
	for (int i = 0; i < numVertices; ++i) {
		x = readFloat(*file);
		y = readFloat(*file);
		std::cout << "vt " << x << " " << y << std::endl;
	}

	std::cout << "usemtl (null)" << std::endl;

	// Faces
	// The head of this section needs quite a bit of rechecking
	int numFaces = 0;
	int hasTexture = 0;
	int texID = 0;
	int flags = 0;
	numFaces = readInt(*file);
	int faceLength = 0;
	for (int j = 0; j < numFaces; j++) {
		flags = readInt(*file);
		hasTexture = readInt(*file);
		if (hasTexture) {
			texID = readInt(*file);
		}
		faceLength = readInt(*file);
		std::cout << "#Face-header: flags: " << flags << " hasTexture: " << hasTexture
				  << " texId: " << texID << " faceLength: " << faceLength << std::endl;
		short xCoord = 0, yCoord = 0, zCoord = 0;
		std::cout << "g " << j << std::endl;
		for (int i = 0; i < faceLength; i += 3) {
			xCoord = readShort(*file) + 1;
			yCoord = readShort(*file) + 1;
			zCoord = readShort(*file) + 1;
			std::cout << "f " << xCoord << "//" << xCoord << " " << yCoord << "//" << yCoord << " " << zCoord << "//" << zCoord <<  std::endl;
		}
	}
	int hasBones = readInt(*file);

	if (hasBones == 1) {
		int numBones = readInt(*file);
		char **boneNames = new char*[numBones];
		for (int i = 0; i < numBones; i++) {
			strLength = readInt(*file);
			boneNames[i] = new char[strLength];
			file->read(boneNames[i], strLength);
			std::cout << "# BoneName " << boneNames[i] << std::endl;
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
			std::cout << "# BoneData: Vertex: " << vertex << " boneNum: "
					  << boneDatanum << " weight: " << boneDataWgt << std::endl;
		}
	}
}
