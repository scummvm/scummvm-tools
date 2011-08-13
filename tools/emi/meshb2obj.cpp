/* Residual - A 3D game interpreter
*
* Residual is the legal property of its developers, whose names
* are too numerous to list here. Please refer to the AUTHORS
* file distributed with this source distribution.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
*
* $URL:
* $Id:
*
*/

#include <fstream>
#include <string>
#include <iostream>
#include "filetools.h"

int main(int argc, char **argv) {
	if(argc < 2){
		std::cout << "Error: filename not specified" << std::endl;
		return 0;
	}
	std::string filename = argv[1];
	
	std::fstream file(filename.c_str(), std::ios::in | std::ios::binary);
	
	if (!file.is_open()) {
		std::cout << "Unable to open file " << filename << std::endl;
		return 0;
	}
	int strLength = 0;
	
	std::string nameString = readString(file);
	// Unknown vector3d
	
	// Then a list of textures 48 bytes later
	
	file.seekg(48, std::ios::cur);
	int numTextures = readInt(file);
	
	std::string *texNames = new std::string[numTextures];
	for(int i = 0;i < numTextures; i++) {
		texNames[i] = readString(file);
		// Every texname seems to be followed by 4 0-bytes
		file.seekg(4, std::ios::cur);
	}
	for(int i = 0;i < numTextures;i++){
		std::cout << "# TexName " << texNames[i] << std::endl;
	}
	// 4 unknown bytes - usually with value 19
	file.seekg(4, std::ios::cur);
/*
 *	 Then follows the weird padding.
 */
	
	// Should create an empty mtl
	std::cout << "mtllib quit.mtl" << std::endl << "o Arrow" << std::endl;

	int numVertices = readInt(file);
	std::cout << "#File has " << numVertices << " Vertices" << std::endl;
	
	float x = 0, y = 0, z = 0;
	
	// Vertices
	Vector3d *vec;
	for (int i = 0; i < numVertices; ++i) {
		vec = readVector3d(file);
		std::cout << "v " << vec->x << " " << vec->y << " " << vec->z << std::endl;
		delete vec;
	}
	// Vertex-normals
	for (int i = 0; i < numVertices; ++i) {
		vec = readVector3d(file);
		std::cout << "vn " << vec->x << " " << vec->y << " " << vec->z << std::endl;
		delete vec;
	}
	// Color map-data, dunno how to interpret them right now.
	file.seekg(numVertices * 4, std::ios::cur);
	for (int i = 0; i < numVertices; ++i) {
		file.read((char *)&x, 4);
		file.read((char *)&y, 4);
		std::cout << "vt " << x << " " << y << " " << z << std::endl;
	}
	
	std::cout << "usemtl (null)"<< std::endl;
	
	// Faces
	// The head of this section needs quite a bit of rechecking
	int numFaces = 0;
	int hasTexture = 0;
	int texID = 0;
	int flags = 0;
	file.read((char *) &numFaces, 4);
	int faceLength = 0;
	for(int j = 0;j < numFaces; j++){
		flags = readInt(file);
		hasTexture = readInt(file);
		if(hasTexture)
			texID = readInt(file);
		faceLength = readInt(file);
		std::cout << "#Face-header: flags: " << flags << " hasTexture: " << hasTexture
			<< " texId: " << texID << " faceLength: " << faceLength << std::endl;
		short x = 0, y = 0, z = 0;
		std::cout << "g " << j << std::endl;
		for (int i = 0; i < faceLength; i += 3) {
			file.read((char *)&x, 2);
			file.read((char *)&y, 2);
			file.read((char *)&z, 2);
			++x;
			++y;
			++z;
			std::cout << "f " << x << "//" << x << " " << y << "//" << y << " " << z << "//" << z <<  std::endl;
		}
	}
	int hasBones = 0;
	file.read((char*)&hasBones, 4);
	
	if (hasBones == 1) {
		int numBones = readInt(file);
		char **boneNames = new char*[numBones];
		for(int i = 0;i < numBones; i++) {
			file.read((char*)&strLength, 4);
			boneNames[i] = new char[strLength];
			file.read(boneNames[i], strLength);
			std::cout << "# BoneName " << boneNames[i] << std::endl;
		}
		
		int numBoneData = readInt(file);
		int unknownVal = 0;
		int boneDatanum;
		float boneDataWgt;
		int vertex = 0;
		for(int i = 0;i < numBoneData; i++) {
			unknownVal = readInt(file);
			boneDatanum = readInt(file);
			boneDataWgt = readFloat(file);
			if(unknownVal)
				vertex++;
			std::cout << "# BoneData: Vertex: " << vertex << " boneNum: "
				<< boneDatanum << " weight: " << boneDataWgt << std::endl;
		}
	}
}
