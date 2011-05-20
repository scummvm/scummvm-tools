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

using namespace std;

int main(int argc, char **argv) {
	if(argc < 2){
		std::cout << "Error: filename not specified" << std::endl;
		return 0;
	}
	std::string filename=argv[1];
	
	std::fstream file(filename.c_str(), std::ios::in | std::ios::binary);
	
	if (!file.is_open()){
		std::cout << "Unable to open file " << filename << std::endl;
		return 0;
	}
	int strLength = 0;
	
	file.read((char*)&strLength, 4);
	
	char* readString = new char[64];
	file.read(readString, strLength);
	
	// Then a list of textures 48 bytes later
	int numTextures = 0;
	file.seekg(48, ios::cur);
	file.read((char*)&numTextures, 4);
	
	char **texNames = new char*[numTextures];
	for(int i=0;i<numTextures; i++){
		file.read((char*)&strLength, 4);
		texNames[i] = new char[strLength];
		file.read(texNames[i], strLength);
		// Every texname seems to be followed by 4 0-bytes
		file.seekg(4, ios::cur);
	}
	for(int i = 0;i < numTextures;i++){
		std::cout << "# TexName " << texNames[i] << std::endl;
	}
	// 4 unknown bytes - usually with value 19
	file.seekg(4, ios::cur);
/*
 *	 Then follows the weird padding.
 */
	
	// Should create an empty mtl
	std::cout << "mtllib quit.mtl" << std::endl << "o Arrow"<< std::endl;
	// Vertices
	//	file.seekg(283);
	int numVertices;
	file.read((char*)&numVertices, 4);
	std::cout << "#File has " << numVertices << " Vertices" << std::endl;
	
	//file.seekg(287);
	float x = 0, y = 0, z = 0;
	
	for (int i = 0; i < numVertices; ++i) {
		file.read((char *)&x, 4);
		file.read((char *)&y, 4);
		file.read((char *)&z, 4);
		std::cout << "v " << x << " " << y << " " << z << std::endl;
	}
// Vertice-normals?
	for (int i = 0; i < numVertices; ++i) {
		file.read((char *)&x, 4);
		file.read((char *)&y, 4);
		file.read((char *)&z, 4);
		std::cout << "vn " << x << " " << y << " " << z << std::endl;
	}
	
	// Actually, this file has 6*4*numVertices floats in this block.
	std::cout<<"usemtl (null)"<<std::endl;
	// And then another block of unknowns
	// Faces
	// The head of this section needs quite a bit of rechecking
	file.seekg(36335);
	int faceLength = 0;
	for(int j = 0;j < numTextures + 1; j++){
		file.read((char*)&faceLength, 4);
		short x = 0, y = 0, z = 0;
		cout << "g " << j << endl;
		for (int i = 0; i < faceLength; i+=3) {
			file.read((char *)&x, 2);
			file.read((char *)&y, 2);
			file.read((char *)&z, 2);
			++x;
			++y;
			++z;
			std::cout << "f " << x << "//" << x << " " << y << "//" << y << " " << z << "//" << z <<  std::endl;
		}
		// 12 bytes of unknown
		file.seekg(12, ios::cur);
	}
}
