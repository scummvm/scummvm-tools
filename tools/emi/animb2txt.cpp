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

// Based on Benjamin Haischs filetype-information.

#include <fstream>
#include <string>
#include <iostream>

using namespace std;

struct Vector3d {
	float x;
	float y;
	float z;
};

std::string readString(std::fstream& file) {
	int strLength = 0;
	file.read((char*)&strLength, 4);
	char* readString = new char[strLength];
	file.read(readString, strLength);
	
	std::string retVal(readString);
	delete readString;
	
	return retVal;
}

float readFloat(std::fstream& file) {
	float retVal = 0.0f;
	file.read((char*)&retVal, 4);
	return retVal;
}

int readInt(std::fstream& file) {
	int retVal = 0.0f;
	file.read((char*)&retVal, 4);
	return retVal;
}

Vector3d *readVector3d(std::fstream& file) {
	Vector3d *vec3d = new Vector3d();
	vec3d->x = readFloat(file);
	vec3d->y = readFloat(file);
	vec3d->z = readFloat(file);
	return vec3d;
}

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
	std::string animName = readString(file);
	float duration = readFloat(file);
	int bones = readInt(file);
	float time = 0.0f;
	Vector3d *vec3d;
	for (int i = 0; i < bones; i++) {
		std::string boneName = readString(file);
		int operation = readInt(file);
		int unknown1 = readInt(file);
		int unknown2 = readInt(file);
		int numKeyframes = readInt(file);
		if (operation == 3) { // Translation
			vec3d = readVector3d(file);
		} else if (operation == 4) { // Rotation
			
		}
		if (operation == 3 || operation == 4) {
			time = readFloat(file);
		}
	}
}
