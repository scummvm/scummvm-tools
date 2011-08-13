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
#include "filetools.h"

using namespace std;

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
	std::cout << "animName: " << animName << " duration: " << duration << " bones: " << bones << std::endl;
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
		std::cout << "Bone: " << i << " Operation: " << operation << " Unknown1: " << unknown1 <<
			" Unknown2: " << unknown2 << " numKeyframes: " << numKeyframes << std::endl;
	}
}
