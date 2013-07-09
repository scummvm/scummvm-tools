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
*/

// Based on Benjamin Haischs filetype-information.

#include <fstream>
#include <string>
#include <iostream>
#include "filetools.h"
#include "tools/lab.h"

using namespace std;

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
	std::string animName = readString(*file);
	float duration = readFloat(*file);
	int bones = readInt(*file);
	std::cout << "animName: " << animName << " duration: " << duration << " bones: " << bones << std::endl;
	float time = 0.0f;
	Vector3d *vec3d;
	Vector4d *vec4d;
	for (int i = 0; i < bones; i++) {
		std::string boneName = readString(*file);
		int operation = readInt(*file);
		int unknown1 = readInt(*file);
		int unknown2 = readInt(*file);
		int numKeyframes = readInt(*file);
		std::cout << "Bone: " << boneName << " Operation: " << operation << " Unknown1: " << unknown1 <<
				  " Unknown2: " << unknown2 << " numKeyframes: " << numKeyframes << std::endl;

		if (operation == 3) { // Translation
			for (int j = 0; j < numKeyframes; j++) {
				vec3d = readVector3d(*file);
				time = readFloat(*file);
				std::cout << "Time : " << time << " Vector: " << vec3d->toString() << std::endl;
				delete vec3d;
			}
		} else if (operation == 4) { // Rotation
			for (int j = 0; j < numKeyframes; j++) {
				vec4d = readVector4d(*file);
				time = readFloat(*file);
				std::cout << "Time : " << time << " Vector: " << vec4d->toString() << std::endl;
				delete vec4d;
			}
		}

	}
}
