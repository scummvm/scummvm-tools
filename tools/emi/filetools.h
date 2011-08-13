#ifndef FILETOOLS_H
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


#define FILETOOLS_H

#include <fstream>
#include <string>
#include <sstream>

struct Vector3d {
	float x;
	float y;
	float z;
	std::string toString() {
		std::stringstream ss;
		ss << x << " " << y << " " << z;
		return ss.str();
	}
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

#endif
