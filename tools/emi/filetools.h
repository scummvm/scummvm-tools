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

template<typename T>
struct Vector3 {
	T _x;
	T _y;
	T _z;
	void setVal(T x, T y, T z) {
		_x = x;
		_y = y;
		_z = z;
	}
};

struct Vector2d {
	float x;
	float y;
	std::string toString() {
		std::stringstream ss;
		ss << x << " " << y;
		return ss.str();
	}
};

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

struct Vector4d {
	float x;
	float y;
	float z;
	float w;
	std::string toString() {
		std::stringstream ss;
		ss << x << " " << y << " " << z << " " << w;
		return ss.str();
	}
};

std::string readString(std::istream& file) {
	int strLength = 0;
	file.read((char*)&strLength, 4);
	char* readString = new char[strLength];
	file.read(readString, strLength);
	
	std::string retVal(readString);
	delete readString;
	
	return retVal;
}

float readFloat(std::istream& file) {
	float retVal = 0.0f;
	file.read((char*)&retVal, 4);
	return retVal;
}

int readInt(std::istream& file) {
	int retVal = 0;
	file.read((char*)&retVal, 4);
	return retVal;
}

short readShort(std::istream& file) {
	short retVal = 0;
	file.read((char*)&retVal, 2);
	return retVal;
}

int readByte(std::istream& file) {
	char retVal = 0;
	file.read((char*)&retVal, 1);
	return retVal;
}

Vector2d *readVector2d(std::istream& file, int count=1) {
	Vector2d *vec2d = new Vector2d[count];
	for (int i = 0; i < count; i++) {
		vec2d[i].x = readFloat(file);
		vec2d[i].y = readFloat(file);
	}
	return vec2d;
}

Vector3d *readVector3d(std::istream& file, int count=1) {
	Vector3d *vec3d = new Vector3d[count];
	for (int i = 0; i < count; i++) {
		vec3d[i].x = readFloat(file);
		vec3d[i].y = readFloat(file);
		vec3d[i].z = readFloat(file);
	}
	return vec3d;
}

Vector4d *readVector4d(std::istream& file) {
	Vector4d *vec4d = new Vector4d();
	vec4d->x = readFloat(file);
	vec4d->y = readFloat(file);
	vec4d->z = readFloat(file);
	vec4d->w = readFloat(file);
	return vec4d;
}

#endif
