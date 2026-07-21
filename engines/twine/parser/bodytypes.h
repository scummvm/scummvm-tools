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

#ifndef TWINE_PARSER_BODYTYPES_H
#define TWINE_PARSER_BODYTYPES_H

#include "common/array.h"
#include "common/scummsys.h"

namespace TwinE {

enum MaterialType {
	MAT_TRISTE = 0,
	MAT_PIERRE = 1,
	MAT_COPPER = 2,
	MAT_BOPPER = 3,
	MAT_MARBRE = 4,
	MAT_TRANS = 5,
	MAT_TRAME = 6,
	MAT_FLAT = 7,
	MAT_GRANIT = 8,
	MAT_GOURAUD = 9,
	MAT_DITHER = 10
};

enum BoneType : uint16 {
	TYPE_ROTATE = 0,
	TYPE_TRANSLATE = 1,
	TYPE_ZOOM = 2,
};

struct BoneFrame {
	BoneType type = TYPE_ROTATE;
	int16 x = 0;
	int16 y = 0;
	int16 z = 0;
};

struct IVec3 {
	int32 x = 0;
	int32 y = 0;
	int32 z = 0;
};

struct BoundingBox {
	IVec3 mins;
	IVec3 maxs;
};

struct BodyVertex {
	int16 x;
	int16 y;
	int16 z;
	uint16 bone;
};

struct BodyLine {
	uint8 color;
	uint16 vertex1;
	uint16 vertex2;
};

struct BodySphere {
	uint8 fillType;
	uint16 color;
	uint16 radius;
	uint16 vertex;
};

struct BodyBone {
	uint16 parent;
	uint16 vertex;
	int16 firstVertex;
	int16 numVertices;
	int32 numNormals;
	BoneFrame initalBoneState;

	inline bool isRoot() const {
		return parent == 0xffff;
	}
};

struct BodyNormal {
	int16 x;
	int16 y;
	int16 z;
	uint16 prenormalizedRange;
};

struct BodyPolygon {
	Common::Array<uint16> indices;
	Common::Array<uint16> normals;
	int8 materialType = 0;
	int16 intensity = 0;
};

} // namespace TwinE

#endif
