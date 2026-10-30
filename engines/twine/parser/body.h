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

#ifndef TWINE_PARSER_BODY_H
#define TWINE_PARSER_BODY_H

#include "common/stream.h"
#include "engines/twine/parser/bodytypes.h"

namespace TwinE {

class BodyData {
private:
	void loadVertices(Common::SeekableReadStream &stream, uint32 streamSize);
	void loadBones(Common::SeekableReadStream &stream);
	void loadNormals(Common::SeekableReadStream &stream);
	void loadPolygons(Common::SeekableReadStream &stream);
	void loadLines(Common::SeekableReadStream &stream);
	void loadSpheres(Common::SeekableReadStream &stream);

	Common::Array<BodyPolygon> _polygons;
	Common::Array<BodyVertex> _vertices;
	Common::Array<BodySphere> _spheres;
	Common::Array<BodyNormal> _normals;
	Common::Array<BodyLine> _lines;
	Common::Array<BodyBone> _bones;

	void reset();

public:
	bool animated = false;
	BoundingBox bbox;
	int16 offsetToData = 0;

	bool loadFromStream(Common::SeekableReadStream &stream, bool lba1);
	bool loadFromBuffer(const uint8 *buf, uint32 size, bool lba1);

	static bool isLikelyLba1Buffer(const uint8 *buf, uint32 size);
	static bool isLikelyLba2Buffer(const uint8 *buf, uint32 size);

	inline uint getNumVertices() const {
		return _vertices.size();
	}

	const Common::Array<BodyPolygon> &getPolygons() const {
		return _polygons;
	}

	const Common::Array<BodyVertex> &getVertices() const {
		return _vertices;
	}

	const Common::Array<BodyNormal> &getNormals() const {
		return _normals;
	}

	const BodyNormal &getNormal(int16 normalIdx) const {
		return _normals[normalIdx];
	}

	const Common::Array<BodyBone> &getBones() const {
		return _bones;
	}
};

} // namespace TwinE

#endif
