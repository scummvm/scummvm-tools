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

#include "engines/twine/parser/body.h"
#include "common/endian.h"
#include "common/memstream.h"

#define INFO_ANIM 2
#define MASK_OBJECT_ANIMATED (1 << 8)

namespace {

static bool bodySeek(Common::SeekableReadStream &stream, int32 pos, uint32 size) {
	if (pos < 0 || (uint32)pos > size)
		return false;
	return stream.seek(pos);
}

static bool isReasonableCount(int32 count, int32 maxCount = 20000) {
	return count >= 0 && count <= maxCount;
}

static bool isValidBodyOffset(int32 offset, uint32 size, uint32 minOffset) {
	return offset >= (int32)minOffset && (uint32)offset <= size;
}

static bool validateLba2Header(int32 nbGroupes, int32 offGroupes, int32 nbPoints, int32 offPoints,
		int32 nbNormals, int32 offNormals, int32 nbPolys, int32 offPolys,
		int32 nbLines, int32 offLines, int32 nbSpheres, int32 offSpheres, uint32 size) {
	static const uint32 kHeaderEnd = 0x60;

	if (!isReasonableCount(nbGroupes, 1024) || !isReasonableCount(nbPoints) ||
		!isReasonableCount(nbNormals) || !isReasonableCount(nbPolys) ||
		!isReasonableCount(nbLines, 5000) || !isReasonableCount(nbSpheres, 5000))
		return false;

	if (nbPoints == 0 || nbPolys == 0)
		return false;

	if (!isValidBodyOffset(offGroupes, size, kHeaderEnd) ||
		!isValidBodyOffset(offPoints, size, kHeaderEnd) ||
		!isValidBodyOffset(offNormals, size, kHeaderEnd) ||
		!isValidBodyOffset(offPolys, size, kHeaderEnd) ||
		!isValidBodyOffset(offLines, size, kHeaderEnd) ||
		!isValidBodyOffset(offSpheres, size, kHeaderEnd))
		return false;

	if (offPoints + (uint32)nbPoints * 8u > size)
		return false;
	if (offGroupes + (uint32)nbGroupes * 8u > size)
		return false;
	if (offNormals + (uint32)nbNormals * 8u > size)
		return false;

	return true;
}

static bool looksLikeLba1BodyHeader(const uint8 *data, uint32 size) {
	static const uint32 kVertexOffset = 0x1A;
	static const uint32 kBoneRecordSize = 36;

	if (size < kVertexOffset + 2)
		return false;

	const uint16 numVertices = READ_LE_UINT16(data + kVertexOffset);
	if (!isReasonableCount(numVertices))
		return false;

	const uint32 bonesOffset = kVertexOffset + 2u + (uint32)numVertices * 6u;
	if (bonesOffset + 2 > size)
		return false;

	const uint16 numBones = READ_LE_UINT16(data + bonesOffset);
	if (!isReasonableCount(numBones, 1024))
		return false;

	if (bonesOffset + 2u + (uint32)numBones * kBoneRecordSize > size)
		return false;

	return true;
}

static bool looksLikeLba2BodyHeader(const uint8 *data, uint32 size) {
	if (size < 0x60)
		return false;

	const int32 nbGroupes = (int32)READ_LE_UINT32(data + 0x20);
	const int32 offGroupes = (int32)READ_LE_UINT32(data + 0x24);
	const int32 nbPoints = (int32)READ_LE_UINT32(data + 0x28);
	const int32 offPoints = (int32)READ_LE_UINT32(data + 0x2C);
	const int32 nbNormals = (int32)READ_LE_UINT32(data + 0x30);
	const int32 offNormals = (int32)READ_LE_UINT32(data + 0x34);
	const int32 nbPolys = (int32)READ_LE_UINT32(data + 0x40);
	const int32 offPolys = (int32)READ_LE_UINT32(data + 0x44);
	const int32 nbLines = (int32)READ_LE_UINT32(data + 0x48);
	const int32 offLines = (int32)READ_LE_UINT32(data + 0x4C);
	const int32 nbSpheres = (int32)READ_LE_UINT32(data + 0x50);
	const int32 offSpheres = (int32)READ_LE_UINT32(data + 0x54);

	return validateLba2Header(nbGroupes, offGroupes, nbPoints, offPoints, nbNormals, offNormals,
		nbPolys, offPolys, nbLines, offLines, nbSpheres, offSpheres, size);
}

static int groupedPolyRecordSize(uint16 typePoly) {
	static const int kPolyDitherTable = 7;
	static const uint16 kMaskQuad = 1 << 15;
	static const uint16 kMaskEnv = 1 << 14;

	const uint8 baseType = typePoly & 0xFF;
	const bool isQuad = (typePoly & kMaskQuad) != 0;
	const bool isEnv = (typePoly & kMaskEnv) != 0;
	const bool isTexture = baseType > kPolyDitherTable;

	if (isQuad) {
		if (isEnv)
			return 16;
		return isTexture ? 32 : 12;
	}
	if (isEnv)
		return 16;
	return isTexture ? 24 : 12;
}

static bool appendGroupedTriangle(Common::Array<TwinE::BodyPolygon> &polygons, uint16 p1, uint16 p2,
		uint16 p3, int16 intensity) {
	TwinE::BodyPolygon poly;
	poly.intensity = intensity;
	poly.indices.push_back(p1);
	poly.indices.push_back(p2);
	poly.indices.push_back(p3);
	if (poly.indices.size() < 3)
		return false;
	polygons.push_back(poly);
	return true;
}

static bool readGroupedPolyRecord(Common::SeekableReadStream &stream, uint32 streamSize,
		uint16 typePoly, Common::Array<TwinE::BodyPolygon> &polygons) {
	static const uint16 kMaskQuad = 1 << 15;

	const int recordSize = groupedPolyRecordSize(typePoly);
	if (stream.pos() + 8 > streamSize)
		return false;

	const bool isQuad = (typePoly & kMaskQuad) != 0;
	const uint16 p1 = stream.readUint16LE();
	const uint16 p2 = stream.readUint16LE();
	const uint16 p3 = stream.readUint16LE();

	if (isQuad) {
		if (stream.pos() + 4 > streamSize)
			return false;
		const uint16 p4 = stream.readUint16LE();
		const int16 intensity = stream.readSint16LE();
		if (stream.pos() + recordSize - 10 > streamSize)
			return false;
		stream.skip(recordSize - 10);
		return appendGroupedTriangle(polygons, p1, p2, p3, intensity) &&
			appendGroupedTriangle(polygons, p1, p3, p4, intensity);
	}

	if (stream.pos() + 4 > streamSize)
		return false;
	stream.skip(2);
	const int16 intensity = stream.readSint16LE();
	if (stream.pos() + recordSize - 10 > streamSize)
		return false;
	stream.skip(recordSize - 10);
	return appendGroupedTriangle(polygons, p1, p2, p3, intensity);
}

static bool loadGroupedPolygonsLba2(Common::SeekableReadStream &stream, uint32 streamSize,
		int32 offPolys, int32 offLines, Common::Array<TwinE::BodyPolygon> &polygons) {
	if (!isValidBodyOffset(offPolys, streamSize, 0x60) ||
		!isValidBodyOffset(offLines, streamSize, offPolys))
		return false;

	uint32 pos = offPolys;
	while (pos < (uint32)offLines) {
		if (pos + 8 > streamSize)
			return false;

		stream.seek(pos);
		const uint16 typePoly = stream.readUint16LE();
		const uint16 nbPoly = stream.readUint16LE();
		const int32 offNext = stream.readSint32LE();
		if (nbPoly == 0 && offNext == 0)
			break;

		for (uint16 i = 0; i < nbPoly; ++i) {
			if (!readGroupedPolyRecord(stream, streamSize, typePoly, polygons))
				return false;
		}

		if (offNext <= 0)
			break;
		pos += (uint32)offNext;
	}

	return !polygons.empty();
}

} // namespace

namespace TwinE {

bool BodyData::isLikelyLba1Buffer(const uint8 *buf, uint32 size) {
	return buf && looksLikeLba1BodyHeader(buf, size);
}

bool BodyData::isLikelyLba2Buffer(const uint8 *buf, uint32 size) {
	return buf && looksLikeLba2BodyHeader(buf, size);
}

void BodyData::reset() {
	_vertices.clear();
	_bones.clear();
	_normals.clear();
	_polygons.clear();
	_spheres.clear();
	_lines.clear();
}

void BodyData::loadVertices(Common::SeekableReadStream &stream, uint32 streamSize) {
	const uint32 start = stream.pos();
	const uint16 numVertices = stream.readUint16LE();
	if (stream.eos() || !isReasonableCount(numVertices) ||
		start + 2u + (uint32)numVertices * 6u > streamSize)
		return;

	_vertices.reserve(numVertices);
	for (uint16 i = 0U; i < numVertices; ++i) {
		const int16 x = stream.readSint16LE();
		const int16 y = stream.readSint16LE();
		const int16 z = stream.readSint16LE();
		const uint16 bone = 0;
		_vertices.push_back({x, y, z, bone});
	}
}

void BodyData::loadBones(Common::SeekableReadStream &stream) {
	const uint16 numBones = stream.readUint16LE();
	if (stream.eos())
		return;

	_bones.reserve(numBones);
	for (uint16 i = 0; i < numBones; ++i) {
		const int16 firstPoint = stream.readSint16LE() / 6;
		const int16 numPoints = stream.readSint16LE();
		const int16 basePoint = stream.readSint16LE() / 6;
		const int16 baseElementOffset = stream.readSint16LE();
		BoneFrame boneframe;
		boneframe.type = (BoneType)stream.readSint16LE();
		boneframe.x = stream.readSint16LE();
		boneframe.y = stream.readSint16LE();
		boneframe.z = stream.readSint16LE();
		stream.readSint16LE();
		const int16 numNormals = stream.readSint16LE();
		stream.readSint16LE();
		stream.readSint32LE();
		stream.readSint32LE();
		stream.readSint32LE();
		stream.readSint32LE();

		BodyBone bone;
		bone.parent = baseElementOffset == -1 ? 0xffff : baseElementOffset / 38;
		bone.vertex = basePoint;
		bone.firstVertex = firstPoint;
		bone.numVertices = numPoints;
		bone.initalBoneState = boneframe;
		bone.numNormals = numNormals;

		for (int j = 0; j < numPoints; ++j) {
			const int idx = firstPoint + j;
			if (idx >= 0 && (uint)idx < _vertices.size())
				_vertices[idx].bone = i;
		}

		_bones.push_back(bone);
	}
}

void BodyData::loadNormals(Common::SeekableReadStream &stream) {
	const uint16 numNormals = stream.readUint16LE();
	if (stream.eos())
		return;

	_normals.reserve(numNormals);
	for (uint16 i = 0; i < numNormals; ++i) {
		BodyNormal shape;
		shape.x = stream.readSint16LE();
		shape.y = stream.readSint16LE();
		shape.z = stream.readSint16LE();
		shape.prenormalizedRange = stream.readUint16LE();
		_normals.push_back(shape);
	}
}

void BodyData::loadPolygons(Common::SeekableReadStream &stream) {
	const uint16 numPolygons = stream.readUint16LE();
	if (stream.eos())
		return;

	_polygons.reserve(numPolygons);
	for (uint16 i = 0; i < numPolygons; ++i) {
		BodyPolygon poly;
		poly.materialType = stream.readByte();
		const uint8 numVertices = stream.readByte();

		poly.intensity = stream.readSint16LE();
		int16 normal = -1;
		if (poly.materialType == MAT_FLAT || poly.materialType == MAT_GRANIT) {
			normal = stream.readSint16LE();
		}

		poly.indices.reserve(numVertices);
		poly.normals.reserve(numVertices);
		for (int k = 0; k < numVertices; ++k) {
			if (poly.materialType >= MAT_GOURAUD) {
				normal = stream.readSint16LE();
			}
			const uint16 vertexIndex = stream.readUint16LE() / 6;
			poly.indices.push_back(vertexIndex);
			poly.normals.push_back(normal);
		}

		_polygons.push_back(poly);
	}
}

void BodyData::loadLines(Common::SeekableReadStream &stream) {
	const uint16 numLines = stream.readUint16LE();
	if (stream.eos())
		return;

	_lines.reserve(numLines);
	for (uint16 i = 0; i < numLines; ++i) {
		BodyLine line;
		stream.skip(1);
		line.color = stream.readByte();
		stream.skip(2);
		line.vertex1 = stream.readUint16LE() / 6;
		line.vertex2 = stream.readUint16LE() / 6;
		_lines.push_back(line);
	}
}

void BodyData::loadSpheres(Common::SeekableReadStream &stream) {
	const uint16 numSpheres = stream.readUint16LE();
	if (stream.eos())
		return;

	_spheres.reserve(numSpheres);
	for (uint16 i = 0; i < numSpheres; ++i) {
		BodySphere sphere;
		sphere.fillType = stream.readByte();
		sphere.color = stream.readUint16LE();
		stream.readByte();
		sphere.radius = stream.readUint16LE();
		sphere.vertex = stream.readUint16LE() / 6;
		_spheres.push_back(sphere);
	}
}

bool BodyData::loadFromStream(Common::SeekableReadStream &stream, bool lba1) {
	reset();
	const uint32 streamSize = stream.size();

	if (lba1) {
		if (streamSize < 0x1C)
			return false;

		const uint16 flags = stream.readUint16LE();
		animated = (flags & INFO_ANIM) != 0;
		bbox.mins.x = stream.readSint16LE();
		bbox.maxs.x = stream.readSint16LE();
		bbox.mins.y = stream.readSint16LE();
		bbox.maxs.y = stream.readSint16LE();
		bbox.mins.z = stream.readSint16LE();
		bbox.maxs.z = stream.readSint16LE();
		offsetToData = stream.readSint16LE();

		if (!bodySeek(stream, 0x1A, streamSize))
			return false;

		loadVertices(stream, streamSize);
		if (_vertices.empty())
			return false;
		loadBones(stream);
		loadNormals(stream);
		loadPolygons(stream);
		loadLines(stream);
		loadSpheres(stream);
	} else {
		if (streamSize < 0x5C)
			return false;

		const uint32 flags = stream.readUint32LE();
		animated = (flags & MASK_OBJECT_ANIMATED) != 0;
		stream.skip(4);
		bbox.mins.x = stream.readSint32LE();
		bbox.maxs.x = stream.readSint32LE();
		bbox.mins.y = stream.readSint32LE();
		bbox.maxs.y = stream.readSint32LE();
		bbox.mins.z = stream.readSint32LE();
		bbox.maxs.z = stream.readSint32LE();

		const int32 nbGroupes = stream.readSint32LE();
		const int32 offGroupes = stream.readSint32LE();
		const int32 nbPoints = stream.readSint32LE();
		const int32 offPoints = stream.readSint32LE();
		const int32 nbNormals = stream.readSint32LE();
		const int32 offNormals = stream.readSint32LE();
		stream.readSint32LE();
		stream.readSint32LE();
		const int32 nbPolys = stream.readSint32LE();
		const int32 offPolys = stream.readSint32LE();
		const int32 nbLines = stream.readSint32LE();
		const int32 offLines = stream.readSint32LE();
		const int32 nbSpheres = stream.readSint32LE();
		const int32 offSpheres = stream.readSint32LE();
		stream.readSint32LE();
		stream.readSint32LE();

		if (!validateLba2Header(nbGroupes, offGroupes, nbPoints, offPoints, nbNormals, offNormals,
				nbPolys, offPolys, nbLines, offLines, nbSpheres, offSpheres, streamSize))
			return false;

		if (!bodySeek(stream, offPoints, streamSize))
			return false;
		_vertices.reserve(nbPoints);
		for (int32 i = 0; i < nbPoints; ++i) {
			if (stream.pos() + 8 > streamSize)
				return false;
			const int16 x = stream.readSint16LE();
			const int16 y = stream.readSint16LE();
			const int16 z = stream.readSint16LE();
			stream.skip(2);
			_vertices.push_back({x, y, z, 0});
		}

		if (!bodySeek(stream, offGroupes, streamSize))
			return false;
		_bones.reserve(nbGroupes);
		int16 vertexOffset = 0;
		for (int32 i = 0; i < nbGroupes; ++i) {
			if (stream.pos() + 8 > streamSize)
				return false;
			const uint16 orgGroupe = stream.readUint16LE();
			const uint16 orgPoint = stream.readUint16LE();
			const uint16 nbPts = stream.readUint16LE();
			const uint16 nbNorm = stream.readUint16LE();

			BodyBone bone;
			bone.parent = (i == 0) ? 0xffff : orgGroupe;
			bone.vertex = orgPoint;
			bone.firstVertex = vertexOffset;
			bone.numVertices = nbPts;
			bone.numNormals = nbNorm;
			bone.initalBoneState.type = TYPE_ROTATE;

			for (int j = 0; j < nbPts; ++j) {
				if (vertexOffset + j < (int)_vertices.size()) {
					_vertices[vertexOffset + j].bone = i;
				}
			}
			vertexOffset += nbPts;

			_bones.push_back(bone);
		}

		if (!bodySeek(stream, offNormals, streamSize))
			return false;
		_normals.reserve(nbNormals);
		for (int32 i = 0; i < nbNormals; ++i) {
			if (stream.pos() + 8 > streamSize)
				return false;
			BodyNormal normal;
			normal.x = stream.readSint16LE();
			normal.y = stream.readSint16LE();
			normal.z = stream.readSint16LE();
			normal.prenormalizedRange = stream.readUint16LE();
			_normals.push_back(normal);
		}

		if (!bodySeek(stream, offPolys, streamSize))
			return false;
		if (!loadGroupedPolygonsLba2(stream, streamSize, offPolys, offLines, _polygons))
			return false;

		if (!bodySeek(stream, offLines, streamSize))
			return false;
		_lines.reserve(nbLines);
		for (int32 i = 0; i < nbLines; ++i) {
			if (stream.pos() + 8 > streamSize)
				return false;
			BodyLine line;
			stream.skip(2);
			line.color = (uint8)stream.readUint16LE();
			line.vertex1 = stream.readUint16LE();
			line.vertex2 = stream.readUint16LE();
			_lines.push_back(line);
		}

		if (!bodySeek(stream, offSpheres, streamSize))
			return false;
		_spheres.reserve(nbSpheres);
		for (int32 i = 0; i < nbSpheres; ++i) {
			if (stream.pos() + 8 > streamSize)
				return false;
			BodySphere sphere;
			sphere.fillType = (uint8)stream.readUint16LE();
			sphere.color = stream.readUint16LE();
			sphere.vertex = stream.readUint16LE();
			sphere.radius = stream.readUint16LE();
			_spheres.push_back(sphere);
		}
	}

	return !stream.err() && !_vertices.empty() && !_polygons.empty();
}

bool BodyData::loadFromBuffer(const uint8 *buf, uint32 size, bool lba1) {
	if (!buf || size == 0)
		return false;
	Common::MemoryReadStream stream(buf, size);
	return loadFromStream(stream, lba1);
}

} // namespace TwinE
