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

#include "engines/twine/extract_twine_convert.h"

#include "common/array.h"
#include "engines/twine/parser/body.h"
#include "engines/twine/parser/bodytypes.h"

#include <math.h>
#include <stdio.h>
#include <string.h>

namespace {

static int16 clampShade(int32 value) {
	if (value < 0)
		return 0;
	if (value > 255)
		return 255;
	return (int16)value;
}

static void buildNormalTable(const TwinE::BodyData &body, uint16 *normalTable) {
	const Common::Array<TwinE::BodyNormal> &normals = body.getNormals();
	for (uint i = 0; i < normals.size(); ++i) {
		const TwinE::BodyNormal &normal = normals[i];
		int32 intensity = normal.x * 0 + normal.y * -8192 + normal.z * 8192;
		if (intensity > 0) {
			intensity >>= 14;
			if (normal.prenormalizedRange > 0)
				intensity /= normal.prenormalizedRange;
		} else {
			intensity = 0;
		}
		normalTable[i] = (uint16)intensity;
	}
}

static int16 polygonShade(const TwinE::BodyData &body, const TwinE::BodyPolygon &poly,
		int cornerIndex, const uint16 *normalTable) {
	if (poly.materialType >= TwinE::MAT_GOURAUD) {
		const uint16 normalIdx = poly.normals[cornerIndex];
		if (normalIdx < body.getNormals().size())
			return clampShade(poly.intensity + normalTable[normalIdx]);
	}
	if (poly.materialType >= TwinE::MAT_FLAT) {
		const uint16 normalIdx = poly.normals.empty() ? 0 : poly.normals[0];
		if (normalIdx < body.getNormals().size())
			return clampShade(poly.intensity + normalTable[normalIdx]);
	}
	return clampShade(poly.intensity);
}

static void paletteColor(const TwinePalette &palette, int16 shadeIndex, float rgb[3]) {
	const uint8 idx = (uint8)shadeIndex;
	if (palette.loaded) {
		rgb[0] = palette.data[idx * 3 + 0] / 255.0f;
		rgb[1] = palette.data[idx * 3 + 1] / 255.0f;
		rgb[2] = palette.data[idx * 3 + 2] / 255.0f;
	} else {
		const float gray = idx / 255.0f;
		rgb[0] = rgb[1] = rgb[2] = gray;
	}
}

static bool writeMaterial(FILE *mtl, int materialId, const TwinePalette &palette, int16 shadeIndex) {
	float rgb[3];
	paletteColor(palette, shadeIndex, rgb);
	fprintf(mtl, "newmtl mat_%d\n", materialId);
	fprintf(mtl, "Kd %g %g %g\n", rgb[0], rgb[1], rgb[2]);
	fprintf(mtl, "Ka %g %g %g\n", rgb[0] * 0.2f, rgb[1] * 0.2f, rgb[2] * 0.2f);
	fprintf(mtl, "d 1.0\n\n");
	return true;
}

static void useShadeMaterial(FILE *obj, FILE *mtl, int16 shade, const TwinePalette &palette,
		bool written[256], int &currentMaterial) {
	const uint8 idx = (uint8)shade;
	if (!written[idx]) {
		writeMaterial(mtl, idx, palette, shade);
		written[idx] = true;
	}
	if (shade != currentMaterial) {
		fprintf(obj, "usemtl mat_%d\n", idx);
		currentMaterial = shade;
	}
}

} // namespace

bool twineTryParseBody(const uint8 *data, int32 size, bool preferLba1, TwinE::BodyData &body, bool &isLba1) {
	if (preferLba1) {
		if (TwinE::BodyData::isLikelyLba1Buffer(data, size) && body.loadFromBuffer(data, size, true)) {
			isLba1 = true;
			return true;
		}
		if (TwinE::BodyData::isLikelyLba2Buffer(data, size) && body.loadFromBuffer(data, size, false)) {
			isLba1 = false;
			return true;
		}
	} else {
		if (TwinE::BodyData::isLikelyLba2Buffer(data, size) && body.loadFromBuffer(data, size, false)) {
			isLba1 = false;
			return true;
		}
		if (TwinE::BodyData::isLikelyLba1Buffer(data, size) && body.loadFromBuffer(data, size, true)) {
			isLba1 = true;
			return true;
		}
	}
	return false;
}

bool twineAppendBodyToObj(const uint8 *data, int32 size, bool preferLba1, const TwinePalette &palette,
		FILE *obj, FILE *mtl, int &vertexBase, int &materialId, const TwineBodyTransform &xform,
		const char *objectName, bool materialsWritten[256]) {
	TwinE::BodyData body;
	bool isLba1 = preferLba1;
	if (!twineTryParseBody(data, size, preferLba1, body, isLba1))
		return false;

	bool localMaterials[256];
	if (!materialsWritten) {
		memset(localMaterials, 0, sizeof(localMaterials));
		materialsWritten = localMaterials;
	}

	if (objectName && objectName[0])
		fprintf(obj, "o %s\n", objectName);

	const float worldScale = 1.0f / 512.0f;
	const float bodyScale = 1.0f / 256.0f;
	const float angle = xform.beta * (2.0f * 3.14159265f / 4096.0f);
	const float c = cosf(angle);
	const float s = sinf(angle);
	const float tx = xform.posX * worldScale;
	const float ty = xform.posY * worldScale;
	const float tz = xform.posZ * worldScale;

	const Common::Array<TwinE::BodyVertex> &vertices = body.getVertices();
	for (uint i = 0; i < vertices.size(); ++i) {
		const TwinE::BodyVertex &v = vertices[i];
		const float lx = v.x * bodyScale;
		const float ly = v.y * bodyScale;
		const float lz = v.z * bodyScale;
		const float rx = lx * c + lz * s;
		const float rz = -lx * s + lz * c;
		fprintf(obj, "v %g %g %g\n", rx + tx, ly + ty, rz + tz);
	}

	uint16 normalTable[512];
	memset(normalTable, 0, sizeof(normalTable));
	buildNormalTable(body, normalTable);

	int currentMaterial = -1;
	const int numVertices = (int)vertices.size();
	const Common::Array<TwinE::BodyPolygon> &polygons = body.getPolygons();
	for (uint p = 0; p < polygons.size(); ++p) {
		const TwinE::BodyPolygon &poly = polygons[p];
		if (poly.indices.size() < 3)
			continue;

		bool valid = true;
		for (uint i = 0; i < poly.indices.size(); ++i) {
			if (poly.indices[i] >= (uint)numVertices) {
				valid = false;
				break;
			}
		}
		if (!valid)
			continue;

		const int16 shade = polygonShade(body, poly, 0, normalTable);
		useShadeMaterial(obj, mtl, shade, palette, materialsWritten, currentMaterial);

		fprintf(obj, "f");
		for (uint i = 0; i < poly.indices.size(); ++i)
			fprintf(obj, " %d", vertexBase + (int)poly.indices[i] + 1);
		fprintf(obj, "\n");
	}

	vertexBase += (int)vertices.size();
	return true;
}

bool twineConvertBodyToObj(const uint8 *data, int32 size, bool preferLba1, const TwinePalette &palette, const char *outPath) {
	char objPath[1100];
	char mtlPath[1100];
	snprintf(objPath, sizeof(objPath), "%s.obj", outPath);
	snprintf(mtlPath, sizeof(mtlPath), "%s.mtl", outPath);

	FILE *obj = fopen(objPath, "w");
	if (!obj)
		return false;

	FILE *mtl = fopen(mtlPath, "w");
	if (!mtl) {
		fclose(obj);
		return false;
	}

	const char *mtlName = strrchr(mtlPath, '/');
	if (!mtlName)
		mtlName = strrchr(mtlPath, '\\');
	mtlName = mtlName ? mtlName + 1 : mtlPath;

	fprintf(obj, "# LBA body exported by extract_twine\n");
	fprintf(obj, "mtllib %s\n\n", mtlName);

	int vertexBase = 0;
	int materialId = 0;
	TwineBodyTransform xform = {0, 0, 0, 0};
	const bool ok = twineAppendBodyToObj(data, size, preferLba1, palette, obj, mtl, vertexBase, materialId, xform, nullptr, nullptr);

	fclose(mtl);
	fclose(obj);
	return ok;
}
