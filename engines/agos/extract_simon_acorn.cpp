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

/* The ADFS filesystem parsing routines are based on Gerald Holdsworth's
 * DiscImageManager:
 * https://github.com/geraldholdsworth/DiscImageManager/
 *
 * Many thanks to Mike Woodroffe of Adventure Soft for giving
 * permission for this tool to include the disk 10 data.
 */

#include "engines/agos/extract_simon_acorn.h"

#include "common/endian.h"
#include "common/file.h"
#include "common/ptr.h"
#include "common/algorithm.h"
#include "common/util.h"

#include <errno.h>
#ifdef WIN32
#include <direct.h>
#else
#include <sys/stat.h>
#include <sys/types.h>
#endif

static bool pathExists(const Common::String &p) {
	return Common::Filename(p).exists();
}


static void makeDir(Tool *tool, const Common::String &path) {
	if (path.empty() || pathExists(path)) {
		return;
	}

#ifdef WIN32
	int result = _mkdir(path.c_str());
#else
	int result = mkdir(path.c_str(), 0755);
#endif

	if (result != 0 && errno != EEXIST && !pathExists(path) && tool != nullptr) {
		tool->warning("Could not create directory: %s", path.c_str());
	}
}

static void ensureDir(Tool *tool, const Common::String &p) {
	if (p.empty())
		return;

	Common::String cur;
	for (uint i = 0; i < p.size(); i++) {
		char c = p[i];
		cur += c;
		if ((c == '/' || c == '\\') && !cur.empty())
			makeDir(tool, cur);
	}

	makeDir(tool, p);
}


static Common::String joinPath(const Common::String &a, const Common::String &b) {
	if (a.empty()) return b;
	if (b.empty()) return a;
	if (a[a.size() - 1] == '/' || a[a.size() - 1] == '\\') return a + b;
	return a + "/" + b;
}

static Common::String dirnameOf(const Common::String &p) {
	int pos = -1;
	for (int i = (int)p.size() - 1; i >= 0; i--) {
		if (p[i] == '/' || p[i] == '\\') { pos = i; break; }
	}
	if (pos < 0) return "";
	return Common::String(p.c_str(), p.c_str() + pos);
}

class AdfsVolume;
struct AdfsObject;

static bool looksLikeAdfsDirBlock(const Common::Array<byte> &buf);
static bool readFileAnyDisk(const Common::HashMap<int, AdfsVolume *> &vols, const Common::String &adfsFilePath, uint32 wantLen, Common::Array<byte> &outData);

static uint32 rd32le(const byte *p) {
	return READ_LE_UINT32(p);
}

static uint32 rd24le(const byte *p) {
	return (uint32)p[0] | ((uint32)p[1] << 8) | ((uint32)p[2] << 16);
}

static Common::String toLower(Common::String s) {
	s.toLowercase();
	return s;
}

static bool ieq(const Common::String &a, const Common::String &b) {
	return toLower(a) == toLower(b);
}

static Common::String trimSpaces(const Common::String &s) {
	uint b = 0;
	while (b < s.size() && (s[b] == ' ' || s[b] == '\t' || s[b] == '\r' || s[b] == '\n')) b++;
	uint e = s.size();
	while (e > b && (s[e - 1] == ' ' || s[e - 1] == '\t' || s[e - 1] == '\r' || s[e - 1] == '\n')) e--;
	return Common::String(s.c_str() + b, s.c_str() + e);
}

static bool matchStarPatternICase(const Common::String &name, const Common::String &pattern) {
	Common::String n = toLower(name);
	Common::String p = toLower(pattern);
	if (p == "*") {
		return true;
	}

	int starPos = -1;
	for (int i = 0; i < (int)p.size(); i++) {
		if (p[i] == '*') { starPos = i; break; }
	}
	if (starPos < 0) {
		return n == p;
	}

	Common::String pre(p.c_str(), p.c_str() + starPos);
	Common::String post(p.c_str() + starPos + 1, p.c_str() + p.size());

	if (!pre.empty()) {
		if (n.size() < pre.size()) return false;
		if (Common::String(n.c_str(), n.c_str() + pre.size()) != pre) return false;
	}
	if (!post.empty()) {
		if (n.size() < post.size()) return false;
		if (Common::String(n.c_str() + n.size() - post.size(), n.c_str() + n.size()) != post) return false;
	}
	return true;
}

static Common::Array<byte> readFileBytes(const Common::String &p) {
	Common::File f;
	f.open(Common::Filename(p), "rb");
	if (!f.isOpen())
		error("Failed to open file: %s", p.c_str());
	uint32 sz = f.size();
	Common::Array<byte> out;
	if (sz > 0) {
		out.resize(sz);
		if (f.read_noThrow(out.begin(), sz) != sz)
			error("Failed to read file: %s", p.c_str());
	}
	f.close();
	return out;
}

static void writeFileBytes(Tool *tool, const Common::String &p, const Common::Array<byte> &data) {
	ensureDir(tool, dirnameOf(p));
	Common::File f;
	f.open(Common::Filename(p), "wb");
	if (!f.isOpen())
		error("Failed to write file: %s", p.c_str());
	if (!data.empty() && f.write(data.begin(), data.size()) != data.size())
		error("Failed to write file: %s", p.c_str());
	f.close();
}

static bool isAllZeros(const Common::Array<byte> &v) {
	for (uint i = 0; i < v.size(); i++) if (v[i] != 0) return false;
	return true;
}

// Write data to path only if it would be an improvement over what's already there.
static bool writeFileBytesIfBetter(Tool *tool, const Common::String &p, const Common::Array<byte> &data) {
	if (data.empty() || isAllZeros(data)) {
		return false;
	}
	if (pathExists(p)) {
		Common::Array<byte> existing = readFileBytes(p);
		if (existing.size() == data.size() && memcmp(existing.begin(), data.begin(), data.size()) == 0) {
			return false;
		}
	}
	writeFileBytes(tool, p, data);
	return true;
}

static void logSuccess(const Common::String &relPath, const char *kind, int disk) {
	debug(1, "Success: %s (%s, disk %d)", relPath.c_str(), kind, disk);
}
static void logSuccess(const Common::String &relPath, const char *kind, const char *label) {
	debug(1, "Success: %s (%s, %s)", relPath.c_str(), kind, label);
}

//0621 patch data from disk 10
static const byte kFIX_0621[] = {
	0x9E, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x55, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06,
	0x02, 0x06, 0x01, 0x02, 0x0A, 0x04, 0x02, 0x0E, 0x1A, 0x02, 0x12, 0x22, 0x02, 0x16, 0x3A, 0x44,
	0x04, 0x18, 0x38, 0x04, 0x1C, 0x3A, 0x18, 0x3C, 0x02, 0x24, 0x3C, 0x18, 0x3B, 0x02, 0x23, 0x6A,
	0x18, 0x3A, 0x00, 0x00, 0x24, 0x28, 0x02, 0xFA, 0x03, 0x1A, 0x04, 0x86, 0x02, 0x3A, 0x17, 0x00,
	0x29, 0x00, 0x03, 0x02, 0x36, 0x65, 0x04, 0x46, 0x00, 0x00, 0x80, 0x31, 0x2D, 0x00, 0x0F, 0x00,
	0x2E, 0x00, 0x10, 0x03, 0x14, 0x06, 0x3E, 0x02, 0x00, 0x18, 0x02, 0x04, 0x05, 0x1F, 0x0C, 0x00,
	0xBA, 0xA9, 0x05, 0x02, 0x0E, 0x03, 0x0C, 0x0E, 0x03, 0x72, 0x09, 0x1C, 0x05, 0x0C, 0x2A, 0x02,
	0x96, 0xFF, 0xFE, 0x08, 0x38, 0x07, 0x02, 0xAA, 0x0A, 0x08, 0x46, 0xAA, 0xE2, 0x11, 0x0C, 0x54,
	0x10, 0x0C, 0x62, 0x0F, 0x0C, 0x70, 0x0E, 0x0C, 0x7E, 0x0D, 0x06, 0xEA, 0x0F, 0x00, 0x62, 0x0A,
	0x58, 0x03, 0xFC, 0x09, 0x74, 0xBE, 0xA6, 0xF6, 0x0C, 0x90, 0x03, 0xBE, 0x0F, 0xAC, 0x0F, 0xC8,
	0x0D, 0xE4, 0x0F, 0x12, 0x32, 0x3C, 0x12, 0x3E, 0x12, 0x1A, 0x04, 0x66, 0x16, 0x06, 0x19, 0x1F,
	0x2E, 0xFF, 0xAF, 0x1F, 0x2E, 0x1F, 0x2E, 0x1F, 0x2E, 0x1F, 0x2E, 0x1F, 0x2E, 0x1F, 0x2E, 0x1F,
	0x2E, 0x1F, 0x2E, 0x1F, 0x2E, 0x1F, 0x2E, 0x1F, 0x2E, 0x16, 0xBE, 0x0C, 0x1C, 0x2E, 0x0B, 0x1C,
	0xDA, 0x5F, 0xFD, 0x03, 0x1C, 0x19, 0xE8, 0x0D, 0x1C, 0x13, 0xBA, 0x29, 0x04, 0x09, 0x2C, 0x12,
	0x08, 0x12, 0xE8, 0xFF, 0x1B, 0xE8, 0x0A, 0x0E, 0x1F, 0x90, 0x1F, 0x90, 0x2F, 0x58, 0x1F, 0x90,
	0xFD, 0xFF, 0x15, 0x90, 0x3B, 0x1F, 0x90, 0x1F, 0x90, 0x2F, 0xBE, 0x2F, 0xBE, 0x2F, 0xBE, 0x2F,
	0xBE, 0x2F, 0xBE, 0x2F, 0xBE, 0x2E, 0xBE, 0x02, 0xF2, 0x2A, 0x74, 0x13, 0x0E, 0x29, 0x82, 0x1D,
	0x2A, 0xFF, 0xFF, 0x1F, 0x62, 0x1F, 0x62, 0x1F, 0x7E, 0x2A, 0xE8, 0x1A, 0x54, 0x32, 0x04, 0x1A,
	0x62, 0x3D, 0x20, 0x3D, 0x3C, 0x3D, 0x58, 0x33, 0x74, 0x3D, 0x90, 0x3F, 0x1C, 0x3F, 0xC8, 0x3F,
	0xE4, 0x3F, 0x1C, 0xAE, 0x01, 0x0F, 0x42, 0x42, 0x3F, 0x1C, 0x35, 0x1C, 0x28, 0x48, 0x3E, 0x18,
	0x44, 0x8C, 0x24, 0x32,
};
static const uint kFIX_0621_LEN = 324u;

//0622 patch data from disk 10
static const byte kFIX_0622[] = {
	0x98, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x02, 0x03, 0x04,
	0x06, 0x90, 0x80, 0x22, 0x00, 0x10, 0x00, 0x00, 0x01, 0x10, 0x80, 0x20, 0x54, 0x1A, 0x00, 0x20,
	0x02, 0x08, 0x7C, 0x06, 0x08, 0xEA, 0x05, 0x10, 0x02, 0x5C, 0x06, 0x08, 0xCE, 0x02, 0x28, 0x02,
	0x20, 0x03, 0x48, 0x80, 0x22, 0x14, 0x18, 0x04, 0x08, 0xA8, 0x80, 0x17, 0x03, 0x30, 0x04, 0x12,
	0x80, 0x1B, 0x04, 0x08, 0x80, 0x06, 0x08, 0xEE, 0x80, 0x1A, 0xA1, 0x50, 0x03, 0x48, 0x05, 0x50,
	0x80, 0x19, 0x04, 0x08, 0xB2, 0x05, 0x20, 0x06, 0x18, 0x80, 0x1D, 0x04, 0x08, 0x74, 0x06, 0x08,
	0xD8, 0x09, 0x08, 0x05, 0x10, 0x07, 0x3C, 0x03, 0x50, 0x1D, 0x00, 0xFD, 0x0D, 0x0D, 0x03, 0x08,
	0x02, 0x06, 0x0C, 0x0C, 0x0B, 0x00, 0x00, 0x00, 0xFE, 0x0D, 0x0D, 0x07, 0xDD, 0xFE, 0x23, 0x00,
	0x02, 0x05, 0xF8, 0x04, 0x00, 0xD3, 0xDC, 0xCC, 0x40, 0x00, 0xCD, 0xDE, 0xDE, 0x04, 0xED, 0x0F,
	0x02, 0x12, 0x44, 0x02, 0x54, 0xFC, 0x55, 0x43, 0xCC, 0x2A, 0x03, 0x00, 0x00, 0xCC, 0xFF, 0xDC,
	0x03, 0xDD, 0xFC, 0xDC, 0xCC, 0xCC, 0xDC, 0x0B, 0xDD, 0xFF, 0x23, 0x04, 0x55, 0x40, 0x10, 0xFD,
	0x23, 0xCC, 0x2A, 0x04, 0xCC, 0x02, 0x13, 0xCD, 0xCC, 0xBC, 0xCC, 0x0B, 0x02, 0x3C, 0x50, 0x02,
	0x55, 0x00, 0x40, 0xF8, 0x54, 0x40, 0xC3, 0x2C, 0xCC, 0xCD, 0xCE, 0xDE, 0x06, 0xDD, 0xFF, 0xCD,
	0x0C, 0x02, 0x27, 0x05, 0x00, 0x20, 0x00, 0xFA, 0xD0, 0xCD, 0xCC, 0xCC, 0xD0, 0xD0, 0x04, 0xE0,
	0x05, 0xDE, 0x09, 0x02, 0x38, 0x18, 0x00, 0x00, 0x00, 0x04, 0xD0, 0xFC, 0xDD, 0xDD, 0xD3, 0x20,
	0x00, 0x15, 0x00, 0x07, 0x0D, 0xFF, 0x02, 0x11, 0x00, 0x00, 0x00, 0xFB, 0x0D, 0x00, 0xD0, 0xD0,
	0xDC, 0x04, 0xDD, 0x03, 0xDE, 0xFF, 0x34, 0x06, 0x00, 0xFA, 0x0D, 0x00, 0x00, 0x0C, 0x0D, 0x02,
	0x3E, 0x20, 0x14, 0x00, 0xF5, 0x0D, 0x0B, 0xDC, 0xC2, 0xDB, 0xDE, 0xEB, 0xB2, 0x00, 0x00, 0xCB,
	0x21, 0xEE, 0x12, 0x00, 0xF3, 0xD0, 0xB0, 0xCD, 0xB2, 0xCB, 0xBD, 0xDB, 0xB2, 0xEB, 0xBD, 0x00,
	0x00, 0xEE, 0xA1, 0xEE, 0x16, 0x00, 0xF7, 0xD0, 0xB0, 0xC0, 0xBD, 0xEC, 0xED, 0xC2, 0x2E, 0xE0,
	0x1D, 0x00, 0x40, 0x00, 0xFE, 0x30, 0x20, 0x06, 0x00, 0xFC, 0x0C, 0x0C, 0x0D, 0x0D, 0x02, 0x0E,
	0x17, 0x02, 0x70, 0xC0, 0x00, 0x00, 0xD0, 0xDD, 0xDD, 0xDE, 0x02, 0xEE, 0xFF, 0x44, 0x1B, 0x00,
	0xFD, 0x30, 0x20, 0x30, 0x7F, 0x00, 0xBE, 0x82, 0x64, 0x0F, 0x6C, 0x0F, 0x6C, 0x0F, 0x6C, 0x0F,
	0x6C, 0x07, 0x6C, 0x06, 0x13, 0x27, 0x14, 0x09, 0x71, 0x0E, 0x1A, 0x00, 0xFA, 0xD3, 0x02, 0x6D,
	0x00, 0xC0, 0xEE, 0x0E, 0x19, 0x00, 0xFA, 0x20, 0xD2, 0xD3, 0xE3, 0xE4, 0x40, 0x7F, 0x00, 0x67,
	0x0F, 0xDA, 0x0F, 0xDA, 0x07, 0xB0, 0x0F, 0xDA, 0x0F, 0xDA, 0x0F, 0x6E, 0x30, 0x20, 0x02, 0x00,
	0xFF, 0x0E, 0x02, 0x00, 0xFD, 0x02, 0x71, 0x02, 0x68, 0xE3, 0x02, 0xDE, 0x80, 0xBE, 0xDE, 0xEE,
	0x18, 0x00, 0xF9, 0x32, 0x24, 0x06, 0x72, 0x69, 0x1F, 0x4C, 0x1F, 0x4C, 0x1F, 0x4C, 0x1F, 0x4C,
	0x17, 0x4C, 0x05, 0x03, 0x64, 0x00, 0x00, 0xCD, 0x13, 0x00, 0xF5, 0x10, 0x2A, 0x32, 0x0C, 0x03,
	0x0C, 0x0C, 0xD0, 0x0D, 0xDD, 0x0E, 0x16, 0x40, 0x40, 0x00, 0xF8, 0x10, 0xCC, 0x22, 0xCC, 0x22,
	0x11, 0xE0, 0x19, 0x00, 0xFC, 0xC0, 0x20, 0x30, 0x02, 0xE3, 0x6E, 0xA8, 0x28, 0x00, 0x00, 0x17,
	0x14, 0xBE, 0x13, 0x1E, 0xBE, 0xF8, 0x13, 0xBE, 0x0E, 0x05, 0x30, 0x12, 0xC0, 0xF3, 0x1A, 0xC0,
	0x05, 0x05, 0x25, 0x02, 0x1F, 0xC2, 0x18, 0x19, 0xC2, 0xE5, 0x07, 0x06, 0x76, 0xF2, 0x10, 0x1A,
	0x03, 0x76, 0xDC, 0xDD, 0xCD, 0x00, 0xDD, 0xDE, 0x20, 0x09, 0x00, 0xE0, 0x15, 0x00, 0xF7, 0x02,
	0x79, 0xBB, 0xBC, 0x22, 0x8A, 0xE0, 0x1A, 0x07, 0x7A, 0x7E, 0x00, 0x00, 0x0D, 0x85, 0x00, 0x24,
	0x38, 0x09, 0x2A, 0x38, 0xFA, 0x34, 0x00, 0x04, 0x02, 0x74, 0x12, 0x00, 0xFC, 0x43, 0x34, 0x00,
	0x05, 0x13, 0x00, 0x28, 0x00, 0xFE, 0x43, 0x54, 0x15, 0x00, 0xFD, 0x40, 0x54, 0x05, 0x08, 0x23,
	0xD7, 0x0C, 0x1D, 0x42, 0x0E, 0x00, 0x10, 0x08, 0xF7, 0x0D, 0xDC, 0xCC, 0x15, 0x45, 0x0D, 0x00,
	0xF6, 0xD0, 0x3D, 0xCC, 0x16, 0x48, 0x0F, 0x00, 0x02, 0xD0, 0xA0, 0x02, 0x7F, 0x00, 0x1E, 0x00,
	0x0C, 0x24, 0x98, 0x08, 0x2A, 0x98, 0xF8, 0x02, 0x60, 0x00, 0x30, 0x20, 0x05, 0x04, 0x0F, 0x00,
	0x04, 0x00, 0xF9, 0x43, 0x30, 0x40, 0x05, 0x00, 0x04, 0x50, 0x0F, 0x02, 0x65, 0x04, 0x02, 0x00,
	0xFE, 0x40, 0x8A, 0x82, 0x30, 0x02, 0x0A, 0x40, 0x03, 0x0A, 0x05, 0x54, 0x03, 0x08, 0x6E, 0x05,
	0x25, 0x1E, 0x02, 0x0F, 0x0E, 0x00, 0xF5, 0x24, 0x20, 0x00, 0x02, 0xFE, 0xEF, 0xFD, 0xFD, 0xF3,
	0x0E, 0x0B, 0x00, 0xF7, 0x25, 0x25, 0xF0, 0x30, 0xE0, 0x7F, 0x00, 0x25, 0x14, 0x00, 0x00, 0x0B,
	0x34, 0x02, 0x0C, 0x3C, 0x02, 0x05, 0x00, 0xFB, 0x04, 0x00, 0x00, 0x25, 0x34, 0x14, 0x00, 0xFA,
	0x52, 0x0A, 0x03, 0x02, 0x6B, 0x04, 0x50, 0x02, 0x09, 0x43, 0x02, 0x12, 0x40, 0x30, 0x02, 0x12,
	0x40, 0x02, 0x1B, 0x05, 0x54, 0x18, 0x00, 0x40, 0x0C, 0xFE, 0x20, 0x30, 0x03, 0x00, 0xFA, 0x23,
	0xF2, 0x0E, 0x0E, 0x04, 0x02, 0x65, 0x28, 0xF4, 0xF7, 0x44, 0xED, 0xE3, 0x20, 0x00, 0xFD, 0xFE,
	0xFC, 0xFD, 0x03, 0x02, 0xE7, 0xF6, 0x30, 0x20, 0x3E, 0xDD, 0xCD, 0xCE, 0xD0, 0x20, 0xD0, 0x69,
	0xE0, 0x02, 0x6D, 0x3F, 0x00, 0x02, 0x6E, 0xFC, 0x03, 0x30, 0x37, 0x71, 0x02, 0xDD, 0xFE, 0xEE,
	0xEE, 0x0A, 0x0F, 0x6E, 0x0F, 0x6E, 0x0F, 0x6E, 0x17, 0x82, 0x0F, 0x6E, 0x0F, 0x6E, 0x08, 0x6E,
	0x20, 0x02, 0x6E, 0x00, 0x0D, 0x0E, 0x0F, 0x06, 0xCF, 0x04, 0x00, 0xFF, 0xE0, 0x0D, 0x07, 0xD3,
	0xAE, 0xC0, 0x13, 0x07, 0xD3, 0x02, 0x09, 0x05, 0xD3, 0x17, 0x05, 0xD3, 0xFB, 0x33, 0x54, 0x0E,
	0x05, 0x0F, 0x0D, 0x00, 0xFB, 0x33, 0xC6, 0x06, 0xD1, 0x32, 0x16, 0xED, 0x02, 0xD1, 0xF3, 0x0E,
	0x12, 0xAC, 0x0B, 0xD1, 0x38, 0x00, 0x21, 0x43, 0xC0, 0x02, 0x17, 0xF5, 0x14, 0x30, 0x00, 0x0D,
	0xD0, 0xD0, 0xC5, 0x00, 0x00, 0xED, 0x0C, 0x17, 0x36, 0x12, 0x17, 0x36, 0x02, 0x09, 0x15, 0x36,
	0x16, 0x0D, 0x63, 0x0C, 0x00, 0xFC, 0x43, 0x29, 0x06, 0x62, 0x12, 0xFD, 0xEE, 0x02, 0x62, 0xDE,
	0x0C, 0x1D, 0x33, 0x31, 0x00, 0x22, 0x07, 0xC4, 0xFA, 0x15, 0xFF, 0x05, 0xC5, 0x27, 0x03, 0x23,
	0x68, 0x25, 0x03, 0x22, 0x72, 0xEF, 0xA3, 0x26, 0x03, 0x02, 0x65, 0x13, 0xFE, 0x07, 0xC8, 0x0E,
	0x0F, 0xC8, 0x22, 0x05, 0x1F, 0x99, 0x22, 0x08, 0x14, 0x2A, 0x10, 0x00, 0xFC, 0x23, 0xC5, 0x07,
	0x12, 0x29, 0x46, 0xE7, 0x0E, 0x25, 0xC9, 0x12, 0xE8, 0x43, 0x54, 0x1A, 0x24, 0xC9, 0x1C, 0x45,
	0xE5, 0x18, 0xF3, 0x22, 0x89, 0x20, 0xD3, 0x1F, 0xF4, 0x02, 0x43, 0x1C, 0xF4, 0xBE, 0xF9, 0x4D,
	0x33, 0x2C, 0x14, 0xF4, 0x37, 0xA7, 0x16, 0xF4, 0x04, 0x69, 0x17, 0x0F, 0x65, 0x0F, 0x65, 0x0F,
	0x10, 0x2F, 0x58, 0x0F, 0x64, 0x07, 0x64, 0x53, 0xC8, 0x02, 0xEB, 0xFF, 0x5B, 0x2C, 0xC6, 0x0F,
	0x64, 0x0F, 0xC9, 0x0F, 0x64, 0x0F, 0xC8, 0x0F, 0xC8, 0x3F, 0xF4, 0x3F, 0xF4, 0x3F, 0xF4, 0x59,
	0xA4, 0x02, 0x12, 0x2E, 0x3B, 0x86, 0x0C, 0x3C, 0x86, 0x2C, 0x00, 0x00, 0x00,
};
static const uint kFIX_0622_LEN = 1053u;

static bool vecEqualsBlob(const Common::Array<byte> &v, const byte *blob, uint blobLen) {
	if (v.size() != blobLen) return false;
	if (blobLen == 0) return true;
	return memcmp(v.begin(), blob, blobLen) == 0;
}

static UcmpResult ucmpDecompress(const Common::Array<byte> &wrapped) {
	UcmpResult r;
	r.ok = false;
	r.error.clear();
	r.expectedOut = 0;
	r.producedOut = 0;
	r.innerMethod = 0;
	r.out.clear();

	if (wrapped.size() < 8) {
		r.error = "tooSmall";
		return r;
	}

	uint32 expected = rd32le(&wrapped[0]);
	r.expectedOut = expected;
	if (expected == 0u || expected > (64u * 1024u * 1024u)) {
		r.error = "badExpected";
		return r;
	}

	const byte *stream = wrapped.begin() + 4;
	size_t streamLen = wrapped.size() - 4;

	if (streamLen < 4) {
		r.error = "innerHeaderTruncated";
		return r;
	}

	byte method = stream[0];
	r.innerMethod = method;
	if (!(method == 0u || method == 1u)) {
		r.error = "badMethod";
		return r;
	}

	size_t src = 4;

	if (method == 1u) {
		r.out.resize((uint)(streamLen - src));
		if (streamLen > src) memcpy(r.out.begin(), stream + src, streamLen - src);
		r.producedOut = (uint32)r.out.size();
		r.ok = (r.producedOut == expected);
		if (!r.ok) r.error = "lenMismatch";
		return r;
	}

	Common::Array<byte> out;
	out.reserve(expected);

	uint16 ctrl = 0;
	int bitsLeft = 0;

	while (src < streamLen && out.size() < (size_t)expected) {
		if (bitsLeft == 0) {
			if (src + 2 > streamLen) break;
			byte lo = stream[src++];
			byte hi = stream[src++];
			ctrl = (uint16)lo | ((uint16)hi << 8);
			bitsLeft = 16;
		}

		uint16 bit = (uint16)(ctrl & 1u);
		ctrl >>= 1;
		bitsLeft--;

		if (bit == 0u) {
			if (src >= streamLen) break;
			out.push_back(stream[src++]);
		} else {
			if (src + 2 > streamLen) break;
			byte t = stream[src++];
			byte b = stream[src++];

			uint32 len = (uint32)(t & 0x0Fu) + 1u;
			uint32 off = (uint32)b + ((uint32)(t & 0xF0u) << 4);

			if (off == 0u || off > out.size()) {
				r.error = "backrefOob";
				return r;
			}

			for (uint32 i = 0; i < len && out.size() < (size_t)expected; i++) {
				out.push_back(out[out.size() - off]);
			}
		}

		if (out.size() > (size_t)expected + 0x100000u) {
			r.error = "runawayOutput";
			return r;
		}
	}

	if (out.size() + 1u == (size_t)expected) {
		out.push_back(0x00);
	}

	r.out = out;
	r.producedOut = (uint32)r.out.size();
	r.ok = (r.producedOut == expected);
	if (!r.ok) r.error = "lenMismatch";
	return r;
}

AdfImage::AdfImage(const Common::String &path) : _path(path), _originalSize(0) {
	_data = readFileBytes(path);

	if (_data.size() == 814080) {
		_originalSize = _data.size();
		_data.resize(819200); memset(_data.begin() + _originalSize, 0, 819200 - _originalSize);
	} else if (_data.size() == 819200) {
		_originalSize = _data.size();
	} else {
		error("Unsupported ADF size (expected 819200 or 814080 bytes): %s", path.c_str());
	}
}

Common::Array<byte> AdfImage::slice(uint off, uint len) const {
	if (len == 0) return Common::Array<byte>();
	Common::Array<byte> out;
	out.resize((uint)len);
	memset(out.begin(), 0, len);
	if (off >= _data.size()) return out;
	size_t avail = _data.size() - off;
	size_t take = (avail < len) ? avail : len;
	if (take > 0) {
		memcpy(out.begin(), _data.begin() + off, take);
	}
	return out;
}

static Common::String readAdfsName(const byte *p, size_t n) {
	Common::String s;
	for (size_t i = 0; i < n; i++) {
		unsigned char uc = (unsigned char)p[i];
		if (uc == 0) break;
		if (uc <= 31) break;
		char c = (char)uc;
		s += c;
	}
	return trimSpaces(s);
}

static bool disk10IsHugoNickDirBlock(const Common::Array<byte> &blk) {
	if (blk.size() < 2048) return false;
	char tag1[5];
	memcpy(tag1, &blk[1], 4);
	tag1[4] = 0;
	if (!(Common::String(tag1) == "Hugo" || Common::String(tag1) == "Nick")) return false;
	char tag2[5];
	memcpy(tag2, &blk[0x7FB], 4);
	tag2[4] = 0;
	if (Common::String(tag2) != Common::String(tag1)) return false;
	return true;
}

static Common::String disk10CleanName10(const byte *p10) {
	Common::String s;
	for (int i = 0; i < 10; i++) {
		byte c = (byte)(p10[i] & 0x7F);
		if (c == 0) break;
		if (c == 0x0D) break;
		if (c < 32) break;
		s += (char)c;
	}
	while (!s.empty() && s.lastChar() == ' ') s.deleteLastChar();
	return s;
}

static Disk10OldDirEntry disk10BrutefindLeafEntry(const AdfImage &img, const Common::String &leafName) {
	Disk10OldDirEntry r;
	r.ok = false;
	r.length = 0;
	r.sector = 0;

	const Common::String want = toLower(trimSpaces(leafName));
	const size_t dirSize = 2048;
	const size_t entryBase = 0x05;
	const size_t entrySize = 0x1A;
	const size_t numentries = 77;

	for (size_t off = 0; off + dirSize <= img.size(); off += 256) {
		Common::Array<byte> blk = img.slice(off, dirSize);
		if (!disk10IsHugoNickDirBlock(blk)) continue;

		for (size_t i = 0; i < numentries; i++) {
			size_t eo = entryBase + i * entrySize;
			if (eo + entrySize > blk.size()) break;
			if (blk[eo + 0] == 0) continue;

			Common::String nm = disk10CleanName10(&blk[eo + 0]);
			if (nm.empty()) continue;
			if (toLower(nm) != want) continue;

			uint32 len = rd32le(&blk[eo + 0x12]);
			uint32 sec = rd24le(&blk[eo + 0x16]);
			if (len == 0 || sec == 0) continue;

			r.ok = true;
			r.length = len;
			r.sector = sec;
			return r;
		}
	}

	return r;
}

static Common::Array<Disk10OldDirEntry> disk10BrutefindLeafEntries(const AdfImage &img, const Common::String &leafName) {
	Common::Array<Disk10OldDirEntry> out;

	const Common::String want = toLower(trimSpaces(leafName));
	const size_t dirSize = 2048;
	const size_t entryBase = 0x05;
	const size_t entrySize = 0x1A;
	const size_t numentries = 77;

	for (size_t off = 0; off + dirSize <= img.size(); off += 256) {
		Common::Array<byte> blk = img.slice(off, dirSize);
		if (!disk10IsHugoNickDirBlock(blk)) continue;

		for (size_t i = 0; i < numentries; i++) {
			size_t eo = entryBase + i * entrySize;
			if (eo + entrySize > blk.size()) break;
			if (blk[eo + 0] == 0) continue;

			Common::String nm = disk10CleanName10(&blk[eo + 0]);
			if (nm.empty()) continue;
			if (toLower(nm) != want) continue;

			uint32 len = rd32le(&blk[eo + 0x12]);
			uint32 sec = rd24le(&blk[eo + 0x16]);
			if (len == 0 || sec == 0) continue;

			Disk10OldDirEntry e;
			e.ok = true;
			e.length = len;
			e.sector = sec;
			out.push_back(e);
		}
	}

	return out;
}

static HugoNickDiscAddrEntry hugonickBrutefindLeafDiscaddr(const AdfImage &img, const Common::String &leafName) {
	HugoNickDiscAddrEntry r;
	r.ok = false;
	r.length = 0;
	r.discaddr = 0;
	r.attr = 0;

	const Common::String want = toLower(trimSpaces(leafName));
	const size_t dirSize = 2048;
	const size_t entryBase = 0x05;
	const size_t entrySize = 0x1A;

	for (size_t off = 0; off + dirSize <= img.size(); off += 256) {
		Common::Array<byte> blk = img.slice(off, dirSize);
		if (!disk10IsHugoNickDirBlock(blk)) continue;

		const size_t maxEntries = (blk.size() - entryBase) / entrySize;
		for (size_t i = 0; i < maxEntries; i++) {
			size_t eo = entryBase + i * entrySize;
			if (eo + entrySize > blk.size()) break;
			const byte *e = &blk[eo];

			Common::String nm = disk10CleanName10(e + 0x00);
			if (nm.empty()) continue;
			if (toLower(nm) != want) continue;

			uint32 len = rd32le(e + 0x12);
			uint32 discaddr = rd24le(e + 0x16);
			byte attr = e[0x19];
			if (discaddr == 0) continue;

			r.ok = true;
			r.length = len;
			r.discaddr = discaddr;
			r.attr = attr;
			return r;
		}
	}

	return r;
}

AdfsVolume::AdfsVolume(const AdfImage &img, bool requireSimon) : _img(img), _valid(false) {
	if (!tryInit(requireSimon)) {
		if (requireSimon)
			error("Failed to parse ADFS volume or locate !Simon directory in: %s", img.path().c_str());
	}
}

bool AdfsVolume::tryInit(bool requireSimon) {
	if (!tryParseDiscRecord()) return false;

	_rootDirDiscaddr = _bootmap + (_nzones * _secSize * 2u);
	_rootParentDirDiscaddr = _rootDirDiscaddr;

	_simonDirDiscaddr = 0xFFFFFFFFu;
	Common::Array<AdfsObject> rootEnts = listDirByDiscaddr(_rootParentDirDiscaddr);
	for (const AdfsObject &e : rootEnts) {
		if (ieq(e.name, "!Simon") && e.isDir) {
			_simonDirDiscaddr = e.startDiscaddr;
			break;
		}
	}
	if (_simonDirDiscaddr == 0xFFFFFFFFu) {
		if (requireSimon) return false;
		_simonDirDiscaddr = 0u;
	}
	_valid = true;
	return true;
}

bool AdfsVolume::findPath(const Common::String &adfsPath, AdfsObject &outObj) const {
	Common::StringList parts = splitDotPath(adfsPath);
	if (parts.empty()) return false;

	uint32 dirDiscaddr = _rootParentDirDiscaddr;

	for (size_t i = 0; i < parts.size(); i++) {
		const Common::String &want = parts[i];
		Common::Array<AdfsObject> ents = listDirByDiscaddr(dirDiscaddr);
		bool found = false;
		AdfsObject got{};

		for (const AdfsObject &e : ents) {
			if (ieq(e.name, want)) {
				found = true;
				got = e;
				break;
			}
		}
		if (!found) return false;

		if (i + 1 == parts.size()) {
			outObj = got;
			return true;
		}

		if (!got.isDir) return false;
		dirDiscaddr = got.startDiscaddr;
	}

	return false;
}

Common::Array<byte> AdfsVolume::readFile(const AdfsObject &f) const {
	if (f.isDir) { error("readFile called on dir"); return Common::Array<byte>(); }

	Common::Array<AdfsVolume::Frag> frags = discaddrToFrags(f.startDiscaddr);
	if (frags.empty()) {
		uint32 fragId = (f.startDiscaddr / 0x100u) & 0xFFFFu;
		uint32 sector = f.startDiscaddr & 0xFFu;
		uint32 secAdj = (sector > 0) ? (sector - 1u) : 0u;
		uint64 guessOff = (uint64)fragId * (uint64)_secSize + (uint64)secAdj * (uint64)_secSize;
		if (guessOff + f.length <= _img.size()) {
			return _img.slice((size_t)guessOff, (size_t)f.length);
		}
		error("ADFS map lookup failed for file start address: %s", f.name.c_str()); return Common::Array<byte>();
	}

	Common::Array<byte> out;
	out.reserve(f.length);

	uint32 remaining = f.length;

	for (const AdfsVolume::Frag &fr : frags) {
		if (remaining == 0) break;
		if (fr.offset >= _img.size()) break;

		uint32 canTake = fr.length;
		uint32 maxAvail = (uint32)(_img.size() - fr.offset);
		if (canTake > maxAvail) canTake = maxAvail;
		if (canTake > remaining) canTake = remaining;

		Common::Array<byte> chunk = _img.slice(fr.offset, canTake);
		out.push_back(chunk);
		remaining -= canTake;
	}

	if (out.size() != (size_t)f.length) {
		warning("Short read: file=%s want=%u got=%u (disk=%s)",
				f.name.c_str(), f.length, (uint)out.size(), _img.path().c_str());
	}

	return out;
}

byte AdfsVolume::readU8(uint32 off) const {
	Common::Array<byte> b = _img.slice(off, 1);
	return b.empty() ? 0 : b[0];
}

uint32 AdfsVolume::readBits(uint32 baseOff, uint32 startBit, uint32 nbits) const {
	uint32 res = 0;
	if (nbits == 0 || nbits > 32) return 0;

	uint32 prevByteOff = 0xFFFFFFFFu;
	byte lastbyte = 0;

	for (uint32 bit = 0; bit < nbits; bit++) {
		uint32 byteOff = baseOff + ((startBit + bit) / 8u);
		uint32 bitInByte = (startBit + bit) & 7u;

		if (byteOff != prevByteOff) {
			prevByteOff = byteOff;
			lastbyte = readU8(byteOff);
		}

		uint32 b = (uint32)((lastbyte >> bitInByte) & 1u);
		res |= (b << bit);
	}

	return res;
}

bool AdfsVolume::tryParseDiscRecord() {
	Common::Array<byte> dr = _img.slice(0, 0x40);
	if (dr.size() < 0x40) return false;

	byte log2sec = dr[4];
	_secSize = 1u << log2sec;

	_idlen = dr[8];
	_zoneSpareBits = (uint32)readUint16LELocal(&dr[14]);

	_bpmb = (uint32)dr[21];
	if (_bpmb == 0) _bpmb = 128;

	_discSize = 819200;
	_bootmap = 0;
	_nzones = 1;

	if (_secSize != 1024) {
		warning("Unexpected sector size %u in %s", _secSize, _img.path().c_str());
	}
	if (_idlen == 0 || _idlen > 31) return false;
	if (_zoneSpareBits == 0) return false;
	return true;
}

Common::Array<AdfsVolume::Frag> AdfsVolume::discaddrToFrags(uint32 addr) const {
	const uint32 drSize = 0x40;
	const uint32 header = 4;

	Common::Array<AdfsVolume::Frag> frags;
	const uint32 mapBase = _bootmap + drSize;

	uint32 idPerZone = (((_secSize * 8u) - _zoneSpareBits) / (_idlen + 1u));
	if (idPerZone == 0) return frags;

	uint32 fragId = (addr / 0x100u) & 0xFFFFu;
	uint32 startZone = (((addr / 0x100u) & 0xFFFFu) / idPerZone);

	for (uint32 zonecounter = 0; zonecounter < _nzones; zonecounter++) {
		uint32 zone = (zonecounter + startZone) % _nzones;

		uint32 allmap = (zone + 1u) * _secSize * 8u - drSize * 8u;
		uint32 i = zone * _secSize * 8u;
		if (zone > 0) i -= (drSize * 8u - header * 8u);

		while (i < allmap) {
			uint32 offBits = i;

			uint32 id = readBits(mapBase, i, _idlen);
			i += _idlen;

			{
				uint32 j = i - 1u;
				while (true) {
					j++;
					if (j >= allmap) break;
					byte b = readU8(mapBase + (j / 8u));
					if (bitIsSet(b, j & 7u)) break;
				}

				i = j;

				if (id == fragId) {
					uint32 offBytes = (offBits - (_zoneSpareBits * zone)) * _bpmb;
					offBytes %= _discSize;
					uint32 lenBytes = (j - offBits + 1u) * _bpmb;

					Frag f;
					f.offset = offBytes;
					f.length = lenBytes;
					frags.push_back(f);
				}
			}

			i++;
		}
	}

	if (frags.empty()) return frags;

	uint32 sector = addr & 0xFFu;
	if (sector >= 1u) sector -= 1u;

	for (AdfsVolume::Frag &f : frags) {
		f.offset = f.offset + sector * _secSize;
	}

	return frags;
}

Common::Array<AdfsObject> AdfsVolume::listDirByDiscaddr(uint32 dirDiscaddr) const {
	Common::Array<AdfsVolume::Frag> fr;
	if (dirDiscaddr == _rootParentDirDiscaddr) {
		Frag f;
		f.offset = dirDiscaddr;
		f.length = 0x800;
		fr.push_back(f);
	} else {
		fr = discaddrToFrags(dirDiscaddr);
	}
	if (fr.empty()) return Common::Array<AdfsObject>();
	if (fr[0].offset >= _img.size()) return Common::Array<AdfsObject>();

	const size_t entrySize = 0x1A;
	const size_t entryBase = 5;

	Common::Array<AdfsObject> out;
	out.reserve(128);

	Common::HashMap<Common::String, bool> seenLower;

	auto parseBlock = [&](const Common::Array<byte> &blk) {
		if (blk.size() < 0x800) return;
		if (blk.size() <= entryBase) return;

		const size_t maxEntries = (blk.size() - entryBase) / entrySize;

		for (size_t i = 0; i < maxEntries; i++) {
			const byte *e = &blk[entryBase + i * entrySize];

			Common::String name = readAdfsName(e + 0x00, 10);
			if (name.empty()) continue;

			uint32 start = rd24le(e + 0x16);
			if (start == 0) continue;

			byte attr = e[0x19];
			bool isDir = (attr & 0x08) != 0;

			AdfsObject obj;
			obj.name = trimSpaces(name);
			obj.isDir = isDir;
			obj.loadAddr = rd32le(e + 0x0A);
			obj.execAddr = rd32le(e + 0x0E);
			obj.length = rd32le(e + 0x12);
			obj.startDiscaddr = start;
			obj.parentDirDiscaddr = dirDiscaddr;

			Common::String low = toLower(obj.name);
			if (!seenLower.contains(low)) {
				seenLower[low] = true;
				out.push_back(obj);
			}
		}
	};

	Common::Array<byte> blk0 = _img.slice(fr[0].offset, 0x800);
	if (blk0.size() < 0x800) return Common::Array<AdfsObject>();

	const bool isHugoNick = disk10IsHugoNickDirBlock(blk0);

	parseBlock(blk0);

	if (isHugoNick) {
		char tag1[5];
		memcpy(tag1, &blk0[1], 4);
		tag1[4] = 0;

		const uint32 maxExtraBlocks = 32;
		for (uint32 b = 1; b <= maxExtraBlocks; b++) {
			uint64 off = static_cast<uint64>(fr[0].offset) + static_cast<uint64>(b) * 0x800ull;
			if (off + 0x800ull > _img.size()) break;

			Common::Array<byte> blkN = _img.slice(static_cast<size_t>(off), 0x800);
			if (blkN.size() < 0x800) break;

			if (!disk10IsHugoNickDirBlock(blkN)) break;

			char tagN[5];
			memcpy(tagN, &blkN[1], 4);
			tagN[4] = 0;
			if (Common::String(tagN) != Common::String(tag1)) break;

			parseBlock(blkN);
		}
	}

	Common::sort(out.begin(), out.end(), [](const AdfsObject &a, const AdfsObject &b) {
		return toLower(a.name) < toLower(b.name);
	});

	return out;
}

Common::Array<AdfsObject> AdfsVolume::listDir(const Common::String &adfsDirPath) const {
	if (adfsDirPath.empty()) {
		return listDirByDiscaddr(_rootParentDirDiscaddr);
	}
	AdfsObject dirObj;
	if (!findPath(adfsDirPath, dirObj)) return Common::Array<AdfsObject>();
	if (!dirObj.isDir) return Common::Array<AdfsObject>();
	return listDirByDiscaddr(dirObj.startDiscaddr);
}

Common::StringList AdfsVolume::splitDotPath(const Common::String &p) const {
	Common::StringList parts;
	Common::String cur;
	for (char c : p) {
		if (c == '.') {
			cur = trimSpaces(cur);
			if (!cur.empty()) parts.push_back(cur);
			cur.clear();
		} else {
			cur += c;
		}
	}
	cur = trimSpaces(cur);
	if (!cur.empty()) parts.push_back(cur);
	return parts;
}

static bool looksLikeAdfsDirBlock(const Common::Array<byte> &buf) {
	if (buf.size() < 0x800) return false;
	if (buf[0] != 0x00) return false;
	const char *tag = (const char *)&buf[1];
	if (!((tag[0] == 'H' && tag[1] == 'u' && tag[2] == 'g' && tag[3] == 'o') ||
			(tag[0] == 'N' && tag[1] == 'i' && tag[2] == 'c' && tag[3] == 'k'))) {
		return false;
	}
	const char *tag2 = (const char *)&buf[0x7FB];
	return tag2[0] == tag[0] && tag2[1] == tag[1] && tag2[2] == tag[2] && tag2[3] == tag[3];
}

static bool readFileAnyDisk(const Common::HashMap<int, AdfsVolume *> &vols, const Common::String &adfsFilePath, uint32 wantLen, Common::Array<byte> &outData) {
	for (Common::HashMap<int, AdfsVolume *>::const_iterator it = vols.begin(); it != vols.end(); ++it) {
		int disk = it->_key;
		AdfsVolume *vol = it->_value;
		if (!vol) continue;
		if (disk == 10) continue;
		AdfsObject obj;
		if (!vol->findPath(adfsFilePath, obj)) continue;
		if (obj.isDir) continue;
		if (wantLen != 0 && obj.length != wantLen) continue;
		Common::Array<byte> data = vol->readFile(obj);
		if (wantLen != 0 && data.size() != (size_t)wantLen) continue;
		if (looksLikeAdfsDirBlock(data)) continue;
		outData = data;
		return true;
	}
	return false;
}

static Common::String expectedDiskFilename(int n) {
	return "Simon the Sorcerer - Acorn Archimedes - (Disk " + Common::String::format("%d", n) + ").adf";
}

static DiskSet loadDisks(const Common::String &inputDir) {
	DiskSet ds;
	for (int i = 1; i <= 10; i++) {
		Common::String p = joinPath(inputDir, expectedDiskFilename(i));
		if (i == 10) {
			if (pathExists(p)) ds.diskPaths[i] = p;
			continue;
		}
		if (!pathExists(p)) error("Missing required disk image: %s", p.c_str());
		ds.diskPaths[i] = p;
	}
	return ds;
}

static void createInstallDirs(Tool *tool, const Common::String &outRoot) {
	static const char * const kDirs[] = {
		"!Simon", "!Simon/Execute", "!Simon/Tables", "!Simon/Text", "!Simon/Tunes",
		"!Simon/00", "!Simon/01", "!Simon/02", "!Simon/03", "!Simon/04", "!Simon/05",
		"!Simon/06", "!Simon/07", "!Simon/08", "!Simon/09", "!Simon/10", "!Simon/11",
		"!Simon/12", "!Simon/13", "!Simon/14", "!Simon/15", "!Simon/16",
	};

	for (int _di = 0; _di < ARRAYSIZE(kDirs); _di++) ensureDir(tool, joinPath(outRoot, kDirs[_di]));

}

static int diskNumberFromScriptPrefix(const Common::String &spec) {
	Common::String s = spec;
	const char *hit = strstr(s.c_str(), "ADFS::Simon");
	if (!hit) return -1;
	const char *q = hit + 11;
	int n = 0;
	while (*q && isdigit((unsigned char)*q)) {
		n = n * 10 + (*q - '0');
		q++;
	}
	if (n <= 0) {
		return -1;
	}
	return n;
}

static Common::String adfsPathFromScript(const Common::String &spec) {
	Common::String s = trimSpaces(spec);
	const char *hit = strstr(s.c_str(), "!Simon");
	if (!hit) {
		return "";
	}
	return Common::String(hit);
}

static Common::String normalizeDestPrefix(const Common::String &destPrefix) {
	Common::String s = trimSpaces(destPrefix);
	while (!s.empty() && s.lastChar() == '.') s.deleteLastChar();
	for (uint ci = 0; ci < s.size(); ci++) {
		if (s[ci] == '.') s.setChar('/', ci);
	}
	return s;
}

static void copyGlobFromAdfs(Tool *tool, const Common::HashMap<int, AdfsVolume *> &vols, const AdfsVolume &vol, const Common::String &adfsDirPath, const Common::String &pattern, const Common::String &outDir, const Common::String &relOutDir, int diskForLog) {
	Common::Array<AdfsObject> entries = vol.listDir(adfsDirPath);
	for (const AdfsObject &e : entries) {
		if (e.isDir) {
			continue;
		}
		if (e.name == "!Boot" || e.name == "!Run" || e.name == "!Sprites") {
			continue;
		}
		if (!matchStarPatternICase(e.name, pattern)) {
			continue;
		}
		Common::Array<byte> data = vol.readFile(e);

		if (data.size() != (size_t)e.length || looksLikeAdfsDirBlock(data)) {
			Common::Array<byte> alt;
			Common::String fullPath = adfsDirPath + "." + e.name;
			if (readFileAnyDisk(vols, fullPath, e.length, alt)) {
				data = alt; alt.clear();
			}
		}
		Common::String outPath = joinPath(outDir, e.name);
		if (writeFileBytesIfBetter(tool, outPath, data)) {
			if (!relOutDir.empty() && diskForLog > 0) logSuccess(relOutDir + "/" + e.name, "raw", diskForLog);
		}
	}
}

static void ucmpGlobFromAdfs(Tool *tool, const Common::HashMap<int, AdfsVolume *> &vols, const AdfsVolume &vol, const Common::String &adfsDirPath, const Common::String &pattern, const Common::String &outDir, const Common::String &relOutDir, int diskForLog) {
	Common::Array<AdfsObject> entries = vol.listDir(adfsDirPath);
	for (const AdfsObject &e : entries) {
		if (e.isDir) {
			continue;
		}
		if (!matchStarPatternICase(e.name, pattern)) {
			continue;
		}

		if (e.name == "!Boot" || e.name == "!Run" || e.name == "!Sprites") {
			continue;
		}

		Common::Array<byte> data = vol.readFile(e);

		if (data.size() != (size_t)e.length || looksLikeAdfsDirBlock(data)) {
			Common::Array<byte> alt;
			Common::String fullPath = adfsDirPath + "." + e.name;
			if (readFileAnyDisk(vols, fullPath, e.length, alt)) {
				data = alt; alt.clear();
			}
		}

		bool ok = false;
		UcmpResult last{};
		Common::Array<byte> out;

		const size_t tryOffs[] = { 8, 4, 0 };
		for (size_t off : tryOffs) {
			if (data.size() <= off) continue;
			Common::Array<byte> view;
			if (data.size() > off) { view.resize((uint)(data.size() - off)); memcpy(view.begin(), data.begin() + off, data.size() - off); }
			UcmpResult res = ucmpDecompress(view);
			last = res;
			if (res.ok) {
				ok = true;
				out = res.out;
				break;
			}
		}

		Common::String outPath = joinPath(outDir, e.name);
		if (ok) {
			if (writeFileBytesIfBetter(tool, outPath, out)) {
				if (!relOutDir.empty() && diskForLog > 0) logSuccess(relOutDir + "/" + e.name, "ucmp", diskForLog);
			}
		} else {
			warning("UCMP decompress failed, writing raw: %s.%s expected=%u got=%u method=%d err=%s",
				adfsDirPath.c_str(), e.name.c_str(), last.expectedOut, last.producedOut, (int)last.innerMethod, last.error.c_str());
			writeFileBytesIfBetter(tool, outPath, data);
			if (!relOutDir.empty() && diskForLog > 0) logSuccess(relOutDir + "/" + e.name, "raw", diskForLog);
		}
	}
}

static void runInstallScriptFake(Tool *tool, const Common::HashMap<int, AdfsVolume *> &vols, const Common::HashMap<int, AdfImage *> &images, const Common::String &outRoot) {
	createInstallDirs(tool, outRoot);

	auto getVol = [&](int disk) -> const AdfsVolume& {
		Common::HashMap<int, AdfsVolume*>::const_iterator it = vols.find(disk);
		if (it == vols.end() || it->_value == nullptr) { error("Disk not loaded: %d", disk); }
		return *it->_value;
	};

	auto COPY_FILE_glob = [&](const Common::String &destPrefix, const Common::String &srcSpec, const Common::String &patternOverride) {
		int disk = diskNumberFromScriptPrefix(srcSpec);
		Common::String adfsPath = adfsPathFromScript(srcSpec);
		Common::String dirPath = adfsPath;
		Common::String patt = patternOverride;
		{
			int _ld = -1;
			for (int _i = (int)adfsPath.size() - 1; _i >= 0; _i--) {
				if (adfsPath[_i] == '.') { _ld = _i; break; }
			}
			if (_ld >= 0) dirPath = Common::String(adfsPath.c_str(), adfsPath.c_str() + _ld);
		}
		Common::String outDir = normalizeDestPrefix(destPrefix);
		Common::String outPathDir = joinPath(outRoot, outDir);
		copyGlobFromAdfs(tool, vols, getVol(disk), dirPath, patt, outPathDir, outDir, disk);
	};

	auto UCMP_FILE_glob = [&](const Common::String &destPrefix, const Common::String &srcSpec, const Common::String &patternOverride) {
		int disk = diskNumberFromScriptPrefix(srcSpec);
		Common::String adfsPath = adfsPathFromScript(srcSpec);
		Common::String dirPath = adfsPath;
		Common::String patt = patternOverride;
		{
			int _ld = -1;
			for (int _i = (int)adfsPath.size() - 1; _i >= 0; _i--) {
				if (adfsPath[_i] == '.') { _ld = _i; break; }
			}
			if (_ld >= 0) dirPath = Common::String(adfsPath.c_str(), adfsPath.c_str() + _ld);
		}
		Common::String outDir = normalizeDestPrefix(destPrefix);
		Common::String outPathDir = joinPath(outRoot, outDir);
		ucmpGlobFromAdfs(tool, vols, getVol(disk), dirPath, patt, outPathDir, outDir, disk);
	};

	// Disk 1
	COPY_FILE_glob("!Simon.Execute.", "ADFS::Simon1.!Simon.Execute.*", "*");
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon1.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon1.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon1.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.01.", "ADFS::Simon1.!Simon.01.*", "*");
	UCMP_FILE_glob("!Simon.06.", "ADFS::Simon1.!Simon.06.*", "*");
	UCMP_FILE_glob("!Simon.12.", "ADFS::Simon1.!Simon.12.*", "*");
	UCMP_FILE_glob("!Simon.13.", "ADFS::Simon1.!Simon.13.*", "*");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon1.!Simon.14.*", "*");
	UCMP_FILE_glob("!Simon.15.", "ADFS::Simon1.!Simon.15.*", "*");
	UCMP_FILE_glob("!Simon.16.", "ADFS::Simon1.!Simon.16.*", "*");

	// Disk 2
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon2.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon2.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon2.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.00.", "ADFS::Simon2.!Simon.00.*", "*");
	UCMP_FILE_glob("!Simon.03.", "ADFS::Simon2.!Simon.03.*", "*");
	UCMP_FILE_glob("!Simon.05.", "ADFS::Simon2.!Simon.05.*", "*");
	UCMP_FILE_glob("!Simon.06.", "ADFS::Simon2.!Simon.06.*", "*");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon2.!Simon.14.1401", "1401");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon2.!Simon.14.1402", "1402");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon2.!Simon.14.1411", "1411");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon2.!Simon.14.1412", "1412");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon2.!Simon.14.1421", "1421");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon2.!Simon.14.1422", "1422");

	// Disk 3
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon3.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon3.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon3.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.01.", "ADFS::Simon3.!Simon.01.*", "*");
	UCMP_FILE_glob("!Simon.04.", "ADFS::Simon3.!Simon.04.*", "*");
	UCMP_FILE_glob("!Simon.05.", "ADFS::Simon3.!Simon.05.*", "*");
	UCMP_FILE_glob("!Simon.07.", "ADFS::Simon3.!Simon.07.*", "*");
	UCMP_FILE_glob("!Simon.08.", "ADFS::Simon3.!Simon.08.*", "*");
	UCMP_FILE_glob("!Simon.09.", "ADFS::Simon3.!Simon.09.*", "*");
	UCMP_FILE_glob("!Simon.10.", "ADFS::Simon3.!Simon.10.*", "*");

	// Disk 4
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon4.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon4.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon4.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.04.", "ADFS::Simon4.!Simon.04.*", "*");
	UCMP_FILE_glob("!Simon.05.", "ADFS::Simon4.!Simon.05.*", "*");
	UCMP_FILE_glob("!Simon.06.", "ADFS::Simon4.!Simon.06.*", "*");
	UCMP_FILE_glob("!Simon.08.", "ADFS::Simon4.!Simon.08.*", "*");
	UCMP_FILE_glob("!Simon.09.", "ADFS::Simon4.!Simon.09.*", "*");
	UCMP_FILE_glob("!Simon.10.", "ADFS::Simon4.!Simon.10.1031", "1031");
	UCMP_FILE_glob("!Simon.10.", "ADFS::Simon4.!Simon.10.1032", "1032");
	UCMP_FILE_glob("!Simon.11.", "ADFS::Simon4.!Simon.11.*", "*");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon4.!Simon.14.*", "*");

	// Disk 5
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon5.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon5.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon5.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.00.", "ADFS::Simon5.!Simon.00.*", "*");
	UCMP_FILE_glob("!Simon.01.", "ADFS::Simon5.!Simon.01.*", "*");
	UCMP_FILE_glob("!Simon.02.", "ADFS::Simon5.!Simon.02.*", "*");
	UCMP_FILE_glob("!Simon.08.", "ADFS::Simon5.!Simon.08.*", "*");

	// Disk 6
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon6.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon6.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon6.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.01.", "ADFS::Simon6.!Simon.01.*", "*");
	UCMP_FILE_glob("!Simon.05.", "ADFS::Simon6.!Simon.05.*", "*");
	UCMP_FILE_glob("!Simon.06.", "ADFS::Simon6.!Simon.06.*", "*");
	UCMP_FILE_glob("!Simon.07.", "ADFS::Simon6.!Simon.07.*", "*");
	UCMP_FILE_glob("!Simon.08.", "ADFS::Simon6.!Simon.08.*", "*");
	UCMP_FILE_glob("!Simon.09.", "ADFS::Simon6.!Simon.09.*", "*");
	UCMP_FILE_glob("!Simon.10.", "ADFS::Simon6.!Simon.10.*", "*");
	UCMP_FILE_glob("!Simon.12.", "ADFS::Simon6.!Simon.12.*", "*");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon6.!Simon.14.*", "*");

	// Disk 7
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon7.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon7.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon7.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.03.", "ADFS::Simon7.!Simon.03.*", "*");
	UCMP_FILE_glob("!Simon.04.", "ADFS::Simon7.!Simon.04.*", "*");
	UCMP_FILE_glob("!Simon.05.", "ADFS::Simon7.!Simon.05.*", "*");
	UCMP_FILE_glob("!Simon.06.", "ADFS::Simon7.!Simon.06.*", "*");
	UCMP_FILE_glob("!Simon.09.", "ADFS::Simon7.!Simon.09.*", "*");
	UCMP_FILE_glob("!Simon.10.", "ADFS::Simon7.!Simon.10.*", "*");
	UCMP_FILE_glob("!Simon.12.", "ADFS::Simon7.!Simon.12.*", "*");

	// Disk 8
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon8.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon8.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon8.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.00.", "ADFS::Simon8.!Simon.00.*", "*");
	UCMP_FILE_glob("!Simon.02.", "ADFS::Simon8.!Simon.02.*", "*");
	UCMP_FILE_glob("!Simon.05.", "ADFS::Simon8.!Simon.05.*", "*");
	UCMP_FILE_glob("!Simon.11.", "ADFS::Simon8.!Simon.11.*", "*");
	UCMP_FILE_glob("!Simon.12.", "ADFS::Simon8.!Simon.12.*", "*");
	UCMP_FILE_glob("!Simon.13.", "ADFS::Simon8.!Simon.13.*", "*");
	UCMP_FILE_glob("!Simon.12.", "ADFS::Simon8.!Simon.12.*", "*");

	// Disk 9
	COPY_FILE_glob("!Simon.Tunes.", "ADFS::Simon9.!Simon.Tunes.*Tune", "*Tune");
	COPY_FILE_glob("!Simon.Tables.", "ADFS::Simon9.!Simon.Tables.Tables*", "Tables*");
	COPY_FILE_glob("!Simon.Text.", "ADFS::Simon9.!Simon.Text.Text*", "Text*");
	UCMP_FILE_glob("!Simon.00.", "ADFS::Simon9.!Simon.00.*", "*");
	UCMP_FILE_glob("!Simon.09.", "ADFS::Simon9.!Simon.09.*", "*");
	UCMP_FILE_glob("!Simon.10.", "ADFS::Simon9.!Simon.10.*", "*");
	UCMP_FILE_glob("!Simon.12.", "ADFS::Simon9.!Simon.12.*", "*");
	UCMP_FILE_glob("!Simon.14.", "ADFS::Simon9.!Simon.14.*", "*");
	UCMP_FILE_glob("!Simon.15.", "ADFS::Simon9.!Simon.15.*", "*");
	UCMP_FILE_glob("!Simon.16.", "ADFS::Simon9.!Simon.16.*", "*");

	{
		auto pathExists = [&](const Common::String &p) -> bool {
			FILE *f2 = fopen(p.c_str(), "rb");
			if (!f2) return false;
			fclose(f2);
			return true;
		};

		auto recoverOne = [&](int disk, const Common::String &relOutDir, const Common::String &leaf, bool tryUcmp) {
			Common::String outDir = joinPath(outRoot, relOutDir);
			ensureDir(tool, outDir);
			Common::String outPath = joinPath(outDir, leaf);
			if (pathExists(outPath)) return;

			Common::HashMap<int, AdfImage*>::const_iterator itImg = images.find(disk);
			if (itImg == images.end() || itImg->_value == nullptr) {
				warning("Disk image not loaded for disk %d while extracting %s/%s", disk, relOutDir.c_str(), leaf.c_str());
				return;
			}
			const AdfImage &img = *itImg->_value;

			Common::Array<byte> data;

			{
				HugoNickDiscAddrEntry he = hugonickBrutefindLeafDiscaddr(img, leaf);
				if (he.ok) {
					Common::HashMap<int, AdfsVolume*>::const_iterator itVol = vols.find(disk);
					if (itVol != vols.end() && itVol->_value != nullptr) {
						AdfsObject fake;
						fake.name = leaf;
						fake.isDir = false;
						fake.loadAddr = 0;
						fake.execAddr = 0;
						fake.length = he.length;
						fake.startDiscaddr = he.discaddr;
						fake.parentDirDiscaddr = 0;

						data = itVol->_value->readFile(fake);
					}
				}
			}

			if (data.empty()) {
				Disk10OldDirEntry fe = disk10BrutefindLeafEntry(img, leaf);
				if (fe.ok) {
					uint32 byteOff = fe.sector * 256u;
					if ((size_t)byteOff + (size_t)fe.length <= img.size()) {
						data = img.slice((size_t)byteOff, (size_t)fe.length);
					}
				}
			}

			if (data.empty()) {
				warning("Leaf not found on disk %d: %s/%s", disk, relOutDir.c_str(), leaf.c_str());
				return;
			}

			if (!tryUcmp) {
				writeFileBytes(tool, outPath, data);
				logSuccess(relOutDir + "/" + leaf, "raw", disk);
				return;
			}

			bool ok = false;
			UcmpResult last{};
			Common::Array<byte> out;
			const size_t tryOffs[] = { 8u, 4u, 0u };
			for (size_t ti = 0; ti < 3; ti++) {
				size_t off = tryOffs[ti];
				if (data.size() <= off) continue;
				Common::Array<byte> view;
			if (data.size() > off) { view.resize((uint)(data.size() - off)); memcpy(view.begin(), data.begin() + off, data.size() - off); }
				UcmpResult res = ucmpDecompress(view);
				last = res;
				if (res.ok) {
					ok = true;
					out = res.out;
					break;
				}
			}

			if (ok) {
				writeFileBytes(tool, outPath, out);
				logSuccess(relOutDir + "/" + leaf, "ucmp", disk);
			} else {
				writeFileBytes(tool, outPath, data);
				warning("UCMP failed, wrote raw: %s/%s expected=%u got=%u method=%d err=%s (disk %d)",
				relOutDir.c_str(), leaf.c_str(), last.expectedOut, last.producedOut, (int)last.innerMethod, last.error.c_str(), disk);
				logSuccess(relOutDir + "/" + leaf, "raw", disk);
			}
		};

		{
			const char *names[] = { "0001","0002","0011","0012","0021","0022","0071","0072","0081","0082" };
			for (size_t i = 0; i < sizeof(names)/sizeof(names[0]); i++) recoverOne(1, "!Simon/00", names[i], true);
		}

		{
			const char *names[] = { "0119","0141","0142","0151","0152","0161","0162","0171","0172","0181","0182","0191","0192" };
			for (size_t i = 0; i < sizeof(names)/sizeof(names[0]); i++) recoverOne(5, "!Simon/01", names[i], true);
		}

		{
			const char *names[] = { "0391","0392" };
			for (size_t i = 0; i < sizeof(names)/sizeof(names[0]); i++) recoverOne(7, "!Simon/03", names[i], true);
		}

		{
			const char *names[] = { "2TUNE","3TUNE","21TUNE","26TUNE","27TUNE" };
			for (size_t i = 0; i < sizeof(names)/sizeof(names[0]); i++) recoverOne(6, "!Simon/Tunes", names[i], false);
		}

	}

	bool disk10Present = false;
	{
		const AdfImage *img10 = nullptr;
		{
			Common::HashMap<int, AdfImage*>::const_iterator itImg = images.find(10);
			if (itImg != images.end()) img10 = itImg->_value;
		}

		Common::HashMap<int, AdfsVolume*>::const_iterator it10 = vols.find(10);
		const AdfsVolume *v10 = (it10 != vols.end()) ? it10->_value : nullptr;

		if (img10 != nullptr) {
			disk10Present = true;
			Common::String out06 = joinPath(outRoot, "!Simon/06");
			ensureDir(tool, out06);

			const char *names[2] = { "0621", "0622" };
			for (int i = 0; i < 2; i++) {
				const char *leaf = names[i];

				Common::Array<byte> data;

				if (v10 != nullptr) {
					AdfsObject obj;
					const Common::String p1 = Common::String("!Update.Resources.Disc6.") + leaf;
					const Common::String p2 = Common::String("!Update.Resources.Disk6.") + leaf;

					if ((v10->findPath(p1, obj) && !obj.isDir) || (v10->findPath(p2, obj) && !obj.isDir)) {
						data = v10->readFile(obj);
					}
				}

				if (data.empty()) {
					Common::Array<Disk10OldDirEntry> cands = disk10BrutefindLeafEntries(*img10, leaf);
					for (size_t ci = 0; ci < cands.size(); ci++) {
						const Disk10OldDirEntry &ce = cands[ci];
						if (!ce.ok) continue;
						uint32 byteOff = ce.sector * 256u;
						if ((size_t)byteOff + (size_t)ce.length > img10->size()) continue;

						Common::Array<byte> cand = img10->slice((size_t)byteOff, (size_t)ce.length);

						if (ieq(leaf, "0621") && vecEqualsBlob(cand, kFIX_0621, kFIX_0621_LEN)) {
							data = cand;
							break;
						}
						if (ieq(leaf, "0622") && vecEqualsBlob(cand, kFIX_0622, kFIX_0622_LEN)) {
							data = cand;
							break;
						}
						if (data.empty() && !cand.empty()) data = cand;
					}
				}

				if (data.empty()) {
					warning("Disk 10 update file not found or unreadable: !Update.Resources.Disc6.%s", leaf);
					continue;
				}

				Common::String outPath = joinPath(out06, Common::String(leaf));
				writeFileBytes(tool, outPath, data);
				logSuccess(Common::String("!Simon/06/") + leaf, "raw-update", 10);
			}
		}
	}
	{
		{
			Common::String p = joinPath(outRoot, "!Simon/06/0621");
			if (pathExists(p)) {
				Common::Array<byte> cur = readFileBytes(p);
				if (!vecEqualsBlob(cur, kFIX_0621, kFIX_0621_LEN)) {
					Common::Array<byte> good(kFIX_0621, (int)kFIX_0621_LEN);
					writeFileBytes(tool, p, good);
					logSuccess("!Simon/06/0621", "patched", disk10Present ? "disk 10" : "files generated");
				}
			}
		}

		{
			Common::String p = joinPath(outRoot, "!Simon/06/0622");
			if (pathExists(p)) {
				Common::Array<byte> cur = readFileBytes(p);
				if (!vecEqualsBlob(cur, kFIX_0622, kFIX_0622_LEN)) {
					Common::Array<byte> good(kFIX_0622, (int)kFIX_0622_LEN);
					writeFileBytes(tool, p, good);
					logSuccess("!Simon/06/0622", "patched", disk10Present ? "disk 10" : "files generated");
				}
			}
		}
	}
}


ExtractSimonAcorn::ExtractSimonAcorn(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	ToolInput input;
	input.format = "*";
	input.file = false;
	_inputPaths.push_back(input);

	_shorthelp = "Extract Simon the Sorcerer Acorn Archimedes data from installer ADF disk images.";

	_helptext =
		"Reads a set of ADF disk images and extracts the game data directly.\n"
		"Decompresses UCMP-compressed scene files as needed. This avoids\n"
		"having to run the original installer inside an emulator.\n"
		"\n"
		"Expected disk images (Disk 10 is the optional update disk):\n"
		"	Simon the Sorcerer - Acorn Archimedes - (Disk 1).adf\n"
		"	...\n"
		"	Simon the Sorcerer - Acorn Archimedes - (Disk 10).adf\n"
		"\n"
		"Usage:\n"
		"  scummvm-tools-cli --tool extract_simon_acorn --input-dir <inputdir> --output-dir <outputdir>\n";
}

void ExtractSimonAcorn::parseExtraArguments() {
	if (_arguments.empty() || _arguments.front() != "--input-dir")
		error("Missing required argument: --input-dir");

	_arguments.pop_front();
	if (_arguments.empty())
		error("Missing value for --input-dir");

	std::string inputDir = _arguments.front();
	_arguments.pop_front();

	if (_arguments.empty() || _arguments.front() != "--output-dir")
		error("Missing required argument: --output-dir");

	_arguments.pop_front();
	if (_arguments.empty())
		error("Missing value for --output-dir");

	_outputPath = _arguments.front();
	_arguments.pop_front();

	if (!_arguments.empty())
		error("Unexpected extra argument: %s", _arguments.front().c_str());

	_arguments.push_back(inputDir);
}

void ExtractSimonAcorn::execute() {
	if (_inputPaths.empty() || _inputPaths[0].path.empty())
		error("Missing input directory.");

	if (_outputPath.empty())
		error("Missing output directory. Use --output-dir <outputdir>.");

	Common::String inputDir = _inputPaths[0].path.c_str();
	Common::String outputDir = _outputPath.getFullPath().c_str();

	DiskSet ds = loadDisks(inputDir);

	Common::HashMap<int, AdfImage *> images;
	Common::HashMap<int, AdfsVolume *> vols;

	Common::Array<AdfImage *> imgStore;
	Common::Array<AdfsVolume *> volStore;

	for (Common::HashMap<int, Common::String>::const_iterator kv = ds.diskPaths.begin(); kv != ds.diskPaths.end(); ++kv) {
		int disk = kv->_key;
		const Common::String &path = kv->_value;

		AdfImage *img = new AdfImage(path);
		imgStore.push_back(img);
		images[disk] = img;

		AdfsVolume *vol = new AdfsVolume(*images[disk], disk != 10);
		if (!vol->isValid() && disk == 10) {
			delete vol;
			vols[disk] = nullptr;
			warning("Disk 10 present but does not contain a usable !Simon ADFS tree, skipping.");
			continue;
		}

		volStore.push_back(vol);
		vols[disk] = vol;
		debug(1, "Loaded disk %d: %s (%u bytes)", disk, path.c_str(), (uint)images[disk]->originalSize());
	}

	ensureDir(this, outputDir);
	runInstallScriptFake(this, vols, images, outputDir);

	debug(1, "Done.");

	for (uint di = 0; di < volStore.size(); di++)
		delete volStore[di];
	for (uint di = 0; di < imgStore.size(); di++)
		delete imgStore[di];
}

#ifdef STANDALONE_MAIN
int main(int argc, char *argv[]) {
	ExtractSimonAcorn tool(argv[0]);
	return tool.run(argc, argv);
}
#endif

