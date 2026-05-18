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


#ifndef EXTRACT_SIMON_ACORN_H
#define EXTRACT_SIMON_ACORN_H

#include "tool.h"
#include "common/scummsys.h"
#include "common/str.h"
#include "common/array.h"
#include "common/hashmap.h"
#include "common/hash-str.h"

struct UcmpResult {
	bool ok;
	Common::String error;
	uint32 expectedOut;
	uint32 producedOut;
	byte innerMethod;
	Common::Array<byte> out;
};

class AdfImage {
public:
	explicit AdfImage(const Common::String &path);

	const Common::String &path() const { return _path; }
	uint size() const { return _data.size(); }
	uint originalSize() const { return _originalSize; }

	Common::Array<byte> slice(uint off, uint len) const;

private:
	Common::String _path;
	Common::Array<byte> _data;
	uint _originalSize = 0;
};

struct AdfsObject {
	Common::String name;
	bool isDir;
	uint32 loadAddr;
	uint32 execAddr;
	uint32 length;
	uint32 startDiscaddr;
	uint32 parentDirDiscaddr;
};

struct Disk10OldDirEntry {
	bool ok;
	uint32 length;
	uint32 sector;
};

struct HugoNickDiscAddrEntry {
	bool ok;
	uint32 length;
	uint32 discaddr;
	byte attr;
};

class AdfsVolume {
public:
	explicit AdfsVolume(const AdfImage &img, bool requireSimon);

	bool isValid() const { return _valid; }
	uint32 rootParentDirDiscaddr() const { return _rootParentDirDiscaddr; }
	uint32 simonDirDiscaddr() const { return _simonDirDiscaddr; }

	bool findPath(const Common::String &adfsPath, AdfsObject &outObj) const;
	Common::Array<AdfsObject> listDir(const Common::String &adfsDirPath) const;
	Common::Array<AdfsObject> listDirByDiscaddr(uint32 discaddr) const;
	Common::Array<byte> readFile(const AdfsObject &f) const;

private:
	struct Frag {
		uint32 offset;
		uint32 length;
	};

	bool tryInit(bool requireSimon);
	bool tryParseDiscRecord();
	Common::Array<Frag> discaddrToFrags(uint32 addr) const;
	Common::StringList splitDotPath(const Common::String &p) const;

	const AdfImage &_img;

	uint32 _secSize = 1024;
	uint32 _nzones = 1;
	uint32 _bootmap = 0;
	uint32 _idlen = 15;
	uint32 _zoneSpareBits = 0;
	uint32 _bpmb = 128;
	uint32 _discSize = 819200;

	uint32 _rootDirDiscaddr = 0xFFFFFFFFu;
	uint32 _rootParentDirDiscaddr = 0xFFFFFFFFu;
	bool _valid = false;
	uint32 _simonDirDiscaddr = 0xFFFFFFFFu;

	static uint16 readUint16LELocal(const byte *p) {
		return (uint16)p[0] | ((uint16)p[1] << 8);
	}
	byte readU8(uint32 off) const;
	static bool bitIsSet(byte v, uint32 b) {
		return (v & (byte)(1u << (b & 7u))) != 0;
	}
	uint32 readBits(uint32 baseOff, uint32 startBit, uint32 nbits) const;
};

struct DiskSet {
	Common::HashMap<int, Common::String> diskPaths;
};

class ExtractSimonAcorn : public Tool {
public:
	ExtractSimonAcorn(const std::string &name = "extract_simon_acorn");

protected:
	void parseExtraArguments() override;
	void execute() override;
};


#endif // EXTRACT_SIMON_ACORN_H
