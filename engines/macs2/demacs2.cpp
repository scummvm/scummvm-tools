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

/* MACS2 Script decompiler */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

static uint8_t *scriptData = nullptr;
static uint32_t scriptSize = 0;
static uint32_t pos = 0;

static uint8_t *stringData = nullptr;
static uint32_t stringDataSize = 0;

static bool amigaMode = false;

static void appendUtf8(std::string &out, uint8_t ch) {
	if (ch < 0x80) {
		out += (char)ch;
		return;
	}
	// CP850 to Unicode mapping for 0x80-0xFF
	static const uint16_t cp850[128] = {
		0x00C7,
		0x00FC,
		0x00E9,
		0x00E2,
		0x00E4,
		0x00E0,
		0x00E5,
		0x00E7,
		0x00EA,
		0x00EB,
		0x00E8,
		0x00EF,
		0x00EC,
		0x00ED,
		0x00C4,
		0x00C5,
		0x00C9,
		0x00E6,
		0x00C6,
		0x00F4,
		0x00F6,
		0x00F2,
		0x00FB,
		0x00F9,
		0x00FF,
		0x00D6,
		0x00DC,
		0x00F8,
		0x00A3,
		0x00D8,
		0x00D7,
		0x0192,
		0x00E1,
		0x00ED,
		0x00F3,
		0x00FA,
		0x00F1,
		0x00D1,
		0x00AA,
		0x00BA,
		0x00BF,
		0x00AE,
		0x00AC,
		0x00BD,
		0x00BC,
		0x00A1,
		0x00AB,
		0x00BB,
		0x2591,
		0x2592,
		0x2593,
		0x2502,
		0x2524,
		0x00C1,
		0x00C2,
		0x00C0,
		0x00A9,
		0x2563,
		0x2551,
		0x2557,
		0x255D,
		0x00A2,
		0x00A5,
		0x2510,
		0x2514,
		0x2534,
		0x252C,
		0x251C,
		0x2500,
		0x253C,
		0x00E3,
		0x00C3,
		0x255A,
		0x2554,
		0x2569,
		0x2566,
		0x2560,
		0x2550,
		0x256C,
		0x00A4,
		0x00F0,
		0x00D0,
		0x00CA,
		0x00CB,
		0x00C8,
		0x0131,
		0x00CD,
		0x00CE,
		0x00CF,
		0x2518,
		0x250C,
		0x2588,
		0x2584,
		0x00A6,
		0x00CC,
		0x2580,
		0x00D3,
		0x00DF,
		0x00D4,
		0x00D2,
		0x00F5,
		0x00D5,
		0x00B5,
		0x00FE,
		0x00DE,
		0x00DA,
		0x00DB,
		0x00D9,
		0x00FD,
		0x00DD,
		0x00AF,
		0x00B4,
		0x00AD,
		0x00B1,
		0x2017,
		0x00BE,
		0x00B6,
		0x00A7,
		0x00F7,
		0x00B8,
		0x00B0,
		0x00A8,
		0x00B7,
		0x00B9,
		0x00B3,
		0x00B2,
		0x25A0,
		0x00A0,
	};
	uint16_t u = cp850[ch - 0x80];
	if (u < 0x800) {
		out += (char)(0xC0 | (u >> 6));
		out += (char)(0x80 | (u & 0x3F));
	} else {
		out += (char)(0xE0 | (u >> 12));
		out += (char)(0x80 | ((u >> 6) & 0x3F));
		out += (char)(0x80 | (u & 0x3F));
	}
}

static std::string decodeStringAmiga(uint32_t offset, uint16_t numLines);

static std::string decodeString(uint32_t offset, uint16_t numLines) {
	if (amigaMode)
		return decodeStringAmiga(offset, numLines);
	if (!stringData || offset >= stringDataSize)
		return "";
	uint32_t p = offset;
	std::string result;
	for (uint16_t line = 0; line < numLines && p + 2 <= stringDataSize; line++) {
		uint16_t length = (uint16_t)stringData[p] | ((uint16_t)stringData[p + 1] << 8);
		p += 2;
		for (int i = 1; i <= length && p < stringDataSize; i++, p++) {
			uint8_t x = (uint8_t)(i * i * 0x0C);
			uint8_t y = (uint8_t)(stringData[p] ^ i);
			appendUtf8(result, x ^ y);
		}
		if (line + 1 < numLines)
			result += " / ";
	}
	return result;
}

static uint8_t readByte() {
	if (pos >= scriptSize)
		return 0;
	return scriptData[pos++];
}

static uint16_t readWord() {
	uint8_t lo = readByte();
	uint8_t hi = readByte();
	return (uint16_t)hi << 8 | lo;
}

static const struct {
	uint16_t id;
	const char *name;
} kSpecialNames[] = {
	{0x01, "interactedUse"},
	{0x02, "interactedLook"},
	{0x03, "interactedTalk"},
	{0x04, "areaAtActor"},
	{0x0B, "isRepeatRun"},
	{0x0D, "dialogueResult"},
	{0x23, "pathWalkable"},
	{0x24, "actorX"},
	{0x25, "actorY"},
	{0x26, "isSceneInit"},
	{0x27, "areaRepeatRun"},
	{0x28, "invCheck"},
	{0x29, "animBlobRange"},
	{0x2A, "invCombine"},
	{0x2B, "invAction"},
	{0x2C, "interactedMap"},
	{0x2D, "curScene"},
	{0x2F, "prevScene"},
	{0x30, "musicActive"},
	{0x31, "soundActive"},
};

static const char *getSpecialName(uint16_t val) {
	for (const auto &s : kSpecialNames) {
		if (s.id == val)
			return s.name;
	}
	return nullptr;
}

static std::string formatValue() {
	uint8_t type = readByte();
	uint16_t val = readWord();
	if (type == 0x00) {
		if (val > 0x400 && val <= 0x600) {
			char buf[32];
			snprintf(buf, sizeof(buf), "obj_0x%x", val - 0x400);
			return buf;
		}
		char buf[32];
		snprintf(buf, sizeof(buf), "%u", val);
		return buf;
	} else if (type == 0xFF) {
		const char *name = getSpecialName(val);
		if (name)
			return name;
		if (val >= 0x0E && val <= 0x22) {
			char buf[32];
			snprintf(buf, sizeof(buf), "%u", val - 0x0D);
			return buf;
		}
		char buf[32];
		snprintf(buf, sizeof(buf), "special[0x%02x]", val);
		return buf;
	}
	char buf[32];
	snprintf(buf, sizeof(buf), "var[%u]", val);
	return buf;
}

static const char *getOpcodeName(uint8_t opcode) {
	switch (opcode) {
	case 0x01:
		return "setVar";
	case 0x02:
		return "setVarOr";
	case 0x03:
		return "ifFalse";
	case 0x04:
		return "ifTrue";
	case 0x05:
		return "compare";
	case 0x06:
		return "ifInteraction";
	case 0x07:
		return "endIf";
	case 0x08:
		return "else";
	case 0x09:
		return "nop09";
	case 0x0A:
		return "printStringLeft";
	case 0x0B:
		return "moveObject";
	case 0x0C:
		return "changeScene";
	case 0x0D:
		return "showDialogue";
	case 0x0E:
		return "changeAnimation";
	case 0x0F:
		return "frameWait";
	case 0x10:
		return "walkToPosition";
	case 0x11:
		return "waitForWalk";
	case 0x12:
		return "setPathfinding";
	case 0x13:
		return "skipUntil14";
	case 0x14:
		return "skipWord";
	case 0x15:
		return "clearDialogueChoices";
	case 0x16:
		return "addDialogueChoice";
	case 0x17:
		return "showDialogueChoice";
	case 0x18:
		return "dismissPanel";
	case 0x19:
		return "walkToAndPickup";
	case 0x1A:
		return "setPickupFrames";
	case 0x1B:
		return "setupObject";
	case 0x1C:
		return "setSkippable";
	case 0x1D:
		return "clearSkippable";
	case 0x1E:
		return "playAnimation";
	case 0x1F:
		return "pathWalkable = testPathfinding";
	case 0x20:
		return "setYOffset";
	case 0x21:
		return "setMotion";
	case 0x22:
		return "setOrientation";
	case 0x23:
		return "moveToPosition";
	case 0x24:
		return "addValues";
	case 0x25:
		return "subValues";
	case 0x26:
		return "loadSpecialAnim";
	case 0x27:
		return "setDirection";
	case 0x28:
		return "stopAnimation";
	case 0x29:
		return "openInventory";
	case 0x2A:
		return "loadObjectAnim";
	case 0x2B:
		return "checkObjectData";
	case 0x2C:
		return "invCheck = checkInventory";
	case 0x2D:
		return "setSnapToTarget";
	case 0x2E:
		return "animRangeTest = testSceneAnimFrame";
	case 0x2F:
		return "animRangeTest = testObjectAnimFrame";
	case 0x30:
		return "printStringRight";
	case 0x31:
		return "setPaletteDarkness";
	case 0x32:
		return "setObjectClickable";
	case 0x33:
		return "setObjectVisible";
	case 0x34:
		return "setHotspotOverride";
	case 0x35:
		return "setObjectBounds";
	case 0x36:
		return "dismissAllPanels";
	case 0x37:
		return "resetToSceneScript";
	case 0x38:
		return "loadOverlayFont";
	case 0x39:
		return "endOverlayText";
	case 0x3A:
		return "addOverlayTextEntry";
	case 0x3B:
		return "clearOverlayText";
	case 0x3C:
		return "fadeToBlack";
	case 0x3D:
		return "fadeFromBlack";
	case 0x3E:
		return "loadPcmSound";
	case 0x3F:
		return "freePcmSound";
	case 0x40:
		return "playPcmSound";
	case 0x41:
		return "waitForSound";
	case 0x42:
		return "stopPcmSound";
	case 0x43:
		return "loadMusicSlot";
	case 0x44:
		return "playMusicSlot";
	case 0x45:
		return "stopMusicSlot";
	case 0x46:
		return "freeMusicSlot";
	case 0x47:
		return "waitForMusic";
	case 0x48:
		return "getObjectX";
	case 0x49:
		return "getObjectY";
	case 0x4A:
		return "getObjectField8";
	case 0x4B:
		return "getObjectOrientation";
	case 0x4C:
		return "clearActorInventory";
	case 0x4D:
		return "setPathfindingRemap";
	case 0x4E:
		return "waitForAdlib";
	default:
		return "???";
	}
}

static const char *cmpOpName(uint8_t op) {
	switch (op) {
	case 0x01:
		return "==";
	case 0x02:
		return "!=";
	case 0x03:
		return "<";
	case 0x04:
		return ">";
	case 0x05:
		return "<=";
	case 0x06:
		return ">=";
	default:
		return "?";
	}
}

struct DecompiledLine {
	uint32_t offset;
	uint8_t opcode;
	int indent;
	std::string params;
	std::vector<uint8_t> args;
};

static std::string decodeParams(uint8_t opcode, uint32_t endPos, int &indent) {
	std::string result;

	char buf[1024];
	switch (opcode) {
	case 0x01:
	case 0x02: {
		readByte();
		uint16_t varIdx = readWord();
		std::string val = formatValue();
		snprintf(buf, sizeof(buf), " var[%u] = %s", varIdx, val.c_str());
		result = buf;
		break;
	}
	case 0x03:
	case 0x04: {
		std::string val = formatValue();
		snprintf(buf, sizeof(buf), " (%s)", val.c_str());
		result = buf;
		indent++;
		break;
	}
	case 0x05: {
		uint8_t cmpOp = readByte();
		std::string a = formatValue();
		std::string b = formatValue();
		snprintf(buf, sizeof(buf), " (%s %s %s)", a.c_str(), cmpOpName(cmpOp), b.c_str());
		result = buf;
		indent++;
		break;
	}
	case 0x06: {
		uint8_t subOp = readByte();
		std::string i = formatValue();
		std::string a = formatValue();
		std::string b = formatValue();
		snprintf(buf, sizeof(buf), " %s(%s, %s, %s)", subOp == 2 ? "NOT " : "", i.c_str(), a.c_str(), b.c_str());
		result = buf;
		indent++;
		break;
	}
	case 0x08: {
		indent++;
		break;
	}
	case 0x0A:
	case 0x30: {
		std::string x = formatValue();
		std::string y = formatValue();
		uint16_t strOffset = readWord();
		uint16_t numLines = readWord();
		std::string decoded = decodeString(strOffset, numLines);
		if (!decoded.empty())
			snprintf(buf, sizeof(buf), " pos=(%s,%s) str=%u lines=%u \"%s\"", x.c_str(), y.c_str(), strOffset, numLines, decoded.c_str());
		else
			snprintf(buf, sizeof(buf), " pos=(%s,%s) str=%u lines=%u", x.c_str(), y.c_str(), strOffset, numLines);
		result = buf;
		break;
	}
	case 0x0B: {
		std::string obj = formatValue();
		std::string scene = formatValue();
		std::string x = formatValue();
		std::string y = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s scene=%s pos=(%s,%s)", obj.c_str(), scene.c_str(), x.c_str(), y.c_str());
		result = buf;
		break;
	}
	case 0x0C: {
		std::string scene = formatValue();
		std::string mode = formatValue();
		std::string speed = formatValue();
		snprintf(buf, sizeof(buf), " scene=%s mode=%s speed=%s", scene.c_str(), mode.c_str(), speed.c_str());
		result = buf;
		break;
	}
	case 0x0D: {
		std::string obj = formatValue();
		std::string x = formatValue();
		std::string y = formatValue();
		std::string side = formatValue();
		uint16_t strOffset = readWord();
		uint16_t numLines = readWord();
		std::string decoded = decodeString(strOffset, numLines);
		if (!decoded.empty())
			snprintf(buf, sizeof(buf), " obj=%s pos=(%s,%s) side=%s str=%u lines=%u \"%s\"",
					 obj.c_str(), x.c_str(), y.c_str(), side.c_str(), strOffset, numLines, decoded.c_str());
		else
			snprintf(buf, sizeof(buf), " obj=%s pos=(%s,%s) side=%s str=%u lines=%u",
					 obj.c_str(), x.c_str(), y.c_str(), side.c_str(), strOffset, numLines);
		result = buf;
		break;
	}
	case 0x0F:
	case 0x31:
	case 0x3C:
	case 0x3D: {
		std::string val = formatValue();
		result = " " + val;
		break;
	}
	case 0x10: {
		std::string obj = formatValue();
		std::string x = formatValue();
		std::string y = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s pos=(%s,%s)", obj.c_str(), x.c_str(), y.c_str());
		result = buf;
		break;
	}
	case 0x11:
	case 0x47: {
		std::string obj = formatValue();
		result = " obj=" + obj;
		break;
	}
	case 0x12: {
		std::string area = formatValue();
		std::string active = formatValue();
		std::string val = formatValue();
		snprintf(buf, sizeof(buf), " area=%s active=%s val=%s", area.c_str(), active.c_str(), val.c_str());
		result = buf;
		break;
	}
	case 0x14: {
		uint16_t w = readWord();
		snprintf(buf, sizeof(buf), " 0x%04x", w);
		result = buf;
		break;
	}
	case 0x16: {
		std::string idx = formatValue();
		uint16_t strOffset = readWord();
		uint16_t numLines = readWord();
		std::string decoded = decodeString(strOffset, numLines);
		if (!decoded.empty())
			snprintf(buf, sizeof(buf), " idx=%s str=%u lines=%u \"%s\"", idx.c_str(), strOffset, numLines, decoded.c_str());
		else
			snprintf(buf, sizeof(buf), " idx=%s str=%u lines=%u", idx.c_str(), strOffset, numLines);
		result = buf;
		break;
	}
	case 0x17: {
		std::string obj = formatValue();
		std::string x = formatValue();
		std::string y = formatValue();
		std::string side = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s pos=(%s,%s) side=%s", obj.c_str(), x.c_str(), y.c_str(), side.c_str());
		result = buf;
		break;
	}
	case 0x19: {
		std::string actor = formatValue();
		std::string obj = formatValue();
		snprintf(buf, sizeof(buf), " actor=%s obj=%s", actor.c_str(), obj.c_str());
		result = buf;
		break;
	}
	case 0x1A: {
		std::string obj = formatValue();
		std::string start = formatValue();
		std::string end = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s start=%s end=%s", obj.c_str(), start.c_str(), end.c_str());
		result = buf;
		break;
	}
	case 0x1B: {
		std::string obj = formatValue();
		std::string slot = formatValue();
		std::string speed = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s slot=%s speed=%s", obj.c_str(), slot.c_str(), speed.c_str());
		result = buf;
		break;
	}
	case 0x1E: {
		std::string obj = formatValue();
		std::string slot = formatValue();
		std::string frame = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s slot=%s frame=%s", obj.c_str(), slot.c_str(), frame.c_str());
		result = buf;
		break;
	}
	case 0x1F: {
		std::string obj = formatValue();
		std::string x = formatValue();
		std::string y = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s pos=(%s,%s)", obj.c_str(), x.c_str(), y.c_str());
		result = buf;
		break;
	}
	case 0x20: {
		std::string obj = formatValue();
		std::string offset = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s offset=%s", obj.c_str(), offset.c_str());
		result = buf;
		break;
	}
	case 0x21: {
		std::string obj = formatValue();
		std::string target = formatValue();
		std::string delta = formatValue();
		std::string dist = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s target=%s delta=%s dist=%s", obj.c_str(), target.c_str(), delta.c_str(), dist.c_str());
		result = buf;
		break;
	}
	case 0x22: {
		std::string obj = formatValue();
		std::string anim = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s anim=%s", obj.c_str(), anim.c_str());
		result = buf;
		break;
	}
	case 0x23: {
		std::string obj = formatValue();
		std::string x = formatValue();
		std::string y = formatValue();
		std::string voff = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s pos=(%s,%s) voff=%s", obj.c_str(), x.c_str(), y.c_str(), voff.c_str());
		result = buf;
		break;
	}
	case 0x24:
	case 0x25: {
		std::string a = formatValue();
		std::string b = formatValue();
		const char *op = (opcode == 0x24) ? "+" : "-";
		snprintf(buf, sizeof(buf), " %s = %s %s %s", a.c_str(), a.c_str(), op, b.c_str());
		result = buf;
		break;
	}
	case 0x26: {
		std::string obj = formatValue();
		std::string decodeFlag = formatValue();
		uint8_t animIdx = readByte();
		snprintf(buf, sizeof(buf), " obj=%s decode=%s anim=%u", obj.c_str(), decodeFlag.c_str(), animIdx);
		result = buf;
		break;
	}
	case 0x27: {
		std::string obj = formatValue();
		std::string maxFrame = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s maxFrame=%s", obj.c_str(), maxFrame.c_str());
		result = buf;
		break;
	}
	case 0x29: {
		std::string obj = formatValue();
		result = " obj=" + obj;
		break;
	}
	case 0x2A: {
		std::string obj = formatValue();
		std::string slot = formatValue();
		std::string decode = formatValue();
		uint8_t idx = readByte();
		snprintf(buf, sizeof(buf), " obj=%s slot=%s decode=%s idx=%u", obj.c_str(), slot.c_str(), decode.c_str(), idx);
		result = buf;
		break;
	}
	case 0x2B: {
		std::string obj = formatValue();
		result = " obj=" + obj;
		break;
	}
	case 0x2C:
	case 0x32:
	case 0x33:
	case 0x2D: {
		std::string obj = formatValue();
		std::string val = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s val=%s", obj.c_str(), val.c_str());
		result = buf;
		break;
	}
	case 0x2E: {
		std::string anim = formatValue();
		std::string lo = formatValue();
		std::string hi = formatValue();
		snprintf(buf, sizeof(buf), " anim=%s range=[%s,%s]", anim.c_str(), lo.c_str(), hi.c_str());
		result = buf;
		break;
	}
	case 0x2F: {
		std::string obj = formatValue();
		std::string slot = formatValue();
		std::string lo = formatValue();
		std::string hi = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s slot=%s range=[%s,%s]", obj.c_str(), slot.c_str(), lo.c_str(), hi.c_str());
		result = buf;
		break;
	}
	case 0x34:
	case 0x4D: {
		std::string a = formatValue();
		std::string b = formatValue();
		snprintf(buf, sizeof(buf), " %s -> %s", a.c_str(), b.c_str());
		result = buf;
		break;
	}
	case 0x35: {
		std::string obj = formatValue();
		std::string parent = formatValue();
		std::string a = formatValue();
		std::string b = formatValue();
		std::string c = formatValue();
		snprintf(buf, sizeof(buf), " obj=%s parent=%s (%s,%s,%s)", obj.c_str(), parent.c_str(), a.c_str(), b.c_str(), c.c_str());
		result = buf;
		break;
	}
	case 0x38:
	case 0x3E: {
		uint8_t resIdx = readByte();
		snprintf(buf, sizeof(buf), " res=%u", resIdx);
		result = buf;
		break;
	}
	case 0x3A: {
		std::string x = formatValue();
		std::string y = formatValue();
		std::string align = formatValue();
		uint16_t strOffset = readWord();
		uint16_t entryType = readWord();
		snprintf(buf, sizeof(buf), " pos=(%s,%s) align=%s str=%u type=%u", x.c_str(), y.c_str(), align.c_str(), strOffset, entryType);
		result = buf;
		break;
	}
	case 0x43: {
		std::string slot = formatValue();
		uint8_t resIdx = readByte();
		snprintf(buf, sizeof(buf), " slot=%s res=%u", slot.c_str(), resIdx);
		result = buf;
		break;
	}
	case 0x44:
	case 0x45: {
		std::string slot = formatValue();
		std::string a = formatValue();
		std::string b = formatValue();
		snprintf(buf, sizeof(buf), " slot=%s %s %s", slot.c_str(), a.c_str(), b.c_str());
		result = buf;
		break;
	}
	case 0x46: {
		std::string slot = formatValue();
		result = " slot=" + slot;
		break;
	}
	case 0x48:
	case 0x49:
	case 0x4A:
	case 0x4B: {
		std::string obj = formatValue();
		if (pos + 3 <= endPos) {
			readByte();
			uint16_t varIdx = readWord();
			snprintf(buf, sizeof(buf), " var[%u] = obj=%s", varIdx, obj.c_str());
			result = buf;
		} else {
			result = " obj=" + obj;
		}
		break;
	}
	case 0x07:
	case 0x09:
	case 0x0E:
	case 0x13:
	case 0x15:
	case 0x18:
	case 0x1C:
	case 0x1D:
	case 0x28:
	case 0x36:
	case 0x37:
	case 0x39:
	case 0x3B:
	case 0x3F:
	case 0x40:
	case 0x41:
	case 0x42:
	case 0x4C:
	case 0x4E:
		break;
	default: {
		uint8_t length = (uint8_t)(endPos - pos);
		if (length > 0 && length <= 30) {
			result = " [";
			uint32_t dataPos = pos;
			for (uint8_t i = 0; i < length && dataPos < scriptSize; i++, dataPos++) {
				if (i)
					result += " ";
				snprintf(buf, sizeof(buf), "%02x", scriptData[dataPos]);
				result += buf;
			}
			result += "]";
		}
		break;
	}
	}
	return result;
}

static std::vector<DecompiledLine> decompileToLines() {
	std::vector<DecompiledLine> lines;
	int indent = 0;

	while (pos < scriptSize - 1) {
		uint32_t instrAddr = pos;
		uint8_t opcode = readByte();
		if (opcode == 0x00)
			continue;

		uint8_t length = readByte();
		uint32_t endPos = pos + length;

		if (opcode == 0x07 && indent > 0)
			indent--;
		if (opcode == 0x08 && indent > 0)
			indent--;

		DecompiledLine line;
		line.offset = instrAddr;
		line.opcode = opcode;
		line.indent = indent;
		uint32_t argsStart = pos;
		line.params = decodeParams(opcode, endPos, indent);
		uint32_t argsEnd = pos;
		line.args.assign(scriptData + argsStart, scriptData + argsEnd);
		lines.push_back(line);

		if (pos != endPos)
			pos = endPos;
	}
	return lines;
}

static void disassemble() {
	auto lines = decompileToLines();
	for (const auto &l : lines) {
		for (int i = 0; i < l.indent; i++)
			printf("  ");
		printf("%04x: %s%s\n", l.offset, getOpcodeName(l.opcode), l.params.c_str());
	}
}

static std::string jsonEscape(const std::string &s) {
	std::string out;
	for (char c : s) {
		switch (c) {
		case '"':
			out += "\\\"";
			break;
		case '\\':
			out += "\\\\";
			break;
		case '\n':
			out += "\\n";
			break;
		case '\t':
			out += "\\t";
			break;
		default:
			out += c;
			break;
		}
	}
	return out;
}

static void disassembleJson(int index, const char *type = "scene") {
	auto lines = decompileToLines();
	printf("  {\"type\": \"%s\", \"index\": %d, \"size\": %u, \"instructions\": [\n", type, index, scriptSize);
	for (size_t i = 0; i < lines.size(); i++) {
		const auto &l = lines[i];
		std::string text = std::string(getOpcodeName(l.opcode)) + l.params;
		printf("    {\"offset\": %u, \"opcode\": %u, \"name\": \"%s\", \"indent\": %d, \"text\": \"%s\", \"args\": [",
			   l.offset, l.opcode, jsonEscape(getOpcodeName(l.opcode)).c_str(),
			   l.indent, jsonEscape(text).c_str());
		for (size_t j = 0; j < l.args.size(); j++) {
			if (j)
				printf(",");
			printf("%u", l.args[j]);
		}
		printf("]}%s\n", (i + 1 < lines.size()) ? "," : "");
	}
	printf("  ]}");
}

static void printHelp(const char *bin) {
	printf("MACS2 Script Decompiler\n\n");
	printf("Usage: %s [--json] [--cpp] [--objects] <game_data> [scene_index]\n\n", bin);
	printf("  --json          - Output as JSON\n");
	printf("  --cpp           - Output as C++ source (navigable via IDE)\n");
	printf("  --objects       - Also decompile object scripts for the scene(s)\n");
	printf("                    Objects with scene=0 (inventory/global) are included\n");
	printf("  game_data       - RESOURCE.MCS file (DOS) or directory with DataA+Mdir (Amiga)\n");
	printf("  scene_index     - Scene number to decompile (1-based)\n");
	printf("                    If omitted, decompiles all scenes\n\n");
	printf("The decompiled script will be written to stdout.\n");
}

static bool loadSceneScript(FILE *f, uint16_t sceneIndex) {
	uint32_t offset = (uint32_t)sceneIndex * 0xC + 0xC + 0x4 - 0x8;
	fseek(f, offset, SEEK_SET);

	uint32_t sceneDataOffset;
	if (fread(&sceneDataOffset, 4, 1, f) != 1)
		return false;

	if (sceneDataOffset == 0)
		return false;

	fseek(f, sceneDataOffset + 0x80, SEEK_SET);

	uint16_t size;
	if (fread(&size, 2, 1, f) != 1)
		return false;

	if (size == 0)
		return false;

	scriptData = (uint8_t *)malloc(size);
	if (!scriptData)
		return false;

	if (fread(scriptData, 1, size, f) != size) {
		free(scriptData);
		scriptData = nullptr;
		return false;
	}

	scriptSize = size;
	pos = 0;

	// Load scene strings
	uint32_t strTableOffset = (uint32_t)sceneIndex * 0xC + 0xC + 0x4 - 0x4;
	fseek(f, strTableOffset, SEEK_SET);
	uint32_t strDataOffset;
	if (fread(&strDataOffset, 4, 1, f) == 1 && strDataOffset != 0) {
		fseek(f, strDataOffset, SEEK_SET);
		uint16_t strSize;
		if (fread(&strSize, 2, 1, f) == 1 && strSize > 0) {
			stringData = (uint8_t *)malloc(strSize);
			if (stringData) {
				if (fread(stringData, 1, strSize, f) == strSize)
					stringDataSize = strSize;
				else {
					free(stringData);
					stringData = nullptr;
				}
			}
		}
	}

	return true;
}

// Returns the scene index for an object, or 0 if the object doesn't exist
static uint16_t getObjectSceneIndex(FILE *f, uint16_t objectIndex) {
	// Object data table: offset at 0x17F4 + 0xC + 0x4 + objectIndex * 0xC
	uint32_t addressOffset = 0x17F4 + (0xC + 0x04) + (uint32_t)objectIndex * 0xC;
	fseek(f, addressOffset, SEEK_SET);
	uint32_t objectOffset;
	if (fread(&objectOffset, 4, 1, f) != 1 || objectOffset == 0)
		return 0;
	// sceneIndex is at +4 in the object data (after x:u16, y:u16)
	fseek(f, objectOffset + 4, SEEK_SET);
	uint16_t sceneIndex;
	if (fread(&sceneIndex, 2, 1, f) != 1)
		return 0;
	return sceneIndex;
}

static bool loadObjectScript(FILE *f, uint16_t objectIndex) {
	// Object script table: each entry is 12 bytes, script offset is at +4
	uint32_t addressOffset = 0x17F8 + (0xC + 0x04) + (uint32_t)objectIndex * 0xC;
	fseek(f, addressOffset, SEEK_SET);

	uint32_t objectOffset;
	if (fread(&objectOffset, 4, 1, f) != 1)
		return false;

	if (objectOffset == 0)
		return false;

	fseek(f, objectOffset, SEEK_SET);

	// Skip 32 resource offsets (128 bytes)
	fseek(f, 128, SEEK_CUR);

	uint16_t size;
	if (fread(&size, 2, 1, f) != 1)
		return false;

	if (size == 0)
		return false;

	scriptData = (uint8_t *)malloc(size);
	if (!scriptData)
		return false;

	if (fread(scriptData, 1, size, f) != size) {
		free(scriptData);
		scriptData = nullptr;
		return false;
	}

	scriptSize = size;
	pos = 0;

	// Load object strings
	uint32_t strTableOffset = (uint32_t)objectIndex * 0xC + 0xC + 0x4 + 0x17FC;
	fseek(f, strTableOffset, SEEK_SET);
	uint32_t strDataOffset;
	if (fread(&strDataOffset, 4, 1, f) == 1 && strDataOffset != 0) {
		fseek(f, strDataOffset, SEEK_SET);
		uint16_t strSize;
		if (fread(&strSize, 2, 1, f) == 1 && strSize > 0) {
			stringData = (uint8_t *)malloc(strSize);
			if (stringData) {
				if (fread(stringData, 1, strSize, f) == strSize)
					stringDataSize = strSize;
				else {
					free(stringData);
					stringData = nullptr;
				}
			}
		}
	}

	return true;
}

// ============================================================================
// Amiga PP20 decompression + MXOO resource loading
// ============================================================================

#ifndef _WIN32
#include <dirent.h>
#include <sys/stat.h>
#endif

static bool pp20Decompress(const uint8_t *src, uint32_t srcLen, std::vector<uint8_t> &dst) {
	if (srcLen < 12 || memcmp(src, "PP20", 4) != 0)
		return false;
	const uint8_t *eff = src + 4;
	const uint8_t *packed = src + 8;
	uint32_t packedLen = srcLen - 12;
	const uint8_t *trailer = src + srcLen - 4;
	uint32_t origSize = ((uint32_t)trailer[0] << 16) | ((uint32_t)trailer[1] << 8) | trailer[2];
	uint8_t bitrot = trailer[3];
	if (origSize == 0)
		return false;
	dst.resize(origSize);

	uint8_t rev[256];
	for (int a = 0; a < 256; a++) {
		uint8_t b = (uint8_t)a;
		b = (uint8_t)(((b & 0x0f) << 4) | ((b >> 4) & 0x0f));
		b = (uint8_t)(((b & 0x33) << 2) | ((b >> 2) & 0x33));
		b = (uint8_t)(((b & 0x55) << 1) | ((b >> 1) & 0x55));
		rev[a] = b;
	}

	uint32_t outPos = origSize;
	int32_t inPos = (int32_t)packedLen;
	uint32_t code = 0, shift = 32;

#define PP_PEEK(x)                                     \
	while (shift > 32u - (x)) {                        \
		if (inPos <= 0)                                \
			goto done;                                 \
		shift -= 8;                                    \
		inPos--;                                       \
		code += (uint32_t)rev[packed[inPos]] << shift; \
	}
#define PP_SHIFT(x)                        \
	do {                                   \
		shift += (x);                      \
		code = (code << (x)) & 0xFFFFFFFF; \
	} while (0)

	PP_PEEK(bitrot);
	PP_SHIFT(bitrot);
	while (outPos > 0) {
		PP_PEEK(1);
		uint32_t bit = code >> 31;
		PP_SHIFT(1);
		if (bit == 0) {
			PP_PEEK(2);
			uint32_t length = (code >> 30) + 1;
			PP_SHIFT(2);
			if (length == 4) {
				for (;;) {
					PP_PEEK(2);
					uint32_t b2 = code >> 30;
					PP_SHIFT(2);
					length += b2;
					if (b2 != 3)
						break;
				}
			}
			for (uint32_t i = 0; i < length && outPos > 0; i++) {
				PP_PEEK(8);
				outPos--;
				dst[outPos] = (uint8_t)(code >> 24);
				PP_SHIFT(8);
			}
			if (outPos == 0)
				break;
		}
		PP_PEEK(2);
		uint32_t cv = code >> 30;
		PP_SHIFT(2);
		uint32_t length, nbits;
		if (cv == 0) {
			length = 2;
			nbits = eff[0];
		} else if (cv == 1) {
			length = 3;
			nbits = eff[1];
		} else if (cv == 2) {
			length = 4;
			nbits = eff[2];
		} else {
			PP_PEEK(1);
			uint32_t extra = code >> 31;
			PP_SHIFT(1);
			length = 5;
			nbits = (extra == 0) ? 7 : eff[3];
		}
		PP_PEEK(nbits);
		uint32_t ptr = (code >> (32 - nbits)) + 1;
		PP_SHIFT(nbits);
		if (length == 5) {
			for (;;) {
				PP_PEEK(3);
				uint32_t b3 = code >> 29;
				PP_SHIFT(3);
				length += b3;
				if (b3 != 7)
					break;
			}
		}
		for (uint32_t i = 0; i < length && outPos > 0; i++) {
			outPos--;
			dst[outPos] = dst[outPos + ptr];
		}
	}
done:
#undef PP_PEEK
#undef PP_SHIFT
	return outPos == 0;
}

static uint32_t readU32BE(const uint8_t *p) { return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) | ((uint32_t)p[2] << 8) | p[3]; }
static uint16_t readU16BE(const uint8_t *p) { return ((uint16_t)p[0] << 8) | p[1]; }

static bool isAmigaDir(const char *path) {
#ifndef _WIN32
	struct stat st;
	if (stat(path, &st) != 0 || !S_ISDIR(st.st_mode))
		return false;
	std::string da = std::string(path) + "/DataA";
	std::string md = std::string(path) + "/Mdir";
	return stat(da.c_str(), &st) == 0 && stat(md.c_str(), &st) == 0;
#else
	return false;
#endif
}

// Load script+strings from an MXOO resource blob (decompressed PP20 data)
static bool loadAmigaMXOO(const uint8_t *data, uint32_t dataLen) {
	if (dataLen < 12 || memcmp(data, "MXOO", 4) != 0)
		return false;

	uint32_t scriptOff = readU32BE(data + 4);
	uint32_t stringOff = readU32BE(data + 8);

	// Load script: at scriptOff there's padding(2) + size_BE(2) + bytecode
	if (scriptOff + 4 > dataLen)
		return false;
	uint16_t sz = readU16BE(data + scriptOff + 2);
	if (sz == 0 || scriptOff + 4 + sz > dataLen)
		return false;

	scriptData = (uint8_t *)malloc(sz);
	memcpy(scriptData, data + scriptOff + 4, sz);
	scriptSize = sz;
	pos = 0;

	// Load strings: at stringOff there's padding(2) + totalSize_BE(2) + string entries
	if (stringOff + 4 <= dataLen) {
		uint16_t strSz = readU16BE(data + stringOff + 2);
		if (strSz > 0 && stringOff + 4 + strSz <= dataLen) {
			// Convert Amiga string format (u16BE length + plaintext) to DOS format (u16LE length + encrypted)
			// Since the decompiler's decodeString expects DOS encrypted format,
			// we store raw and override string decoding for Amiga.
			// Actually: store the raw Amiga string block and handle in decode.
			stringData = (uint8_t *)malloc(strSz);
			memcpy(stringData, data + stringOff + 4, strSz);
			stringDataSize = strSz;
		}
	}
	return true;
}

// Override string decode for Amiga (plaintext with u16BE lengths)

static std::string decodeStringAmiga(uint32_t offset, uint16_t numLines) {
	if (!stringData || offset >= stringDataSize)
		return "";
	uint32_t p = offset;
	std::string result;
	for (uint16_t line = 0; line < numLines && p + 2 <= stringDataSize; line++) {
		uint16_t length = readU16BE(stringData + p);
		p += 2;
		if (length == 0 || p + length > stringDataSize)
			break;
		for (uint16_t i = 0; i < length && p < stringDataSize; i++, p++) {
			appendUtf8(result, stringData[p]);
		}
		if (line + 1 < numLines)
			result += " / ";
	}
	return result;
}

static int runAmiga(const char *dir, bool jsonMode, bool objectsMode) {
	std::string dataAPath = std::string(dir) + "/DataA";

	FILE *df = fopen(dataAPath.c_str(), "rb");
	if (!df) {
		fprintf(stderr, "Cannot open %s\n", dataAPath.c_str());
		return 1;
	}
	fseek(df, 0, SEEK_END);
	uint32_t fileSize = (uint32_t)ftell(df);
	fseek(df, 0, SEEK_SET);

	// Read MXMF header
	uint8_t hdr[14];
	fread(hdr, 1, 14, df);
	if (memcmp(hdr, "MXMF", 4) != 0) {
		fprintf(stderr, "Not MXMF\n");
		fclose(df);
		return 1;
	}
	uint16_t totalRes = readU16BE(hdr + 8);
	uint32_t firstBlockSize = readU32BE(hdr + 10);

	amigaMode = true;

	if (!jsonMode) {
		printf("; MACS2 Amiga Script Decompiler\n");
		printf("; Scripts are byte-identical to DOS version\n");
		printf("; Strings are plaintext (not encrypted)\n;\n\n");
	} else {
		printf("{\"resources\": [\n");
	}

	// Iterate all resources sequentially
	fseek(df, 14 + firstBlockSize, SEEK_SET);
	bool first = true;
	int resIdx = 0;
	for (uint16_t i = 0; i < totalRes - 1; i++) {
		uint8_t ehdr[8];
		if (fread(ehdr, 1, 8, df) != 8)
			break;
		char eType[3] = {(char)ehdr[0], (char)ehdr[1], '\0'};
		uint16_t eId = readU16BE(ehdr + 2);
		uint32_t eCompSize = readU32BE(ehdr + 4);
		if (eCompSize == 0 || eCompSize > fileSize)
			break;

		std::vector<uint8_t> payload(eCompSize);
		if (fread(payload.data(), 1, eCompSize, df) != eCompSize)
			break;

		// Only process OO (object) resources that contain PP20
		if (memcmp(payload.data(), "PP20", 4) != 0)
			continue;
		// Skip non-OO types (MM=music, OS=sound, FF=font)
		if (strcmp(eType, "OO") != 0 && strcmp(eType, "FF") != 0)
			continue;

		std::vector<uint8_t> dec;
		if (!pp20Decompress(payload.data(), eCompSize, dec))
			continue;
		if (dec.size() < 12 || memcmp(dec.data(), "MXOO", 4) != 0)
			continue;

		// Check if this MXOO has a script (scriptOff != 0 and has valid script size)
		uint32_t scriptOff = readU32BE(dec.data() + 4);
		if (scriptOff + 4 > dec.size())
			continue;
		uint16_t sz = readU16BE(dec.data() + scriptOff + 2);
		if (sz == 0)
			continue;

		if (loadAmigaMXOO(dec.data(), (uint32_t)dec.size())) {
			if (jsonMode) {
				if (!first)
					printf(",\n");
				first = false;
				disassembleJson(eId, eType);
			} else {
				printf("=== %s_%04d (size: %u bytes) ===\n", eType, eId, scriptSize);
				disassemble();
				printf("\n");
			}
			free(scriptData);
			scriptData = nullptr;
			scriptSize = 0;
			free(stringData);
			stringData = nullptr;
			stringDataSize = 0;
		}
		resIdx++;
	}

	if (jsonMode)
		printf("\n]}\n");
	fclose(df);
	return 0;
}

// ============================================================================
// C source output mode
// ============================================================================

#include <set>

static std::set<uint16_t> usedObjects;
static std::set<uint16_t> usedVars;
static uint16_t maxVarIdx = 0;

static const char *getVarName(uint16_t idx) {
	switch (idx) {
	case 3:
		return "VAR_MUSIC_SLOT";
	case 10:
		return "VAR_KNIFE_FIGHT_DONE";
	case 11:
		return "VAR_CHAPTER";
	case 12:
		return "VAR_SCENE_PROGRESS";
	case 13:
		return "VAR_TEMP_X";
	case 14:
		return "VAR_BGANIM_TOGGLED";
	case 15:
		return "VAR_TALKED_TO_NPC";
	case 16:
		return "VAR_HAS_ITEM";
	case 18:
		return "VAR_HAS_PLOT_ITEM";
	case 19:
		return "VAR_MET_CHARACTER";
	case 22:
		return "VAR_CUTSCENE_TRIGGERED";
	case 51:
		return "VAR_KNIFE_SEPARATED";
	case 263:
		return "VAR_DIALOGUE_CHOICE";
	default:
		return nullptr;
	}
}

static std::string formatVarAccess(uint16_t idx) {
	const char *name = getVarName(idx);
	if (name)
		return std::string("_vars[") + name + "]";
	char buf[32];
	snprintf(buf, sizeof(buf), "_vars[%u]", idx);
	return buf;
}

static const char *getObjectName(uint16_t idx) {
	// Full game object names from Macs2::GameObjects::init()
	// Characters/NPCs
	switch (idx) {
	case 1:
		return "OBJ_PLAYER";
	case 2:
		return "OBJ_CAPTAIN";
	case 3:
		return "OBJ_AUNT";
	case 6:
		return "OBJ_CORNEL";
	case 7:
		return "OBJ_BEAR";
	case 9:
		return "OBJ_WOLF";
	case 12:
		return "OBJ_INDIAN";
	case 13:
		return "OBJ_PATTERSON";
	case 15:
		return "OBJ_ROLLINS";
	case 19:
		return "OBJ_HILTON";
	// Items (full game, indices from _objectNames)
	case 0x08:
		return "OBJ_BOARD";
	case 0x0E:
		return "OBJ_BUCKET_WATER";
	case 0x10:
		return "OBJ_BARREL";
	case 0x11:
		return "OBJ_BOWIE_KNIFE";
	case 0x14:
		return "OBJ_BUCKET_WATER2";
	case 0x17:
		return "OBJ_HATBOX_OPEN";
	case 0x18:
		return "OBJ_HAT";
	case 0x19:
		return "OBJ_HATBOX_CLOSED";
	case 0x1A:
		return "OBJ_METAL_BUCKET";
	case 0x1B:
		return "OBJ_FIRE_POKER";
	case 0x1C:
		return "OBJ_POT_HOLDER";
	case 0x1D:
		return "OBJ_BOARD_WRAPPED";
	case 0x1E:
		return "OBJ_COAL_SHOVEL";
	case 0x20:
		return "OBJ_COCKROACH";
	case 0x22:
		return "OBJ_CUP_FULL";
	case 0x23:
		return "OBJ_CUP_EMPTY";
	case 0x24:
		return "OBJ_AXE";
	case 0x25:
		return "OBJ_AXE_BLADE";
	case 0x26:
		return "OBJ_AXE_HANDLE";
	case 0x28:
		return "OBJ_LETTER";
	case 0x29:
		return "OBJ_BREAD_MOLDY";
	case 0x2A:
		return "OBJ_BREAD_STALE";
	case 0x2B:
		return "OBJ_ENVELOPE_OPEN";
	case 0x2C:
		return "OBJ_ENVELOPE_SEALED";
	case 0x2D:
		return "OBJ_RACCOON_FUR";
	case 0x2E:
		return "OBJ_WHISKY_GLASS";
	case 0x2F:
		return "OBJ_LEATHER_BELT";
	case 0x30:
		return "OBJ_POKER";
	case 0x34:
		return "OBJ_BOARD_SOLID";
	case 0x36:
		return "OBJ_BIRDCAGE_BIRD";
	case 0x37:
		return "OBJ_BIRDCAGE_BROKEN";
	case 0x38:
		return "OBJ_BIRDCAGE_EMPTY";
	case 0x3A:
		return "OBJ_MAP";
	case 0x3B:
		return "OBJ_CANDLE";
	case 0x3C:
		return "OBJ_PEBBLES";
	case 0x3D:
		return "OBJ_SUITCASE_DYNAMITE";
	case 0x3E:
		return "OBJ_BANKNOTES";
	case 0x3F:
		return "OBJ_LEATHER_POUCH";
	case 0x40:
		return "OBJ_SLINGSHOT";
	case 0x41:
		return "OBJ_FIRECRACKERS";
	case 0x42:
		return "OBJ_SUITCASE_OPEN";
	case 0x43:
		return "OBJ_SUITCASE_CLOSED";
	case 0x44:
		return "OBJ_PAPER";
	case 0x47:
		return "OBJ_KNIFE_RUSTY";
	case 0x4B:
		return "OBJ_REEDS_DRY";
	case 0x4C:
		return "OBJ_REEDS";
	case 0x4F:
		return "OBJ_LIQUOR_BOTTLE";
	case 0x51:
		return "OBJ_CLOTH_BAG";
	case 0x52:
		return "OBJ_CLOTH_BAG_MARBLES";
	case 0x53:
		return "OBJ_BELLOWS";
	case 0x54:
		return "OBJ_FIREWOOD";
	case 0x55:
		return "OBJ_BREAD_SLICED";
	case 0x56:
		return "OBJ_BREAD_KNIFE";
	case 0x62:
		return "OBJ_PICKAXE";
	case 0x6B:
		return "OBJ_CORKSCREW";
	case 0x6F:
		return "OBJ_MARBLES";
	case 0x70:
		return "OBJ_MUSKETS";
	case 0x71:
		return "OBJ_WOODEN_STAKE";
	case 0x74:
		return "OBJ_SHOVEL";
	case 0x78:
		return "OBJ_BRASS_KEY";
	case 0x7E:
		return "OBJ_GUNPOWDER";
	case 0x7F:
		return "OBJ_SULPHUR";
	case 0x80:
		return "OBJ_HEMP_ROPE";
	case 0x82:
		return "OBJ_ROPES_TIED";
	case 0x84:
		return "OBJ_SPATULA";
	case 0x86:
		return "OBJ_MATCH";
	case 0x94:
		return "OBJ_MINE_CART";
	case 0x99:
		return "OBJ_CROWBAR";
	case 0x9B:
		return "OBJ_DYNAMITE";
	case 0x9C:
		return "OBJ_TORCH_LIT";
	case 0x9D:
		return "OBJ_TORCH";
	case 0xA1:
		return "OBJ_PULLEY";
	case 0xA2:
		return "OBJ_HOOK";
	case 0xA3:
		return "OBJ_HOOK_ROPE";
	case 0xAE:
		return "OBJ_HEMP_ROPE2";
	case 0xB2:
		return "OBJ_SCREWDRIVER";
	case 0xB3:
		return "OBJ_IRON_BAR";
	case 0xB4:
		return "OBJ_TOMAHAWK";
	default:
		return nullptr;
	}
}

static std::string formatValueC() {
	uint8_t type = readByte();
	uint16_t val = readWord();
	char buf[64];
	if (type == 0x00) {
		if (val > 0x400 && val <= 0x600) {
			uint16_t idx = val - 0x400;
			usedObjects.insert(idx);
			const char *name = getObjectName(idx);
			if (name)
				return name;
			snprintf(buf, sizeof(buf), "_objects[%u]", idx);
			return buf;
		}
		snprintf(buf, sizeof(buf), "%d", (int)(int16_t)val);
		return buf;
	} else if (type == 0xFF) {
		const char *name = getSpecialName(val);
		if (name)
			return std::string("_rt.") + name;
		if (val >= 0x0E && val <= 0x22) {
			snprintf(buf, sizeof(buf), "%u", val - 0x0D);
			return buf;
		}
		switch (val) {
		case 0x06:
			return "TRUE";
		case 0x07:
			return "FALSE";
		case 0x08:
			return "FADE_CUT";
		case 0x09:
			return "SIDE_LEFT";
		case 0x0A:
			return "SIDE_RIGHT";
		case 0x0C:
			return "LOOP";
		case 0x2E:
			return "ALIGN_CENTER";
		}
		snprintf(buf, sizeof(buf), "_rt.special[0x%02x]", val);
		return buf;
	}
	usedVars.insert(val);
	if (val > maxVarIdx)
		maxVarIdx = val;
	return formatVarAccess(val);
}

static std::string decodeCLine(uint8_t opcode, uint32_t endPos, int &indent) {
	char buf[1024];
	switch (opcode) {
	case 0x01: {
		readByte();
		uint16_t v = readWord();
		usedVars.insert(v);
		if (v > maxVarIdx)
			maxVarIdx = v;
		std::string val = formatValueC();
		std::string dest = formatVarAccess(v);
		snprintf(buf, sizeof(buf), "%s = %s;", dest.c_str(), val.c_str());
		return buf;
	}
	case 0x02: {
		readByte();
		uint16_t v = readWord();
		usedVars.insert(v);
		if (v > maxVarIdx)
			maxVarIdx = v;
		std::string val = formatValueC();
		std::string dest = formatVarAccess(v);
		snprintf(buf, sizeof(buf), "%s |= %s;", dest.c_str(), val.c_str());
		return buf;
	}
	case 0x03: {
		std::string val = formatValueC();
		snprintf(buf, sizeof(buf), "if (!%s) {", val.c_str());
		indent++;
		return buf;
	}
	case 0x04: {
		std::string val = formatValueC();
		snprintf(buf, sizeof(buf), "if (%s) {", val.c_str());
		indent++;
		return buf;
	}
	case 0x05: {
		uint8_t op = readByte();
		std::string a = formatValueC();
		std::string b = formatValueC();
		snprintf(buf, sizeof(buf), "if (%s %s %s) {", a.c_str(), cmpOpName(op), b.c_str());
		indent++;
		return buf;
	}
	case 0x06: {
		uint8_t sub = readByte();
		std::string i = formatValueC();
		std::string a = formatValueC();
		std::string b = formatValueC();
		snprintf(buf, sizeof(buf), "if (%sifInteraction(%s, %s, %s)) {", sub == 2 ? "!" : "", i.c_str(), a.c_str(), b.c_str());
		indent++;
		return buf;
	}
	case 0x07:
		return "}";
	case 0x08:
		indent++;
		return "} else {";
	case 0x0A:
	case 0x30: {
		std::string x = formatValueC();
		std::string y = formatValueC();
		uint16_t s = readWord();
		uint16_t n = readWord();
		std::string decoded = decodeString(s, n);
		std::string result2 = std::string(opcode == 0x0A ? "printStringLeft(" : "printStringRight(") + x + ", " + y + ", " + std::to_string(s) + ", " + std::to_string(n) + ");";
		if (!decoded.empty())
			result2 += " // \"" + decoded + "\"";
		return result2;
	}
	case 0x0B: {
		std::string o = formatValueC();
		std::string s = formatValueC();
		std::string x = formatValueC();
		std::string y = formatValueC();
		if (s == "0" && x == "0" && y == "0")
			snprintf(buf, sizeof(buf), "removeFromScene(%s);", o.c_str());
		else
			snprintf(buf, sizeof(buf), "moveObject(%s, %s, %s, %s);", o.c_str(), s.c_str(), x.c_str(), y.c_str());
		return buf;
	}
	case 0x0C: {
		std::string s = formatValueC();
		std::string m = formatValueC();
		std::string sp = formatValueC();
		snprintf(buf, sizeof(buf), "changeScene(%s, %s, %s);", s.c_str(), m.c_str(), sp.c_str());
		return buf;
	}
	case 0x0D: {
		std::string o = formatValueC();
		std::string x = formatValueC();
		std::string y = formatValueC();
		std::string s = formatValueC();
		uint16_t so = readWord();
		uint16_t n = readWord();
		std::string decoded = decodeString(so, n);
		std::string result2 = std::string("showDialogue(") + o + ", " + x + ", " + y + ", " + s + ", " + std::to_string(so) + ", " + std::to_string(n) + ");";
		if (!decoded.empty())
			result2 += " // \"" + decoded + "\"";
		return result2;
	}
	case 0x0E:
		return "changeAnimation();";
	case 0x0F: {
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "frameWait(%s);", v.c_str());
		return buf;
	}
	case 0x10: {
		std::string o = formatValueC();
		std::string x = formatValueC();
		std::string y = formatValueC();
		snprintf(buf, sizeof(buf), "walkToPosition(%s, %s, %s);", o.c_str(), x.c_str(), y.c_str());
		return buf;
	}
	case 0x11: {
		std::string o = formatValueC();
		snprintf(buf, sizeof(buf), "waitForWalk(%s);", o.c_str());
		return buf;
	}
	case 0x12: {
		std::string a = formatValueC();
		std::string b = formatValueC();
		std::string c = formatValueC();
		snprintf(buf, sizeof(buf), "setPathfinding(%s, %s, %s);", a.c_str(), b.c_str(), c.c_str());
		return buf;
	}
	case 0x13:
		return "skipUntil14();";
	case 0x14: {
		uint16_t w = readWord();
		snprintf(buf, sizeof(buf), "skipWord(0x%04x);", w);
		return buf;
	}
	case 0x15:
		return "clearDialogueChoices();";
	case 0x16: {
		std::string i = formatValueC();
		uint16_t s = readWord();
		uint16_t n = readWord();
		std::string decoded = decodeString(s, n);
		std::string result2 = std::string("addDialogueChoice(") + i + ", " + std::to_string(s) + ", " + std::to_string(n) + ");";
		if (!decoded.empty())
			result2 += " // \"" + decoded + "\"";
		return result2;
	}
	case 0x17: {
		std::string o = formatValueC();
		std::string x = formatValueC();
		std::string y = formatValueC();
		std::string s = formatValueC();
		snprintf(buf, sizeof(buf), "showDialogueChoice(%s, %s, %s, %s);", o.c_str(), x.c_str(), y.c_str(), s.c_str());
		return buf;
	}
	case 0x18:
		return "dismissPanel();";
	case 0x19: {
		std::string a = formatValueC();
		std::string o = formatValueC();
		snprintf(buf, sizeof(buf), "walkToAndPickup(%s, %s);", a.c_str(), o.c_str());
		return buf;
	}
	case 0x1A: {
		std::string o = formatValueC();
		std::string s = formatValueC();
		std::string e = formatValueC();
		snprintf(buf, sizeof(buf), "setPickupFrames(%s, %s, %s);", o.c_str(), s.c_str(), e.c_str());
		return buf;
	}
	case 0x1B: {
		std::string o = formatValueC();
		std::string s = formatValueC();
		std::string sp = formatValueC();
		snprintf(buf, sizeof(buf), "setupObject(%s, %s, %s);", o.c_str(), s.c_str(), sp.c_str());
		return buf;
	}
	case 0x1C:
		return "setSkippable();";
	case 0x1D:
		return "clearSkippable();";
	case 0x1E: {
		std::string o = formatValueC();
		std::string s = formatValueC();
		std::string f = formatValueC();
		snprintf(buf, sizeof(buf), "playAnimation(%s, %s, %s);", o.c_str(), s.c_str(), f.c_str());
		return buf;
	}
	case 0x1F: {
		std::string o = formatValueC();
		std::string x = formatValueC();
		std::string y = formatValueC();
		snprintf(buf, sizeof(buf), "_rt.pathWalkable = testPathfinding(%s, %s, %s);", o.c_str(), x.c_str(), y.c_str());
		return buf;
	}
	case 0x20: {
		std::string o = formatValueC();
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "setYOffset(%s, %s);", o.c_str(), v.c_str());
		return buf;
	}
	case 0x21: {
		std::string o = formatValueC();
		std::string t = formatValueC();
		std::string d = formatValueC();
		std::string di = formatValueC();
		snprintf(buf, sizeof(buf), "setMotion(%s, %s, %s, %s);", o.c_str(), t.c_str(), d.c_str(), di.c_str());
		return buf;
	}
	case 0x22: {
		std::string o = formatValueC();
		std::string a = formatValueC();
		// Replace numeric orientation with named constant
		static const char *orientNames[] = {nullptr, "ORIENT_WALK_N", "ORIENT_WALK_NE", "ORIENT_WALK_E", "ORIENT_WALK_SE", "ORIENT_WALK_S", "ORIENT_WALK_SW", "ORIENT_WALK_W", "ORIENT_WALK_NW", "ORIENT_STAND_N", "ORIENT_STAND_NE", "ORIENT_STAND_E", "ORIENT_STAND_SE", "ORIENT_STAND_S", "ORIENT_STAND_SW", "ORIENT_STAND_W", "ORIENT_STAND_NW", "ORIENT_PICKUP"};
		int ov = atoi(a.c_str());
		if (ov >= 1 && ov <= 17)
			a = orientNames[ov];
		snprintf(buf, sizeof(buf), "setOrientation(%s, %s);", o.c_str(), a.c_str());
		return buf;
	}
	case 0x23: {
		std::string o = formatValueC();
		std::string x = formatValueC();
		std::string y = formatValueC();
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "moveToPosition(%s, %s, %s, %s);", o.c_str(), x.c_str(), y.c_str(), v.c_str());
		return buf;
	}
	case 0x24:
	case 0x25: {
		std::string a = formatValueC();
		std::string b = formatValueC();
		snprintf(buf, sizeof(buf), "%s %s= %s;", a.c_str(), opcode == 0x24 ? "+" : "-", b.c_str());
		return buf;
	}
	case 0x26: {
		std::string o = formatValueC();
		std::string d = formatValueC();
		uint8_t i = readByte();
		snprintf(buf, sizeof(buf), "loadSpecialAnim(%s, %s, %u);", o.c_str(), d.c_str(), i);
		return buf;
	}
	case 0x27: {
		std::string o = formatValueC();
		std::string m = formatValueC();
		snprintf(buf, sizeof(buf), "setDirection(%s, %s);", o.c_str(), m.c_str());
		return buf;
	}
	case 0x28:
		return "stopAnimation();";
	case 0x29: {
		std::string o = formatValueC();
		snprintf(buf, sizeof(buf), "openInventory(%s);", o.c_str());
		return buf;
	}
	case 0x2A: {
		std::string o = formatValueC();
		std::string s = formatValueC();
		std::string d = formatValueC();
		uint8_t i = readByte();
		snprintf(buf, sizeof(buf), "loadObjectAnim(%s, %s, %s, %u);", o.c_str(), s.c_str(), d.c_str(), i);
		return buf;
	}
	case 0x2B: {
		std::string o = formatValueC();
		snprintf(buf, sizeof(buf), "checkObjectData(%s);", o.c_str());
		return buf;
	}
	case 0x2C: {
		std::string o = formatValueC();
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "_rt.invCheck = checkInventory(%s, %s);", o.c_str(), v.c_str());
		return buf;
	}
	case 0x2D: {
		std::string o = formatValueC();
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "setSnapToTarget(%s, %s);", o.c_str(), v.c_str());
		return buf;
	}
	case 0x2E: {
		std::string a = formatValueC();
		std::string lo = formatValueC();
		std::string hi = formatValueC();
		snprintf(buf, sizeof(buf), "_rt.animBlobRange = testSceneAnimFrame(%s, %s, %s);", a.c_str(), lo.c_str(), hi.c_str());
		return buf;
	}
	case 0x2F: {
		std::string o = formatValueC();
		std::string s = formatValueC();
		std::string lo = formatValueC();
		std::string hi = formatValueC();
		snprintf(buf, sizeof(buf), "_rt.animBlobRange = testObjectAnimFrame(%s, %s, %s, %s);", o.c_str(), s.c_str(), lo.c_str(), hi.c_str());
		return buf;
	}
	case 0x31: {
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "setPaletteDarkness(%s);", v.c_str());
		return buf;
	}
	case 0x32: {
		std::string o = formatValueC();
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "setObjectClickable(%s, %s);", o.c_str(), v.c_str());
		return buf;
	}
	case 0x33: {
		std::string o = formatValueC();
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "setObjectVisible(%s, %s);", o.c_str(), v.c_str());
		return buf;
	}
	case 0x34: {
		std::string a = formatValueC();
		std::string b = formatValueC();
		snprintf(buf, sizeof(buf), "setHotspotOverride(%s, %s);", a.c_str(), b.c_str());
		return buf;
	}
	case 0x35: {
		std::string o = formatValueC();
		std::string p = formatValueC();
		std::string a = formatValueC();
		std::string b = formatValueC();
		std::string c = formatValueC();
		snprintf(buf, sizeof(buf), "setObjectBounds(%s, %s, %s, %s, %s);", o.c_str(), p.c_str(), a.c_str(), b.c_str(), c.c_str());
		return buf;
	}
	case 0x36:
		return "dismissAllPanels();";
	case 0x37:
		return "resetToSceneScript();";
	case 0x38: {
		uint8_t r = readByte();
		snprintf(buf, sizeof(buf), "loadOverlayFont(%u);", r);
		return buf;
	}
	case 0x39:
		return "endOverlayText();";
	case 0x3A: {
		std::string x = formatValueC();
		std::string y = formatValueC();
		std::string a = formatValueC();
		uint16_t s = readWord();
		uint16_t t = readWord();
		std::string decoded = decodeString(s, 1);
		std::string result2 = std::string("addOverlayTextEntry(") + x + ", " + y + ", " + a + ", " + std::to_string(s) + ", " + std::to_string(t) + ");";
		if (!decoded.empty())
			result2 += " // \"" + decoded + "\"";
		return result2;
	}
	case 0x3B:
		return "clearOverlayText();";
	case 0x3C: {
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "fadeToBlack(%s);", v.c_str());
		return buf;
	}
	case 0x3D: {
		std::string v = formatValueC();
		snprintf(buf, sizeof(buf), "fadeFromBlack(%s);", v.c_str());
		return buf;
	}
	case 0x3E: {
		uint8_t r = readByte();
		snprintf(buf, sizeof(buf), "loadPcmSound(%u);", r);
		return buf;
	}
	case 0x3F:
		return "freePcmSound();";
	case 0x40:
		return "playPcmSound();";
	case 0x41:
		return "waitForSound();";
	case 0x42:
		return "stopPcmSound();";
	case 0x43: {
		std::string s = formatValueC();
		uint8_t r = readByte();
		snprintf(buf, sizeof(buf), "loadMusicSlot(%s, %u);", s.c_str(), r);
		return buf;
	}
	case 0x44:
	case 0x45: {
		std::string s = formatValueC();
		std::string a = formatValueC();
		std::string b = formatValueC();
		snprintf(buf, sizeof(buf), "%s(%s, %s, %s);", opcode == 0x44 ? "playMusicSlot" : "stopMusicSlot", s.c_str(), a.c_str(), b.c_str());
		return buf;
	}
	case 0x46: {
		std::string s = formatValueC();
		snprintf(buf, sizeof(buf), "freeMusicSlot(%s);", s.c_str());
		return buf;
	}
	case 0x47: {
		std::string o = formatValueC();
		snprintf(buf, sizeof(buf), "waitForMusic(%s);", o.c_str());
		return buf;
	}
	case 0x48:
	case 0x49:
	case 0x4A:
	case 0x4B: {
		std::string o = formatValueC();
		if (pos + 3 <= endPos) {
			readByte();
			uint16_t v = readWord();
			usedVars.insert(v);
			if (v > maxVarIdx)
				maxVarIdx = v;
			snprintf(buf, sizeof(buf), "%s = %s(%s);", formatVarAccess(v).c_str(), getOpcodeName(opcode), o.c_str());
		} else {
			snprintf(buf, sizeof(buf), "%s(%s);", getOpcodeName(opcode), o.c_str());
		}
		return buf;
	}
	case 0x4C:
		return "clearActorInventory();";
	case 0x4D: {
		std::string a = formatValueC();
		std::string b = formatValueC();
		snprintf(buf, sizeof(buf), "setPathfindingRemap(%s, %s);", a.c_str(), b.c_str());
		return buf;
	}
	case 0x4E:
		return "waitForAdlib();";
	default:
		snprintf(buf, sizeof(buf), "/* unknown opcode 0x%02x */", opcode);
		return buf;
	}
}

static void disassembleC(int sceneIndex, const char *type = "scene") {
	// First pass: collect used objects and vars
	usedObjects.clear();
	usedVars.clear();
	maxVarIdx = 0;

	uint32_t savedPos = pos;
	uint32_t savedSize = scriptSize;
	// Dry run to collect references
	int dummyIndent = 0;
	while (pos < scriptSize - 1) {
		uint8_t opcode = readByte();
		if (opcode == 0x00)
			continue;
		uint8_t length = readByte();
		uint32_t endPos = pos + length;
		if (opcode == 0x07 && dummyIndent > 0)
			dummyIndent--;
		if (opcode == 0x08 && dummyIndent > 0)
			dummyIndent--;
		decodeCLine(opcode, endPos, dummyIndent);
		if (pos != endPos)
			pos = endPos;
	}

	// Second pass: emit C
	pos = savedPos;
	scriptSize = savedSize;

	printf("// --- %s %d ---\n", type, sceneIndex);

	// Function body
	printf("void %s_%d_script(void) {\n", type, sceneIndex);

	int indent = 0;
	while (pos < scriptSize - 1) {
		uint8_t opcode = readByte();
		if (opcode == 0x00)
			continue;
		uint8_t length = readByte();
		uint32_t endPos = pos + length;
		if (opcode == 0x07 && indent > 0)
			indent--;
		if (opcode == 0x08 && indent > 0)
			indent--;
		std::string line = decodeCLine(opcode, endPos, indent);
		printf("\t");
		for (int i = 0; i < indent - (opcode == 0x03 || opcode == 0x04 || opcode == 0x05 || opcode == 0x06 || opcode == 0x08 ? 1 : 0); i++)
			printf("\t");
		printf("%s\n", line.c_str());
		if (pos != endPos)
			pos = endPos;
	}

	printf("}\n");
}

int main(int argc, char **argv) {
	if (argc < 2 || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
		printHelp(argv[0]);
		return 1;
	}

	bool jsonMode = false;
	bool cMode = false;
	bool objectsMode = false;
	int argIdx = 1;
	while (argIdx < argc && argv[argIdx][0] == '-') {
		if (!strcmp(argv[argIdx], "--json")) {
			jsonMode = true;
		} else if (!strcmp(argv[argIdx], "--c") || !strcmp(argv[argIdx], "--cpp")) {
			cMode = true;
		} else if (!strcmp(argv[argIdx], "--objects")) {
			objectsMode = true;
		} else {
			printHelp(argv[0]);
			return 1;
		}
		argIdx++;
	}

	if (argIdx >= argc) {
		printHelp(argv[0]);
		return 1;
	}

	// Check if it's an Amiga data directory first
	if (isAmigaDir(argv[argIdx])) {
		return runAmiga(argv[argIdx], jsonMode, objectsMode);
	}

	FILE *f = fopen(argv[argIdx], "rb");
	if (!f) {
		fprintf(stderr, "Error: Cannot open file '%s'\n", argv[argIdx]);
		return 1;
	}

	// Also check: if it opens but starts with MXMF, redirect to Amiga mode
	uint8_t magic[4];
	fread(magic, 1, 4, f);
	fseek(f, 0, SEEK_SET);
	if (memcmp(magic, "MXMF", 4) == 0) {
		fclose(f);
		fprintf(stderr, "Error: '%s' is an Amiga DataA file. Pass the directory containing it.\n", argv[argIdx]);
		return 1;
	}
	argIdx++;

	int startScene = 1;
	int endScene = 512;

	if (argIdx < argc) {
		startScene = atoi(argv[argIdx]);
		endScene = startScene;
	}

	if (cMode) {
		printf("// MACS2 auto-decompiled scripts\n");
		printf("#define DEMACS2\n");
		printf("#include \"engines/macs2/scriptexecutor.h\"\n\n");
		printf("using namespace Macs2::Script;\n\n");
		printf("static ScriptExecutor _executor;\n\n");
		printf("typedef uint16_t Object;\ntypedef uint16_t Value;\n\n");
		printf("static const Value TRUE = 1;\n");
		printf("static const Value FALSE = 0;\n");
		printf("static const Value FADE_CUT = 0;\n");
		printf("static const Value SIDE_LEFT = 0;\n");
		printf("static const Value SIDE_RIGHT = 1;\n");
		printf("static const Value ALIGN_LEFT = 0;\n");
		printf("static const Value ALIGN_RIGHT = 1;\n");
		printf("static const Value ALIGN_CENTER = 2;\n");
		printf("static const Value LOOP = 1;\n\n");
		printf("/* Orientation constants (1-8=walking, 9-16=standing, 17=pickup) */\n");
		printf("#define ORIENT_WALK_N  1\n#define ORIENT_WALK_NE 2\n#define ORIENT_WALK_E  3\n#define ORIENT_WALK_SE 4\n");
		printf("#define ORIENT_WALK_S  5\n#define ORIENT_WALK_SW 6\n#define ORIENT_WALK_W  7\n#define ORIENT_WALK_NW 8\n");
		printf("#define ORIENT_STAND_N  9\n#define ORIENT_STAND_NE 10\n#define ORIENT_STAND_E  11\n#define ORIENT_STAND_SE 12\n");
		printf("#define ORIENT_STAND_S  13\n#define ORIENT_STAND_SW 14\n#define ORIENT_STAND_W  15\n#define ORIENT_STAND_NW 16\n");
		printf("#define ORIENT_PICKUP 17\n\n");
		printf("/* Known script variable indices */\n");
		for (uint16_t i = 0; i <= 512; i++) {
			const char *n = getVarName(i);
			if (n)
				printf("#define %s %u\n", n, i);
		}
		printf("\n");
		printf("static struct {\n");
		for (const auto &s : kSpecialNames)
			printf("\tValue %s;\n", s.name);
		printf("} _rt;\n\n");
		printf("static Object _objects[512];\nstatic Value _vars[512];\n\n");
		printf("/* Named objects */\n");
		for (uint16_t i = 1; i <= 0xBF; i++) {
			const char *n = getObjectName(i);
			if (n)
				printf("#define %s 0x%02x\n", n, i);
		}
		printf("\n");
		printf("/* Wrappers to ScriptExecutor::scriptXXX */\n");
		printf("static void moveObject(Object obj, Value scene, Value x, Value y) { _executor.scriptMoveObject(); }\n");
		printf("static void removeFromScene(Object obj) { _executor.scriptMoveObject(); }\n");
		printf("static void changeScene(Value scene, Value mode, Value speed) { _executor.scriptChangeScene(); }\n");
		printf("static void showDialogue(Object obj, Value x, Value y, Value side, uint16_t s, uint16_t n) { _executor.scriptShowDialogue(); }\n");
		printf("static void printStringLeft(Value x, Value y, uint16_t s, uint16_t n) { _executor.scriptPrintStringLeft(); }\n");
		printf("static void printStringRight(Value x, Value y, uint16_t s, uint16_t n) { _executor.scriptPrintStringRight(); }\n");
		printf("static void frameWait(Value t) { _executor.scriptFrameWait(); }\n");
		printf("static void walkToPosition(Object o, Value x, Value y) { _executor.scriptWalkToPosition(); }\n");
		printf("static void waitForWalk(Object o) { _executor.scriptWaitForWalk(); }\n");
		printf("static void setPathfinding(Value a, Value b, Value c) { _executor.scriptSetPathfinding(); }\n");
		printf("static void skipWord(uint16_t w) { _executor.scriptSkipWord(); }\n");
		printf("static void clearDialogueChoices() { _executor.scriptClearDialogueChoices(); }\n");
		printf("static void addDialogueChoice(Value i, uint16_t s, uint16_t n) { _executor.scriptAddDialogueChoice(); }\n");
		printf("static void showDialogueChoice(Object o, Value x, Value y, Value s) { _executor.scriptShowDialogueChoice(); }\n");
		printf("static void dismissPanel() { _executor.scriptDismissPanel(); }\n");
		printf("static void walkToAndPickup(Object a, Object o) { _executor.scriptWalkToAndPickup(); }\n");
		printf("static void setPickupFrames(Object o, Value s, Value e) { _executor.scriptSetPickupFrames(); }\n");
		printf("static void setupObject(Object o, Value s, Value sp) { _executor.scriptSetupObject(); }\n");
		printf("static void setSkippable() { _executor.scriptSetSkippable(); }\n");
		printf("static void clearSkippable() { _executor.scriptClearSkippable(); }\n");
		printf("static void playAnimation(Object o, Value s, Value f) { _executor.scriptPlayAnimation(); }\n");
		printf("static Value testPathfinding(Object o, Value x, Value y) { _executor.scriptTestPathfinding(); return 0; }\n");
		printf("static void setYOffset(Object o, Value v) { _executor.scriptSetYOffset(); }\n");
		printf("static void setMotion(Object o, Value t, Value d, Value di) { _executor.scriptSetMotion(); }\n");
		printf("static void setOrientation(Object o, Value a) { _executor.scriptSetOrientation(); }\n");
		printf("static void moveToPosition(Object o, Value x, Value y, Value v) { _executor.scriptMoveToPosition(); }\n");
		printf("static void loadSpecialAnim(Object o, Value d, uint8_t i) { _executor.scriptLoadSpecialAnim(); }\n");
		printf("static void setDirection(Object o, Value m) { _executor.scriptSetDirection(); }\n");
		printf("static void stopAnimation() { _executor.scriptStopAnimation(); }\n");
		printf("static void changeAnimation() { _executor.scriptChangeAnimation(); }\n");
		printf("static void openInventory(Object o) { _executor.scriptOpenInventory(); }\n");
		printf("static void loadObjectAnim(Object o, Value s, Value d, uint8_t i) { _executor.scriptLoadObjectAnim(); }\n");
		printf("static void checkObjectData(Object o) { _executor.scriptCheckObjectData(); }\n");
		printf("static Value checkInventory(Object o, Value v) { _executor.scriptCheckInventory(); return 0; }\n");
		printf("static void setSnapToTarget(Object o, Value v) { _executor.scriptSetSnapToTarget(); }\n");
		printf("static Value testSceneAnimFrame(Value a, Value lo, Value hi) { _executor.scriptTestSceneAnimFrame(); return 0; }\n");
		printf("static Value testObjectAnimFrame(Object o, Value s, Value lo, Value hi) { _executor.scriptTestObjectAnimFrame(); return 0; }\n");
		printf("static void setPaletteDarkness(Value v) { _executor.scriptSetPaletteDarkness(); }\n");
		printf("static void setObjectClickable(Object o, Value v) { _executor.scriptSetObjectClickable(); }\n");
		printf("static void setObjectVisible(Object o, Value v) { _executor.scriptSetObjectVisible(); }\n");
		printf("static void setHotspotOverride(Value a, Value b) { _executor.scriptSetHotspotOverride(); }\n");
		printf("static void setObjectBounds(Object o, Object p, Value a, Value b, Value c) { _executor.scriptSetObjectBounds(); }\n");
		printf("static void dismissAllPanels() { _executor.scriptDismissAllPanels(); }\n");
		printf("static void resetToSceneScript() { _executor.scriptResetToSceneScript(); }\n");
		printf("static void loadOverlayFont(uint8_t r) { _executor.scriptLoadOverlayFont(); }\n");
		printf("static void endOverlayText() { _executor.scriptEndOverlayText(); }\n");
		printf("static void addOverlayTextEntry(Value x, Value y, Value a, uint16_t s, uint16_t t) { _executor.scriptAddOverlayTextEntry(); }\n");
		printf("static void clearOverlayText() { _executor.scriptClearOverlayText(); }\n");
		printf("static void fadeToBlack(Value s) { _executor.scriptFadeToBlack(); }\n");
		printf("static void fadeFromBlack(Value s) { _executor.scriptFadeFromBlack(); }\n");
		printf("static void loadPcmSound(uint8_t r) { _executor.scriptLoadPcmSound(); }\n");
		printf("static void freePcmSound() { _executor.scriptFreePcmSound(); }\n");
		printf("static void playPcmSound() { _executor.scriptPlayPcmSound(); }\n");
		printf("static void waitForSound() { _executor.scriptWaitForSound(); }\n");
		printf("static void stopPcmSound() { _executor.scriptStopPcmSound(); }\n");
		printf("static void loadMusicSlot(Value s, uint8_t r) { _executor.scriptLoadMusicSlot(); }\n");
		printf("static void playMusicSlot(Value s, Value a, Value b) { _executor.scriptPlayMusicSlot(); }\n");
		printf("static void stopMusicSlot(Value s, Value a, Value b) { _executor.scriptStopMusicSlot(); }\n");
		printf("static void freeMusicSlot(Value s) { _executor.scriptFreeMusicSlot(); }\n");
		printf("static void waitForMusic(Object o) { _executor.scriptWaitForMusic(); }\n");
		printf("static Value getObjectX(Object o) { _executor.scriptGetObjectX(); return 0; }\n");
		printf("static Value getObjectY(Object o) { _executor.scriptGetObjectY(); return 0; }\n");
		printf("static Value getObjectField8(Object o) { _executor.scriptGetObjectField8(); return 0; }\n");
		printf("static Value getObjectOrientation(Object o) { _executor.scriptGetObjectOrientation(); return 0; }\n");
		printf("static void clearActorInventory() { _executor.scriptClearActorInventory(); }\n");
		printf("static void setPathfindingRemap(Value a, Value b) { _executor.scriptSetPathfindingRemap(); }\n");
		printf("static void waitForAdlib() { _executor.scriptWaitForAdlib(); }\n");
		printf("static void skipUntil14() { _executor.scriptSkipUntil14(); }\n");
		printf("static int ifInteraction(Value i, Value a, Value b) { _executor.scriptIfInteraction(); return 0; }\n");
		printf("static void nop() { _executor.scriptNop09(); }\n\n");

		for (int scene = startScene; scene <= endScene; scene++) {
			if (loadSceneScript(f, (uint16_t)scene)) {
				disassembleC(scene, "scene");
				free(scriptData);
				scriptData = nullptr;
				scriptSize = 0;
				free(stringData);
				stringData = nullptr;
				stringDataSize = 0;
			}
			if (objectsMode) {
				for (int obj = 1; obj <= 0x200; obj++) {
					uint16_t objScene = getObjectSceneIndex(f, (uint16_t)obj);
					if (objScene != (uint16_t)scene)
						continue;
					if (loadObjectScript(f, (uint16_t)obj)) {
						printf("\n");
						disassembleC(obj, "object");
						free(scriptData);
						scriptData = nullptr;
						scriptSize = 0;
						free(stringData);
						stringData = nullptr;
						stringDataSize = 0;
					}
				}
			}
		}
		if (objectsMode) {
			for (int obj = 1; obj <= 0x200; obj++) {
				uint16_t objScene = getObjectSceneIndex(f, (uint16_t)obj);
				if (objScene != 0)
					continue;
				if (loadObjectScript(f, (uint16_t)obj)) {
					printf("\n");
					disassembleC(obj, "inventory");
					free(scriptData);
					scriptData = nullptr;
					scriptSize = 0;
					free(stringData);
					stringData = nullptr;
					stringDataSize = 0;
				}
			}
		}
	} else if (jsonMode) {
		printf("{\"scenes\": [\n");
		bool first = true;
		for (int scene = startScene; scene <= endScene; scene++) {
			if (loadSceneScript(f, (uint16_t)scene)) {
				if (!first)
					printf(",\n");
				first = false;
				disassembleJson(scene, "scene");
				free(scriptData);
				scriptData = nullptr;
				scriptSize = 0;
				free(stringData);
				stringData = nullptr;
				stringDataSize = 0;
			}
			if (objectsMode) {
				for (int obj = 1; obj <= 0x200; obj++) {
					uint16_t objScene = getObjectSceneIndex(f, (uint16_t)obj);
					if (objScene != (uint16_t)scene && objScene != 0)
						continue;
					if (loadObjectScript(f, (uint16_t)obj)) {
						if (!first)
							printf(",\n");
						first = false;
						disassembleJson(obj, objScene == 0 ? "inventory" : "object");
						free(scriptData);
						scriptData = nullptr;
						scriptSize = 0;
						free(stringData);
						stringData = nullptr;
						stringDataSize = 0;
					}
				}
			}
		}
		printf("\n]}\n");
	} else {
		printf("; MACS2 Script Runtime Variables (type 0xFF, read-only)\n");
		printf("; These are computed by the engine at read-time, not stored in script data.\n");
		for (const auto &s : kSpecialNames) {
			printf("; %-18s (FF:%02x)\n", s.name, s.id);
		}
		printf(";\n");
		printf("; Script variables: var[N] (read/write, all zeroed on scene load)\n");
		printf("; Object IDs in moveObject/etc.: raw value - 0x400 = object index\n");
		printf(";\n\n");

		for (int scene = startScene; scene <= endScene; scene++) {
			if (loadSceneScript(f, (uint16_t)scene)) {
				printf("=== Scene %d (size: %u bytes) ===\n", scene, scriptSize);
				disassemble();
				printf("\n");
				free(scriptData);
				scriptData = nullptr;
				scriptSize = 0;
				free(stringData);
				stringData = nullptr;
				stringDataSize = 0;
			}
			if (objectsMode) {
				bool hasSceneObjs = false;
				bool hasInventoryObjs = false;
				// Scene objects first
				for (int obj = 1; obj <= 0x200; obj++) {
					uint16_t objScene = getObjectSceneIndex(f, (uint16_t)obj);
					if (objScene != (uint16_t)scene)
						continue;
					if (!hasSceneObjs) {
						printf("  [Scene Objects]\n");
						hasSceneObjs = true;
					}
					if (loadObjectScript(f, (uint16_t)obj)) {
						printf("--- Object 0x%x (size: %u bytes) ---\n", obj, scriptSize);
						disassemble();
						printf("\n");
						free(scriptData);
						scriptData = nullptr;
						scriptSize = 0;
						free(stringData);
						stringData = nullptr;
						stringDataSize = 0;
					}
				}
				// Inventory/global objects (scene == 0)
				for (int obj = 1; obj <= 0x200; obj++) {
					if (getObjectSceneIndex(f, (uint16_t)obj) != 0)
						continue;
					if (!hasInventoryObjs) {
						printf("  [Inventory/Global Objects]\n");
						hasInventoryObjs = true;
					}
					if (loadObjectScript(f, (uint16_t)obj)) {
						printf("--- Object 0x%x [inventory] (size: %u bytes) ---\n", obj, scriptSize);
						disassemble();
						printf("\n");
						free(scriptData);
						scriptData = nullptr;
						scriptSize = 0;
						free(stringData);
						stringData = nullptr;
						stringDataSize = 0;
					}
				}
			}
		}
	}

	fclose(f);
	return 0;
}
