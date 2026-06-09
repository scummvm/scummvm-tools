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

static void appendUtf8(std::string &out, uint8_t ch) {
	if (ch < 0x80) {
		out += (char)ch;
		return;
	}
	// CP850 to Unicode mapping for 0x80-0xFF
	static const uint16_t cp850[128] = {
		0x00C7,0x00FC,0x00E9,0x00E2,0x00E4,0x00E0,0x00E5,0x00E7,
		0x00EA,0x00EB,0x00E8,0x00EF,0x00EC,0x00ED,0x00C4,0x00C5,
		0x00C9,0x00E6,0x00C6,0x00F4,0x00F6,0x00F2,0x00FB,0x00F9,
		0x00FF,0x00D6,0x00DC,0x00F8,0x00A3,0x00D8,0x00D7,0x0192,
		0x00E1,0x00ED,0x00F3,0x00FA,0x00F1,0x00D1,0x00AA,0x00BA,
		0x00BF,0x00AE,0x00AC,0x00BD,0x00BC,0x00A1,0x00AB,0x00BB,
		0x2591,0x2592,0x2593,0x2502,0x2524,0x00C1,0x00C2,0x00C0,
		0x00A9,0x2563,0x2551,0x2557,0x255D,0x00A2,0x00A5,0x2510,
		0x2514,0x2534,0x252C,0x251C,0x2500,0x253C,0x00E3,0x00C3,
		0x255A,0x2554,0x2569,0x2566,0x2560,0x2550,0x256C,0x00A4,
		0x00F0,0x00D0,0x00CA,0x00CB,0x00C8,0x0131,0x00CD,0x00CE,
		0x00CF,0x2518,0x250C,0x2588,0x2584,0x00A6,0x00CC,0x2580,
		0x00D3,0x00DF,0x00D4,0x00D2,0x00F5,0x00D5,0x00B5,0x00FE,
		0x00DE,0x00DA,0x00DB,0x00D9,0x00FD,0x00DD,0x00AF,0x00B4,
		0x00AD,0x00B1,0x2017,0x00BE,0x00B6,0x00A7,0x00F7,0x00B8,
		0x00B0,0x00A8,0x00B7,0x00B9,0x00B3,0x00B2,0x25A0,0x00A0,
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

static std::string decodeString(uint32_t offset, uint16_t numLines) {
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
	{0x2E, "const2"},
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
		line.params = decodeParams(opcode, endPos, indent);
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
		printf("    {\"offset\": %u, \"opcode\": %u, \"name\": \"%s\", \"indent\": %d, \"text\": \"%s\"}%s\n",
			   l.offset, l.opcode, jsonEscape(getOpcodeName(l.opcode)).c_str(),
			   l.indent, jsonEscape(text).c_str(),
			   (i + 1 < lines.size()) ? "," : "");
	}
	printf("  ]}");
}

static void printHelp(const char *bin) {
	printf("MACS2 Script Decompiler\n\n");
	printf("Usage: %s [--json] [--objects] <game_data_file> [scene_index]\n\n", bin);
	printf("  --json          - Output as JSON\n");
	printf("  --objects       - Also decompile object scripts for the scene(s)\n");
	printf("                    Objects with scene=0 (inventory/global) are included\n");
	printf("  game_data_file  - The main game data file\n");
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

int main(int argc, char **argv) {
	if (argc < 2 || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
		printHelp(argv[0]);
		return 1;
	}

	bool jsonMode = false;
	bool objectsMode = false;
	int argIdx = 1;
	while (argIdx < argc && argv[argIdx][0] == '-') {
		if (!strcmp(argv[argIdx], "--json")) {
			jsonMode = true;
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

	FILE *f = fopen(argv[argIdx], "rb");
	if (!f) {
		fprintf(stderr, "Error: Cannot open file '%s'\n", argv[argIdx]);
		return 1;
	}
	argIdx++;

	int startScene = 1;
	int endScene = 512;

	if (argIdx < argc) {
		startScene = atoi(argv[argIdx]);
		endScene = startScene;
	}

	if (jsonMode) {
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
