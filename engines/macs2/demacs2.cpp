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

/* MACS2 Script disassembler */

#include <cstdio>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <vector>
#include <string>

static uint8_t *scriptData = nullptr;
static uint32_t scriptSize = 0;
static uint32_t pos = 0;

static uint8_t readByte() {
	if (pos >= scriptSize) return 0;
	return scriptData[pos++];
}

static uint16_t readWord() {
	uint8_t lo = readByte();
	uint8_t hi = readByte();
	return (uint16_t)hi << 8 | lo;
}

static std::string formatValue() {
	uint8_t type = readByte();
	uint16_t index = readWord();
	if (type == 0x00) {
		char buf[32];
		snprintf(buf, sizeof(buf), "%u", index);
		return buf;
	} else if (type == 0xFF) {
		// Special runtime values
		const char *name = nullptr;
		switch (index) {
		case 0x01: name = "interacted_use"; break;
		case 0x02: name = "interacted_look"; break;
		case 0x03: name = "interacted_talk"; break;
		case 0x04: name = "char_area"; break;
		case 0x05: name = "noop"; break;
		case 0x06: return "1";
		case 0x07: return "0";
		case 0x08: return "0";
		case 0x09: return "0";
		case 0x0A: return "1";
		case 0x0B: name = "repeat_run_flag"; break;
		case 0x0C: return "1";
		case 0x0D: name = "chosen_dialogue_option"; break;
		case 0x23: name = "path_walkable_result"; break;
		case 0x24: name = "actor_x"; break;
		case 0x25: name = "actor_y"; break;
		case 0x26: name = "is_scene_init"; break;
		case 0x27: name = "repeat_char_area"; break;
		case 0x28: name = "inventory_check_result"; break;
		case 0x29: name = "anim_range_test_result"; break;
		case 0x2A: name = "inventory_combine_flag"; break;
		case 0x2B: name = "inventory_action_flag"; break;
		case 0x2C: name = "interacted_panel_use"; break;
		case 0x2D: name = "current_scene"; break;
		case 0x2E: return "2";
		case 0x2F: name = "last_scene"; break;
		case 0x30: name = "music_enabled"; break;
		case 0x31: name = "sound_enabled"; break;
		default:
			if (index >= 0x0E && index <= 0x22) {
				char buf[32];
				snprintf(buf, sizeof(buf), "%u", index - 0x0D);
				return buf;
			}
			char buf[32];
			snprintf(buf, sizeof(buf), "$special_0x%02x", index);
			return buf;
		}
		return std::string("$") + name;
	} else {
		char buf[32];
		snprintf(buf, sizeof(buf), "var[%u]", index);
		return buf;
	}
}

// Read a value but return both words as "val" (for 32-bit context)
static std::string formatValue32() {
	return formatValue();
}

static std::string formatObjectId() {
	std::string v = formatValue();
	return v + " - 0x400";
}

static std::string formatSaveTarget() {
	readByte(); // type (ignored for save target)
	uint16_t index = readWord();
	char buf[32];
	snprintf(buf, sizeof(buf), "var[%u]", index);
	return buf;
}

static const char *cmpOpName(uint8_t op) {
	switch (op) {
	case 0x01: return "==";
	case 0x02: return "!=";
	case 0x03: return "<";
	case 0x04: return ">";
	case 0x05: return "<=";
	case 0x06: return ">=";
	default: return "??";
	}
}

static void disassemble() {
	int indent = 0;

	while (pos < scriptSize - 1) {
		uint32_t instrAddr = pos;
		uint8_t opcode = readByte();
		if (opcode == 0x00) continue;

		uint8_t length = readByte();
		uint32_t endPos = pos + length;

		// Adjust indent before printing for block-end opcodes
		if (opcode == 0x07 && indent > 0) indent--;

		// Print indent
		for (int i = 0; i < indent; i++) printf("  ");
		printf("[%04X] ", instrAddr);

		switch (opcode) {
		case 0x01: {
			readByte(); // padding
			uint16_t varIdx = readWord();
			std::string val = formatValue();
			printf("SET var[%u] = %s\n", varIdx, val.c_str());
			break;
		}
		case 0x02: {
			readByte(); // padding
			uint16_t varIdx = readWord();
			std::string val1 = formatValue();
			std::string val2 = formatValue();
			printf("SET_OR var[%u] = %s | %s\n", varIdx, val1.c_str(), val2.c_str());
			break;
		}
		case 0x03: {
			std::string val = formatValue();
			printf("IF_TRUE %s THEN SKIP {\n", val.c_str());
			indent++;
			break;
		}
		case 0x04: {
			std::string val = formatValue();
			printf("IF_FALSE %s THEN SKIP {\n", val.c_str());
			indent++;
			break;
		}
		case 0x05: {
			uint8_t cmpOp = readByte();
			std::string v1 = formatValue();
			std::string v2 = formatValue();
			printf("IF %s %s %s {\n", v1.c_str(), cmpOpName(cmpOp), v2.c_str());
			indent++;
			break;
		}
		case 0x06: {
			uint8_t subOp = readByte();
			std::string obj1 = formatValue();
			std::string obj2 = formatValue();
			std::string cmp1 = formatValue();
			std::string cmp2 = formatValue();
			if (subOp == 0x01)
				printf("IF_USE_ON (%s, %s) MATCHES (%s, %s) {\n", obj1.c_str(), obj2.c_str(), cmp1.c_str(), cmp2.c_str());
			else
				printf("IF_USE_ON (%s, %s) NOT_MATCHES (%s, %s) {\n", obj1.c_str(), obj2.c_str(), cmp1.c_str(), cmp2.c_str());
			indent++;
			break;
		}
		case 0x07: {
			printf("}\n");
			break;
		}
		case 0x08: {
			printf("} ELSE {\n");
			break;
		}
		case 0x0A: {
			std::string x = formatValue();
			std::string y = formatValue();
			uint16_t strOffset = readWord();
			uint16_t numLines = readWord();
			printf("PRINT_STRING pos=(%s, %s) strOffset=%u numLines=%u\n", x.c_str(), y.c_str(), strOffset, numLines);
			break;
		}
		case 0x0B: {
			std::string objId = formatObjectId();
			std::string scene = formatValue();
			std::string x = formatValue();
			std::string y = formatValue();
			printf("MOVE_OBJECT obj=%s toScene=%s pos=(%s, %s)\n", objId.c_str(), scene.c_str(), x.c_str(), y.c_str());
			break;
		}
		case 0x0C: {
			std::string scene = formatValue32();
			std::string mode = formatValue();
			std::string speed = formatValue();
			printf("CHANGE_SCENE scene=%s mode=%s speed=%s\n", scene.c_str(), mode.c_str(), speed.c_str());
			break;
		}
		case 0x0D: {
			std::string objId = formatObjectId();
			std::string x = formatValue();
			std::string y = formatValue();
			std::string side = formatValue();
			uint16_t strOffset = readWord();
			uint16_t numLines = readWord();
			printf("DIALOGUE speaker=%s pos=(%s, %s) side=%s strOffset=%u numLines=%u\n",
				   objId.c_str(), x.c_str(), y.c_str(), side.c_str(), strOffset, numLines);
			break;
		}
		case 0x0E: {
			std::string id = formatValue32();
			std::string frame = formatValue();
			printf("CHANGE_ANIM id=%s frame=%s\n", id.c_str(), frame.c_str());
			break;
		}
		case 0x0F: {
			std::string duration = formatValue();
			printf("WAIT_FRAMES %s\n", duration.c_str());
			break;
		}
		case 0x10: {
			std::string objId = formatObjectId();
			std::string x = formatValue();
			std::string y = formatValue();
			printf("WALK_TO obj=%s pos=(%s, %s)\n", objId.c_str(), x.c_str(), y.c_str());
			break;
		}
		case 0x11: {
			std::string objId = formatObjectId();
			printf("WAIT_WALK obj=%s\n", objId.c_str());
			break;
		}
		case 0x12: {
			std::string area = formatValue();
			std::string active = formatValue();
			std::string override_val = formatValue();
			printf("SET_PATHFINDING area=%s active=%s override=%s\n", area.c_str(), active.c_str(), override_val.c_str());
			break;
		}
		case 0x13: {
			uint16_t tag = readWord();
			printf("GOTO_TAG 0x%04X\n", tag);
			break;
		}
		case 0x14: {
			uint16_t tag = readWord();
			printf("TAG 0x%04X\n", tag);
			break;
		}
		case 0x15: {
			printf("DIALOGUE_CHOICES_BEGIN\n");
			break;
		}
		case 0x16: {
			std::string idx = formatValue();
			uint16_t strOffset = readWord();
			uint16_t numLines = readWord();
			printf("DIALOGUE_CHOICE index=%s strOffset=%u numLines=%u\n", idx.c_str(), strOffset, numLines);
			break;
		}
		case 0x17: {
			std::string x = formatValue32();
			std::string y = formatValue32();
			std::string side = formatValue();
			printf("DIALOGUE_CHOICES_SHOW pos=(%s, %s) side=%s\n", x.c_str(), y.c_str(), side.c_str());
			break;
		}
		case 0x18: {
			printf("END_SCRIPT\n");
			break;
		}
		case 0x19: {
			std::string actor = formatObjectId();
			std::string target = formatObjectId();
			printf("PICKUP actor=%s target=%s\n", actor.c_str(), target.c_str());
			break;
		}
		case 0x1A: {
			std::string objId = formatObjectId();
			std::string val217 = formatValue();
			std::string val219 = formatValue();
			printf("SET_OBJECT_RUNTIME obj=%s val217=%s val219=%s\n", objId.c_str(), val217.c_str(), val219.c_str());
			break;
		}
		case 0x1B: {
			std::string objId = formatObjectId();
			std::string slot = formatValue();
			std::string val = formatValue();
			printf("SET_OBJECT_SLOT obj=%s slot=%s value=%s\n", objId.c_str(), slot.c_str(), val.c_str());
			break;
		}
		case 0x1C: {
			printf("SKIPPABLE_BEGIN\n");
			break;
		}
		case 0x1D: {
			printf("SKIPPABLE_END\n");
			break;
		}
		case 0x1E: {
			std::string objId = formatObjectId();
			std::string slot = formatValue();
			std::string frame = formatValue();
			printf("PLAY_ANIM obj=%s slot=%s frame=%s\n", objId.c_str(), slot.c_str(), frame.c_str());
			break;
		}
		case 0x1F: {
			std::string objId = formatObjectId();
			std::string x = formatValue32();
			std::string y = formatValue32();
			printf("TEST_PATH_WALKABLE obj=%s to=(%s, %s)\n", objId.c_str(), x.c_str(), y.c_str());
			break;
		}
		case 0x20: {
			std::string objId = formatObjectId();
			std::string offset = formatValue();
			printf("SET_Y_OFFSET obj=%s offset=%s\n", objId.c_str(), offset.c_str());
			break;
		}
		case 0x21: {
			std::string objId = formatObjectId();
			std::string target = formatValue();
			std::string delta = formatValue();
			std::string dist = formatValue();
			printf("SET_MOTION obj=%s targetOffset=%s delta=%s distance=%s\n", objId.c_str(), target.c_str(), delta.c_str(), dist.c_str());
			break;
		}
		case 0x22: {
			std::string objId = formatObjectId();
			std::string orient = formatValue();
			printf("SET_ORIENTATION obj=%s orientation=%s\n", objId.c_str(), orient.c_str());
			break;
		}
		case 0x23: {
			std::string objId = formatObjectId();
			std::string x = formatValue32();
			std::string y = formatValue32();
			std::string targetOffset = formatValue();
			printf("MOVE_TO_POSITION obj=%s pos=(%s, %s) targetOffset=%s\n", objId.c_str(), x.c_str(), y.c_str(), targetOffset.c_str());
			break;
		}
		case 0x24: {
			std::string a = formatValue32();
			std::string b = formatValue32();
			// The save target is embedded in the first value's position
			// We can't perfectly reconstruct it without re-reading, so show the operands
			printf("ADD %s + %s -> (save to first operand var)\n", a.c_str(), b.c_str());
			break;
		}
		case 0x25: {
			std::string a = formatValue32();
			std::string b = formatValue32();
			printf("SUB %s - %s -> (save to first operand var)\n", a.c_str(), b.c_str());
			break;
		}
		case 0x26: {
			std::string objId = formatObjectId();
			std::string val = formatValue();
			uint8_t animId = readByte();
			printf("LOAD_SPECIAL_ANIM obj=%s val=%s animId=%u\n", objId.c_str(), val.c_str(), animId);
			break;
		}
		case 0x27: {
			std::string charId = formatValue();
			std::string val = formatValue();
			printf("SET_DIRECTION charId=%s value=%s\n", charId.c_str(), val.c_str());
			break;
		}
		case 0x28: {
			std::string objId = formatObjectId();
			printf("STOP_ANIM obj=%s\n", objId.c_str());
			break;
		}
		case 0x29: {
			std::string objId = formatObjectId();
			printf("OPEN_INVENTORY source=%s\n", objId.c_str());
			break;
		}
		case 0x2A: {
			std::string objId = formatObjectId();
			std::string slot = formatValue();
			std::string decode = formatValue();
			uint8_t arrayIdx = readByte();
			printf("LOAD_ANIM obj=%s slot=%s decode=%s arrayIdx=%u\n", objId.c_str(), slot.c_str(), decode.c_str(), arrayIdx);
			break;
		}
		case 0x2B: {
			std::string objId = formatValue();
			printf("REFRESH_OBJECT obj=%s\n", objId.c_str());
			break;
		}
		case 0x2C: {
			std::string objId = formatValue();
			std::string parentId = formatValue();
			printf("CHECK_INVENTORY obj=%s parent=%s\n", objId.c_str(), parentId.c_str());
			break;
		}
		case 0x2D: {
			std::string objId = formatValue();
			std::string enabled = formatValue();
			printf("SET_RUNTIME_FLAG obj=%s enabled=%s\n", objId.c_str(), enabled.c_str());
			break;
		}
		case 0x2E: {
			std::string animIdx = formatValue32();
			std::string minFrame = formatValue32();
			std::string maxFrame = formatValue32();
			printf("TEST_SCENE_ANIM_FRAME animIdx=%s min=%s max=%s\n", animIdx.c_str(), minFrame.c_str(), maxFrame.c_str());
			break;
		}
		case 0x2F: {
			std::string objId = formatObjectId();
			std::string slot = formatValue();
			std::string minFrame = formatValue();
			std::string maxFrame = formatValue();
			printf("TEST_OBJECT_ANIM_FRAME obj=%s slot=%s min=%s max=%s\n", objId.c_str(), slot.c_str(), minFrame.c_str(), maxFrame.c_str());
			break;
		}
		case 0x30: {
			std::string x = formatValue();
			std::string y = formatValue();
			uint16_t strOffset = readWord();
			uint16_t numLines = readWord();
			printf("PRINT_STRING_RIGHT pos=(%s, %s) strOffset=%u numLines=%u\n", x.c_str(), y.c_str(), strOffset, numLines);
			break;
		}
		case 0x31: {
			std::string vol = formatValue();
			printf("SET_VOLUME %s\n", vol.c_str());
			break;
		}
		case 0x32: {
			std::string objId = formatValue();
			std::string clickable = formatValue();
			printf("SET_CLICKABLE obj=%s clickable=%s\n", objId.c_str(), clickable.c_str());
			break;
		}
		case 0x33: {
			std::string objId = formatValue();
			std::string visible = formatValue();
			printf("SET_VISIBLE obj=%s visible=%s\n", objId.c_str(), visible.c_str());
			break;
		}
		case 0x34: {
			std::string v1 = formatValue();
			std::string v2 = formatValue();
			printf("SET_HOTSPOT_OVERRIDE %s -> %s\n", v1.c_str(), v2.c_str());
			break;
		}
		case 0x35: {
			std::string objId = formatValue();
			std::string otherId = formatValue();
			std::string val1 = formatValue();
			std::string val2 = formatValue();
			std::string val3 = formatValue();
			printf("SET_BOUNDS_ATTACHMENT obj=%s other=%s v1=%s v2=%s v3=%s\n",
				   objId.c_str(), otherId.c_str(), val1.c_str(), val2.c_str(), val3.c_str());
			break;
		}
		case 0x36: {
			printf("DISMISS_PANEL\n");
			break;
		}
		case 0x37: {
			printf("RESET_TO_SCENE_SCRIPT\n");
			break;
		}
		case 0x38: {
			uint8_t resIdx = readByte();
			printf("LOAD_OVERLAY_FONT resource=%u\n", resIdx);
			break;
		}
		case 0x39: {
			printf("END_OVERLAY_TEXT\n");
			break;
		}
		case 0x3A: {
			std::string x = formatValue();
			std::string y = formatValue();
			std::string align = formatValue();
			uint16_t strOffset = readWord();
			uint16_t entryType = readWord();
			printf("OVERLAY_TEXT_ENTRY pos=(%s, %s) align=%s strOffset=%u type=%u\n",
				   x.c_str(), y.c_str(), align.c_str(), strOffset, entryType);
			break;
		}
		case 0x3B: {
			printf("CLEAR_OVERLAY_TEXT\n");
			break;
		}
		case 0x3C: {
			std::string speed = formatValue();
			printf("FADE_TO_BLACK speed=%s\n", speed.c_str());
			break;
		}
		case 0x3D: {
			std::string speed = formatValue();
			printf("FADE_IN speed=%s\n", speed.c_str());
			break;
		}
		case 0x3E: {
			uint8_t resIdx = readByte();
			printf("LOAD_SOUND resource=%u\n", resIdx);
			break;
		}
		case 0x3F: {
			printf("STOP_SOUND\n");
			break;
		}
		case 0x40: {
			printf("PLAY_SOUND\n");
			break;
		}
		case 0x41: {
			printf("WAIT_SOUND\n");
			break;
		}
		case 0x42: {
			printf("STOP_CURRENT_SOUND\n");
			break;
		}
		case 0x43: {
			std::string slot = formatValue();
			uint8_t resIdx = readByte();
			printf("LOAD_MUSIC slot=%s resource=%u\n", slot.c_str(), resIdx);
			break;
		}
		case 0x44: {
			std::string slot = formatValue();
			std::string startMuted = formatValue();
			std::string fadeParam = formatValue();
			printf("PLAY_MUSIC slot=%s startMuted=%s fade=%s\n", slot.c_str(), startMuted.c_str(), fadeParam.c_str());
			break;
		}
		case 0x45: {
			std::string slot = formatValue();
			std::string immediate = formatValue();
			std::string fadeParam = formatValue();
			printf("STOP_MUSIC slot=%s immediate=%s fade=%s\n", slot.c_str(), immediate.c_str(), fadeParam.c_str());
			break;
		}
		case 0x46: {
			std::string slot = formatValue();
			printf("FREE_MUSIC slot=%s\n", slot.c_str());
			break;
		}
		case 0x47: {
			printf("WAIT_MUSIC\n");
			break;
		}
		case 0x48: {
			std::string objId = formatObjectId();
			std::string target = formatSaveTarget();
			printf("GET_OBJECT_X obj=%s -> %s\n", objId.c_str(), target.c_str());
			break;
		}
		case 0x49: {
			std::string objId = formatObjectId();
			std::string target = formatSaveTarget();
			printf("GET_OBJECT_Y obj=%s -> %s\n", objId.c_str(), target.c_str());
			break;
		}
		case 0x4A: {
			std::string objId = formatObjectId();
			std::string target = formatSaveTarget();
			printf("GET_OBJECT_FIELD obj=%s -> %s\n", objId.c_str(), target.c_str());
			break;
		}
		case 0x4B: {
			std::string objId = formatObjectId();
			std::string target = formatSaveTarget();
			printf("GET_OBJECT_ORIENTATION obj=%s -> %s\n", objId.c_str(), target.c_str());
			break;
		}
		case 0x4C: {
			printf("CLEAR_INVENTORY\n");
			break;
		}
		case 0x4D: {
			std::string src = formatValue();
			std::string dst = formatValue();
			printf("SET_AREA_REMAP %s -> %s\n", src.c_str(), dst.c_str());
			break;
		}
		case 0x4E: {
			printf("WAIT_ADLIB_READY\n");
			break;
		}
		default: {
			printf("UNKNOWN_OPCODE 0x%02X (len=%u)\n", opcode, length);
			break;
		}
		}

		// Ensure we advance to the correct position regardless of parsing
		if (pos != endPos) {
			pos = endPos;
		}
	}
}

static void printHelp(const char *bin) {
	printf("MACS2 Script Disassembler\n\n");
	printf("Usage: %s <game_data_file> [scene_index]\n\n", bin);
	printf("  game_data_file  - The main game data file\n");
	printf("  scene_index     - Scene number to disassemble (1-based)\n");
	printf("                    If omitted, disassembles all scenes\n\n");
	printf("The disassembled script will be written to stdout.\n");
}

static bool loadSceneScript(FILE *f, uint16_t sceneIndex) {
	// Calculate offset: sceneIndex * 0xC + 0xC + 0x4 - 0x8
	uint32_t offset = (uint32_t)sceneIndex * 0xC + 0xC + 0x4 - 0x8;
	fseek(f, offset, SEEK_SET);

	uint32_t sceneDataOffset;
	if (fread(&sceneDataOffset, 4, 1, f) != 1) return false;
	// Little-endian conversion (assuming host is LE for simplicity)
	// For portability we should do proper byte swapping but this matches the game's target

	fseek(f, sceneDataOffset + 0x80, SEEK_SET);

	uint16_t size;
	if (fread(&size, 2, 1, f) != 1) return false;

	if (size == 0) return false;

	scriptData = (uint8_t *)malloc(size);
	if (!scriptData) return false;

	if (fread(scriptData, 1, size, f) != size) {
		free(scriptData);
		scriptData = nullptr;
		return false;
	}

	scriptSize = size;
	pos = 0;
	return true;
}

int main(int argc, char **argv) {
	if (argc < 2 || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
		printHelp(argv[0]);
		return 1;
	}

	FILE *f = fopen(argv[1], "rb");
	if (!f) {
		fprintf(stderr, "Error: Cannot open file '%s'\n", argv[1]);
		return 1;
	}

	int startScene = -1;
	int endScene = -1;

	if (argc >= 3) {
		startScene = atoi(argv[2]);
		endScene = startScene;
	} else {
		// Try all scenes from 1 to 512
		startScene = 1;
		endScene = 512;
	}

	printf("; MACS2 Script Runtime Variables (type 0xFF, read-only)\n");
	printf("; These are computed by the engine at read-time, not stored in script data.\n");
	printf("; $interacted_use         (FF:01) Object ID interacted with via Use/UseInventory cursor\n");
	printf("; $interacted_look        (FF:02) Object ID interacted with via Look cursor\n");
	printf("; $interacted_talk        (FF:03) Object ID interacted with via Talk cursor\n");
	printf("; $char_area              (FF:04) Area ID at protagonist's current position\n");
	printf("; $repeat_run_flag        (FF:0B) 1 during repeat/object script pass, 0 otherwise\n");
	printf("; $chosen_dialogue_option (FF:0D) Index of last chosen dialogue option\n");
	printf("; $path_walkable_result   (FF:23) 1 if last TEST_PATH_WALKABLE succeeded\n");
	printf("; $actor_x                (FF:24) Protagonist X position\n");
	printf("; $actor_y                (FF:25) Protagonist Y position\n");
	printf("; $is_scene_init          (FF:26) 1 during scene initialization pass\n");
	printf("; $repeat_char_area       (FF:27) Area at char position (only during repeat run)\n");
	printf("; $inventory_check_result (FF:28) 1 if last CHECK_INVENTORY matched\n");
	printf("; $anim_range_test_result (FF:29) 1 if last TEST_*_ANIM_FRAME matched\n");
	printf("; $inventory_combine_flag (FF:2A) 1 if inventory combine pending (no UI open)\n");
	printf("; $inventory_action_flag  (FF:2B) 1 if inventory action pending (no UI open)\n");
	printf("; $interacted_panel_use   (FF:2C) Object ID interacted with via PanelUse cursor\n");
	printf("; $current_scene          (FF:2D) Current scene index\n");
	printf("; $last_scene             (FF:2F) Previous scene index\n");
	printf("; $music_enabled          (FF:30) 1 if music enabled and sound system active\n");
	printf("; $sound_enabled          (FF:31) 1 if sound enabled and sound system active\n");
	printf(";\n");
	printf("; Script variables: var[1]..var[2048] (read/write, all zeroed on scene load)\n");
	printf("; Object IDs: raw value - 0x400 = object index (1..512)\n");
	printf(";\n\n");

	for (int scene = startScene; scene <= endScene; scene++) {
		if (loadSceneScript(f, (uint16_t)scene)) {
			printf("=== Scene %d (size: %u bytes) ===\n", scene, scriptSize);
			disassemble();
			printf("\n");
			free(scriptData);
			scriptData = nullptr;
			scriptSize = 0;
		}
	}

	fclose(f);
	return 0;
}
