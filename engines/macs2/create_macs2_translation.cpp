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

/*
 * MACS2 Translation File Creator
 *
 * Workflow:
 *   1. Extract: create_macs2_translation extract <RESOURCE.MCS> <output.pot>
 *   2. Translate the .po file (Poedit, Weblate, etc.)
 *   3. Pack:    create_macs2_translation pack <translated.po> <macs2_translation.dat>
 *
 * PO format:
 *   msgctxt "scene:2"  (or "object:42")
 *   msgid "line1\nline2\nline3"
 *   msgstr "translated1\ntranslated2\ntranslated3"
 *
 * Each msgid groups consecutive strings that form one dialog/description unit.
 * The \n separates individual lines that the engine displays separately.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <algorithm>
#include <string>
#include <vector>
#include <map>
#include <set>

static FILE *resFile = nullptr;

static uint32_t readU32(FILE *f) {
	uint8_t buf[4];
	fread(buf, 1, 4, f);
	return buf[0] | (buf[1] << 8) | (buf[2] << 16) | (buf[3] << 24);
}

static uint16_t readU16(FILE *f) {
	uint8_t buf[2];
	fread(buf, 1, 2, f);
	return buf[0] | (buf[1] << 8);
}

static void writeU32(FILE *f, uint32_t v) {
	uint8_t buf[4] = {(uint8_t)(v & 0xFF), (uint8_t)((v >> 8) & 0xFF),
	                  (uint8_t)((v >> 16) & 0xFF), (uint8_t)((v >> 24) & 0xFF)};
	fwrite(buf, 1, 4, f);
}

static void writeU16(FILE *f, uint16_t v) {
	uint8_t buf[2] = {(uint8_t)(v & 0xFF), (uint8_t)((v >> 8) & 0xFF)};
	fwrite(buf, 1, 2, f);
}

// CP850 to UTF-8
static std::string cp850ToUtf8(const std::string &s) {
	static const uint16_t cp850map[128] = {
		0x00C7,0x00FC,0x00E9,0x00E2,0x00E4,0x00E0,0x00E5,0x00E7,
		0x00EA,0x00EB,0x00E8,0x00EF,0x00EE,0x00EC,0x00C4,0x00C5,
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
	std::string out;
	for (unsigned char c : s) {
		if (c < 0x80) {
			out += (char)c;
		} else {
			uint16_t u = cp850map[c - 0x80];
			if (u < 0x80) {
				out += (char)u;
			} else if (u < 0x800) {
				out += (char)(0xC0 | (u >> 6));
				out += (char)(0x80 | (u & 0x3F));
			} else {
				out += (char)(0xE0 | (u >> 12));
				out += (char)(0x80 | ((u >> 6) & 0x3F));
				out += (char)(0x80 | (u & 0x3F));
			}
		}
	}
	return out;
}

static std::string decryptString(const uint8_t *data, uint16_t length) {
	std::string result;
	for (int i = 1; i <= length; i++) {
		uint8_t x = (uint8_t)(i * i * 0x0C);
		uint8_t y = (uint8_t)(data[i - 1] ^ i);
		uint8_t r = (uint8_t)(x ^ y);
		result += (char)r;
	}
	return result;
}

static std::vector<std::string> readStringsFromBlob(uint32_t blobOffset) {
	std::vector<std::string> result;
	fseek(resFile, blobOffset, SEEK_SET);
	uint16_t totalSize = readU16(resFile);
	if (totalSize == 0)
		return result;
	std::vector<uint8_t> data(totalSize);
	fread(data.data(), 1, totalSize, resFile);
	uint32_t pos = 0;
	while (pos + 2 <= totalSize) {
		uint16_t len = data[pos] | (data[pos + 1] << 8);
		pos += 2;
		if (len == 0 || pos + len > totalSize)
			break;
		result.push_back(decryptString(data.data() + pos, len));
		pos += len;
	}
	return result;
}

// Compute byte offset -> string index mapping for a blob
static std::vector<uint32_t> computeStringOffsets(uint32_t blobOffset) {
	std::vector<uint32_t> offsets; // offsets[stringIndex] = byte offset
	fseek(resFile, blobOffset, SEEK_SET);
	uint16_t totalSize = readU16(resFile);
	if (totalSize == 0)
		return offsets;
	std::vector<uint8_t> data(totalSize);
	fread(data.data(), 1, totalSize, resFile);
	uint32_t pos = 0;
	while (pos + 2 <= totalSize) {
		offsets.push_back(pos);
		uint16_t len = data[pos] | (data[pos + 1] << 8);
		pos += 2;
		if (len == 0 || pos + len > totalSize)
			break;
		pos += len;
	}
	return offsets;
}

// A string reference found in a script: (byteOffset, numLines)
struct StringRef {
	uint16_t offset;
	uint16_t numLines;
};

// Parse a scene/object script to find all string references (offset, numLines)
static std::vector<StringRef> parseScriptStringRefs(uint32_t scriptDataOffset) {
	std::vector<StringRef> refs;
	fseek(resFile, scriptDataOffset, SEEK_SET);

	// Skip 0x80 bytes of special anim offsets
	fseek(resFile, 0x80, SEEK_CUR);
	uint16_t scriptSize = readU16(resFile);
	if (scriptSize == 0)
		return refs;

	std::vector<uint8_t> script(scriptSize);
	fread(script.data(), 1, scriptSize, resFile);

	uint32_t pos = 0;
	while (pos + 1 < scriptSize) {
		uint8_t opcode = script[pos++];
		if (opcode == 0x00)
			continue;
		if (pos >= scriptSize)
			break;
		uint8_t length = script[pos++];
		uint32_t endPos = pos + length;
		if (endPos > scriptSize)
			break;

		// Opcodes that contain string references:
		// 0x0A (printStringLeft): 2x value(3) + offset(2) + numLines(2) = 10
		// 0x0D (dialogue): 4x value(3) + offset(2) + numLines(2) = 16
		// 0x16 (addDialogueChoice): 1x value(3) + offset(2) + numLines(2) = 7
		// 0x30 (printStringRight): same as 0x0A = 10
		// 0x3A (overlayText): 3x value(3) + offset(2) + numLines(2) = 13

		if (opcode == 0x0A || opcode == 0x30) {
			// 2x value(3) + uint16 offset + uint16 numLines = 10
			if (length >= 10) {
				uint16_t off = script[pos + 6] | (script[pos + 7] << 8);
				uint16_t num = script[pos + 8] | (script[pos + 9] << 8);
				if (num > 0 && num < 200)
					refs.push_back({off, num});
			}
		} else if (opcode == 0x0D) {
			// 4x value(3) + uint16 offset + uint16 numLines
			if (length >= 16) {
				uint16_t off = script[pos + 12] | (script[pos + 13] << 8);
				uint16_t num = script[pos + 14] | (script[pos + 15] << 8);
				if (num > 0 && num < 200)
					refs.push_back({off, num});
			}
		} else if (opcode == 0x16) {
			// 1x value(3) + uint16 offset + uint16 numLines
			if (length >= 7) {
				uint16_t off = script[pos + 3] | (script[pos + 4] << 8);
				uint16_t num = script[pos + 5] | (script[pos + 6] << 8);
				if (num > 0 && num < 200)
					refs.push_back({off, num});
			}
		} else if (opcode == 0x3A) {
			// 3x value(3) + uint16 offset + uint16 entryType
			// Here entryType is numLines=1 always, offset references 1 string
			if (length >= 13) {
				uint16_t off = script[pos + 9] | (script[pos + 10] << 8);
				refs.push_back({off, 1});
			}
		}

		pos = endPos;
	}
	return refs;
}

// Group string indices by their dialog references
// Returns vector of (startIndex, count) pairs, sorted by startIndex
static std::vector<std::pair<int, int>> groupStrings(
		const std::vector<uint32_t> &stringOffsets,
		const std::vector<StringRef> &scriptRefs,
		int totalStrings) {

	// Map byte offset -> string index
	std::map<uint32_t, int> offsetToIndex;
	for (int i = 0; i < (int)stringOffsets.size(); i++)
		offsetToIndex[stringOffsets[i]] = i;

	// Collect which string indices are the start of a group
	std::set<int> groupStarts;
	std::map<int, int> groupSizes; // startIndex -> numLines

	for (const auto &ref : scriptRefs) {
		auto it = offsetToIndex.find(ref.offset);
		if (it != offsetToIndex.end()) {
			int idx = it->second;
			groupStarts.insert(idx);
			// Keep the largest numLines for this start
			if (groupSizes.find(idx) == groupSizes.end() || ref.numLines > (uint16_t)groupSizes[idx])
				groupSizes[idx] = ref.numLines;
		}
	}

	// Build groups. Strings not referenced by any opcode get grouped with
	// the nearest preceding group start, or form their own single-line group.
	std::vector<std::pair<int, int>> groups;
	if (groupStarts.empty()) {
		// No script refs found - put all strings in one group
		if (totalStrings > 0)
			groups.push_back({0, totalStrings});
		return groups;
	}

	for (auto it = groupStarts.begin(); it != groupStarts.end(); ++it) {
		int start = *it;
		int count = groupSizes[start];
		if (start + count > totalStrings)
			count = totalStrings - start;
		groups.push_back({start, count});
	}

	// Add any orphan strings (not covered by any group) as single-line entries
	std::set<int> covered;
	for (const auto &g : groups)
		for (int i = g.first; i < g.first + g.second; i++)
			covered.insert(i);

	for (int i = 0; i < totalStrings; i++) {
		if (covered.find(i) == covered.end())
			groups.push_back({i, 1});
	}

	// Sort by start index
	std::sort(groups.begin(), groups.end());

	// Remove duplicates/overlaps
	std::vector<std::pair<int, int>> merged;
	for (const auto &g : groups) {
		if (!merged.empty() && g.first < merged.back().first + merged.back().second)
			continue; // skip overlap
		merged.push_back(g);
	}
	return merged;
}

static std::string poEscape(const std::string &s) {
	std::string out;
	for (char c : s) {
		if (c == '\\') out += "\\\\";
		else if (c == '"') out += "\\\"";
		else if (c == '\n') out += "\\n";
		else out += c;
	}
	return out;
}

static std::string poUnescape(const std::string &s) {
	std::string out;
	for (size_t i = 0; i < s.size(); i++) {
		if (s[i] == '\\' && i + 1 < s.size()) {
			if (s[i + 1] == 'n') { out += '\n'; i++; }
			else if (s[i + 1] == '\\') { out += '\\'; i++; }
			else if (s[i + 1] == '"') { out += '"'; i++; }
			else out += s[i];
		} else {
			out += s[i];
		}
	}
	return out;
}

static void writePoEntry(FILE *out, const char *ctx, int startIdx,
                         const std::vector<std::string> &strings, int offset, int count) {
	fprintf(out, "msgctxt \"%s:%d\"\n", ctx, startIdx);
	fprintf(out, "msgid \"\"\n");
	for (int i = 0; i < count; i++) {
		std::string line = poEscape(cp850ToUtf8(strings[offset + i]));
		if (i < count - 1)
			fprintf(out, "\"%s\\n\"\n", line.c_str());
		else
			fprintf(out, "\"%s\"\n", line.c_str());
	}
	fprintf(out, "msgstr \"\"\n\n");
}

static int doExtract(const char *resPath, const char *outPath) {
	resFile = fopen(resPath, "rb");
	if (!resFile) {
		fprintf(stderr, "Error: Cannot open '%s'\n", resPath);
		return 1;
	}

	FILE *out = fopen(outPath, "w");
	if (!out) {
		fprintf(stderr, "Error: Cannot create '%s'\n", outPath);
		fclose(resFile);
		return 1;
	}

	fprintf(out, "# MACS2 - Schatz im Silbersee translation file\n");
	fprintf(out, "# Copyright (C) ScummVM Team\n");
	fprintf(out, "#\n");
	fprintf(out, "msgid \"\"\n");
	fprintf(out, "msgstr \"\"\n");
	fprintf(out, "\"Project-Id-Version: macs2\\n\"\n");
	fprintf(out, "\"Report-Msgid-Bugs-To: scummvm-devel@lists.scummvm.org\\n\"\n");
	fprintf(out, "\"MIME-Version: 1.0\\n\"\n");
	fprintf(out, "\"Content-Type: text/plain; charset=UTF-8\\n\"\n");
	fprintf(out, "\"Content-Transfer-Encoding: 8bit\\n\"\n\n");

	int totalEntries = 0;

	// Extract scene strings grouped by dialog
	for (int scene = 1; scene <= 512; scene++) {
		// Get strings offset
		uint32_t strOff;
		fseek(resFile, (uint32_t)scene * 0xC + 0xC + 0x4 - 0x4, SEEK_SET);
		strOff = readU32(resFile);
		if (strOff == 0)
			continue;

		// Get script/data offset
		fseek(resFile, (uint32_t)scene * 0xC + 0xC + 0x4 - 0x8, SEEK_SET);
		uint32_t dataOff = readU32(resFile);

		std::vector<std::string> strings = readStringsFromBlob(strOff);
		if (strings.empty())
			continue;

		std::vector<uint32_t> offsets = computeStringOffsets(strOff);
		std::vector<StringRef> refs;
		if (dataOff != 0)
			refs = parseScriptStringRefs(dataOff);

		auto groups = groupStrings(offsets, refs, (int)strings.size());

		char ctx[64];
		snprintf(ctx, sizeof(ctx), "scene:%d", scene);
		for (const auto &g : groups) {
			writePoEntry(out, ctx, g.first, strings, g.first, g.second);
			totalEntries++;
		}
	}

	// Extract object strings grouped by dialog
	for (int obj = 1; obj <= 512; obj++) {
		// Engine formula: index * 0xC + 0xC + 0x4 + 0x17FC = strings offset address
		uint32_t strAddr = (uint32_t)obj * 0xC + 0xC + 0x4 + 0x17FC;

		fseek(resFile, strAddr, SEEK_SET);
		uint32_t strOff = readU32(resFile);
		if (strOff == 0)
			continue;

		// Script/data offset is 4 bytes before strings offset in the table
		fseek(resFile, strAddr - 4, SEEK_SET);
		uint32_t dataOff = readU32(resFile);

		std::vector<std::string> strings = readStringsFromBlob(strOff);
		if (strings.empty())
			continue;

		std::vector<uint32_t> offsets = computeStringOffsets(strOff);
		std::vector<StringRef> refs;
		if (dataOff != 0)
			refs = parseScriptStringRefs(dataOff);

		auto groups = groupStrings(offsets, refs, (int)strings.size());

		char ctx[64];
		snprintf(ctx, sizeof(ctx), "object:%d", obj);
		for (const auto &g : groups) {
			writePoEntry(out, ctx, g.first, strings, g.first, g.second);
			totalEntries++;
		}
	}

	fclose(out);
	fclose(resFile);
	printf("Extracted %d dialog entries to %s\n", totalEntries, outPath);
	return 0;
}

struct TranslationBlock {
	uint16_t id;
	std::vector<std::string> strings;
};

static int doPack(const char *poPath, const char *outPath) {
	FILE *in = fopen(poPath, "r");
	if (!in) {
		fprintf(stderr, "Error: Cannot open '%s'\n", poPath);
		return 1;
	}

	// Parse PO: msgctxt "scene:N:startIdx" or "object:N:startIdx"
	// msgstr contains \n-separated translated lines
	std::map<uint16_t, std::map<int, std::vector<std::string>>> sceneStrings;
	std::map<uint16_t, std::map<int, std::vector<std::string>>> objectStrings;

	char line[8192];
	bool isScene = false;
	uint16_t currentId = 0;
	int currentStartIdx = 0;
	bool hasCtx = false;
	bool inMsgstr = false;
	bool inMsgid = false;
	std::string currentMsgstr;

	auto flushEntry = [&]() {
		if (hasCtx && !currentMsgstr.empty()) {
			std::string text = poUnescape(currentMsgstr);
			std::vector<std::string> lines;
			size_t start = 0;
			while (start < text.size()) {
				size_t nl = text.find('\n', start);
				if (nl == std::string::npos) {
					lines.push_back(text.substr(start));
					break;
				}
				lines.push_back(text.substr(start, nl - start));
				start = nl + 1;
			}
			if (!lines.empty()) {
				if (isScene)
					sceneStrings[currentId][currentStartIdx] = lines;
				else
					objectStrings[currentId][currentStartIdx] = lines;
			}
		}
		hasCtx = false;
		currentMsgstr.clear();
		inMsgstr = false;
		inMsgid = false;
	};

	while (fgets(line, sizeof(line), in)) {
		size_t len = strlen(line);
		while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r'))
			line[--len] = '\0';

		if (strncmp(line, "msgctxt \"", 9) == 0) {
			flushEntry();
			int id = 0, idx = 0;
			if (sscanf(line + 9, "scene:%d:%d", &id, &idx) == 2) {
				isScene = true; currentId = (uint16_t)id; currentStartIdx = idx; hasCtx = true;
			} else if (sscanf(line + 9, "object:%d:%d", &id, &idx) == 2) {
				isScene = false; currentId = (uint16_t)id; currentStartIdx = idx; hasCtx = true;
			}
			continue;
		}
		if (strncmp(line, "msgstr ", 7) == 0) {
			inMsgstr = true; inMsgid = false;
			char *s = strchr(line + 7, '"');
			if (s) { s++; char *e = strrchr(s, '"'); if (e) currentMsgstr = std::string(s, e - s); }
			continue;
		}
		if (strncmp(line, "msgid ", 6) == 0) { inMsgid = true; inMsgstr = false; continue; }
		if (line[0] == '"' && inMsgstr) {
			char *s = line + 1; char *e = strrchr(s, '"');
			if (e) currentMsgstr += std::string(s, e - s);
			continue;
		}
		if (line[0] == '"' && inMsgid) { continue; } // skip msgid continuation
		if (line[0] == '\0') flushEntry();
	}
	flushEntry();
	fclose(in);

	// Build translation blocks - flatten indexed groups into sequential strings
	std::vector<TranslationBlock> sceneBlocks;
	for (auto &kv : sceneStrings) {
		TranslationBlock block;
		block.id = kv.first;
		// Find max string index
		int maxIdx = 0;
		for (auto &gv : kv.second)
			for (int i = 0; i < (int)gv.second.size(); i++)
				if (gv.first + i > maxIdx) maxIdx = gv.first + i;
		block.strings.resize(maxIdx + 1);
		for (auto &gv : kv.second)
			for (int i = 0; i < (int)gv.second.size(); i++)
				block.strings[gv.first + i] = gv.second[i];
		sceneBlocks.push_back(block);
	}

	std::vector<TranslationBlock> objectBlocks;
	for (auto &kv : objectStrings) {
		TranslationBlock block;
		block.id = kv.first;
		int maxIdx = 0;
		for (auto &gv : kv.second)
			for (int i = 0; i < (int)gv.second.size(); i++)
				if (gv.first + i > maxIdx) maxIdx = gv.first + i;
		block.strings.resize(maxIdx + 1);
		for (auto &gv : kv.second)
			for (int i = 0; i < (int)gv.second.size(); i++)
				block.strings[gv.first + i] = gv.second[i];
		objectBlocks.push_back(block);
	}

	// Write binary DAT
	FILE *out = fopen(outPath, "wb");
	if (!out) { fprintf(stderr, "Error: Cannot create '%s'\n", outPath); return 1; }

	fwrite("MCS2", 1, 4, out);
	writeU16(out, 1);
	writeU16(out, (uint16_t)sceneBlocks.size());
	writeU16(out, (uint16_t)objectBlocks.size());

	long indexStart = ftell(out);
	uint32_t indexSize = ((uint32_t)sceneBlocks.size() + (uint32_t)objectBlocks.size()) * 8;
	for (uint32_t i = 0; i < indexSize; i++) fputc(0, out);

	std::vector<uint32_t> sceneOffsets;
	for (const auto &block : sceneBlocks) {
		sceneOffsets.push_back((uint32_t)ftell(out));
		for (const auto &s : block.strings) {
			writeU16(out, (uint16_t)s.size());
			if (!s.empty()) fwrite(s.data(), 1, s.size(), out);
		}
	}
	std::vector<uint32_t> objectOffsets;
	for (const auto &block : objectBlocks) {
		objectOffsets.push_back((uint32_t)ftell(out));
		for (const auto &s : block.strings) {
			writeU16(out, (uint16_t)s.size());
			if (!s.empty()) fwrite(s.data(), 1, s.size(), out);
		}
	}

	fseek(out, indexStart, SEEK_SET);
	for (size_t i = 0; i < sceneBlocks.size(); i++) {
		writeU16(out, sceneBlocks[i].id);
		writeU16(out, (uint16_t)sceneBlocks[i].strings.size());
		writeU32(out, sceneOffsets[i]);
	}
	for (size_t i = 0; i < objectBlocks.size(); i++) {
		writeU16(out, objectBlocks[i].id);
		writeU16(out, (uint16_t)objectBlocks[i].strings.size());
		writeU32(out, objectOffsets[i]);
	}

	fclose(out);
	printf("Packed %zu scene + %zu object blocks into %s\n",
	       sceneBlocks.size(), objectBlocks.size(), outPath);
	return 0;
}

static void printHelp(const char *bin) {
	printf("MACS2 Translation File Creator\n\n");
	printf("  %s extract <RESOURCE.MCS> <macs2.pot>\n", bin);
	printf("  %s pack <translated.po> <macs2_translation.dat>\n", bin);
}

int main(int argc, char **argv) {
	if (argc < 4 || !strcmp(argv[1], "-h") || !strcmp(argv[1], "--help")) {
		printHelp(argv[0]);
		return 1;
	}
	if (!strcmp(argv[1], "extract")) return doExtract(argv[2], argv[3]);
	if (!strcmp(argv[1], "pack")) return doPack(argv[2], argv[3]);
	printHelp(argv[0]);
	return 1;
}
