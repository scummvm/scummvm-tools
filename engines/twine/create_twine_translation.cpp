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

/**
 * LBA (TwinE) TEXT.HQR translation tool
 *
 * Workflow:
 *   1. Extract: create_twine_translation extract <TEXT.HQR> <output.pot> [--lang N] [--lba1|--lba2]
 *   2. Translate the .po file (Poedit, Weblate, etc.)
 *   3. Pack:    create_twine_translation pack <translated.po> <template TEXT.HQR> <output TEXT.HQR> [--lang N]
 *
 * PO format:
 *   msgctxt "bank:2:slot:0:textId:123"
 *   msgid "Original dialog line"
 *   msgstr "Translated dialog line"
 */

#include "common/file.h"
#include "common/memstream.h"
#include "engines/twine/hqr.h"

#include <map>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

namespace {

static const char *const kBankNames[] = {
	"Options_and_menus",
	"Credits",
	"Inventory_Intro_and_Holomap",
	"Citadel_Island",
	"Principal_Island",
	"White_Leaf_Desert",
	"Proxima_Island",
	"Rebellion_Island",
	"Hamalayi_mountains_southern_range",
	"Hamalayi_mountains_northern_range",
	"Tippet_Island",
	"Brundle_Island",
	"Fortress_Island",
	"Polar_Island",
	"Extra_Bank",
	nullptr
};

struct GameTextInfo {
	bool lba1;
	int entryCount;
	int numBanks;
	int numLanguages;
};

struct TextLine {
	int slot;
	int16_t textId;
	std::string text;
};

static void printUsage(const char *bin) {
	printf("LBA TEXT.HQR translation tool\n\n");
	printf("Usage:\n");
	printf("  %s extract <TEXT.HQR> <output.pot> [--lang N] [--lba1|--lba2]\n", bin);
	printf("  %s pack <translated.po> <template TEXT.HQR> <output TEXT.HQR> [--lang N]\n\n", bin);
	printf("Options:\n");
	printf("  --lang N   Language index to extract or replace (default: 0)\n");
	printf("  --lba1     Force LBA 1 layout (28 entries per language, 14 banks)\n");
	printf("  --lba2     Force LBA 2 layout (30 entries per language, 15 banks)\n");
}

static bool detectGameTextInfo(const Common::Filename &filename, bool forceLba1, bool forceLba2, GameTextInfo &info) {
	const int32 numEntries = TwinE::HQR::numEntries(filename);
	if (numEntries <= 0)
		return false;

	if (forceLba1) {
		info.lba1 = true;
		info.entryCount = 28;
	} else if (forceLba2) {
		info.lba1 = false;
		info.entryCount = 30;
	} else if (numEntries == 140) {
		info.lba1 = true;
		info.entryCount = 28;
	} else if (numEntries == 180) {
		info.lba1 = false;
		info.entryCount = 30;
	} else {
		fprintf(stderr, "Unknown TEXT.HQR size (%d entries). Use --lba1 or --lba2.\n", numEntries);
		return false;
	}

	info.numBanks = info.entryCount / 2;
	info.numLanguages = numEntries / info.entryCount;
	if (info.numLanguages <= 0 || info.numBanks <= 0) {
		fprintf(stderr, "Invalid TEXT.HQR layout\n");
		return false;
	}
	return true;
}

static std::string cp850ToUtf8(const std::string &s) {
	static const uint16_t cp850map[128] = {
		0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7, 0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
		0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9, 0x00FF, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x00D7, 0x0192,
		0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA, 0x00BF, 0x00AE, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
		0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x00C0, 0x00A9, 0x2563, 0x2551, 0x2557, 0x255D, 0x00A2, 0x00A5, 0x2510,
		0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x00E3, 0x00C3, 0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
		0x00F0, 0x00D0, 0x00CA, 0x00CB, 0x00C8, 0x0131, 0x00CD, 0x00CE, 0x00CF, 0x2518, 0x250C, 0x2588, 0x2584, 0x00A6, 0x00CC, 0x2580,
		0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x00F5, 0x00D5, 0x00B5, 0x00FE, 0x00DE, 0x00DA, 0x00DB, 0x00D9, 0x00FD, 0x00DD, 0x00AF, 0x00B4,
		0x00AD, 0x00B1, 0x2017, 0x00BE, 0x00B6, 0x00A7, 0x00F7, 0x00B8, 0x00B0, 0x00A8, 0x00B7, 0x00B9, 0x00B3, 0x00B2, 0x25A0, 0x00A0
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

static std::string utf8ToCp850(const std::string &s) {
	static std::map<uint32_t, uint8_t> reverseMap;
	if (reverseMap.empty()) {
		static const uint16_t cp850map[128] = {
			0x00C7, 0x00FC, 0x00E9, 0x00E2, 0x00E4, 0x00E0, 0x00E5, 0x00E7, 0x00EA, 0x00EB, 0x00E8, 0x00EF, 0x00EE, 0x00EC, 0x00C4, 0x00C5,
			0x00C9, 0x00E6, 0x00C6, 0x00F4, 0x00F6, 0x00F2, 0x00FB, 0x00F9, 0x00FF, 0x00D6, 0x00DC, 0x00F8, 0x00A3, 0x00D8, 0x00D7, 0x0192,
			0x00E1, 0x00ED, 0x00F3, 0x00FA, 0x00F1, 0x00D1, 0x00AA, 0x00BA, 0x00BF, 0x00AE, 0x00AC, 0x00BD, 0x00BC, 0x00A1, 0x00AB, 0x00BB,
			0x2591, 0x2592, 0x2593, 0x2502, 0x2524, 0x00C1, 0x00C2, 0x00C0, 0x00A9, 0x2563, 0x2551, 0x2557, 0x255D, 0x00A2, 0x00A5, 0x2510,
			0x2514, 0x2534, 0x252C, 0x251C, 0x2500, 0x253C, 0x00E3, 0x00C3, 0x255A, 0x2554, 0x2569, 0x2566, 0x2560, 0x2550, 0x256C, 0x00A4,
			0x00F0, 0x00D0, 0x00CA, 0x00CB, 0x00C8, 0x0131, 0x00CD, 0x00CE, 0x00CF, 0x2518, 0x250C, 0x2588, 0x2584, 0x00A6, 0x00CC, 0x2580,
			0x00D3, 0x00DF, 0x00D4, 0x00D2, 0x00F5, 0x00D5, 0x00B5, 0x00FE, 0x00DE, 0x00DA, 0x00DB, 0x00D9, 0x00FD, 0x00DD, 0x00AF, 0x00B4,
			0x00AD, 0x00B1, 0x2017, 0x00BE, 0x00B6, 0x00A7, 0x00F7, 0x00B8, 0x00B0, 0x00A8, 0x00B7, 0x00B9, 0x00B3, 0x00B2, 0x25A0, 0x00A0
		};
		for (int i = 0; i < 128; i++)
			reverseMap[cp850map[i]] = (uint8_t)(0x80 + i);
	}

	std::string out;
	for (size_t i = 0; i < s.size();) {
		unsigned char c = s[i];
		if (c < 0x80) {
			out += (char)c;
			i++;
			continue;
		}

		uint32_t codepoint = 0;
		if ((c & 0xE0) == 0xC0 && i + 1 < s.size()) {
			codepoint = ((c & 0x1F) << 6) | (s[i + 1] & 0x3F);
			i += 2;
		} else if ((c & 0xF0) == 0xE0 && i + 2 < s.size()) {
			codepoint = ((c & 0x0F) << 12) | ((s[i + 1] & 0x3F) << 6) | (s[i + 2] & 0x3F);
			i += 3;
		} else {
			out += '?';
			i++;
			continue;
		}

		auto it = reverseMap.find(codepoint);
		if (it != reverseMap.end())
			out += (char)it->second;
		else if (codepoint < 0x80)
			out += (char)codepoint;
		else
			out += '?';
	}
	return out;
}

static std::string poEscape(const std::string &s) {
	std::string out;
	for (char c : s) {
		if (c == '\\')
			out += "\\\\";
		else if (c == '"')
			out += "\\\"";
		else if (c == '\n')
			out += "\\n";
		else if (c == '\r')
			out += "\\r";
		else if (c == '\t')
			out += "\\t";
		else
			out += c;
	}
	return out;
}

static std::string poUnescape(const std::string &s) {
	std::string out;
	for (size_t i = 0; i < s.size(); i++) {
		if (s[i] == '\\' && i + 1 < s.size()) {
			if (s[i + 1] == 'n') {
				out += '\n';
				i++;
			} else if (s[i + 1] == 'r') {
				out += '\r';
				i++;
			} else if (s[i + 1] == 't') {
				out += '\t';
				i++;
			} else if (s[i + 1] == '\\') {
				out += '\\';
				i++;
			} else if (s[i + 1] == '"') {
				out += '"';
				i++;
			} else {
				out += s[i];
			}
		} else {
			out += s[i];
		}
	}
	return out;
}

static bool parseTextBank(const uint8 *ordData, int32 ordSize, const uint8 *lbtData, int32 lbtSize, bool lba1,
		std::vector<TextLine> &lines) {
	if (!ordData || !lbtData || ordSize < 2 || lbtSize < 4)
		return false;

	const int numIdxEntries = ordSize / 2;
	Common::MemoryReadStream ordStream(ordData, ordSize);
	Common::MemoryReadStream lbtStream(lbtData, lbtSize);

	lines.clear();
	lines.reserve(numIdxEntries);

	for (int entry = 0; entry < numIdxEntries; ++entry) {
		if (lbtStream.pos() + 4 > lbtSize)
			break;

		const int16_t textId = (int16_t)ordStream.readUint16LE();
		uint16 start = lbtStream.readUint16LE();
		const int32 offsetPos = lbtStream.pos();
		const uint16 end = lbtStream.readUint16LE();

		if (end >= (uint16)lbtSize)
			break;

		if (!lba1)
			++start;

		std::string result;
		if (start < lbtSize && end > start) {
			lbtStream.seek(start);
			for (int32 i = start; i < (int32)end - 1 && i < lbtSize; ++i) {
				const char c = (char)lbtStream.readByte();
				if (c == '\0')
					break;
				result += c;
			}
		}

		TextLine line;
		line.slot = entry;
		line.textId = textId;
		line.text = result;
		lines.push_back(line);

		lbtStream.seek(offsetPos);
	}

	return !lines.empty();
}

static std::vector<uint8> buildLbtBlob(const std::vector<std::string> &strings, bool lba1) {
	std::vector<uint16_t> offsets;
	std::vector<uint8> stringData;

	const uint16_t tableSize = (uint16_t)((strings.size() + 1) * 2);
	uint16_t pos = tableSize;

	for (size_t i = 0; i < strings.size(); i++) {
		const uint16_t stored = lba1 ? pos : (uint16_t)(pos - 1);
		offsets.push_back(stored);
		for (unsigned char c : strings[i])
			stringData.push_back(c);
		stringData.push_back(0);
		pos = (uint16_t)(tableSize + stringData.size());
	}
	offsets.push_back(pos);

	std::vector<uint8> out;
	out.reserve(tableSize + stringData.size());
	for (uint16_t off : offsets) {
		out.push_back((uint8)(off & 0xFF));
		out.push_back((uint8)(off >> 8));
	}
	out.insert(out.end(), stringData.begin(), stringData.end());
	return out;
}

static int langBaseIndex(int bank, int language, int entryCount) {
	return bank * 2 + entryCount * language;
}

static const char *bankName(int bank) {
	if (bank >= 0 && kBankNames[bank])
		return kBankNames[bank];
	return "unknown";
}

static int doExtract(const Common::Filename &textFile, const char *outPath, const GameTextInfo &info, int language) {
	if (language < 0 || language >= info.numLanguages) {
		fprintf(stderr, "Invalid language index %d (available: 0-%d)\n", language, info.numLanguages - 1);
		return 1;
	}

	FILE *out = fopen(outPath, "w");
	if (!out) {
		fprintf(stderr, "Error: Cannot create '%s'\n", outPath);
		return 1;
	}

	fprintf(out, "# Little Big Adventure TEXT.HQR translation file\n");
	fprintf(out, "# Copyright (C) ScummVM Team\n");
	fprintf(out, "#\n");
	fprintf(out, "msgid \"\"\n");
	fprintf(out, "msgstr \"\"\n");
	fprintf(out, "\"Project-Id-Version: twine\\n\"\n");
	fprintf(out, "\"Report-Msgid-Bugs-To: scummvm-devel@lists.scummvm.org\\n\"\n");
	fprintf(out, "\"MIME-Version: 1.0\\n\"\n");
	fprintf(out, "\"Content-Type: text/plain; charset=UTF-8\\n\"\n");
	fprintf(out, "\"Content-Transfer-Encoding: 8bit\\n\"\n\n");

	int totalEntries = 0;
	for (int bank = 0; bank < info.numBanks; bank++) {
		const int baseIndex = langBaseIndex(bank, language, info.entryCount);
		const int lbtIndex = baseIndex;
		const int ordIndex = baseIndex + 1;

		TwinE::HQR::HqrPackEntry ordEntry;
		TwinE::HQR::HqrPackEntry lbtEntry;
		if (!TwinE::HQR::loadHqrEntry(textFile, ordIndex, ordEntry) || ordEntry.blank) {
			fprintf(stderr, "Warning: missing ORD entry for bank %d (%s)\n", bank, bankName(bank));
			continue;
		}
		if (!TwinE::HQR::loadHqrEntry(textFile, lbtIndex, lbtEntry) || lbtEntry.blank) {
			fprintf(stderr, "Warning: missing LBT entry for bank %d (%s)\n", bank, bankName(bank));
			continue;
		}

		std::vector<TextLine> lines;
		if (!parseTextBank(ordEntry.data.data(), (int32)ordEntry.data.size(), lbtEntry.data.data(),
					(int32)lbtEntry.data.size(), info.lba1, lines)) {
			fprintf(stderr, "Warning: failed to parse bank %d (%s)\n", bank, bankName(bank));
			continue;
		}

		fprintf(out, "#. bank: %d (%s)\n", bank, bankName(bank));
		for (const TextLine &line : lines) {
			if (line.text.empty())
				continue;
			fprintf(out, "msgctxt \"bank:%d:slot:%d:textId:%d\"\n", bank, line.slot, (int)line.textId);
			fprintf(out, "msgid \"%s\"\n", poEscape(cp850ToUtf8(line.text)).c_str());
			fprintf(out, "msgstr \"\"\n\n");
			totalEntries++;
		}
	}

	fclose(out);
	printf("Extracted %d text entries (language %d) to %s\n", totalEntries, language, outPath);
	return 0;
}

struct PoTranslation {
	int bank;
	int slot;
	std::string text;
};

static int doPack(const char *poPath, const Common::Filename &templateFile, const char *outPath,
		const GameTextInfo &info, int language) {
	if (language < 0 || language >= info.numLanguages) {
		fprintf(stderr, "Invalid language index %d (available: 0-%d)\n", language, info.numLanguages - 1);
		return 1;
	}

	FILE *in = fopen(poPath, "r");
	if (!in) {
		fprintf(stderr, "Error: Cannot open '%s'\n", poPath);
		return 1;
	}

	std::map<std::pair<int, int>, std::string> translations;
	char buf[8192];
	int currentBank = -1;
	int currentSlot = -1;
	bool inMsgstr = false;
	std::string currentMsgstr;

	auto flushEntry = [&]() {
		if (currentBank >= 0 && currentSlot >= 0 && !currentMsgstr.empty()) {
			translations[std::make_pair(currentBank, currentSlot)] = poUnescape(currentMsgstr);
		}
		currentBank = -1;
		currentSlot = -1;
		currentMsgstr.clear();
		inMsgstr = false;
	};

	while (fgets(buf, sizeof(buf), in)) {
		size_t len = strlen(buf);
		while (len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r'))
			buf[--len] = '\0';

		if (strncmp(buf, "msgctxt \"", 9) == 0) {
			flushEntry();
			int bank = 0, slot = 0, textId = 0;
			if (sscanf(buf + 9, "bank:%d:slot:%d:textId:%d", &bank, &slot, &textId) == 3) {
				currentBank = bank;
				currentSlot = slot;
				(void)textId;
			}
			continue;
		}
		if (strncmp(buf, "msgstr ", 7) == 0) {
			inMsgstr = true;
			char *s = strchr(buf + 7, '"');
			currentMsgstr.clear();
			if (s) {
				s++;
				char *e = strrchr(s, '"');
				if (e)
					currentMsgstr = std::string(s, e - s);
			}
			continue;
		}
		if (buf[0] == '"' && inMsgstr) {
			char *s = buf + 1;
			char *e = strrchr(s, '"');
			if (e)
				currentMsgstr += std::string(s, e - s);
			continue;
		}
		if (buf[0] == '\0')
			flushEntry();
	}
	flushEntry();
	fclose(in);

	const int32 numEntries = TwinE::HQR::numEntries(templateFile);
	std::vector<TwinE::HQR::HqrPackEntry> entries((size_t)numEntries);

	for (int32 i = 0; i < numEntries; i++) {
		if (!TwinE::HQR::loadHqrEntry(templateFile, i, entries[(size_t)i])) {
			fprintf(stderr, "Failed to load template entry %d\n", (int)i);
			return 1;
		}
	}

	int replacedBanks = 0;
	for (int bank = 0; bank < info.numBanks; bank++) {
		const int baseIndex = langBaseIndex(bank, language, info.entryCount);
		const int lbtIndex = baseIndex;
		const int ordIndex = baseIndex + 1;
		if (ordIndex < 0 || lbtIndex >= numEntries)
			continue;

		TwinE::HQR::HqrPackEntry &ordEntry = entries[(size_t)ordIndex];
		TwinE::HQR::HqrPackEntry &lbtEntry = entries[(size_t)lbtIndex];
		if (ordEntry.blank || ordEntry.data.empty())
			continue;

		std::vector<TextLine> lines;
		if (lbtEntry.blank) {
			// Create a new LBT from ORD only.
			const int numIdxEntries = (int)(ordEntry.data.size() / 2);
			lines.reserve(numIdxEntries);
			for (int slot = 0; slot < numIdxEntries; slot++) {
				TextLine line;
				line.slot = slot;
				line.textId = (int16_t)(ordEntry.data[slot * 2] | (ordEntry.data[slot * 2 + 1] << 8));
				line.text.clear();
				lines.push_back(line);
			}
		} else if (!parseTextBank(ordEntry.data.data(), (int32)ordEntry.data.size(), lbtEntry.data.data(),
					(int32)lbtEntry.data.size(), info.lba1, lines)) {
			fprintf(stderr, "Warning: failed to parse bank %d (%s), skipping\n", bank, bankName(bank));
			continue;
		}

		std::vector<std::string> packedStrings;
		packedStrings.reserve(lines.size());
		for (const TextLine &line : lines) {
			std::string translated = line.text;
			auto it = translations.find(std::make_pair(bank, line.slot));
			if (it != translations.end() && !it->second.empty())
				translated = it->second;
			packedStrings.push_back(utf8ToCp850(translated));
		}

		lbtEntry.blank = false;
		lbtEntry.data = buildLbtBlob(packedStrings, info.lba1);
		replacedBanks++;
	}

	const Common::Filename outFile(outPath);
	if (!TwinE::HQR::writeHqrArchive(outFile, entries)) {
		fprintf(stderr, "Failed to write %s\n", outPath);
		return 1;
	}

	printf("Packed %d text banks for language %d into %s\n", replacedBanks, language, outPath);
	return 0;
}

} // namespace

int main(int argc, char *argv[]) {
	if (argc < 2) {
		printUsage(argv[0]);
		return 1;
	}

	if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) {
		printUsage(argv[0]);
		return 0;
	}

	int language = 0;
	bool forceLba1 = false;
	bool forceLba2 = false;

	for (int i = 2; i < argc; i++) {
		if (strcmp(argv[i], "--lang") == 0 && i + 1 < argc) {
			language = atoi(argv[++i]);
		} else if (strcmp(argv[i], "--lba1") == 0) {
			forceLba1 = true;
		} else if (strcmp(argv[i], "--lba2") == 0) {
			forceLba2 = true;
		}
	}

	if (strcmp(argv[1], "extract") == 0) {
		if (argc < 4) {
			printUsage(argv[0]);
			return 1;
		}
		const Common::Filename textFile(argv[2]);
		GameTextInfo info;
		if (!detectGameTextInfo(textFile, forceLba1, forceLba2, info))
			return 1;
		return doExtract(textFile, argv[3], info, language);
	}

	if (strcmp(argv[1], "pack") == 0) {
		if (argc < 5) {
			printUsage(argv[0]);
			return 1;
		}
		const Common::Filename templateFile(argv[3]);
		GameTextInfo info;
		if (!detectGameTextInfo(templateFile, forceLba1, forceLba2, info))
			return 1;
		return doPack(argv[2], templateFile, argv[4], info, language);
	}

	printUsage(argv[0]);
	return 1;
}
