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
 * PO format (Weblate / game-translations compatible):
 *   # 1:Options and Menus dialogs
 *   #: 1:0
 *   msgid "Normal"
 *   msgstr "..."
 *
 * The first number in the comments is (bank * 2 + 1); the second is the slot index.
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

static const char *const kBankDisplayNames[] = {
	"Options and Menus",
	"Credits",
	"Inventory, Intro and Holomap",
	"Citadel Island",
	"Principal Island",
	"White Leaf Desert",
	"Proxima Island",
	"Rebellion Island",
	"Hamalayi Mountains - Southern Range",
	"Hamalayi Mountains - Northern Range",
	"Tippet Island",
	"Brundle Island",
	"Fortress Island",
	"Polar Island",
	nullptr
};

static const char *const kBankDisplayNamesLba2[] = {
	"Options and Menus",
	"Credits",
	"Inventory and Holomap",
	"Citadel Island",
	"Unused",
	"Desert Island",
	"Emerald Moon",
	"Otringal",
	"Celebration Island",
	"Wannies Island",
	"Mosquibees Island",
	"Francos Island",
	"Island CX",
	"Undergas elevator",
	"Volcano Island",
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
	uint8_t flag;
	std::string text;
};

struct TextBankIndices {
	int ordIndex;
	int lbtIndex;
};

static TextBankIndices getTextBankIndices(const GameTextInfo &info, int bank, int language) {
	const int base = bank * 2 + info.entryCount * language;
	TextBankIndices indices;
	indices.lbtIndex = base;
	if (!info.lba1 && bank == 0)
		indices.ordIndex = base + 3;
	else
		indices.ordIndex = base + 1;
	return indices;
}

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

		uint8 flag = 0;
		uint16 textStart = start;
		if (!lba1) {
			if (textStart < lbtSize)
				flag = lbtData[textStart];
			++textStart;
		}

		std::string result;
		if (textStart < lbtSize && end > textStart) {
			lbtStream.seek(textStart);
			for (int32 i = textStart; i < (int32)end - 1 && i < lbtSize; ++i) {
				const char c = (char)lbtStream.readByte();
				if (c == '\0')
					break;
				result += c;
			}
		}

		TextLine line;
		line.slot = entry;
		line.textId = textId;
		line.flag = flag;
		line.text = result;
		lines.push_back(line);

		lbtStream.seek(offsetPos);
	}

	return !lines.empty();
}

static std::vector<uint8> buildLbtBlob(const std::vector<TextLine> &lines, bool lba1) {
	std::vector<uint16_t> offsets;
	std::vector<uint8> stringData;

	const uint16_t tableSize = (uint16_t)((lines.size() + 1) * 2);
	uint16_t pos = tableSize;

	for (const TextLine &line : lines) {
		const uint16_t stored = lba1 ? pos : (uint16_t)(pos - 1);
		offsets.push_back(stored);
		if (!lba1)
			stringData.push_back(line.flag);
		for (unsigned char c : line.text)
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


static const char *bankName(int bank) {
	if (bank >= 0 && kBankNames[bank])
		return kBankNames[bank];
	return "unknown";
}

static const char *bankDisplayName(int bank, bool lba1) {
	const char *const *names = lba1 ? kBankDisplayNames : kBankDisplayNamesLba2;
	if (bank >= 0 && names[bank])
		return names[bank];
	return bankName(bank);
}

static int bankFromDisplaySection(const char *section, bool lba1) {
	if (!section)
		return -1;

	const char *const *names = lba1 ? kBankDisplayNames : kBankDisplayNamesLba2;
	for (int bank = 0; names[bank]; bank++) {
		const char *name = names[bank];
		const size_t nameLen = strlen(name);
		if (strncmp(section, name, nameLen) == 0)
			return bank;
	}
	// Accept LBA1 section names when packing LBA2 PO files from mixed sources.
	if (!lba1) {
		for (int bank = 0; kBankDisplayNames[bank]; bank++) {
			const char *name = kBankDisplayNames[bank];
			const size_t nameLen = strlen(name);
			if (strncmp(section, name, nameLen) == 0)
				return bank;
		}
	}
	return -1;
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

	fprintf(out, "# Little Big Adventure %d language file\n", info.lba1 ? 1 : 2);
	fprintf(out, "# Copyright (C) ScummVM Team\n");
	fprintf(out, "# This file is distributed under the same license as the ScummVM package.\n");
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
		const TextBankIndices indices = getTextBankIndices(info, bank, language);
		const int lbtIndex = indices.lbtIndex;
		const int ordIndex = indices.ordIndex;

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

		const int bankRef = bank * 2 + 1;
		for (const TextLine &line : lines) {
			fprintf(out, "# %d:%s dialogs\n", bankRef, bankDisplayName(bank, info.lba1));
			fprintf(out, "#: %d:%d\n", bankRef, line.slot);
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
	bool hasEntry = false;
	bool inMsgstr = false;
	std::string currentMsgstr;

	auto flushEntry = [&]() {
		if (hasEntry && currentBank >= 0 && currentSlot >= 0 && !currentMsgstr.empty()) {
			translations[std::make_pair(currentBank, currentSlot)] = poUnescape(currentMsgstr);
		}
		currentSlot = -1;
		hasEntry = false;
		currentMsgstr.clear();
		inMsgstr = false;
	};

	while (fgets(buf, sizeof(buf), in)) {
		size_t len = strlen(buf);
		while (len > 0 && (buf[len - 1] == '\n' || buf[len - 1] == '\r'))
			buf[--len] = '\0';

		if (buf[0] == '#' && buf[1] == ' ') {
			const char *section = strchr(buf + 2, ':');
			if (section) {
				flushEntry();
				section++;
				const int bank = bankFromDisplaySection(section, info.lba1);
				if (bank >= 0)
					currentBank = bank;
			}
			continue;
		}
		if (strncmp(buf, "#: ", 3) == 0) {
			flushEntry();
			int refId = 0, slot = 0;
			if (sscanf(buf + 3, "%d:%d", &refId, &slot) == 2) {
				currentSlot = slot;
				if (currentBank < 0 && refId > 0)
					currentBank = (refId - 1) / 2;
				hasEntry = true;
			}
			continue;
		}
		if (strncmp(buf, "msgctxt \"", 9) == 0) {
			flushEntry();
			int bank = 0, slot = 0, textId = 0;
			if (sscanf(buf + 9, "bank:%d:slot:%d:textId:%d", &bank, &slot, &textId) == 3) {
				currentBank = bank;
				currentSlot = slot;
				hasEntry = true;
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

	if (translations.empty()) {
		const Common::Filename outFile(outPath);
		const Common::Filename inFile(templateFile);
		if (inFile.getFullPath() != outFile.getFullPath()) {
			if (!TwinE::HQR::copyHqrFile(templateFile, outFile)) {
				fprintf(stderr, "Failed to copy %s to %s\n", templateFile.getFullPath().c_str(), outPath);
				return 1;
			}
		}
		printf("Packed 0 text banks for language %d into %s (unchanged)\n", language, outPath);
		return 0;
	}

	std::map<int32_t, std::vector<uint8>> patches;
	int replacedBanks = 0;

	for (int bank = 0; bank < info.numBanks; bank++) {
		const TextBankIndices indices = getTextBankIndices(info, bank, language);
		const int lbtIndex = indices.lbtIndex;
		const int ordIndex = indices.ordIndex;

		TwinE::HQR::HqrPackEntry ordEntry;
		TwinE::HQR::HqrPackEntry lbtEntry;
		if (!TwinE::HQR::loadHqrEntry(templateFile, ordIndex, ordEntry) || ordEntry.blank)
			continue;
		if (!TwinE::HQR::loadHqrEntry(templateFile, lbtIndex, lbtEntry) || lbtEntry.blank)
			continue;

		std::vector<TextLine> lines;
		if (!parseTextBank(ordEntry.data.data(), (int32)ordEntry.data.size(), lbtEntry.data.data(),
					(int32)lbtEntry.data.size(), info.lba1, lines)) {
			fprintf(stderr, "Warning: failed to parse bank %d (%s), skipping\n", bank, bankName(bank));
			continue;
		}

		bool changed = false;
		for (TextLine &line : lines) {
			auto it = translations.find(std::make_pair(bank, line.slot));
			if (it != translations.end()) {
				const std::string packed = utf8ToCp850(it->second);
				if (packed != line.text) {
					line.text = packed;
					changed = true;
				}
			}
		}

		if (!changed)
			continue;

		patches[lbtIndex] = TwinE::HQR::makeUncompressedDiskBlock(buildLbtBlob(lines, info.lba1));
		replacedBanks++;
	}

	const Common::Filename outFile(outPath);
	const Common::Filename inFile(templateFile);
	Common::Filename writeTarget(outPath);
	std::string tempPath;
	const bool inPlace = (inFile.getFullPath() == outFile.getFullPath());

	if (inPlace && !patches.empty()) {
		tempPath = std::string(outPath) + ".tmp";
		writeTarget = Common::Filename(tempPath.c_str());
	}

	if (!TwinE::HQR::patchHqrArchive(templateFile, writeTarget, patches)) {
		fprintf(stderr, "Failed to write %s\n", outPath);
		return 1;
	}

	if (inPlace && !patches.empty()) {
		if (rename(tempPath.c_str(), outPath) != 0) {
			fprintf(stderr, "Failed to replace %s\n", outPath);
			return 1;
		}
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
