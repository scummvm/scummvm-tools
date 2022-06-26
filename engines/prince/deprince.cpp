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

/* Prince script decompiler */

#include "common/array.h"
#include "common/file.h"
#include "common/endian.h"
#include "common/str.h"
#include "common/util.h"

#include "utils.h"
#include "flags.h"

#include <assert.h>

static const int16 kMaxRooms = 63;
static const int kMaxBackAnims = 64;
static const int kMaxMobs = 64;
static const int kMaxObjects = 64;
static const int kStructSizeBAS = 28;
static const int kStructSizeBASA = 8;

struct OpCodes {
	const char *name;
	const char *params;
	bool nf;
} static opcodes[] = {
	{ "O_WAITFOREVER", "", true },
	{ "O_BLACKPALETTE", "", false },
	{ "O_SETUPPALETTE", "", false },
	{ "O_INITROOM", "f", false /* true */},
	{ "O_SETSAMPLE", "fs", false },
	{ "O_FREESAMPLE", "f", false }, 	// 5
	{ "O_PLAYSAMPLE", "fh", false },
	{ "O_PUTOBJECT", "fff", false },
	{ "O_REMOBJECT", "ff", false },
	{ "O_SHOWANIM", "ff", false },
	{ "O_CHECKANIMEND", "f", false },	// 10
	{ "O_FREEANIM", "f", false },
	{ "O_CHECKANIMFRAME", "ff", false },
	{ "O_PUTBACKANIM", "ffB", false },
	{ "O_REMBACKANIM", "ff", false },
	{ "O_CHECKBACKANIMFRAME", "ff", false }, // 15
	{ "O_FREEALLSAMPLES", "r", false },
	{ "O_SETMUSIC", "h", false },
	{ "O_STOPMUSIC", "", false },
	{ "O__WAIT", "f", false },
	{ "O_UPDATEOFF", "r", false },		// 20
	{ "O_UPDATEON", "r", false },
	{ "O_UPDATE", "r", false },
	{ "O_CLS", "r", false },
	{ "O__CALL", "o", false },
	{ "O_RETURN", "", true },			// 25
	{ "O_GO", "O", false },
	{ "O_BACKANIMUPDATEOFF", "f", false },
	{ "O_BACKANIMUPDATEON", "f", false },
	{ "O_CHANGECURSOR", "f", false },
	{ "O_CHANGEANIMTYPE", "r", false },	// 30
	{ "O__SETFLAG", "df", false },
	{ "O_COMPARE", "df", false },
	{ "O_JUMPZ", "O", false },
	{ "O_JUMPNZ", "O", false },
	{ "O_EXIT", "", true },				// 35
	{ "O_ADDFLAG", "df", false },
	{ "O_TALKANIM", "ff", false },
	{ "O_SUBFLAG", "df", false },
	{ "O_SETSTRING", "v", false },
	{ "O_ANDFLAG", "df", false },		// 40
	{ "O_GETMOBDATA", "dff", false },
	{ "O_ORFLAG", "df", false },
	{ "O_SETMOBDATA", "fff", false },
	{ "O_XORFLAG", "df", false },
	{ "O_GETMOBTEXT", "f", false },		// 45
	{ "O_MOVEHERO", "ffff", false },
	{ "O_WALKHERO", "f", false /* true */ },
	{ "O_SETHERO", "ffff", false },
	{ "O_HEROOFF", "f", false },
	{ "O_HEROON", "f", false },			// 50
	{ "O_CLSTEXT", "f", false },
	{ "O_CALLTABLE", "dt", false },
	{ "O_CHANGEMOB", "ff", false },
	{ "O_ADDINV", "ff", false },
	{ "O_REMINV", "ff", false },		// 55
	{ "O_REPINV", "r", false },
	{ "O_OBSOLETE_GETACTION", "r", false },
	{ "O_ADDWALKAREA", "r", false },
	{ "O_REMWALKAREA", "r", false },
	{ "O_RESTOREWALKAREA", "r", false },// 60
	{ "O_WAITFRAME", "", false /* true */ },
	{ "O_SETFRAME", "ff", false },
	{ "O_RUNACTION", "r", false },
	{ "O_COMPAREHI", "df", false },
	{ "O_COMPARELO", "df", false },		// 65
	{ "O_PRELOADSET", "r", false },
	{ "O_FREEPRELOAD", "r", false },
	{ "O_CHECKINV", "r", false },
	{ "O_TALKHERO", "f", false },
	{ "O_WAITTEXT", "f", false },		// 70
	{ "O_SETHEROANIM", "fS", false },
	{ "O_WAITHEROANIM", "f", false /* true */ },
	{ "O_GETHERODATA", "dff", false },
	{ "O_GETMOUSEBUTTON", "", false },
	{ "O_CHANGEFRAMES", "ffff", false },// 75
	{ "O_CHANGEBACKFRAMES", "ffff", false },
	{ "O_GETBACKANIMDATA", "dff", false },
	{ "O_GETANIMDATA", "dff", false },
	{ "O_SETBGCODE", "o", false },
	{ "O_SETBACKFRAME", "ff", false },	// 80
	{ "O_GETRND", "dh", false },
	{ "O_TALKBACKANIM", "ff", false },
	{ "O_LOADPATH", "s", false },
	{ "O_GETCHAR", "d", false },
	{ "O_SETDFLAG", "do", false },		// 85
	{ "O_CALLDFLAG", "d", false },
	{ "O_PRINTAT", "fff", false },
	{ "O_ZOOMIN", "f", false },
	{ "O_ZOOMOUT", "f", false },
	{ "O_SETSTRINGOFFSET", "r", false },// 90
	{ "O_GETOBJDATA", "dff", false },
	{ "O_SETOBJDATA", "fff", false },
	{ "O_SWAPOBJECTS", "r", false },
	{ "O_CHANGEHEROSET", "ff", false },
	{ "O_ADDSTRING", "r", false },		// 95
	{ "O_SUBSTRING", "f", false },
	{ "O_INITDIALOG", "", false },
	{ "O_ENABLEDIALOGOPT", "f", false },
	{ "O_DISABLEDIALOGOPT", "f", false },
	{ "O_SHOWDIALOGBOX", "f", false },	// 100
	{ "O_STOPSAMPLE", "f", false },
	{ "O_BACKANIMRANGE", "fhff", false },
	{ "O_CLEARPATH", "", false },
	{ "O_SETPATH", "", false },
	{ "O_GETHEROX", "fd", false },		// 105
	{ "O_GETHEROY", "fd", false },
	{ "O_GETHEROD", "fd", false },
	{ "O_PUSHSTRING", "", false },
	{ "O_POPSTRING", "", false },
	{ "O_SETFGCODE", "o", false },		// 110
	{ "O_STOPHERO", "f", false },
	{ "O_ANIMUPDATEOFF", "f", false },
	{ "O_ANIMUPDATEON", "f", false },
	{ "O_FREECURSOR", "", false },
	{ "O_ADDINVQUIET", "ff", false },	// 115
	{ "O_RUNHERO", "ffff", false },
	{ "O_SETBACKANIMDATA", "hhd", false },
	{ "O_VIEWFLC", "f", false },
	{ "O_CHECKFLCFRAME", "f", false },
	{ "O_CHECKFLCEND", "", false },		// 120
	{ "O_FREEFLC", "", false },
	{ "O_TALKHEROSTOP", "f", false },
	{ "O_HEROCOLOR", "ff", false },
	{ "O_GRABMAPA", "", false },
	{ "O_ENABLENAK", "f", false },		// 125
	{ "O_DISABLENAK", "f", false },
	{ "O_GETMOBNAME", "f", false },
	{ "O_SWAPINVENTORY", "f", false },
	{ "O_CLEARINVENTORY", "f", false },
	{ "O_SKIPTEXT", "", false },		// 130
	{ "O_SETVOICEH", "f", false },
	{ "O_SETVOICEA", "f", false },
	{ "O_SETVOICEB", "f", false },
	{ "O_SETVOICEC", "f", false },
	{ "O_VIEWFLCLOOP", "f", false },	// 135
	{ "O_FLCSPEED", "f", false },
	{ "O_OPENINVENTORY", "", false /* true */ },
	{ "O_KRZYWA", "", false },
	{ "O_GETKRZYWA", "", false },
	{ "O_GETMOB", "dff", false },		// 140
	{ "O_INPUTLINE", "", false },
	{ "O_SETVOICED", "f", false },
	{ "O_BREAK_POINT", "r", false }
};

struct ScriptInfo {
	int rooms;
	int startGame;
	int restoreGame;
	int stdExamine;
	int stdPickup;
	int stdUse;
	int stdOpen;
	int stdClose;
	int stdTalk;
	int stdGive;
	int usdCode;
	int invObjExam;
	int invObjUse;
	int invObjUU;
	int stdUseItem;
	int lightSources;
	int specRout;
	int invObjGive;
	int stdGiveItem;
	int goTester;
};

struct Room {
	int mobs; // mob flag offset
	int backAnim; // offset to array of animation numbers
	int obj; // offset to array of object numbers
	int nak; // offset to array of masks
	int itemUse;
	int itemGive;
	int walkTo; // offset to array of WALKTO events or 0
	int examine; // offset to array of EXAMINE events or 0
	int pickup;
	int use;
	int pushOpen;
	int pullClose;
	int talk;
	int give;
	int unk1;
	int unk2;
};

struct Mask {
	uint16 _state; // visible / invisible
	int16 _flags; // turning on / turning off of an mask
	int16 _x1;
	int16 _y1;
	int16 _x2;
	int16 _y2;
	int16 _z;
	int16 _number; // number of mask for background recreating
	int16 _width;
	int16 _height;
};

void printUsage(const char *appName) {
	printf("Usage: %s skrypt.dat|databank.ptc [dump|renum]\n", appName);
}

byte *data;
uint32 dataLen;
bool *dataMark;
bool *dataDecompile;
int numscripts = 0;

bool modeRenum = false;

Common::String *labels;

void loadBackAnim(int anum, int offset, bool printOut = true);

#define ADVANCE() dataMark[pos] = true; pos++
#define ADVANCE2() ADVANCE(); ADVANCE()
#define ADVANCE4() ADVANCE2(); ADVANCE2()

#define ADVANCES() dataMark[pos] = dataDecompile[pos] = true; pos++
#define ADVANCES2() ADVANCES(); ADVANCES()
#define ADVANCES4() ADVANCES2(); ADVANCES2()

void printArray(int offset, int type, int size, bool split = true, bool offsets = false) {
	if (!offset) {
		printf("\n");
		return;
	}

	printf("[");

	int pos = offset;

	for (int i = 0; i < size; i++) {
		if (split && i && !(i % 10))
			printf("\n ");

		if (type == 1) {
			printf("%d", data[pos]); ADVANCE();
		} else if (type == 2) {
			printf("%d", (uint16)READ_LE_UINT16(&data[pos])); ADVANCE2();
		} else if (type == 4) {
			uint32 v = (uint32)READ_LE_UINT32(&data[pos]); ADVANCE4();
			if (offsets && v && !labels[v].empty())
				printf("%s[%d]", labels[v].c_str(), v);
			else
				printf("%d", v);
		} else {
			error("printArray: unknown type %d", type);
		}

		if (i != size - 1)
			printf(", ");
	}

	printf("]\n");
}

int decompile(const char *sname, int pos, bool printOut = false) {
	if (pos == 0)
		return 0;

	if (labels[pos].empty()) {
		labels[pos] = sname;
	} else if ((labels[pos].hasPrefix("script") || labels[pos].hasPrefix("loc")) && strncmp(sname, "loc", 3)) {
		labels[pos] = sname;
	}

	bool nf = false;
	int tableOffset = -1;

	char buf[100];
	uint32 backAnims[20];
	int numBackAnims = 0;

	while (!nf) {
		if (!printOut && dataDecompile[pos])
			break;

		if (printOut) {
			if (!labels[pos].empty()) {
				if (modeRenum)
					printf("\n%s:\n", labels[pos].c_str());
				else
					printf("\n%s: ; %d 0x%x\n", labels[pos].c_str(), pos, pos);
			}
		}

		uint16 op = READ_LE_UINT16(&data[pos]); ADVANCES2();

		if (op >= ARRAYSIZE(opcodes))
			error("Invalid op: %d at %d", op, pos - 2);

		nf = opcodes[op].nf;

		const char *param = opcodes[op].params;

		if (printOut)
			printf("  %s", opcodes[op].name);

		if (*param && printOut)
			printf(" ");

		int v;

		while (*param) {
			switch (*param) {
			case 'f':
				v = READ_LE_UINT16(&data[pos]); ADVANCES2();

				if (!printOut)
					break;

				if (v & 0x8000) {
					printf("%s", Flags::getFlagName(v));
				} else {
					printf("%d", v);
				}
				break;
			case 'h':
				v = READ_LE_UINT16(&data[pos]); ADVANCES2();

				if (printOut)
					printf("%d", v);
				break;
			case 'v':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();

				if (printOut) {
					if (v > 80000)
						printf("variatxt[%d]", v - 80000);
					else
						printf("%d", v);
				}
				break;
			case 'd':
				v = READ_LE_UINT16(&data[pos]); ADVANCES2();

				if (printOut)
					printf("%s", Flags::getFlagName(v));
				break;
			case 'o':
			case 'O':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();
				v = pos + v - 4;

				if (printOut) {
					printf("%s", labels[v].c_str());

					if (!modeRenum)
						printf("<%d>", v);
				} else {
					sprintf(buf, "%s%06d", (*param == 'o' ? "script" : "loc"), v);
					decompile(buf, v);
				}

				break;
			case 'S':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();

				if (v < 100) {
					if (printOut)
						printf("%d", v);
				} else {
					if (!printOut) {
						sprintf(buf, "string%d", v);
						labels[v] = buf;
					}

					if (printOut)
						printf("\"%s\"[%s]", &data[v], labels[v].c_str());

					while (data[v]) {
						dataMark[v] = dataDecompile[v] = true;
						v++;
					}
					dataMark[v] = dataDecompile[v] = true;
				}
				break;
			case 's':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();
				v = pos + v - 4;

				if (!printOut) {
					sprintf(buf, "string%d", v);
					labels[v] = buf;
				}

				if (printOut)
					printf("\"%s\"[%s]", &data[v], labels[v].c_str());

				while (data[v]) {
					dataMark[v] = dataDecompile[v] = true;
					v++;
				}
				dataMark[v] = dataDecompile[v] = true;

				break;
			case 't':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();
				if (tableOffset != -1 && tableOffset != v) {
					error("Duplicate tableOffset: %d vs %d", tableOffset, v);
				}
				tableOffset = v;

				if (printOut)
					printf("<tableOffset>");
				break;
			case 'B':
				v = READ_LE_UINT32(&data[pos]); ADVANCES4();

				if (printOut)
					printf("backanim%d", v);

				if (labels[v].empty() || !printOut) {
					backAnims[numBackAnims++] = v;

					if (printOut) {
						sprintf(buf, "backanim%d", v);
						labels[v] = buf;
					}
				}
				break;
			case 'r':
				error("Unsupported op %s at %d (%x)", opcodes[op].name, pos - 2, pos - 2);
				return pos;
			default:
				error("Unhandled param '%c' for %s", *param, opcodes[op].name);
				return pos;
			}

			param++;

			if (*param && printOut)
				printf(", ");
		}

		if (printOut)
			printf("\n");
	}

	int retpos = pos;

	if (tableOffset != -1 && printOut) {
		printf("\ntableOffset: %d\n", tableOffset);

		printArray(tableOffset, 4, kMaxRooms, true, true);
	}

	if (numBackAnims > 0) {
		for (int i = 0; i < numBackAnims; i++) {
			if (printOut)
				printf("\n");
			loadBackAnim(backAnims[i], backAnims[i], printOut);
		}
	}

	if (tableOffset != -1) {
		pos = tableOffset;

		for (int i = 0; i < kMaxRooms; i++) {
			sprintf(buf, "tableOffset%02d", i);

			uint off = READ_LE_UINT32(&data[pos]); ADVANCES4();

			decompile(buf, off);
		}
	}

	return retpos;
}

void loadMask(int offset) {
	if (!offset)
		return;

	Mask tempMask;

	int pos = offset;
	int n = 0;

	while (1) {
		tempMask._state = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._flags = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._x1 = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._y1 = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._x2 = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._y2 = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._z = READ_LE_UINT16(&data[pos]); ADVANCE2();
		tempMask._number = READ_LE_UINT16(&data[pos]); ADVANCE2();

		printf("  mask%d[%d] state=%d fl=%d x1=%d y1=%d x2=%d y2=%d z=%d number=%d\n", n, pos-16, tempMask._state,
					tempMask._flags, tempMask._x1, tempMask._y1, tempMask._x2, tempMask._y2,
					tempMask._z, tempMask._number);

		if (tempMask._state == 0xffff) {
			break;
		}

		n++;
	}
}

void loadMobEvents(int offset, const char *name, bool printOut) {
	if (!offset)
		return;

	int pos = offset;
	int i = 0;
	int16 mob;
	int32 code;

	char buf[100];

	while (1) {
		mob = (int16)READ_LE_UINT16(&data[pos]); ADVANCE2();
		code = READ_LE_UINT32(&data[pos]); ADVANCE4();

		if (printOut)
			printf("  mob%02d[%d]: mob=%d code=%s\n", i, pos-6, mob, (mob == -1 ? "0" : labels[code].c_str()));

		if (mob == -1)
			break;

		if (!printOut) {
			sprintf(buf, "%s.mob%d", name, i);
			decompile(buf, code, printOut);
		}

		i++;
	}
}

void loadMobEventsWithItem(int offset, const char *name, bool printOut) {
	if (!offset)
		return;

	int pos = offset;
	int i = 0;
	int16 mob;
	int16 item;
	int32 code;

	char buf[100];

	while (1) {
		mob = (int16)READ_LE_UINT16(&data[pos]); ADVANCE2();
		item = READ_LE_UINT16(&data[pos]); ADVANCE2();
		code = READ_LE_UINT32(&data[pos]); ADVANCE4();

		if (printOut)
			printf("  mobitem%02d[%d]: mob=%d item=%d code=%s\n", i, pos-8, mob, item, (mob == -1 ? "0" : labels[code].c_str()));

		if (mob == -1)
			break;

		if (!printOut) {
			sprintf(buf, "%s.mobitem%d", name, i);
			decompile(buf, code, printOut);
		}

		i++;
	}
}

void loadLightSources(int offset) {
	int pos = offset;

	for (int i = 0; i < kMaxRooms; i++) {
		int x = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int y = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int scale = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int unk = READ_LE_UINT16(&data[pos]); ADVANCE2();

		printf("  light%02d[%d]: x=%d y=%d scale=%d unk=%d\n", i, pos-8, x, y, scale, unk);
	}
}

void loadBackAnim(int anum, int offset, bool printOut) {
	if (!offset)
		return;

	int pos = offset;

	// Anim BAS data
	int type = READ_LE_UINT32(&data[pos]); ADVANCE4();
	int bdata = READ_LE_UINT32(&data[pos]); ADVANCE4();
	int anims = READ_LE_UINT32(&data[pos]); ADVANCE4();
	int unk1 = READ_LE_UINT32(&data[pos]); ADVANCE4();
	int unk2 = READ_LE_UINT32(&data[pos]); ADVANCE4();
	int unk3 = READ_LE_UINT32(&data[pos]); ADVANCE4();
	int data2 = READ_LE_UINT32(&data[pos]); ADVANCE4();

	if (printOut)
		printf("backanim%02d[%d]: type=%x data=%x anims=%x unk1=%x unk2=%x unk3=%x data2=%x\n", anum, offset,
			type, bdata, anims, unk1, unk2, unk3, data2);

	if (anims == 0) {
		anims = 1; // anims with 0 as amount in game data has just 1 animation
	}

	for (int i = 0; i < anims; i++) {
		pos = offset + kStructSizeBAS + kStructSizeBASA * i;
		// Anim BASA data
		int num = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int start = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int end = READ_LE_UINT16(&data[pos]); ADVANCE2();
		int unk = READ_LE_UINT16(&data[pos]); ADVANCE2();

		if (printOut)
			printf("  backanim%02d.%d[%d]: num=%d start=%d end=%d unk=%d\n", anum, i, pos-8, num, start, end, unk);
	}
}

int main(int argc, char *argv[]) {
	if (argc < 2) {
		printUsage(argv[0]);
		return 1;
	}

	bool modeDump = false;

	if (argc == 3) {
		if (!scumm_stricmp(argv[2], "dump"))
			modeDump = true;
		if (!scumm_stricmp(argv[2], "renum"))
			modeRenum = true;
	}

	Common::String fname = argv[1];
	fname.toLowercase();

	if (fname.contains("databank.ptc")) {
		Databank databank(argv[1]);
		FileData fdata;

		fdata = databank.loadFile("skrypt.dat");

		if (fdata._size == 0)
			error("databank.ptc does not contain skrypt.dat");

		data = fdata._fileTable;
		dataLen = fdata._size;
	} else {
		// Plain file
		Common::File scriptFile(argv[1], "rb");
		if (!scriptFile.isOpen()) {
			error("couldn't load file '%s'", argv[1]);
			return 1;
		}

		uint32 size = scriptFile.size();
		uint8 *fdata = new uint8[size];
		assert(fdata);
		if (size != scriptFile.read_noThrow(fdata, size)) {
			delete [] fdata;
			error("couldn't read all bytes from file '%s'", argv[1]);
			return 1;
		}

		scriptFile.close();

		Decompressor dec;
		dataLen = READ_BE_UINT32(fdata + 14);
		data = (byte *)malloc(dataLen);
		dec.decompress(fdata + 18, data, dataLen);
		delete [] fdata;
	}

	if (modeDump) {
		Common::File dumpFile("skrypt.dump", "wb");
		if (!dumpFile.isOpen()) {
			error("couldn't load file '%s'", argv[1]);
			return 1;
		}
		dumpFile.write(data, dataLen);
		dumpFile.close();
	}

	dataMark = (bool *)calloc(dataLen, sizeof(bool));
	dataDecompile = (bool *)calloc(dataLen, sizeof(bool));
	labels = new Common::String[dataLen];

	int pos = 0;

	ScriptInfo scriptInfo;

	scriptInfo.rooms = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.startGame = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.restoreGame = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdExamine = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdPickup = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdUse = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdOpen = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdClose = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdTalk = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdGive = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.usdCode = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.invObjExam = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.invObjUse = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.invObjUU = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdUseItem = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.lightSources = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.specRout = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.invObjGive = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.stdGiveItem = READ_LE_UINT32(&data[pos]); ADVANCE4();
	scriptInfo.goTester = READ_LE_UINT32(&data[pos]); ADVANCE4();

	// Decompile offsets
	loadMobEvents(scriptInfo.invObjExam, "invObjExam", false);
	loadMobEvents(scriptInfo.invObjUse, "invObjUse", false);
	loadMobEventsWithItem(scriptInfo.invObjUU, "invObjUU", false);
	loadMobEvents(scriptInfo.invObjGive, "invObjGive", false);

	char buf[100];

	// Load rooms
	Room rooms[kMaxRooms + 1];

	for (int i = 0; i < kMaxRooms + 1; i++) {
		pos = scriptInfo.rooms + i * 64;

		rooms[i].mobs = READ_LE_UINT32(&data[pos]); ADVANCE4();			// byte[kMaxMobs]
		rooms[i].backAnim = READ_LE_UINT32(&data[pos]); ADVANCE4();		// int32[kMaxBackAnims]
		rooms[i].obj = READ_LE_UINT32(&data[pos]); ADVANCE4();			// byte [kMaxObjects]
		rooms[i].nak = READ_LE_UINT32(&data[pos]); ADVANCE4();			// offset pointing to Mask structure
		rooms[i].itemUse = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].itemGive = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].walkTo = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].examine = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].pickup = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].use = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].pushOpen = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].pullClose = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].talk = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].give = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].unk1 = READ_LE_UINT32(&data[pos]); ADVANCE4();
		rooms[i].unk2 = READ_LE_UINT32(&data[pos]); ADVANCE4();

		sprintf(buf, "rooms%02d.itemUse", i);
		loadMobEventsWithItem(rooms[i].itemUse, buf, false);
		sprintf(buf, "rooms%02d.itemGive", i);
		loadMobEventsWithItem(rooms[i].itemGive, buf, false);
		sprintf(buf, "rooms%02d.walkTo", i);
		loadMobEvents(rooms[i].walkTo, buf, false);
		sprintf(buf, "rooms%02d.examine", i);
		loadMobEvents(rooms[i].examine, buf, false);
		sprintf(buf, "rooms%02d.pickup", i);
		loadMobEvents(rooms[i].pickup, buf, false);
		sprintf(buf, "rooms%02d.use", i);
		loadMobEvents(rooms[i].use, buf, false);
		sprintf(buf, "rooms%02d.pushOpen", i);
		loadMobEvents(rooms[i].pushOpen, buf, false);
		sprintf(buf, "rooms%02d.pullClose", i);
		loadMobEvents(rooms[i].pullClose, buf, false);
		sprintf(buf, "rooms%02d.talk", i);
		loadMobEvents(rooms[i].talk, buf, false);
		sprintf(buf, "rooms%02d.give", i);
		loadMobEvents(rooms[i].give, buf, false);
	}

	decompile("startGame", scriptInfo.startGame);
	decompile("restoreGame", scriptInfo.restoreGame);
	decompile("stdExamine", scriptInfo.stdExamine);
	decompile("stdPickup", scriptInfo.stdPickup);
	decompile("stdUse", scriptInfo.stdUse);
	decompile("stdOpen", scriptInfo.stdOpen);
	decompile("stdClose", scriptInfo.stdClose);
	decompile("stdTalk", scriptInfo.stdTalk);
	decompile("stdGive", scriptInfo.stdGive);
	decompile("usdCode", scriptInfo.usdCode);
	decompile("stdUseItem", scriptInfo.stdUseItem);
	decompile("stdGiveItem", scriptInfo.stdGiveItem);
	decompile("specRout", scriptInfo.specRout);
	decompile("goTester", scriptInfo.goTester);

	int nlabel = 1;

	// Heuristics to decompile the rest
	for (uint32 i = 0; i < dataLen; i++) {
		if (!dataMark[i]) {
			if (i > 53000 && i < 124348 && READ_LE_UINT16(&data[i]) < 244) {
				sprintf(buf, "unused%d", (modeRenum ? nlabel : i));
				nlabel++;
				decompile(buf, i);
			} else if (i > 124348) {
				if (data[i] && data[i] < 127) {
					sprintf(buf, "unusedstring%d", i);
					labels[i] = buf;

					while (data[i] != 0) {
						dataMark[i] = true;
						i++;
					}
					dataMark[i] = true;
				}
			}
		}
	}

	// Print out header
	printf("rooms: [%d]\n", scriptInfo.rooms);
	printf("startGame: [%d]\n", scriptInfo.startGame);
	printf("restoreGame: [%d]\n", scriptInfo.restoreGame);
	printf("stdExamine: %s\n", labels[scriptInfo.stdExamine].c_str());
	printf("stdPickup: %s\n", labels[scriptInfo.stdPickup].c_str());
	printf("stdUse: %s\n", labels[scriptInfo.stdUse].c_str());
	printf("stdOpen: %s\n", labels[scriptInfo.stdOpen].c_str());
	printf("stdClose: %s\n", labels[scriptInfo.stdClose].c_str());
	printf("stdTalk: %s\n", labels[scriptInfo.stdTalk].c_str());
	printf("stdGive: %s\n", labels[scriptInfo.stdGive].c_str());
	printf("usdCode: %s\n", labels[scriptInfo.usdCode].c_str());
	printf("invObjExam: [%d]\n", scriptInfo.invObjExam);
	loadMobEvents(scriptInfo.invObjExam, "invObjExam", true);
	printf("end invObjExam\n");
	printf("invObjUse: [%d]\n", scriptInfo.invObjUse);
	loadMobEvents(scriptInfo.invObjUse, "invObjUse", true);
	printf("end invObjUse\n");
	printf("invObjUU: [%d]\n", scriptInfo.invObjUU);
	loadMobEventsWithItem(scriptInfo.invObjUU, "invObjUU", true);
	printf("end invObjUU\n");
	printf("stdUseItem: %s\n", labels[scriptInfo.stdUseItem].c_str());
	printf("lightSources: [%d]\n", scriptInfo.lightSources);
	loadLightSources(scriptInfo.lightSources);
	printf("end lightSources\n");
	printf("specRout: %s\n", labels[scriptInfo.specRout].c_str());
	printf("invObjGive: [%d]\n", scriptInfo.invObjGive);
	loadMobEvents(scriptInfo.invObjGive, "invObjGive", true);
	printf("end invObjGive\n");
	printf("stdGiveItem: %s\n", labels[scriptInfo.stdGiveItem].c_str());
	printf("goTester: %s\n", labels[scriptInfo.goTester].c_str());

	// Print out rooms
	for (int i = 0; i < kMaxRooms + 1; i++) {
		pos = scriptInfo.rooms + i * 64;
		printf("room%02d: [%d]\n", i, pos);

		printf("r%02d mobs: [%d]: ", i, rooms[i].mobs);
		printArray(rooms[i].mobs, 1, kMaxMobs, false);
		printf("r%02d backAnim: [%d]: ", i, rooms[i].backAnim);
		printArray(rooms[i].backAnim, 4, kMaxBackAnims, false);
		printf("r%02d obj: [%d]: ", i, rooms[i].obj);
		printArray(rooms[i].obj, 1, kMaxObjects, false);
		printf("r%02d masks [%d]\n", i, rooms[i].nak);
		loadMask(rooms[i].nak);
		printf("end masks\n");
		printf("r%02d itemUse: [%d]\n", i, rooms[i].itemUse);
		sprintf(buf, "rooms%02d.itemUse", i);
		loadMobEventsWithItem(rooms[i].itemUse, buf, true);
		printf("end itemUse\n");
		printf("r%02d itemGive: [%d]\n", i, rooms[i].itemGive);
		sprintf(buf, "rooms%02d.itemGive", i);
		loadMobEventsWithItem(rooms[i].itemGive, buf, true);
		printf("end itemGive\n");
		printf("r%02d walkTo: [%d]\n", i, rooms[i].walkTo);
		sprintf(buf, "rooms%02d.walkTo", i);
		loadMobEvents(rooms[i].walkTo, buf, true);
		printf("end walkTo\n");
		printf("r%02d examine: [%d]\n", i, rooms[i].examine);
		sprintf(buf, "rooms%02d.examine", i);
		loadMobEvents(rooms[i].examine, buf, true);
		printf("end examine\n");
		printf("r%02d pickup: [%d]\n", i, rooms[i].pickup);
		sprintf(buf, "rooms%02d.pickup", i);
		loadMobEvents(rooms[i].pickup, buf, true);
		printf("end pickup\n");
		printf("r%02d use: [%d]\n", i, rooms[i].use);
		sprintf(buf, "rooms%02d.use", i);
		loadMobEvents(rooms[i].use, buf, true);
		printf("end use\n");
		printf("r%02d pushOpen: [%d]\n", i, rooms[i].pushOpen);
		sprintf(buf, "rooms%02d.pushOpen", i);
		loadMobEvents(rooms[i].pushOpen, buf, true);
		printf("end pushOpen\n");
		printf("r%02d pullClose: [%d]\n", i, rooms[i].pullClose);
		sprintf(buf, "rooms%02d.pullClose", i);
		loadMobEvents(rooms[i].pullClose, buf, true);
		printf("end pullClose\n");
		printf("r%02d talk: [%d]\n", i, rooms[i].talk);
		sprintf(buf, "rooms%02d.talk", i);
		loadMobEvents(rooms[i].talk, buf, true);
		printf("end talk\n");
		printf("r%02d give: [%d]\n", i, rooms[i].give);
		sprintf(buf, "rooms%02d.give", i);
		loadMobEvents(rooms[i].give, buf, true);
		printf("end give\n");
		printf("r%02d unk1: %d\n", i, rooms[i].unk1);
		printf("r%02d unk2: %d\n", i, rooms[i].unk2);

		if (rooms[i].backAnim)
			for (int b = 0; b < kMaxBackAnims; b++) {
				loadBackAnim(b, READ_LE_UINT32(&data[rooms[i].backAnim + b * 4]));
			}
	}

	#if 1
		int n = 0;
		const char *shades[] = {" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█"};
		for (uint i = 0; i < dataLen; i++) {
			if (i % 8 == 0 && i) {
				printf("%s", shades[n]);
				n = 0;
			}

			if (i % 800 == 0 && i)
				printf("\n");

			if (dataMark[i])
				n++;
		}

		printf("\n");
	#endif

	if (modeRenum) {
		const char *pref[] = { "loc", "script", "string", "unusedstring", 0 };

		for (const char **p = pref; *p; p++) {
			int nn = 1;

			for (uint32 i = 0; i < dataLen; i++) {
				if (!labels[i].empty() && labels[i].hasPrefix(*p)) {
					sprintf(buf, "%s%d", *p, nn);
					labels[i] = buf;
					nn++;
				}
			}
		}
	}

	int nunmapped = 0;
	bool inDB = false;

	nlabel = 1;

	for (uint32 i = 0; i < dataLen; i++) {
		if (!labels[i].empty() && !labels[i].hasPrefix("backanim")) {
			if (inDB) {
				printf("\n\n");
				inDB = false;
			}

			if (labels[i].hasPrefix("string") || labels[i].hasPrefix("unusedstring") ) {
				printf("%s:\n  db \"%s\", 0\n", labels[i].c_str(), &data[i]);
			} else {
				i = decompile(labels[i].c_str(), i, true) - 1; // -1 to compensate the for() loop increment
				numscripts++;
			}
		} else if (!dataMark[i]) {
			nunmapped++;

			if (!inDB) {
				if (modeRenum) {
					printf("label%d:\n  db %d", nlabel, data[i]);
					nlabel++;
				} else {
					printf("label%d: ; 0x%x\n  db %d", i, i, data[i]);
				}
				inDB = true;
			} else {
				printf(", %d", data[i]);
			}
		} else {
			if (inDB) {
				printf("\n\n");
				inDB = false;
			}
		}
	}

	printf("\nTotal scripts: %d  Unmapped bytes: %d\n", numscripts, nunmapped);

	return 0;
}
