/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose
 * names are too numerous to list here. Please refer to the
 * COPYRIGHT file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/* Prince script decompiler */

#include "common/file.h"
#include "common/endian.h"
#include "common/util.h"

#include "utils.h"

#include <assert.h>

static const int16 kMaxRooms = 60;

enum OpCode {
	O_WAITFOREVER,
	O_BLACKPALETTE,
	O_SETUPPALETTE,
	O_INITROOM,
	O_SETSAMPLE,
	O_FREESAMPLE,
	O_PLAYSAMPLE,
	O_PUTOBJECT,
	O_REMOBJECT,
	O_SHOWANIM,
	O_CHECKANIMEND,
	O_FREEANIM,
	O_CHECKANIMFRAME,
	O_PUTBACKANIM,
	O_REMBACKANIM,
	O_CHECKBACKANIMFRAME,
	O_FREEALLSAMPLES,
	O_SETMUSIC,
	O_STOPMUSIC,
	O__WAIT,
	O_UPDATEOFF,
	O_UPDATEON,
	O_UPDATE,
	O_CLS,
	O__CALL,
	O_RETURN,
	O_GO,
	O_BACKANIMUPDATEOFF,
	O_BACKANIMUPDATEON,
	O_CHANGECURSOR,
	O_CHANGEANIMTYPE,
	O__SETFLAG,
	O_COMPARE,
	O_JUMPZ,
	O_JUMPNZ,
	O_EXIT,
	O_ADDFLAG,
	O_TALKANIM,
	O_SUBFLAG,
	O_SETSTRING,
	O_ANDFLAG,
	O_GETMOBDATA,
	O_ORFLAG,
	O_SETMOBDATA,
	O_XORFLAG,
	O_GETMOBTEXT,
	O_MOVEHERO,
	O_WALKHERO,
	O_SETHERO,
	O_HEROOFF,
	O_HEROON,
	O_CLSTEXT,
	O_CALLTABLE,
	O_CHANGEMOB,
	O_ADDINV,
	O_REMINV,
	O_REPINV,
	O_OBSOLETE_GETACTION,
	O_ADDWALKAREA,
	O_REMWALKAREA,
	O_RESTOREWALKAREA,
	O_WAITFRAME,
	O_SETFRAME,
	O_RUNACTION,
	O_COMPAREHI,
	O_COMPARELO,
	O_PRELOADSET,
	O_FREEPRELOAD,
	O_CHECKINV,
	O_TALKHERO,
	O_WAITTEXT,
	O_SETHEROANIM,
	O_WAITHEROANIM,
	O_GETHERODATA,
	O_GETMOUSEBUTTON,
	O_CHANGEFRAMES,
	O_CHANGEBACKFRAMES,
	O_GETBACKANIMDATA,
	O_GETANIMDATA,
	O_SETBGCODE,
	O_SETBACKFRAME,
	O_GETRND,
	O_TALKBACKANIM,
	O_LOADPATH,
	O_GETCHAR,
	O_SETDFLAG,
	O_CALLDFLAG,
	O_PRINTAT,
	O_ZOOMIN,
	O_ZOOMOUT,
	O_SETSTRINGOFFSET,
	O_GETOBJDATA,
	O_SETOBJDATA,
	O_SWAPOBJECTS,
	O_CHANGEHEROSET,
	O_ADDSTRING,
	O_SUBSTRING,
	O_INITDIALOG,
	O_ENABLEDIALOGOPT,
	O_DISABLEDIALOGOPT,
	O_SHOWDIALOGBOX,
	O_STOPSAMPLE,
	O_BACKANIMRANGE,
	O_CLEARPATH,
	O_SETPATH,
	O_GETHEROX,
	O_GETHEROY,
	O_GETHEROD,
	O_PUSHSTRING,
	O_POPSTRING,
	O_SETFGCODE,
	O_STOPHERO,
	O_ANIMUPDATEOFF,
	O_ANIMUPDATEON,
	O_FREECURSOR,
	O_ADDINVQUIET,
	O_RUNHERO,
	O_SETBACKANIMDATA,
	O_VIEWFLC,
	O_CHECKFLCFRAME,
	O_CHECKFLCEND,
	O_FREEFLC,
	O_TALKHEROSTOP,
	O_HEROCOLOR,
	O_GRABMAPA,
	O_ENABLENAK,
	O_DISABLENAK,
	O_GETMOBNAME,
	O_SWAPINVENTORY,
	O_CLEARINVENTORY,
	O_SKIPTEXT,
	O_SETVOICEH,
	O_SETVOICEA,
	O_SETVOICEB,
	O_SETVOICEC,
	O_VIEWFLCLOOP,
	O_FLCSPEED,
	O_OPENINVENTORY,
	O_KRZYWA,
	O_GETKRZYWA,
	O_GETMOB,
	O_INPUTLINE,
	O_SETVOICED,
	O_BREAK_POINT
};

static const char *opcode_names[] = {
	"O_WAITFOREVER",
	"O_BLACKPALETTE",
	"O_SETUPPALETTE",
	"O_INITROOM",
	"O_SETSAMPLE",
	"O_FREESAMPLE",
	"O_PLAYSAMPLE",
	"O_PUTOBJECT",
	"O_REMOBJECT",
	"O_SHOWANIM",
	"O_CHECKANIMEND",
	"O_FREEANIM",
	"O_CHECKANIMFRAME",
	"O_PUTBACKANIM",
	"O_REMBACKANIM",
	"O_CHECKBACKANIMFRAME",
	"O_FREEALLSAMPLES",
	"O_SETMUSIC",
	"O_STOPMUSIC",
	"O__WAIT",
	"O_UPDATEOFF",
	"O_UPDATEON",
	"O_UPDATE",
	"O_CLS",
	"O__CALL",
	"O_RETURN",
	"O_GO",
	"O_BACKANIMUPDATEOFF",
	"O_BACKANIMUPDATEON",
	"O_CHANGECURSOR",
	"O_CHANGEANIMTYPE",
	"O__SETFLAG",
	"O_COMPARE",
	"O_JUMPZ",
	"O_JUMPNZ",
	"O_EXIT",
	"O_ADDFLAG",
	"O_TALKANIM",
	"O_SUBFLAG",
	"O_SETSTRING",
	"O_ANDFLAG",
	"O_GETMOBDATA",
	"O_ORFLAG",
	"O_SETMOBDATA",
	"O_XORFLAG",
	"O_GETMOBTEXT",
	"O_MOVEHERO",
	"O_WALKHERO",
	"O_SETHERO",
	"O_HEROOFF",
	"O_HEROON",
	"O_CLSTEXT",
	"O_CALLTABLE",
	"O_CHANGEMOB",
	"O_ADDINV",
	"O_REMINV",
	"O_REPINV",
	"O_OBSOLETE_GETACTION",
	"O_ADDWALKAREA",
	"O_REMWALKAREA",
	"O_RESTOREWALKAREA",
	"O_WAITFRAME",
	"O_SETFRAME",
	"O_RUNACTION",
	"O_COMPAREHI",
	"O_COMPARELO",
	"O_PRELOADSET",
	"O_FREEPRELOAD",
	"O_CHECKINV",
	"O_TALKHERO",
	"O_WAITTEXT",
	"O_SETHEROANIM",
	"O_WAITHEROANIM",
	"O_GETHERODATA",
	"O_GETMOUSEBUTTON",
	"O_CHANGEFRAMES",
	"O_CHANGEBACKFRAMES",
	"O_GETBACKANIMDATA",
	"O_GETANIMDATA",
	"O_SETBGCODE",
	"O_SETBACKFRAME",
	"O_GETRND",
	"O_TALKBACKANIM",
	"O_LOADPATH",
	"O_GETCHAR",
	"O_SETDFLAG",
	"O_CALLDFLAG",
	"O_PRINTAT",
	"O_ZOOMIN",
	"O_ZOOMOUT",
	"O_SETSTRINGOFFSET",
	"O_GETOBJDATA",
	"O_SETOBJDATA",
	"O_SWAPOBJECTS",
	"O_CHANGEHEROSET",
	"O_ADDSTRING",
	"O_SUBSTRING",
	"O_INITDIALOG",
	"O_ENABLEDIALOGOPT",
	"O_DISABLEDIALOGOPT",
	"O_SHOWDIALOGBOX",
	"O_STOPSAMPLE",
	"O_BACKANIMRANGE",
	"O_CLEARPATH",
	"O_SETPATH",
	"O_GETHEROX",
	"O_GETHEROY",
	"O_GETHEROD",
	"O_PUSHSTRING",
	"O_POPSTRING",
	"O_SETFGCODE",
	"O_STOPHERO",
	"O_ANIMUPDATEOFF",
	"O_ANIMUPDATEON",
	"O_FREECURSOR",
	"O_ADDINVQUIET",
	"O_RUNHERO",
	"O_SETBACKANIMDATA",
	"O_VIEWFLC",
	"O_CHECKFLCFRAME",
	"O_CHECKFLCEND",
	"O_FREEFLC",
	"O_TALKHEROSTOP",
	"O_HEROCOLOR",
	"O_GRABMAPA",
	"O_ENABLENAK",
	"O_DISABLENAK",
	"O_GETMOBNAME",
	"O_SWAPINVENTORY",
	"O_CLEARINVENTORY",
	"O_SKIPTEXT",
	"O_SETVOICEH",
	"O_SETVOICEA",
	"O_SETVOICEB",
	"O_SETVOICEC",
	"O_VIEWFLCLOOP",
	"O_FLCSPEED",
	"O_OPENINVENTORY",
	"O_KRZYWA",
	"O_GETKRZYWA",
	"O_GETMOB",
	"O_INPUTLINE",
	"O_SETVOICED",
	"O_BREAK_POINT"
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
};

void printUsage(const char *appName) {
	printf("Usage: %s skrypt.dat\n", appName);
}

void decompile(const char *sname, int pos) {
	printf("Script %s\n", sname);
}

int main(int argc, char *argv[]) {
	if (argc != 2) {
		printUsage(argv[0]);
		return 1;
	}

	Common::File scriptFile(argv[1], "rb");
	if (!scriptFile.isOpen()) {
		error("couldn't load file '%s'", argv[1]);
		return 1;
	}

	uint32 size = scriptFile.size();
	uint8 *data = new uint8[size];
	assert(data);
	if (size != scriptFile.read_noThrow(data, size)) {
		delete [] data;
		error("couldn't read all bytes from file '%s'", argv[1]);
		return 1;
	}

	scriptFile.close();

	Decompressor dec;
	uint32 decompLen = READ_BE_UINT32(data + 14);
	byte *decompData = (byte *)malloc(decompLen);
	dec.decompress(data + 18, decompData, decompLen);
	delete [] data;

	byte *pos = decompData;

	ScriptInfo scriptInfo;

	scriptInfo.rooms = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.startGame = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.restoreGame = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdExamine = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdPickup = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdUse = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdOpen = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdClose = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdTalk = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdGive = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.usdCode = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.invObjExam = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.invObjUse = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.invObjUU = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdUseItem = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.lightSources = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.specRout = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.invObjGive = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.stdGiveItem = READ_LE_UINT32(pos); pos += 4;
	scriptInfo.goTester = READ_LE_UINT32(pos); pos += 4;

	printf("Rooms: %d\n", scriptInfo.rooms);
	printf("StartGame: %d\n", scriptInfo.startGame);

	Room rooms[kMaxRooms];

	for (int i = 0; i < kMaxRooms; i++) {
		pos = &decompData[scriptInfo.rooms + i * 64];

		rooms[i].mobs = READ_LE_UINT32(pos); pos += 4;
		rooms[i].backAnim = READ_LE_UINT32(pos); pos += 4;
		rooms[i].obj = READ_LE_UINT32(pos); pos += 4;
		rooms[i].nak = READ_LE_UINT32(pos); pos += 4;
		rooms[i].itemUse = READ_LE_UINT32(pos); pos += 4;
		rooms[i].itemGive = READ_LE_UINT32(pos); pos += 4;
		rooms[i].walkTo = READ_LE_UINT32(pos); pos += 4;
		rooms[i].examine = READ_LE_UINT32(pos); pos += 4;
		rooms[i].pickup = READ_LE_UINT32(pos); pos += 4;
		rooms[i].use = READ_LE_UINT32(pos); pos += 4;
		rooms[i].pushOpen = READ_LE_UINT32(pos); pos += 4;
		rooms[i].pullClose = READ_LE_UINT32(pos); pos += 4;
		rooms[i].talk = READ_LE_UINT32(pos); pos += 4;
		rooms[i].give = READ_LE_UINT32(pos); pos += 4;
	}

	decompile("StartGame", scriptInfo.startGame);

	return 0;
}
