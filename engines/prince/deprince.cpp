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
#include "common/util.h"

#include <assert.h>

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

void printUsage(const char *appName) {
	printf("Usage: %s skrypt.dat\n", appName);
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

	scriptFile.seek(0, SEEK_SET);

	ScriptInfo scriptInfo;

	scriptInfo.rooms = scriptFile.readSint32LE();
	scriptInfo.startGame = scriptFile.readSint32LE();
	scriptInfo.restoreGame = scriptFile.readSint32LE();
	scriptInfo.stdExamine = scriptFile.readSint32LE();
	scriptInfo.stdPickup = scriptFile.readSint32LE();
	scriptInfo.stdUse = scriptFile.readSint32LE();
	scriptInfo.stdOpen = scriptFile.readSint32LE();
	scriptInfo.stdClose = scriptFile.readSint32LE();
	scriptInfo.stdTalk = scriptFile.readSint32LE();
	scriptInfo.stdGive = scriptFile.readSint32LE();
	scriptInfo.usdCode = scriptFile.readSint32LE();
	scriptInfo.invObjExam = scriptFile.readSint32LE();
	scriptInfo.invObjUse = scriptFile.readSint32LE();
	scriptInfo.invObjUU = scriptFile.readSint32LE();
	scriptInfo.stdUseItem = scriptFile.readSint32LE();
	scriptInfo.lightSources = scriptFile.readSint32LE();
	scriptInfo.specRout = scriptFile.readSint32LE();
	scriptInfo.invObjGive = scriptFile.readSint32LE();
	scriptInfo.stdGiveItem = scriptFile.readSint32LE();
	scriptInfo.goTester = scriptFile.readSint32LE();

	printf("Rooms: %d\n", scriptInfo.rooms);
	printf("StartGame: %d\n", scriptInfo.startGame);

	scriptFile.close();

	return 0;
}
