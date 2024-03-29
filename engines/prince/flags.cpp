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

#include "flags.h"

const char *Flags::getFlagName(uint16 flagId) {
	FlagDebug *flagd = NULL;
	flagd = (FlagDebug *)bsearch(&flagId, _flagNames, kFlagDebugAmount, sizeof(FlagDebug), Flags::compareFlagDebug);
	if (flagd) {
		return flagd->flagName;
	} else {
		return "unknown_flag";
	}
}

int Flags::compareFlagDebug(const void *a, const void *b) {
	const uint32 *flagId = (const uint32 *)a;
	const FlagDebug *entry = (const FlagDebug *)b;
	if (*flagId < (uint32)entry->id) {
		return -1;
	} else if (*flagId > (uint32)entry->id) {
		return 1;
	}
	return 0;
}

const Flags::FlagDebug Flags::_flagNames[Flags::kFlagDebugAmount] = {
	{ Flags::FLAGA1, "FLAGA1" },
	{ Flags::FLAGA2, "FLAGA2" },
	{ Flags::FLAGA3, "FLAGA3" },
	{ Flags::DESTX, "DESTX" },
	{ Flags::DESTY, "DESTY" },
	{ Flags::DESTD, "DESTD" },
	{ Flags::DwarfDone, "DwarfDone" },
	{ Flags::GRABARZCOUNTER, "GRABARZCOUNTER" },
	{ Flags::KIERUNEK, "KIERUNEK" },
	{ Flags::BACKFLAG1, "BACKFLAG1" },
	{ Flags::BACKFLAG2, "BACKFLAG2" },
	{ Flags::BACKFLAG3, "BACKFLAG3" },
	{ Flags::BACKFLAG4, "BACKFLAG4" },
	{ Flags::MACROFLAG1, "MACROFLAG1" },
	{ Flags::MACROFLAG2, "MACROFLAG2" },
	{ Flags::MACROFLAG3, "MACROFLAG3" },
	{ Flags::HEROLDDONE, "HEROLDDONE" },
	{ Flags::BRIDGESET, "BRIDGESET" },
	{ Flags::U_BT_1, "U_BT_1" },
	{ Flags::U_BT_2, "U_BT_2" },
	{ Flags::U_BT_3, "U_BT_3" },
	{ Flags::U_BT_4, "U_BT_4" },
	{ Flags::U_BT_5, "U_BT_5" },
	{ Flags::U_BT_6, "U_BT_6" },
	{ Flags::U_BT_7, "U_BT_7" },
	{ Flags::U_BT_8, "U_BT_8" },
	{ Flags::U_BT_9, "U_BT_9" },
	{ Flags::U_BT_COUNTER, "U_BT_COUNTER" },
	{ Flags::ARIVALDALIVE, "ARIVALDALIVE" },
	{ Flags::TALKCHAR1, "TALKCHAR1" },
	{ Flags::TalkType1, "TalkType1" },
	{ Flags::TALKROUT1, "TALKROUT1" },
	{ Flags::TALKROUT2, "TALKROUT2" },
	{ Flags::TALKROUT3, "TALKROUT3" },
	{ Flags::TALKROUT4, "TALKROUT4" },
	{ Flags::TALKANIM1, "TALKANIM1" },
	{ Flags::TALKANIM2, "TALKANIM2" },
	{ Flags::TALKCOLOR1, "TALKCOLOR1" },
	{ Flags::TALKCOLOR2, "TALKCOLOR2" },
	{ Flags::KapciuchTaken, "KapciuchTaken" },
	{ Flags::CurrentBeggarA, "CurrentBeggarA" },
	{ Flags::TempKapc, "TempKapc" },
	{ Flags::HomTaken, "HomTaken" },
	{ Flags::WizardTalk, "WizardTalk" },
	{ Flags::SunlordTalk, "SunlordTalk" },
	{ Flags::HermitTalk, "HermitTalk" },
	{ Flags::RunyMode, "RunyMode" },
	{ Flags::FatMerchantTalk, "FatMerchantTalk" },
	{ Flags::HotDogTalk, "HotDogTalk" },
	{ Flags::ThiefTalk, "ThiefTalk" },
	{ Flags::BeggarTalk, "BeggarTalk" },
	{ Flags::MonkTalk, "MonkTalk" },
	{ Flags::BardTalk, "BardTalk" },
	{ Flags::BarmanTalk, "BarmanTalk" },
	{ Flags::LeftPlayerTalk, "LeftPlayerTalk" },
	{ Flags::OczySowy, "OczySowy" },
	{ Flags::CzachySpeed1, "CzachySpeed1" },
	{ Flags::CzachySpeed2, "CzachySpeed2" },
	{ Flags::CzachySpeed3, "CzachySpeed3" },
	{ Flags::CzachySlowDown1, "CzachySlowDown1" },
	{ Flags::CzachySlowDown2, "CzachySlowDown2" },
	{ Flags::CzachySlowDown3, "CzachySlowDown3" },
	{ Flags::FjordDane, "FjordDane" },
	{ Flags::GKopany1, "GKopany1" },
	{ Flags::GKopany2, "GKopany2" },
	{ Flags::GKopany3, "GKopany3" },
	{ Flags::GKopany4, "GKopany4" },
	{ Flags::KnowGodWord, "KnowGodWord" },
	{ Flags::TALKROUT21, "TALKROUT21" },
	{ Flags::TALKROUT22, "TALKROUT22" },
	{ Flags::TALKROUT23, "TALKROUT23" },
	{ Flags::TALKROUT24, "TALKROUT24" },
	{ Flags::TalkType2, "TalkType2" },
	{ Flags::GrabarzTalk, "GrabarzTalk" },
	{ Flags::LastTalker, "LastTalker" },
	{ Flags::MapaPustelniaEnabled, "MapaPustelniaEnabled" },
	{ Flags::MapaTempleEnabled, "MapaTempleEnabled" },
	{ Flags::MapaFjordEnabled, "MapaFjordEnabled" },
	{ Flags::MapaSilmanionaEnabled, "MapaSilmanionaEnabled" },
	{ Flags::MapaKurhanEnabled, "MapaKurhanEnabled" },
	{ Flags::MapaDragonEnabled, "MapaDragonEnabled" },
	{ Flags::MapaMillEnabled, "MapaMillEnabled" },
	{ Flags::DwarfRunning, "DwarfRunning" },
	{ Flags::DwarfTalk, "DwarfTalk" },
	{ Flags::CurseLift, "CurseLift" },
	{ Flags::KosciSwapped, "KosciSwapped" },
	{ Flags::BookStolen, "BookStolen" },
	{ Flags::MapaUsable, "MapaUsable" },
	{ Flags::FjordBoss, "FjordBoss" },
	{ Flags::FjordHotDog, "FjordHotDog" },
	{ Flags::FjordLewy, "FjordLewy" },
	{ Flags::FjordPrawy, "FjordPrawy" },
	{ Flags::TalkArivald, "TalkArivald" },
	{ Flags::ShootDone, "ShootDone" },
	{ Flags::ShootRunning, "ShootRunning" },
	{ Flags::ShootKnow, "ShootKnow" },
	{ Flags::MirrorKnow, "MirrorKnow" },
	{ Flags::Gar1stTime, "Gar1stTime" },
	{ Flags::KosciTaken, "KosciTaken" },
	{ Flags::ArivGotSpell, "ArivGotSpell" },
	{ Flags::BookGiven, "BookGiven" },
	{ Flags::Wywieszka, "Wywieszka" },
	{ Flags::TalkSheila, "TalkSheila" },
	{ Flags::TalkSheila2, "TalkSheila2" },
	{ Flags::BackHuman, "BackHuman" },
	{ Flags::SkarbiecOpen, "SkarbiecOpen" },
	{ Flags::LustroTaken, "LustroTaken" },
	{ Flags::GargoyleHom, "GargoyleHom" },
	{ Flags::GargoyleBroken, "GargoyleBroken" },
	{ Flags::FjordDzien, "FjordDzien" },
	{ Flags::GargoyleHom2, "GargoyleHom2" },
	{ Flags::RunMonstersRunning, "RunMonstersRunning" },
	{ Flags::FoundPaperInCoffin, "FoundPaperInCoffin" },
	{ Flags::KnowSunlord, "KnowSunlord" },
	{ Flags::KnowSunlordTalk, "KnowSunlordTalk" },
	{ Flags::ArivaldCzyta, "ArivaldCzyta" },
	{ Flags::TelepX, "TelepX" },
	{ Flags::TelepY, "TelepY" },
	{ Flags::TelepDir, "TelepDir" },
	{ Flags::TelepRoom, "TelepRoom" },
	{ Flags::ListStolen, "ListStolen" },
	{ Flags::WifeInDoor, "WifeInDoor" },
	{ Flags::TalkWifeFlag, "TalkWifeFlag" },
	{ Flags::LetterGiven, "LetterGiven" },
	{ Flags::LutniaTaken, "LutniaTaken" },
	{ Flags::BardHomeOpen, "BardHomeOpen" },
	{ Flags::FjordNoMonsters, "FjordNoMonsters" },
	{ Flags::ShandriaWallTalking, "ShandriaWallTalking" },
	{ Flags::ShandriaWallCounter, "ShandriaWallCounter" },
	{ Flags::ShandriaWallDone, "ShandriaWallDone" },
	{ Flags::FutureDone, "FutureDone" },
	{ Flags::TalkButch, "TalkButch" },
	{ Flags::GotSzalik, "GotSzalik" },
	{ Flags::GotCzosnek, "GotCzosnek" },
	{ Flags::BearDone, "BearDone" },
	{ Flags::NekrVisited, "NekrVisited" },
	{ Flags::SunRiddle, "SunRiddle" },
	{ Flags::PtaszekAway, "PtaszekAway" },
	{ Flags::KotGadanie, "KotGadanie" },
	{ Flags::SzlafmycaTaken, "SzlafmycaTaken" },
	{ Flags::BabkaTalk, "BabkaTalk" },
	{ Flags::SellerTalk, "SellerTalk" },
	{ Flags::CzosnekDone, "CzosnekDone" },
	{ Flags::PriestCounter, "PriestCounter" },
	{ Flags::PriestGest1, "PriestGest1" },
	{ Flags::PriestGest2, "PriestGest2" },
	{ Flags::PriestGest3, "PriestGest3" },
	{ Flags::PriestGest4, "PriestGest4" },
	{ Flags::PriestAnim, "PriestAnim" },
	{ Flags::HolyWaterTaken, "HolyWaterTaken" },
	{ Flags::AxeTaken, "AxeTaken" },
	{ Flags::BadylTaken1, "BadylTaken1" },
	{ Flags::BadylTaken2, "BadylTaken2" },
	{ Flags::BadylSharpened, "BadylSharpened" },
	{ Flags::PorwanieSmoka, "PorwanieSmoka" },
	{ Flags::ShopReOpen, "ShopReOpen" },
	{ Flags::LuskaShown, "LuskaShown" },
	{ Flags::CudKnow, "CudKnow" },
	{ Flags::VampireDead, "VampireDead" },
	{ Flags::MapaVisible1, "MapaVisible1" },
	{ Flags::MapaVisible2, "MapaVisible2" },
	{ Flags::MapaVisible3, "MapaVisible3" },
	{ Flags::MapaVisible4, "MapaVisible4" },
	{ Flags::MapaVisible5, "MapaVisible5" },
	{ Flags::MapaVisible6, "MapaVisible6" },
	{ Flags::MapaVisible7, "MapaVisible7" },
	{ Flags::MapaVisible8, "MapaVisible8" },
	{ Flags::MapaVisible9, "MapaVisible9" },
	{ Flags::MapaX, "MapaX" },
	{ Flags::MapaY, "MapaY" },
	{ Flags::MapaD, "MapaD" },
	{ Flags::OldMapaX, "OldMapaX" },
	{ Flags::OldMapaY, "OldMapaY" },
	{ Flags::OldMapaD, "OldMapaD" },
	{ Flags::MovingBack, "MovingBack" },
	{ Flags::MapaCount, "MapaCount" },
	{ Flags::Pustelnia1st, "Pustelnia1st" },
	{ Flags::CzarnePole1st, "CzarnePole1st" },
	{ Flags::TalkArivNum, "TalkArivNum" },
	{ Flags::Pfui, "Pfui" },
	{ Flags::MapaSunlordEnabled, "MapaSunlordEnabled" },
	{ Flags::WebDone, "WebDone" },
	{ Flags::DragonDone, "DragonDone" },
	{ Flags::KanPlay, "KanPlay" },
	{ Flags::OldKanPlay, "OldKanPlay" },
	{ Flags::LapkiWait, "LapkiWait" },
	{ Flags::WebNoCheck, "WebNoCheck" },
	{ Flags::Perfumeria, "Perfumeria" },
	{ Flags::SmokNoCheck, "SmokNoCheck" },
	{ Flags::IluzjaBroken, "IluzjaBroken" },
	{ Flags::IluzjaWorking, "IluzjaWorking" },
	{ Flags::IluzjaCounter, "IluzjaCounter" },
	{ Flags::KurhanOpen1, "KurhanOpen1" },
	{ Flags::KastetTaken, "KastetTaken" },
	{ Flags::KastetDown, "KastetDown" },
	{ Flags::KurhanDone, "KurhanDone" },
	{ Flags::SkelCounter, "SkelCounter" },
	{ Flags::SkelDial1, "SkelDial1" },
	{ Flags::SkelDial2, "SkelDial2" },
	{ Flags::SkelDial3, "SkelDial3" },
	{ Flags::SkelDial4, "SkelDial4" },
	{ Flags::SameTalker, "SameTalker" },
	{ Flags::RunMonstersText, "RunMonstersText" },
	{ Flags::PiwnicaChecked, "PiwnicaChecked" },
	{ Flags::DragonTalked, "DragonTalked" },
	{ Flags::ToldAboutBook, "ToldAboutBook" },
	{ Flags::SilmanionaDone, "SilmanionaDone" },
	{ Flags::ToldBookCount, "ToldBookCount" },
	{ Flags::SmrodNoCheck, "SmrodNoCheck" },
	{ Flags::RopeTaken, "RopeTaken" },
	{ Flags::RopeTime, "RopeTime" },
	{ Flags::LaskaFree, "LaskaFree" },
	{ Flags::ShanSmokTalked, "ShanSmokTalked" },
	{ Flags::SwordTaken, "SwordTaken" },
	{ Flags::Mill1st, "Mill1st" },
	{ Flags::SawRat, "SawRat" },
	{ Flags::KnowRat, "KnowRat" },
	{ Flags::DziuraTimer, "DziuraTimer" },
	{ Flags::LaskaInside, "LaskaInside" },
	{ Flags::HoleBig, "HoleBig" },
	{ Flags::EnableWiedzmin, "EnableWiedzmin" },
	{ Flags::EnableTrucizna, "EnableTrucizna" },
	{ Flags::KnowPoison, "KnowPoison" },
	{ Flags::KufelTaken, "KufelTaken" },
	{ Flags::BojkaEnabled, "BojkaEnabled" },
	{ Flags::BitwaNot1st, "BitwaNot1st" },
	{ Flags::BojkaTimer, "BojkaTimer" },
	{ Flags::BojkaGirl, "BojkaGirl" },
	{ Flags::Look1st, "Look1st" },
	{ Flags::RatTaken, "RatTaken" },
	{ Flags::LaskaTalkedGr, "LaskaTalkedGr" },
	{ Flags::RatusGivus, "RatusGivus" },
	{ Flags::MamObole, "MamObole" },
	{ Flags::Speed1st, "Speed1st" },
	{ Flags::SpeedTimer, "SpeedTimer" },
	{ Flags::ProveIt, "ProveIt" },
	{ Flags::Proven, "Proven" },
	{ Flags::ShowWoalka, "ShowWoalka" },
	{ Flags::PoisonTaken, "PoisonTaken" },
	{ Flags::HellOpened, "HellOpened" },
	{ Flags::HellNoCheck, "HellNoCheck" },
	{ Flags::TalAn1, "TalAn1" },
	{ Flags::TalAn2, "TalAn2" },
	{ Flags::TalAn3, "TalAn3" },
	{ Flags::TalkDevilGuard, "TalkDevilGuard" },
	{ Flags::Sword1st, "Sword1st" },
	{ Flags::IluzjaNoCheck, "IluzjaNoCheck" },
	{ Flags::RozdzielniaNumber, "RozdzielniaNumber" },
	{ Flags::JailChecked, "JailChecked" },
	{ Flags::JailTalked, "JailTalked" },
	{ Flags::TrickFailed, "TrickFailed" },
	{ Flags::WegielVisible, "WegielVisible" },
	{ Flags::WegielTimer1, "WegielTimer1" },
	{ Flags::RandomSample, "RandomSample" },
	{ Flags::RandomSampleTimer, "RandomSampleTimer" },
	{ Flags::SampleTimer, "SampleTimer" },
	{ Flags::ZonaSample, "ZonaSample" },
	{ Flags::HoleTryAgain, "HoleTryAgain" },
	{ Flags::TeleportTimer, "TeleportTimer" },
	{ Flags::RozLezy, "RozLezy" },
	{ Flags::UdkoTimer, "UdkoTimer" },
	{ Flags::ZaworZatkany, "ZaworZatkany" },
	{ Flags::ZaworOpened, "ZaworOpened" },
	{ Flags::DoorExploded, "DoorExploded" },
	{ Flags::SkoraTaken, "SkoraTaken" },
	{ Flags::CiezkieByl, "CiezkieByl" },
	{ Flags::MamWegiel, "MamWegiel" },
	{ Flags::SwiecaAway, "SwiecaAway" },
	{ Flags::ITSAVE, "ITSAVE" },
	{ Flags::RozpadlSie, "RozpadlSie" },
	{ Flags::WegielFullTimer, "WegielFullTimer" },
	{ Flags::WegielDown, "WegielDown" },
	{ Flags::WegielDownTimer, "WegielDownTimer" },
	{ Flags::PaliSie, "PaliSie" },
	{ Flags::DiabGuardTalked, "DiabGuardTalked" },
	{ Flags::GuardsNoCheck, "GuardsNoCheck" },
	{ Flags::TalkedPowloka, "TalkedPowloka" },
	{ Flags::JailOpen, "JailOpen" },
	{ Flags::PrzytulTimer, "PrzytulTimer" },
	{ Flags::JailDone, "JailDone" },
	{ Flags::MamMonety, "MamMonety" },
	{ Flags::LotTimer, "LotTimer" },
	{ Flags::LotObj, "LotObj" },
	{ Flags::PtakTimer, "PtakTimer" },
	{ Flags::BookTimer, "BookTimer" },
	{ Flags::BookGiba, "BookGiba" },
	{ Flags::PtakLata, "PtakLata" },
	{ Flags::Podej, "Podej" },
	{ Flags::GotHint, "GotHint" },
	{ Flags::LawaLeci, "LawaLeci" },
	{ Flags::PowerKlik, "PowerKlik" },
	{ Flags::LucekBad, "LucekBad" },
	{ Flags::LucekBad1st, "LucekBad1st" },
	{ Flags::IntroDial1, "IntroDial1" },
	{ Flags::IntroDial2, "IntroDial2" },
	{ Flags::ItsOutro, "ItsOutro" },
	{ Flags::KamienComment, "KamienComment" },
	{ Flags::KamienSkip, "KamienSkip" },
	{ Flags::TesterFlag, "TesterFlag" },
	{ Flags::RememberLine, "RememberLine" },
	{ Flags::OpisLapek, "OpisLapek" },
	{ Flags::TalWait, "TalWait" },
	{ Flags::OpisKamienia, "OpisKamienia" },
	{ Flags::JumpBox, "JumpBox" },
	{ Flags::JumpBox1, "JumpBox1" },
	{ Flags::JumpBox2, "JumpBox2" },
	{ Flags::JumpBox3, "JumpBox3" },
	{ Flags::SpecPiesek, "SpecPiesek" },
	{ Flags::SpecPiesekCount, "SpecPiesekCount" },
	{ Flags::SpecPiesekGadanie, "SpecPiesekGadanie" },
	{ Flags::ZnikaFlag, "ZnikaFlag" },
	{ Flags::ZnikaTimer, "ZnikaTimer" },
	{ Flags::SowaTimer, "SowaTimer" },
	{ Flags::MamrotanieOff, "MamrotanieOff" },
	{ Flags::CURRMOB, "CURRMOB" },
	{ Flags::KOLOR, "KOLOR" },
	{ Flags::MBFLAG, "MBFLAG" },
	{ Flags::MXFLAG, "MXFLAG" },
	{ Flags::MYFLAG, "MYFLAG" },
	{ Flags::SCROLLTYPE, "SCROLLTYPE" },
	{ Flags::SCROLLVALUE, "SCROLLVALUE" },
	{ Flags::SCROLLVALUE2, "SCROLLVALUE2" },
	{ Flags::TALKEXITCODE, "TALKEXITCODE" },
	{ Flags::SPECROUTFLAG1, "SPECROUTFLAG1" },
	{ Flags::SPECROUTFLAG2, "SPECROUTFLAG2" },
	{ Flags::SPECROUTFLAG3, "SPECROUTFLAG3" },
	{ Flags::TALKFLAGCODE, "TALKFLAGCODE" },
	{ Flags::CURRROOM, "CURRROOM" },
	{ Flags::Talker1Init, "Talker1Init" },
	{ Flags::Talker2Init, "Talker2Init" },
	{ Flags::RESTOREROOM, "RESTOREROOM" },
	{ Flags::INVALLOWED, "INVALLOWED" },
	{ Flags::BOXSEL, "BOXSEL" },
	{ Flags::CURSEBLINK, "CURSEBLINK" },
	{ Flags::EXACTMOVE, "EXACTMOVE" },
	{ Flags::MOVEDESTX, "MOVEDESTX" },
	{ Flags::MOVEDESTY, "MOVEDESTY" },
	{ Flags::NOANTIALIAS, "NOANTIALIAS" },
	{ Flags::ESCAPED, "ESCAPED" },
	{ Flags::ALLOW1OPTION, "ALLOW1OPTION" },
	{ Flags::VOICE_H_LINE, "VOICE_H_LINE" },
	{ Flags::VOICE_A_LINE, "VOICE_A_LINE" },
	{ Flags::VOICE_B_LINE, "VOICE_B_LINE" },
	{ Flags::VOICE_C_LINE, "VOICE_C_LINE" },
	{ Flags::NOHEROATALL, "NOHEROATALL" },
	{ Flags::MOUSEENABLED, "MOUSEENABLED" },
	{ Flags::DIALINES, "DIALINES" },
	{ Flags::SHANWALK, "SHANWALK" },
	{ Flags::SHANDOG, "SHANDOG" },
	{ Flags::GETACTIONBACK, "GETACTIONBACK" },
	{ Flags::GETACTIONDATA, "GETACTIONDATA" },
	{ Flags::GETACTION, "GETACTION" },
	{ Flags::HEROFAST, "HEROFAST" },
	{ Flags::SELITEM, "SELITEM" },
	{ Flags::LMOUSE, "LMOUSE" },
	{ Flags::MINMX, "MINMX" },
	{ Flags::MAXMX, "MAXMX" },
	{ Flags::MINMY, "MINMY" },
	{ Flags::MAXMY, "MAXMY" },
	{ Flags::TORX1, "TORX1" },
	{ Flags::TORY1, "TORY1" },
	{ Flags::TORX2, "TORX2" },
	{ Flags::TORY2, "TORY2" },
	{ Flags::POWER, "POWER" },
	{ Flags::POWERENABLED, "POWERENABLED" },
	{ Flags::FLCRESTORE, "FLCRESTORE" },
	{ Flags::NOCLSTEXT, "NOCLSTEXT" },
	{ Flags::ESCAPED2, "ESCAPED2" },
};
