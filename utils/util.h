#ifndef COMMON_UTIL_H
#define COMMON_UTIL_H

namespace Common {


#ifdef MIN
#undef MIN
#endif

#ifdef MAX
#undef MAX
#endif

template<typename T> inline T ABS (T x)			{ return (x>=0) ? x : -x; }
template<typename T> inline T MIN (T a, T b)	{ return (a<b) ? a : b; }
template<typename T> inline T MAX (T a, T b)	{ return (a>b) ? a : b; }
template<typename T> inline T CLIP (T v, T amin, T amax) 
		{ if (v < amin) return amin; else if (v > amax) return amax; else return v; }

/**
 * List of game language.
 */
enum Language {
	EN_ANY,     // Generic English (when only one game version exist)
	EN_USA,
	EN_GRB,

	DE_DEU,
	FR_FRA,
	IT_ITA,
	PT_BRA,
	ES_ESP,
	JA_JPN,
	ZH_TWN,
	KO_KOR,
	SE_SWE,
	HB_ISR,
	RU_RUS,
	CZ_CZE,
	NL_NLD,
	NB_NOR,
	PL_POL,

	UNK_LANG = -1	// Use default language (i.e. none specified)
};

/**
 * List of game platforms. Specifying a platform for a target can be used to
 * give the game engines a hint for which platform the game data file are.
 * This may be optional or required, depending on the game engine and the
 * game in question.
 */
enum Platform {
	kPlatformPC,
	kPlatformAmiga,
	kPlatformAtariST,
	kPlatformMacintosh,
	kPlatformFMTowns,
	kPlatformWindows,
	kPlatformNES,
	kPlatformC64,
	kPlatformLinux,
	kPlatformAcorn,
	kPlatformSegaCD,
	kPlatform3DO,
//	kPlatformPCEngine,

	kPlatformUnknown = -1
};
}

#endif
