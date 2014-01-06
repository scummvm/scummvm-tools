/* ScummVM Tools
 * Copyright (C) 2002-2009 The ScummVM project
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

#include "common/scummsys.h"
#include "internal_version.h"
#include "version.h"

/*
 * Version string and build date string. These can be used by anything that
 * wants to display this information to the user (e.g. about dialog).
 *
 * Note: it would be very nice if we could instead of (or in addition to) the
 * build date present a date which corresponds to the date our source files
 * were last changed. To understand the difference, imagine that a user
 * makes a checkout on January 1, then after a week compiles it
 * (e.g. after doing a 'make clean'). The build date then will say January 8
 * even though the files were last changed on January 1.
 *
 * Another problem is that __DATE__/__TIME__ depend on the local time zone.
 *
 * It's clear that such a "last changed" date would be much more useful to us
 * for feedback purposes. After all, when somebody files a bug report, we
 * don't care about the build date, we want to know which date their checkout
 * was made.
 *
 * So, how could we implement this? At least on unix systems, a special script
 * could do it. Basically, that script could parse the output of "svn info" or
 * "svnversion" to determine the revision of the checkout, and insert that
 * information somehow into the build process (e.g. by generating a tiny
 * header file, analog to internal_version.h, maybe called svn_rev.h or so.)
 *
 * Drawback: This only works on systems which can run suitable scripts as part
 * of the build proces (so I guess Visual C++ would be out of the game here?
 * I don't know VC enough to be sure). And of course it must be robust enough
 * to properly work in exports (i.e. release tar balls etc.).
 */
const char *gScummVMToolsVersion = SCUMMVM_TOOLS_VERSION;
const char *gScummVMToolsBuildDate = __DATE__ " " __TIME__;
const char *gScummVMToolsVersionDate = SCUMMVM_TOOLS_VERSION " (" __DATE__ " " __TIME__ ")";
const char *gScummVMToolsFullVersion = "ScummVM Tools " SCUMMVM_TOOLS_VERSION " (" __DATE__ " " __TIME__ ")";
