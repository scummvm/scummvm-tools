/* tool_entry-points - Entry points for all the tools supported by the GUI
 * Copyright (C) 2009 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL
 * $Id
 *
 */

#ifndef TOOL_ENTRY_POINTS_H
#define TOOL_ENTRY_POINTS_H

typedef int (MainFunction*)(int argc, char **argv);
#define import_main(tool_name) main_ ## tool_name

extern int import_main(compress_agos)(int argc, char **argv);
extern int import_main(compress_gob)(int argc, char **argv);
extern int import_main(compress_kyra)(int argc, char **argv);
extern int import_main(compress_queen)(int argc, char **argv);
extern int import_main(compress_saga)(int argc, char **argv);
extern int import_main(compress_scumm_bun)(int argc, char **argv);
extern int import_main(compress_scumm_san)(int argc, char **argv);
extern int import_main(compress_scumm_sou)(int argc, char **argv);
extern int import_main(compress_sword1)(int argc, char **argv);
extern int import_main(compress_sword2)(int argc, char **argv);
extern int import_main(compress_touche)(int argc, char **argv);
extern int import_main(compress_tucker)(int argc, char **argv);
extern int import_main(encode_dxa)(int argc, char **argv);
extern int import_main(extract_agos)(int argc, char **argv);
extern int import_main(extract_gob_stk)(int argc, char **argv);
extern int import_main(extract_kyra)(int argc, char **argv);
extern int import_main(extract_loom_tg16)(int argc, char **argv);
extern int import_main(extract_mm_apple)(int argc, char **argv);
extern int import_main(extract_mm_c64)(int argc, char **argv);
extern int import_main(extract_mm_nes)(int argc, char **argv);
extern int import_main(extract_parallaction)(int argc, char **argv);
extern int import_main(extract_scumm_mac)(int argc, char **argv);
extern int import_main(extract_zak_c64)(int argc, char **argv);

#endif

