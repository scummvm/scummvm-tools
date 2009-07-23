/* tools - Interface for accessing all the tools in a conveinient fashion
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


#include "tools.h"
#include "tool.h"

#include "compress_agos.h"
#include "compress_gob.h"
#include "compress_kyra.h"
#include "compress_queen.h"
#include "compress_saga.h"
#include "compress_scumm_bun.h"
#include "compress_scumm_san.h"
#include "compress_scumm_sou.h"
#include "compress_sword1.h"
#include "compress_sword2.h"
#include "compress_touche.h"
#include "compress_tinsel.h"
#include "compress_touche.h"
#include "compress_tucker.h"
#include "encode_dxa.h"
#include "extract_agos.h"
#include "extract_gob_stk.h"
#include "extract_kyra.h"
#include "extract_loom_tg16.h"
#include "extract_mm_apple.h"
#include "extract_mm_c64.h"
#include "extract_mm_nes.h"
#include "extract_parallaction.h"
#include "extract_scumm_mac.h"
#include "extract_zak_c64.h"

Tools::Tools() {

	// TODO: Rather than having the list here and in gui/tools.cpp
	// those "tools" should inherit this class with the complete list
	_tools.push_back(new CompressAgos());
	_tools.push_back(new CompressGob());
	_tools.push_back(new CompressKyra());
	_tools.push_back(new CompressQueen());
	_tools.push_back(new CompressSaga());
	_tools.push_back(new CompressScummBun());
	_tools.push_back(new CompressScummSan());
	_tools.push_back(new CompressScummSou());
	_tools.push_back(new CompressSword1());
	_tools.push_back(new CompressSword2());
	_tools.push_back(new CompressTinsel());
	_tools.push_back(new CompressTouche());
	_tools.push_back(new CompressTucker());
	//_tools.push_back(new EncodeDXA());

	_tools.push_back(new ExtractAgos());
	_tools.push_back(new ExtractGobStk());
	_tools.push_back(new ExtractKyra());
	_tools.push_back(new ExtractLoomTG16());
	_tools.push_back(new ExtractMMApple());
	_tools.push_back(new ExtractMMC64());
	_tools.push_back(new ExtractMMNes());
	_tools.push_back(new ExtractParallaction());
	_tools.push_back(new ExtractScummMac());
	_tools.push_back(new ExtractZakC64());
}

Tools::~Tools() {
	for (ToolList::iterator iter = _tools.begin(); iter != _tools.end(); ++iter) {
		delete *iter;
	}
}

Tools::ToolList Tools::inspectInput(ToolType type, std::deque<char *> arguments) {
	ToolList choices;
	for (ToolList::iterator tool = _tools.begin(); tool != _tools.end(); ++tool) {
		if (type == TOOLTYPE_ALL || (*tool)->getType() == type) {
			if ((*tool)->_inputPaths.size() == arguments.size()) {
				for (std::deque<char *>::const_iterator filename = arguments.begin(); filename != arguments.end(); ++filename) {
					if((*tool)->inspectInput(*filename)) {
						choices.push_back(*tool);
						break;
					}
				}
			}
		}
	}
	return choices;
}
