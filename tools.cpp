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
 * $URL$
 * $Id$
 *
 */


#include "tools.h"
#include "tool.h"

#include "engines/agos/compress_agos.h"
#include "engines/gob/compress_gob.h"
#include "engines/kyra/compress_kyra.h"
#include "engines/queen/compress_queen.h"
#include "engines/saga/compress_saga.h"
#include "engines/scumm/compress_scumm_bun.h"
#include "engines/scumm/compress_scumm_san.h"
#include "engines/scumm/compress_scumm_sou.h"
#include "engines/sci/compress_sci.h"
#include "engines/sword1/compress_sword1.h"
#include "engines/sword2/compress_sword2.h"
#include "engines/touche/compress_touche.h"
#include "engines/tinsel/compress_tinsel.h"
#include "engines/touche/compress_touche.h"
#include "engines/tucker/compress_tucker.h"

#ifdef USE_PNG
#include "encode_dxa.h"
#endif

#include "engines/agos/extract_agos.h"
#include "engines/cge/extract_cge.h"
#include "engines/cge/pack_cge.h"
#include "engines/cine/extract_cine.h"
#include "engines/cruise/extract_cruise_pc.h"
#include "engines/gob/extract_gob_stk.h"
#include "engines/gob/extract_fascination_cd.h"
#include "engines/kyra/extract_kyra.h"
#include "engines/scumm/extract_loom_tg16.h"
#include "engines/scumm/extract_mm_apple.h"
#include "engines/scumm/extract_mm_c64.h"
#include "engines/scumm/extract_mm_nes.h"
#include "engines/parallaction/extract_parallaction.h"
#include "engines/scumm/extract_scumm_mac.h"
#include "engines/scumm/extract_zak_c64.h"

Tools::Tools() {

	_tools.push_back(new CompressAgos());
	_tools.push_back(new CompressGob());
	_tools.push_back(new CompressKyra());
	_tools.push_back(new CompressQueen());
	_tools.push_back(new CompressSaga());
	_tools.push_back(new CompressSci());
	_tools.push_back(new CompressScummBun());
	_tools.push_back(new CompressScummSan());
	_tools.push_back(new CompressScummSou());
	_tools.push_back(new CompressSword1());
	_tools.push_back(new CompressSword2());
	_tools.push_back(new CompressTinsel());
	_tools.push_back(new CompressTouche());
	_tools.push_back(new CompressTucker());

#ifdef USE_PNG
	_tools.push_back(new EncodeDXA());
#endif

	_tools.push_back(new ExtractAgos());
	_tools.push_back(new ExtractCge());
	_tools.push_back(new PackCge());
	_tools.push_back(new ExtractCine());
	_tools.push_back(new ExtractCruisePC());
	_tools.push_back(new ExtractGobStk());
	_tools.push_back(new ExtractFascinationCD());
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
	for (ToolList::iterator iter = _tools.begin(); iter != _tools.end(); ++iter)
		delete *iter;
}

Tools::ToolList Tools::inspectInput(const Common::Filename &filename, ToolType type) const {
	ToolList perfect_choices;
	ToolList good_choices;
	ToolList awful_choices;

	for (ToolList::const_iterator tool = _tools.begin(); tool != _tools.end(); ++tool) {
		if (type == TOOLTYPE_ALL || (*tool)->getType() == type) {
			InspectionMatch m = (*tool)->inspectInput(filename);

			if (m == IMATCH_PERFECT)
				perfect_choices.push_back(*tool);
			else if (m == IMATCH_POSSIBLE)
				good_choices.push_back(*tool);
			else
				awful_choices.push_back(*tool);
		}
	}

	if (perfect_choices.size() > 0)
		return perfect_choices;
	if (good_choices.size() > 0)
		return good_choices;

	return awful_choices;
}
