/* tools.cpp - List & description of all supported tools
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

#include "wx/wxprec.h"

#ifdef __BORLANDC__
	#pragma hdrstop
#endif

#ifndef WX_PRECOMP
	#include "wx/wx.h"
#endif

#include <algorithm>

#include "tools.h"

// Include all tools
#include "../compress_agos.h"
#include "../compress_gob.h"
#include "../compress_kyra.h"
#include "../compress_queen.h"
#include "../compress_saga.h"
#include "../compress_scumm_bun.h"
#include "../compress_scumm_san.h"
#include "../compress_scumm_sou.h"
#include "../compress_sword1.h"
#include "../compress_sword2.h"
#include "../compress_touche.h"
#include "../compress_tinsel.h"
#include "../compress_touche.h"
#include "../compress_tucker.h"
#include "../encode_dxa.h"
#include "../extract_agos.h"
#include "../extract_gob_stk.h"
#include "../extract_kyra.h"
#include "../extract_loom_tg16.h"
#include "../extract_mm_apple.h"
#include "../extract_mm_c64.h"
#include "../extract_mm_nes.h"
#include "../extract_parallaction.h"
#include "../extract_scumm_mac.h"
#include "../extract_zak_c64.h"


// Our global tools object, which holds all tools
Tools g_tools;

Tools::Tools() {
}

void Tools::init() {

	// Compress agos also has a --mac parameter, need to add an additional page / option for this
	addTool(new ToolGUI(new CompressAgos()));
	// Compress gob also has a --f parameter, need to add an additional page / option for this
	addTool(new ToolGUI(new CompressGob()));
	addTool(new ToolGUI(new CompressKyra()));
	addTool(new ToolGUI(new CompressQueen()));
	addTool(new ToolGUI(new CompressSaga()));
	addTool(new ToolGUI(new CompressScummBun()));
	addTool(new ToolGUI(new CompressScummSan()));
	addTool(new ToolGUI(new CompressScummSou()));
	addTool(new ToolGUI(new CompressSword1()));
	addTool(new ToolGUI(new CompressSword2()));
	addTool(new ToolGUI(new CompressTinsel()));
	addTool(new ToolGUI(new CompressTouche()));
	addTool(new ToolGUI(new CompressTucker()));
	addTool(new ToolGUI(new EncodeDXA(), TOOLTYPE_COMPRESSION));

	addTool(new ToolGUI(new ExtractAgos()));
	addTool(new ToolGUI(new ExtractGobStk()));
	addTool(new ToolGUI(new ExtractKyra()));
	addTool(new ToolGUI(new ExtractLoomTG16()));
	addTool(new ToolGUI(new ExtractMMApple()));
	addTool(new ToolGUI(new ExtractMMC64()));
	addTool(new ToolGUI(new ExtractMMNes()));
	addTool(new ToolGUI(new ExtractParallaction()));
	addTool(new ToolGUI(new ExtractScummMac()));
	addTool(new ToolGUI(new ExtractZakC64()));
}

Tools::~Tools() {
	for (std::map<wxString, ToolGUI *>::iterator iter = tools.begin(); iter != tools.end(); ++iter)
		delete iter->second;
}

void Tools::addTool(ToolGUI* tool) {
	tools[tool->_name] = tool;
}

wxArrayString Tools::getToolList(ToolType tt) const {
	wxArrayString l;
	for (std::map<wxString, ToolGUI *>::const_iterator iter = tools.begin(); iter != tools.end(); ++iter)
		if (tt == TOOLTYPE_ALL || iter->second->_type == tt)
			l.Add(iter->first);
	l.Sort();
	std::unique(l.begin(), l.end());
	return l;
}

wxArrayString Tools::getToolList(const Filename &filename, ToolType tt) const {
	wxArrayString l;
	for (std::map<wxString, ToolGUI *>::const_iterator iter = tools.begin(); iter != tools.end(); ++iter)
		if (tt == TOOLTYPE_ALL || iter->second->_type == tt)
			if (iter->second->inspectInput(filename))
				l.Add(iter->second->_name);
	l.Sort();
	std::unique(l.begin(), l.end());
	return l;
}

const ToolGUI &Tools::operator[](const wxString& name) const {
	std::map<wxString, ToolGUI *>::const_iterator iter = tools.find(name);

	wxASSERT_MSG(iter != tools.end(), wxT("All tools should be added, never try to access a tool that does not exist."));

	return *iter->second;
}

const ToolGUI *Tools::get(const wxString& name) const {
	std::map<wxString, ToolGUI *>::const_iterator iter = tools.find(name);

	if (iter == tools.end())
		return NULL;

	return iter->second;
}

// The Tool class

ToolGUI::ToolGUI(Tool *tool, ToolType type) {
	_backend = tool;
	_name = wxString(tool->_name.c_str(), wxConvUTF8);

	if(type == TOOLTYPE_UNKNOWN) {
		if (_name.Find(wxT("extract")) != wxNOT_FOUND)
			_type = TOOLTYPE_EXTRACTION;
		else if (_name.Find(wxT("compress")) != wxNOT_FOUND)
			_type = TOOLTYPE_COMPRESSION;
		else {
			wxLogError(wxT("Tools with unknown type shouldn't exist."));
			_type = TOOLTYPE_UNKNOWN;
		}
	} else
		_type = type;

	_inHelpText = wxT("Please select any additional input files.");
}

ToolGUI::~ToolGUI() {
	delete _backend;
}

bool ToolGUI::inspectInput(const Filename &filename) const {
	return _backend->inspectInput(filename);
}

ToolInputs ToolGUI::getInputList() const {
	return _backend->_inputPaths;
}

bool ToolGUI::supportsAudioFormat(AudioFormat format) const {
	return (_backend->_supportedFormats & format) == format;
}

bool ToolGUI::supportsProgressBar() const {
	return _backend->_supportsProgressBar;
}

bool ToolGUI::outputToDirectory() const {
	return _backend->_outputToDirectory;
}

void ToolGUI::run(const Configuration &conf) const {
	size_t i = 0;
	for (wxArrayString::const_iterator iter = conf.inputFilePaths.begin(); iter != conf.inputFilePaths.end(); ++iter, ++i)
		_backend->_inputPaths[i].path = (const char *)iter->mb_str();
	_backend->_outputPath = std::string(conf.outputPath.mb_str());

	CompressionTool *compression = dynamic_cast<CompressionTool *>(_backend);
	if (compression) {
		// mp3
		compression->_mp3ABRBitrate        = (const char *)conf.mp3ABRBitrate.mb_str();
		compression->_mp3CompressionType   = (const char *)conf.mp3CompressionType.mb_str();
		compression->_mp3MpegQuality       = (const char *)conf.mp3MpegQuality.mb_str();
		compression->_mp3ABRBitrate        = (const char *)conf.mp3ABRBitrate.mb_str();
		compression->_mp3VBRMinBitrate     = (const char *)conf.mp3VBRMinBitrate.mb_str();
		compression->_mp3VBRMaxBitrate     = (const char *)conf.mp3VBRMaxBitrate.mb_str();
		compression->_mp3VBRQuality        = (const char *)conf.mp3VBRQuality.mb_str();

		// flac
		compression->_flacCompressionLevel = (const char *)conf.flacCompressionLevel.mb_str();
		compression->_flacBlockSize        = (const char *)conf.flacBlockSize.mb_str();
		
		// vorbis
		compression->_oggQuality           = (const char *)conf.oggQuality.mb_str();
		compression->_oggMinBitrate        = (const char *)conf.oggMinBitrate.mb_str();
		compression->_oggAvgBitrate        = (const char *)conf.oggAvgBitrate.mb_str();
		compression->_oggMaxBitrate        = (const char *)conf.oggMaxBitrate.mb_str();
	}

	_backend->run();
}

wxString ToolGUI::getExecutable() const {
#ifdef WIN32
	return _name + wxT(".exe");
#else
	return _name;
#endif
}
