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

#include "../compress.h"
#include "gui_tools.h"

// Our global tools object, which holds all tools
ToolsGUI g_tools;

ToolsGUI::ToolsGUI() {
}

void ToolsGUI::init() {
	for (ToolList::iterator tool = _tools.begin(); tool != _tools.end(); ++tool)
		_toolmap[wxString((*tool)->getName().c_str(), wxConvUTF8)] = new ToolGUI(*tool);
}

ToolsGUI::~ToolsGUI() {
	for (std::map<wxString, ToolGUI *>::iterator iter = _toolmap.begin(); iter != _toolmap.end(); ++iter)
		delete iter->second;
}

wxArrayString ToolsGUI::getToolList(ToolType tt) const {
	wxArrayString l;
	for (std::map<wxString, ToolGUI *>::const_iterator iter = _toolmap.begin(); iter != _toolmap.end(); ++iter)
		if (tt == TOOLTYPE_ALL || iter->second->getType() == tt)
			l.Add(iter->first);
	l.Sort();
	std::unique(l.begin(), l.end());
	return l;
}

wxArrayString ToolsGUI::getToolList(const Filename &filename, ToolType tt) const {
	ToolList choices = inspectInput(filename, tt);
	wxArrayString l;

	for (ToolList::const_iterator tool = choices.begin(); tool != choices.end(); ++tool)
		l.Add(wxString((*tool)->getName().c_str(), wxConvUTF8));

	l.Sort();
	std::unique(l.begin(), l.end());
	return l;
}

const ToolGUI &ToolsGUI::operator[](const wxString& name) const {
	std::map<wxString, ToolGUI *>::const_iterator iter = _toolmap.find(name);

	wxASSERT_MSG(iter != _toolmap.end(), wxT("All tools should be added, never try to access a tool that does not exist."));

	return *iter->second;
}

const ToolGUI *ToolsGUI::get(const wxString& name) const {
	std::map<wxString, ToolGUI *>::const_iterator iter = _toolmap.find(name);

	if (iter == _toolmap.end())
		return NULL;

	return iter->second;
}

// The Tool class

ToolGUI::ToolGUI(Tool *tool, ToolType type) {
	_backend = tool;
}

ToolGUI::~ToolGUI() {
	//The parent Tools client deletes the backends
	//delete _backend;
}

ToolInputs ToolGUI::getInputList() const {
	return _backend->_inputPaths;
}

wxString ToolGUI::getName() const {
	return wxString(_backend->getName().c_str(), wxConvUTF8);
}

wxString ToolGUI::getHelp() const {
	return wxString(_backend->getHelp().c_str(), wxConvUTF8);
}

wxString ToolGUI::getShortHelp() const {
	return wxString(_backend->getShortHelp().c_str(), wxConvUTF8);
}

ToolType ToolGUI::getType() const {
	return _backend->getType();
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
		compression->_format               = conf.selectedAudioFormat;

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
	return getName() + wxT(".exe");
#else
	return getName();
#endif
}
