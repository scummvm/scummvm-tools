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

wxArrayString ToolsGUI::getToolList(const Common::Filename &filename, ToolType tt) const {
	ToolList choices = inspectInput(filename, tt, true);
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
	// FIXME: This is a HACK!
	CompressionTool *compression = dynamic_cast<CompressionTool *>(_backend);
	return (compression->_supportedFormats & format) == format;
}

bool ToolGUI::supportsProgressBar() const {
	return _backend->_supportsProgressBar;
}

bool ToolGUI::outputToDirectory() const {
	return _backend->_outputToDirectory;
}

void ToolGUI::run(const Configuration &conf) const {
	// Set the input paths
	_backend->clearInputPaths();
	if (conf.inputFilePaths.size() < _backend->_inputPaths.size())
		_backend->error("Too few input files!");
	for (wxArrayString::const_iterator iter = conf.inputFilePaths.begin(); iter != conf.inputFilePaths.end(); ++iter) {
		if (!_backend->addInputPath(std::string(iter->mb_str())))
			_backend->error("Unexpected input file '%s'!", (const char*)iter->mb_str());
	}
		
	// Set the output path
	_backend->_outputPath = std::string(conf.outputPath.mb_str());

	CompressionTool *compression = dynamic_cast<CompressionTool *>(_backend);
	if (compression) {
		compression->_format               = conf.selectedAudioFormat;

		// mp3
		compression->setMp3LamePath       ( (const char *)conf.mp3LamePath.mb_str()        );
		compression->setMp3CompressionType( (const char *)conf.mp3CompressionType.mb_str() );
		compression->setMp3MpegQuality    ( (const char *)conf.mp3MpegQuality.mb_str()     );
		if (conf.mp3CompressionType == wxT("ABR")) {
			compression->setMp3TargetBitrate  ( (const char *)conf.mp3ABRBitrate.mb_str()      );
			compression->unsetMp3MinBitrate();
			compression->unsetMp3MaxBitrate();
		} else {
			compression->setMp3MinBitrate  ( (const char *)conf.mp3VBRMinBitrate.mb_str()   );
			compression->setMp3MaxBitrate  ( (const char *)conf.mp3VBRMaxBitrate.mb_str()   );
		}
		compression->setMp3VBRQuality     ( (const char *)conf.mp3VBRQuality.mb_str()      );

		// flac
		compression->setFlacCompressionLevel( (const char *)conf.flacCompressionLevel.mb_str() );
		compression->setFlacBlockSize       ( (const char *)conf.flacBlockSize.mb_str()        );

		// vorbis
		if (conf.useOggQuality)
			compression->setOggQuality    ( (const char *)conf.oggQuality.mb_str()    );
		else
			compression->setOggAvgBitrate ( (const char *)conf.oggAvgBitrate.mb_str() );
		if (conf.oggMinBitrate == wxT("None"))
			compression->unsetOggMinBitrate();
		else
			compression->setOggMinBitrate ( (const char *)conf.oggMinBitrate.mb_str() );
		if (conf.oggMaxBitrate == wxT("None"))
			compression->unsetOggMaxBitrate();
		else
			compression->setOggMaxBitrate ( (const char *)conf.oggMaxBitrate.mb_str() );
	}

	_backend->run();
}
