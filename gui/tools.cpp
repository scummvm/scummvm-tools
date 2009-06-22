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

#include "tools.h"

Tools g_tools;

Tools::Tools() {

	// Compress agos also has a --mac parameter, need to add an additional page / option for this
	Tool compress_agos(wxT("compress_agos"), wxT("*."));
	addTool(compress_agos);

	Tool compress_gob(wxT("compress_gob"), wxT("*.*"));
	addTool(compress_gob);

	Tool compress_kyra(wxT("compress_kyra"), wxT("*.*"));
	addTool(compress_kyra);
}

void Tools::addTool(const Tool& tool) {
	tools[tool._name] = tool;
}

wxArrayString Tools::getToolList() {
	wxArrayString l;
	for(std::map<wxString, Tool>::const_iterator iter = tools.begin(); iter != tools.end(); ++iter)
		l.Add(iter->first);
	return l;
}

const Tool &Tools::operator[](const wxString& name) {
	std::map<wxString, Tool>::const_iterator iter = tools.find(name);

	wxASSERT_MSG(iter != tools.end(), wxT("All tools should be added, never try to access a tool that does not exist."));

	return iter->second;
}


// The Tool class

Tool::Tool() {
	// should never be called
	// required for std::map template to work
	
	// Seems std is allowed to create dummy objects in maps.
	//wxLogError(wxT("Created empty tool, should never happened."));
}

Tool::Tool(wxString name, wxString input_extensions) {
	_name = name;
	
	// Sensible defaults
	_supportedFormats = AUDIO_ALL;
	ToolInput input;
	input._extension = input_extensions;
	input._file = true;
	_inputs.push_back(input);
}

bool Tool::supportsAudioFormat(AudioFormat format) {
	return (_supportedFormats & format) == format;
}

bool Tool::pickFiles() {
	for(ToolInputs::const_iterator iter = _inputs.begin(); iter != _inputs.end(); ++iter)
		if(iter->_file)
			return true;
	return false;
}

bool Tool::pickDirs() {
	return !pickFiles();
}

wxString Tool::getExecutable() {
#ifdef WIN32
	return _name + wxT(".exe");
#else
	return _name;
#endif
}
