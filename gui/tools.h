/* tools.h - List & description of all supported tools
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

#ifndef TOOLS_H
#define TOOLS_H

#include <wx/string.h>

#include <map>
#include <vector>

#include "configuration.h"

// Different types of tools, used to differentiate them when 
// fetching lists of games & tools
enum ToolType {
	TOOLTYPE_COMPRESSION,
	TOOLTYPE_EXTRACTION,
	TOOLTYPE_UNKNOWN,
	TOOLTYPE_ALL,
};


// Describes a possible input to the tool (since some take two seperate files, 
// some a dir and some a single file
struct ToolInput {
	wxString _extension;
	wxString _description;
	bool _file; // dir otherwise
};

typedef std::vector<ToolInput> ToolInputs;

// A tool, contains all info necessary to run it
class Tool {
public:
	Tool();
	Tool(wxString name, wxString input_extensions = wxT("*.*"));

	void addGame(const wxString &game_name);

	// Helpfer functions to get info about the tool
	bool supportsAudioFormat(AudioFormat format);
	bool pickFiles();
	bool pickDirs();
	wxString getExecutable();


	wxString _name;
	ToolType _type;
	AudioFormat _supportedFormats;
	ToolInputs _inputs;
	bool _outputToDirectory;
	wxString _inoutHelpText;
	wxArrayString _games;
};

// Collection of all tools
class Tools {
public:
	Tools();

	const Tool &operator[](const wxString &name) const;
	const Tool *get(const wxString &name) const;
	const Tool *getByGame(const wxString &game) const;

	// Returns all tool/game names in a list
	// conveinent for creating the choose tool page
	// List will be sorted and unique
	wxArrayString getToolList(ToolType tt) const;
	wxArrayString getGameList(ToolType tt) const;

protected:
	void addTool(const Tool &tool);

	std::map<wxString, Tool> tools;
};

extern Tools g_tools;

#endif
