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
#include "../tool_entry_points.h"


/** Different types of tools, used to differentiate them when 
 * fetching lists of games & tools
 */
enum ToolType {
	TOOLTYPE_COMPRESSION,
	TOOLTYPE_EXTRACTION,
	TOOLTYPE_UNKNOWN,
	TOOLTYPE_ALL,
};


/**
 * Describes a possible input to the tool (since some take two seperate files, 
 * some a dir and some a single file
 */
struct ToolInput {
	/** The extension of this input file, ignored for directories */
	wxString _extension;
	/** A short description of what file is expected, displayed in the UI */
	wxString _description;
	/** If false, this input is a directory */
	bool _file;
};

typedef std::vector<ToolInput> ToolInputs;

/**
 * A tool supported by the Wizard, holds all information about what format it supports
 * what input it requires etc.
 *
 * @todo Add some way to represent extra arguments to the tool
 */
class Tool {
public:
	Tool();
	/**
	 * Creates a new tool, can be stack allocated and copied without problems
	 * The type of tool is deduced from the name, if it contains 'extract', it's an extraction tool
	 * and if it contains 'compress' it's a compression tool. If the tool does not contain either,
	 * you must set the type manually.
	 *
	 * @param name The name of the tool, should match the executable name (without the extension)
	 * @param main The tool entry point, defined in tool_entry_point.h
	 * @param input_extenion Filename filter of the input  to expect.
	 */
	Tool(wxString name, MainFunction main, wxString input_extension = wxT("*.*"));

	/**
	 * Adds a supported game to this tool
	 *
	 * @param game_name The name of the game this tool supports
	 */
	void addGame(const wxString &game_name);

	// Helper functions to get info about the tool
	
	/**
	 * Returns true if the audio format(s) is supported by this tool
	 * 
	 * @param format The audio format(s) to test for
	 */
	bool supportsAudioFormat(AudioFormat format) const;
	/**
	 * Returns the name of the executable of this tool.
	 * This simple returns the name under *nix, and name.exe under Windows
	 */
	wxString getExecutable() const;


	/** Name of the tool */
	wxString _name;
	/** Entry point of the tool, for invoking it, accepts CLI in the same format as the classic main function */
	MainFunction invoke;
	/** Type of tool, either extract, compress or unknown */
	ToolType _type;
	/* Formats supported by the tool, bitwise ORed */
	AudioFormat _supportedFormats;
	/** List of all inputs this tool expects */
	ToolInputs _inputs;
	/** Try if this tool does not output a single file, but rather an entire directory */
	bool _outputToDirectory;
	/** The help text displayed on the input/output page */
	wxString _inoutHelpText;
	/** A list of all games supported by this tool */
	wxArrayString _games;
};

// Collection of all tools
class Tools {
public:
	Tools();
	
	/**
	 * Returns a tool by name
	 * asserts if the tool is not found
	 *
	 * @param name Name of the tool to fetch
	 * @return A reference to the tool, tools cannot be modified.
	 */

	const Tool &operator[](const wxString &name) const;
	/**
	 * Returns a tool by name
	 *
	 * @param name Name of the tool to fetch
	 * @return A pointer to the tool, NULL if there is no tool by that name.
	 */
	const Tool *get(const wxString &name) const;

	/**
	 * Returns a tool that supports the selected game
	 * 
	 * @param game Name of the game
	 * @param type The type of tool we're looking for
	 * @return The tool that supports this game, and NULL if no tool does
	 */
	const Tool *getByGame(const wxString &game, ToolType type = TOOLTYPE_ALL) const;

	/**
	 * Returns a list of all tools
	 *
	 * @param tt Filter by this type of tool
	 * @return Returns all tools of this type, list is sorted and contains no duplicates
	 */
	wxArrayString getToolList(ToolType tt = TOOLTYPE_ALL) const;

	/**
	 * Returns a list of all games supported by all tools (of the specified type)
	 *
	 * @param tt Filter by this type of tool
	 * @return Returns all games supported, list is sorted and contains no duplicates
	 */
	wxArrayString getGameList(ToolType tt = TOOLTYPE_ALL) const;

protected:
	/**
	 * Adds a supported tool. Should not be done after construction, since it might break pointers
	 *
	 * @param tool the tool to add.
	 */
	void addTool(const Tool &tool);

	std::map<wxString, Tool> tools;
};

extern Tools g_tools;

#endif
