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
#include "../tool.h"


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
 * This is just the frontend, for the backend, the 'Tool' class is used, which this class
 * holds an instance off.
 *
 * @todo Move some logic to the 'Tool' class
 * @todo Add some way to represent extra arguments to the tool
 */
class ToolGUI {
	// Block copy-construction
	ToolGUI(const ToolGUI &);
public:
	ToolGUI();
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
	ToolGUI(Tool *tool, wxString input_extension = wxT("*.*"));
	~ToolGUI();

	/**
	 * Adds a supported game to this tool
	 *
	 * @param game_name The name of the game this tool supports
	 */
	void addGame(const wxString &game_name);

	/**
	 * Adds a file input to the tool.
	 *
	 * @param input_wildcard The wildcard filename of the input, like "*.zip".
	 * @param input_is_directory True if input is a directory (false by default).
	 */
	void addInput(const wxString &input_wildcard, bool input_is_directory = false);

	// Helper functions to get info about the tool
	
	/**
	 * Returns true if the audio format(s) is supported by this tool
	 * 
	 * @param format The audio format(s) to test for
	 */
	bool supportsAudioFormat(AudioFormat format) const;

	/**
	 * Returns true if the tool outputs to an entire directory, not a single file
	 */
	bool outputToDirectory() const;

	/**
	 * Returns the name of the executable of this tool.
	 * This simple returns the name under *nix, and name.exe under Windows
	 */
	wxString getExecutable() const;

	/**
	 * Runs the actual tool, will throw errors if it fails
	 */
	void run(const Configuration &conf) const;


	/** Name of the tool */
	wxString _name;
	/** The actual tool instance, which runs the compression/extraction */
	Tool *_backend;
	/** Type of tool, either extract, compress or unknown */
	ToolType _type;
	/** List of all inputs this tool expects */
	ToolInputs _inputs;
	/** The help text displayed on the input/output page */
	wxString _inHelpText;
	/** A list of all games supported by this tool */
	wxArrayString _games;
};

// Collection of all tools
class Tools {
public:
	Tools();
	~Tools();

	/**
	 * Must be called before the tools can be used
	 * Setup cannot be done in the constructor since it depends on wx setup code
	 * that must be run before it.
	 */
	void init();

	/**
	 * Returns a tool by name
	 * asserts if the tool is not found
	 *
	 * @param name Name of the tool to fetch
	 * @return A reference to the tool, tools cannot be modified.
	 */

	const ToolGUI &operator[](const wxString &name) const;
	/**
	 * Returns a tool by name
	 *
	 * @param name Name of the tool to fetch
	 * @return A pointer to the tool, NULL if there is no tool by that name.
	 */
	const ToolGUI *get(const wxString &name) const;

	/**
	 * Returns a tool that supports the selected game
	 * 
	 * @param game Name of the game
	 * @param type The type of tool we're looking for
	 * @return The tool that supports this game, and NULL if no tool does
	 */
	const ToolGUI *getByGame(const wxString &game, ToolType type = TOOLTYPE_ALL) const;

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
	void addTool(ToolGUI *tool);

	std::map<wxString, ToolGUI *> tools;
};

extern Tools g_tools;

#endif
