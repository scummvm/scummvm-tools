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

#ifndef GUI_TOOLS_H
#define GUI_TOOLS_H

#include <wx/string.h>

#include <map>
#include <vector>

#include "configuration.h"
#include "../tools.h"


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
	/**
	 * Creates a new tool, can be stack allocated and copied without problems
	 * The type of tool is deduced from the name, if it contains 'extract', it's an extraction tool
	 * and if it contains 'compress' it's a compression tool. If the tool does not contain either,
	 * you must set the type manually.
	 *
	 * @param name The name of the tool, should match the executable name (without the extension)
	 */
	ToolGUI(Tool *tool, ToolType type = TOOLTYPE_UNKNOWN);
	~ToolGUI();

	/**
	 * Adds a supported game to this tool
	 *
	 * @param game_name The name of the game this tool supports
	 */
	void addGame(const wxString &game_name);

	/**
	 * Returns true if the file appears to be valid input to this tool.
	 *
	 * @param filename The file to inspect.
	 * @return True if we can possibly parse this file.
	 */
	bool inspectInput(const Filename &filename) const;

	/**
	 * Returns the list of valid inputs of this tool, along with the 
	 * paths already set (if applicable)
	 */
	ToolInputs getInputList() const;

	/**
	 * Returns the name of the tool
	 */
	wxString getName() const;

	/**
	 * Returns the helptext of the tool
	 */
	wxString getHelp() const;

	/**
	 * Returns the type of the tool
	 */
	ToolType getType() const;

	// Helper functions to get info about the tool
	
	/**
	 * Returns true if the audio format(s) is supported by this tool
	 * 
	 * @param format The audio format(s) to test for
	 */
	bool supportsAudioFormat(AudioFormat format) const;
	
	/**
	 * Returns true if the tool supports a load bar for displaying progress
	 */
	bool supportsProgressBar() const;

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

	/** The actual tool instance, which runs the compression/extraction */
	Tool *_backend;
};

// Collection of all tools
class ToolsGUI : public Tools {
public:
	ToolsGUI();
	~ToolsGUI();

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
	 * Returns a list of all tools
	 *
	 * @param tt Filter by this type of tool
	 * @return Returns all tools of this type, list is sorted and contains no duplicates
	 */
	wxArrayString getToolList(ToolType tt = TOOLTYPE_ALL) const;

	/**
	 * Inspects the file and returns a list of tools that might be able to handle it
	 *
	 * @param filename The path to the file to inspect
	 * @param tt Only check tools of this type
	 * @return Returns all tools might be able to handle the file
	 */
	wxArrayString getToolList(const Filename &filename, ToolType tt = TOOLTYPE_ALL) const;

protected:
	std::map<wxString, ToolGUI *> _toolmap;
};

extern ToolsGUI g_tools;

#endif
