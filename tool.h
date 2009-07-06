/* tool.h - Common base class for all tools
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

#ifndef TOOL_H
#define TOOL_H

#include <vector>
#include <string>

#include "util.h"

class ToolGUI;

class Tool {
public:
	Tool(const std::string &name);
	virtual ~Tool();

	// Run with CLI args (parses them, and then calls run())
	// This version also catches all errors and prints them before exiting
	int run(int argc, char *argv[]);
	// Parse with args set already
	// passes through errors
	void run();

	void error(const char *format, ...);
	void print(const char *format, ...);

	/** Returns name of the tool */
	std::string getName() const;

	/**
	 * This function will be called when the tool needs to output something
	 * 
	 * @param f the function to be called, it takes a userdata argument in addition to text to print
	 * @param udata The userdata to call to the print function each time it is called
	 */
	void setPrintFunction(void f(void *, const char *), void *udata);

protected:
	virtual void parseAudioArguments();
	void parseOutputArguments();

	// Parses the arguments only this tool takes
	virtual void parseExtraArguments();

	// Runs the internal tool (the 'main')
	virtual void execute() = 0;

public:

	// Input
	std::vector<std::string> _inputPaths;

	// Output
	Filename _outputPath;

protected:
	// Command line arguments we are parsing...
	std::vector<std::string> _arguments;
	size_t _arguments_parsed;
	// We need to keep the raw arguments to invoke some functions
	char **_argv;

	/** If this tools outputs to a directory, not a file */
	bool _outputToDirectory;
	/** We don't take a single file, but an entire directory as input */
	bool _inputFromDirectory;
	/** */
	AudioFormat _supported_formats;

	/** Name of the tool */
	std::string _name;
	/** The text to display to help the user */
	std::string _helptext;

private:
	typedef void (*PrintFunction)(void *, const char *);
	PrintFunction _internalPrint;
	void *_print_udata;
	static void printToSTDOUT(void *udata, const char *message);

	friend class ToolGUI;
};

#endif

