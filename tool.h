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

	/**
	 * Aborts executing of the tool, can be called from another thread
	 * The progress will not be aborted until the next call to notifyProgress
	 */
	void abort();

	/**
	 * Fatal error in the tool, throws a ToolException,
	 * you shouldn't really catch this exception.
	 */
	void error(const char *format, ...);
	/**
	 * A warning, the same as print but WARNING: is prepended to the message.
	 */
	void warning(const char *format, ...);
	/**
	 * Prints a message, to either stdout or the GUI, always use this instead of printf
	 */
	void print(const char *format, ...);

	/** Returns name of the tool */
	std::string getName() const;


	/**
	 * Notifies of progress, normally just prints a dot if enough time has passed since the last call
	 * This may through an AbortException, you should generally not catch this
	 * (more than to do cleanup)
	 *
	 * @param print_dot Provides visual feedback to the user, defaults to true
	 */
	void notifyProgress(bool print_dot = true);

	/**
	 * Update progress in a more distinct way, if we know the estimated runtime
	 * This may through an AbortException, you should generally not catch this
	 * (more than to do cleanup)
	 *
	 * @param done how many parts that have been done
	 * @param total parts in total
	 */
	void updateProgress(int done, int total = 100);

	/**
	 * This function sets the function which will be called needs to output something
	 * 
	 * @param f the function to be called, it takes a userdata argument in addition to text to print
	 * @param udata The userdata to call to the print function each time it is called
	 */
	void setPrintFunction(void f(void *, const char *), void *udata);
	
	/**
	 * Set the function that is called on status updates
	 * Parameters to the function are 'done' and 'total', if total is 0, 
	 * it's a simple status notification (print a dot or something)
	 *
	 * @param f this function will be called with udata arguments and 'done' / 'total'
	 * @param udata Userdata that will be passed to the function each time it is 
	 */
	void setProgressFunction(void f(void *, int, int), void *udata);

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
	/* Command line arguments we are parsing. */
	std::vector<std::string> _arguments;
	/* How many of the arguments we have parsed so far */
	size_t _arguments_parsed;
	/**
	 * The raw arguments, necossary to invoke some functions properly, 
	 * argc is the same as _arguments.size() */
	char **_argv;

	/** If this tools outputs to a directory, not a file. */
	bool _outputToDirectory;
	/** We don't take a single file, but an entire directory as input .*/
	bool _inputFromDirectory;
	/** Formats supported by this tool. */
	AudioFormat _supportedFormats;
	/** If this tool can display output progress in % */
	bool _supportsProgressBar;

	/** Name of the tool */
	std::string _name;
	/** The text to display to help the user */
	std::string _helptext;

	/** Status of internal abort flag, if set, next call to *Progress will throw */
	bool _abort;
	
private:
	typedef void (*PrintFunction)(void *, const char *);
	PrintFunction _internalPrint;
	void *_print_udata;

	typedef void (*ProgressFunction)(void *, int, int);
	ProgressFunction _internalProgress;
	void *_progress_udata;

	// Standard print function
	static void printToSTDOUT(void *udata, const char *message);
	
	// Standard progress function
	static void standardProgress(void *udata, int done, int total);

	friend class ToolGUI;
};

#endif

