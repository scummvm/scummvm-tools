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

/** 
 * Different types of tools, used to differentiate them when 
 * fetching lists of games & tools.
 */
enum ToolType {
	TOOLTYPE_COMPRESSION,
	TOOLTYPE_EXTRACTION,
	TOOLTYPE_UNKNOWN,
	TOOLTYPE_ALL,
};

/**
 * Return type of the inspectInput function, perfect match means we know we can
 * parse this file, possible means we might be able to, Awful means we most likely
 * can't read this file.
 * If there are perfect results, those are displayed first, if there are none, 
 * possible results are displayed and finally awful results are dispalyed.
 */
enum InspectionMatch {
	IMATCH_PERFECT,
	IMATCH_POSSIBLE,
	IMATCH_AWFUL,
};

/**
 * Describes a possible input to the tool (since some take two seperate files, 
 * some a dir and some a single file.
 */
struct ToolInput {
	ToolInput() : format("*.*"), file(true) {}

	/** The expected format of the input file, in wildcard fashion. */
	std::string format;
	/** A short description of what file is expected, displayed in the UI. */
	std::string description;
	/** The path filled in. */
	std::string path;
	/** If false, this input is a directory. */
	bool file;
};

typedef std::vector<ToolInput> ToolInputs;

class Tool {
public:
	Tool(const std::string &name, ToolType type);
	virtual ~Tool();

	/**
	 * Run tool with CLI args (parses them, and then calls run())
	 * This version also catches all errors and prints them before exiting
	 *
	 * @param argc Argument count
	 * @param argv Argument values
	 */
	int run(int argc, char *argv[]);
	/** Same as the above, but accepts vector of string instead */
	int run(std::vector<std::string> args);

	/**
	 * Parse with args set already (modify the public members to set them)
	 * Exceptions are not caught, so this function may throw
	 */
	void run();

	/**
	 * Returns true if the file appears to be a valid input to this tool,
	 * Default implementation always return true
	 *
	 * @param filename The file to inspect
	 */
	virtual InspectionMatch inspectInput(const Filename &filename);

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

	/** Returns the helpstring of the tool */
	virtual std::string getHelp() const;

	/** Returns the helpstring of the tool */
	virtual std::string getShortHelp() const;

	/** Returns the type of the tool */
	ToolType getType() const;

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
	 * Spawns a subprocess with the given commandline
	 * this acts exactly the same as 'system()', but hides the process window
	 *
	 * @param cmd The commandline to run
	 */
	int spawnSubprocess(const char *cmd);

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
	 * @param udata Userdata that will be passed to the function on each call
	 */
	void setProgressFunction(void f(void *, int, int), void *udata);

	/**
	 * Sets the function to use to execute a process
	 * this defaults to the function 'system()', GUI overloads this
	 * to not spawn a window.
	 *
	 * @param f this function will be called when a process needs to be spawned
	 * @param udata Userdata that will be passed to the function on each call
	 */
	void setSubprocessFunction(int f(void *, const char *), void *udata);

protected:
	virtual void parseAudioArguments();
	void parseOutputArguments();

	// Parses the arguments only this tool takes
	virtual void parseExtraArguments();

	// Runs the internal tool (the 'main')
	virtual void execute() = 0;

public:

	/** List of all inputs this tool expects, also contains the paths filled in */
	ToolInputs _inputPaths;

	// Output
	Filename _outputPath;

protected:
	/* Command line arguments we are parsing. */
	std::vector<std::string> _arguments;
	/* How many of the arguments we have parsed so far */
	size_t _arguments_parsed;

	/** If this tools outputs to a directory, not a file. */
	bool _outputToDirectory;
	/** Formats supported by this tool. */
	AudioFormat _supportedFormats;
	/** If this tool can display output progress in % */
	bool _supportsProgressBar;

	/** Name of the tool. */
	std::string _name;
	/** Type of the tool. */
	ToolType _type;
	/** The text to display to help the user. */
	std::string _helptext;
	/** The text to display to help the user. */
	std::string _shorthelp;

	/** Status of internal abort flag, if set, next call to *Progress will throw */
	bool _abort;
	
private:
	typedef void (*PrintFunction)(void *, const char *);
	PrintFunction _internalPrint;
	void *_print_udata;

	typedef void (*ProgressFunction)(void *, int, int);
	ProgressFunction _internalProgress;
	void *_progress_udata;

	typedef int (*SubprocessFunction)(void *, const char *);
	SubprocessFunction _internalSubprocess;
	void *_subprocess_udata;

	// Standard print function
	static void standardPrint(void *udata, const char *message);
	
	// Standard progress function
	static void standardProgress(void *udata, int done, int total);

	// Standard subprocess function
	static int standardSpawnSubprocess(void *udata, const char *);

	friend class ToolGUI;
};

#endif

