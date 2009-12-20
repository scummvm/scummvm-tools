/* tool.cpp - Common base class for all tools (implementation)
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
 * $URL$
 * $Id$
 *
 */

#include <stdarg.h>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>

#include "common/file.h"
#include "tool.h"

Tool::Tool(const std::string &name, ToolType type) {
	_name = name;
	_type = type;

	_outputToDirectory = true;
	_supportsProgressBar = false;

	_internalPrint = standardPrint;
	_print_udata = NULL;

	_internalProgress = standardProgress;
	_progress_udata = this;

	_internalSubprocess = standardSpawnSubprocess;
	_subprocess_udata = NULL;

	_abort = false;

	_helptext = "\nUsage: tool [-o outputname] <infile>\n";
}

Tool::~Tool() {
	// ...
}

int Tool::run(const std::deque<std::string> &args) {
	_arguments = args;

	// Pop the first argument (name of ourselves)
	_arguments.pop_front();

	// Check for help
	if (_arguments.empty() || _arguments.front() == "-h" || _arguments.front() == "--help") {
		print(getHelp().c_str());
		return 2;
	}

	// Read standard arguments
	parseAudioArguments();
	parseOutputArguments();
	// Read tool specific arguments
	parseExtraArguments();

	if (!_arguments.empty() && _arguments.front()[0] == '-') {
		std::string s = "Possibly ignored option " + _arguments.front() + ".";
		print(s.c_str());
	}

	// Make sure we have enough input files.
	if (_arguments.size() < _inputPaths.size()) {
		print("Too few input files!\n");
		return -2;
	}

	// Read input files from CLI
	for (ToolInputs::iterator iter = _inputPaths.begin(); iter != _inputPaths.end(); ++iter) {
		std::string in = _arguments.front();
		_arguments.pop_front();
		if (!iter->file) {
			// Append '/' to input if it's not already done
			// TODO: We need a way to detect a proper directory here!
			size_t s = in.size();
			if (in[s-1] == '/' || in[s-1] == '\\') {
				in[s] = '/';
				in[s+1] = '\0';
			}
		}

		iter->path = in;
	}

	// We should have parsed all arguments by now
	if (_inputPaths.size() < _arguments.size()) {
		std::ostringstream os;
		os << "Too many inputs files ( ";
		while (!_arguments.empty()) {
			os << "'" << _arguments.front() << "' ";
			_arguments.pop_front();
		}
		os << ")\n";
		print(os.str().c_str());
		return -2;
	}

	if (_inputPaths.empty()) {
		// Display help text if we got no input
		print(_helptext.c_str());
		return 2;
	}

	// Run the tool, with error handling
	try {
		run();
	} catch(ToolException &err) {
		const char *what = err.what();
		print("Fatal Error : %s\n", what);
		return err._retcode;
	}
	return 0;
}

void Tool::run() {
	// Reset abort state
	_abort = false;
	

	setTempFileName();

	// Change output to directory if necessary
	if (_outputToDirectory && _outputPath.empty() == false) {
		// Ensure last character is a /, this way we force directory output
		char lastchr = _outputPath.getFullPath()[_outputPath.getFullPath().size() - 1];
		if (lastchr != '/' && lastchr != '\\') {
			_outputPath._path += '/';
		}
	}

	execute();
}

InspectionMatch Tool::inspectInput(const Common::Filename &filename) {
	for (ToolInputs::iterator iter = _inputPaths.begin(); iter != _inputPaths.end(); ++iter) {
		std::string p = iter->format;
		if (p == "/") {
			// TODO
			// Directory, we don't handle this yet, don't display at all
			return IMATCH_AWFUL;
		}
		
		Common::Filename cmp_filename = p;

		if (cmp_filename.getName() == "*") {
			if (cmp_filename.getExtension() == "*")
				// Match anything!
				return IMATCH_POSSIBLE;
			else if (scumm_stricmp(cmp_filename.getExtension().c_str(), filename.getExtension().c_str()) == 0)
				// Extensions are the same
				return IMATCH_PERFECT;
		} else {
			// Match on filename
			if (cmp_filename.getName() == filename.getName()) {
				if (cmp_filename.getExtension() == "*")
					return IMATCH_PERFECT;
				else if (scumm_stricmp(cmp_filename.getExtension().c_str(), filename.getExtension().c_str()) == 0)
					// Filenames are identical
					return IMATCH_PERFECT;
			}
		}
	}

	// Didn't match any of our inputs
	return IMATCH_AWFUL;
}

void Tool::setPrintFunction(void (*f)(void *, const char *), void *udata) {
	_internalPrint = f;
	_print_udata = udata;
}

void Tool::setProgressFunction(void (*f)(void *, int, int), void *udata) {
	_internalProgress = f;
	_progress_udata = udata;
}

void Tool::setSubprocessFunction(int (*f)(void *, const char *), void *udata) {
	_internalSubprocess = f;
	_subprocess_udata = udata;
}

int Tool::spawnSubprocess(const char *cmd) {
	return _internalSubprocess(_subprocess_udata, cmd);
}

void Tool::abort() {
	// Set abort safe
	// (Non-concurrent) writes are atomic on x86
	_abort = true;
}

void Tool::error(const char *format, ...) {
	char buf[4096];
	va_list va;

	va_start(va, format);
	vsnprintf(buf, 4096, format, va);
	va_end(va);

	throw ToolException(buf);
}

void Tool::warning(const char *format, ...) {
	char buf[4096];
	va_list va;

	va_start(va, format);
	vsnprintf(buf, 4096, format, va);
	va_end(va);

	_internalPrint(_print_udata, (std::string("Warning: ") + buf).c_str());
}

void Tool::print(const char *format, ...) {
	char buf[4096] = "";
	va_list va;

	va_start(va, format);
	vsnprintf(buf, 4096, format, va);
	va_end(va);

	_internalPrint(_print_udata, buf);

	// We notify of progress here
	// This way, almost all tools will be able to exit gracefully (as they print stuff)
	notifyProgress(false);
}

void Tool::notifyProgress(bool print_dot) {
	if (_abort)
		throw AbortException();
	if (print_dot)
		_internalProgress(_progress_udata, 0, 0);
}

void Tool::updateProgress(int done, int total) {
	if (_abort)
		throw AbortException();
	_internalProgress(_progress_udata, done, total);
}

void Tool::parseAudioArguments() {
}

void Tool::setTempFileName() {
}

void Tool::parseOutputArguments() {
	if (_arguments.empty())
		return;
	if (_arguments.front() == "-o" || _arguments.front() == "--output") {
		// It's an -o argument

		_arguments.pop_front();
		if (_arguments.empty())
			throw ToolException("Could not parse arguments: Expected path after '-o' or '--output'.");

		_outputPath = _arguments.front();
		_arguments.pop_front();
	}
}

void Tool::parseExtraArguments() {
}

std::string Tool::getName() const {
	return _name;
}

std::string Tool::getHelp() const {
	return _helptext;
}

std::string Tool::getShortHelp() const {
	if (_shorthelp.empty()) {
		if (getHelp().size() && getHelp()[0] == '\n')
			return getHelp().substr(1);
		return getHelp();
	}
	return _shorthelp;
}

ToolType Tool::getType() const {
	return _type;
}

// Standard print function
void Tool::standardPrint(void * /*udata*/, const char *text) {
	fputs(text, stdout);
}

// Standard progress function (does nothing)
void Tool::standardProgress(void *udata, int done, int total) {
}

// Standard subprocess function
int Tool::standardSpawnSubprocess(void *udata, const char *cmd) {
	return system(cmd);
}
