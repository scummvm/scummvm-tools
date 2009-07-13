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
 * $URL
 * $Id
 *
 */


#include <stdarg.h>

#include "util.h"
#include "tool.h"

Tool::Tool(const std::string &name) {
	_name = name;

	_arguments_parsed = 0;
	_argv = NULL;

	_inputFromDirectory = false;
	_outputToDirectory = true;
	_supportedFormats = AUDIO_NONE;
	_supportsProgressBar = false;

	_internalPrint = printToSTDOUT;
	_print_udata = NULL;

	_internalProgress = standardProgress;
	_progress_udata = this;

	_abort = false;

	_helptext = "\nUsage: tool [-o outputname] <infile>\n";
}

Tool::~Tool() {
	// ...
}

int Tool::run(int argc, char *argv[]) {
	argc -= 1;
	argv += 1; 

	_arguments.clear();
	for (int i = 0; i < argc; ++i)
		_arguments.push_back(argv[i]);
	_arguments_parsed = 0;
	_argv = 0;

	// Check for help
	if (_arguments.empty() || _arguments[0] == "-h" || _arguments[0] == "--help") {
		print(_helptext.c_str());
		return 2;
	}

	// Read standard arguments
	if (_supportedFormats != AUDIO_NONE)
		parseAudioArguments();
	parseOutputArguments();
	// Read tool specific arguments
	parseExtraArguments();

	// Read input files from CLI
	while (_arguments_parsed < _arguments.size()) {
		std::string &in = _arguments[_arguments_parsed++];
		if (_inputFromDirectory) {
			// Append '/' to input if it's not already done
			// TODO: We need a way to detect a proper directory here!
			size_t s = in.size();
			if (in[s-1] == '/' || in[s-1] == '\\') {
				in[s] = '/';
				in[s+1] = '\0';
			}
		}
		_inputPaths.push_back(in);
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
		print("Fatal Error : %s", what);
		return err._retcode;
	}
	return 0;
}

void Tool::run() {
	// Reset abort state
	_abort = false;
	
	execute();
}

void Tool::setPrintFunction(void (*f)(void *, const char *), void *udata) {
	_internalPrint = f;
	_print_udata = udata;
}

void Tool::setProgressFunction(void (*f)(void *, int, int), void *udata) {
	_internalProgress = f;
	_progress_udata = udata;
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
	if(_abort)
		throw AbortException();
	if(print_dot)
		_internalProgress(_progress_udata, 0, 0);
}

void Tool::updateProgress(int done, int total) {
	if(_abort)
		throw AbortException();
	_internalProgress(_progress_udata, done, total);
}

void Tool::parseAudioArguments() {
}

void Tool::parseOutputArguments() {
	if (_arguments_parsed >= _arguments.size())
		return;
	if (_arguments[_arguments_parsed] == "-o" || _arguments[_arguments_parsed] == "--output") {
		// It's an -o argument

		if (_arguments_parsed + 1 < _arguments.size()) {
			_outputPath = _arguments[_arguments_parsed + 1];

			if (_outputToDirectory) {
				// Ensure last character is a /, this way we force directory output
				char lastchr = _outputPath.getFullPath()[_outputPath.getFullPath().size() - 1];
				if (lastchr != '/' && lastchr != '\\') {
					_outputPath._path += '/';
				}
			}
			_arguments_parsed += 2;
		} else {
			throw ToolException("Could not parse arguments: Expected path after '-o' or '--output'.");
		}
	}
}

void Tool::parseExtraArguments() {
}

// Standard print function
void Tool::printToSTDOUT(void * /*udata*/, const char *text) {
	puts(text);
}

// Standard progress function (does nothing)
void Tool::standardProgress(void *udata, int done, int total) {
	if(total == 0)
		reinterpret_cast<Tool*>(udata)->print(".");
}
