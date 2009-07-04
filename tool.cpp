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

	_outputToDirectory = true;
	_supported_formats = AUDIO_NONE;

	_internalPrint = printToSTDOUT;
	_print_udata = NULL;

	_helptext = "\nUsage: tool [-o outputname] <infile>\n";
}

Tool::~Tool() {
	// ...
}

int Tool::run(int argc, char *argv[]) {
	argc -= 1;
	argv += 1; 

	_arguments.clear();
	for(int i = 0; i < argc; ++i)
		_arguments.push_back(argv[i]);
	_arguments_parsed = 0;
	_argv = 0;

	// Check for help
	if (_arguments.empty() || _arguments[0] == "-h" || _arguments[0] == "--help") {
		print(_helptext.c_str());
		return 2;
	}

	// Read standard arguments
	if (_supported_formats != AUDIO_NONE)
		parseAudioArguments();
	parseOutputArguments();
	// Read tool specific arguments
	parseExtraArguments();

	// Read input files from CLI
	while (_arguments_parsed < _arguments.size()) {
		_inputPaths.push_back(_arguments[_arguments_parsed++]);
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
	// Not much done here, but we might want extra handling later
	execute();
}

void Tool::setPrintFunction(void (*f)(void *, const char *), void *udata) {
	_internalPrint = f;
	_print_udata = udata;
}

void Tool::error(const char *format, ...) {
	char buf[4096];
	va_list va;

	va_start(va, format);
	vsnprintf(buf, 4096, format, va);
	va_end(va);

	throw ToolException(buf);
}

void Tool::print(const char *format, ...) {
	char buf[4096] = "";
	va_list va;

	va_start(va, format);
	vsnprintf(buf, 4096, format, va);
	va_end(va);

	_internalPrint(_print_udata, buf);
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