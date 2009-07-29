/* tools_cli - CLI interface for the tools
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

#include <iostream>
#include <algorithm>

#include "tools_cli.h"

ToolsCLI::ToolsCLI() {
}

ToolsCLI::~ToolsCLI() {
}

int ToolsCLI::run(int argc, char *argv[]) {
	// No matter what we ouput, we should begin with a newline
	std::cout << "\n"; 

	if (argc == 1) {
		// Run without any arguments
		printHelp();
		return 2;
	}

	std::deque<std::string> arguments(argv, argv + argc);
	arguments.pop_front(); // Pop our own name

	ToolType type = TOOLTYPE_ALL;

	if (arguments.empty())
		std::cout << "\tExpected more arguments\n";

	// Check first argument
	std::string option = arguments.front();
	if (option == "--tool" || option == "-t") {
		arguments.pop_front();
		for (ToolList::iterator iter = _tools.begin(); iter != _tools.end(); ++iter) {
			Tool *tool = *iter;
			if (arguments.front() == tool->getName()) {
				// Run the tool, first argument will be name, very nice!
				return tool->run(std::vector<std::string>(arguments.begin(), arguments.end()));
			}
		}
		std::cout << "\tUnknown tool, make sure you input one of the following:\n";
		printTools();
	} else if (option == "--help" || option == "-h") {
		arguments.pop_front();

		if(arguments.size()) {
			for (ToolList::iterator iter = _tools.begin(); iter != _tools.end(); ++iter) {
				Tool *tool = *iter;
				if (arguments.front() == tool->getName()) {
					// Run the tool, first argument will be name, very nice!
					std::cout << tool->getHelp() << std::endl;
					return 2;
				}
			}
			std::cout << "\nUnknown help topic '" << arguments[1] << "'\n";
		}
		printHelp();
		return 2;
	} else if (option == "--list" || option == "-l") {
		printTools();
	} else {
		ToolList choices;
		std::deque<std::string>::reverse_iterator reader = arguments.rbegin();
		std::deque<std::string>::iterator hint_arg;
		std::string infile;
		
		hint_arg = std::find(arguments.begin(), arguments.end(), "compress");
		if (hint_arg != arguments.end()) {
			type = TOOLTYPE_COMPRESSION;
		} else {
			hint_arg = std::find(arguments.begin(), arguments.end(), "extract");
			if (hint_arg != arguments.end())
				type = TOOLTYPE_EXTRACTION;
		}

		while (reader != arguments.rend()) {
			//std::cout << "Checking backarg " << *reader << "\n";
			if (hint_arg != arguments.end() && *reader == *hint_arg) {
				//std::cout << "It is the hint! begin anew" << "\n";
				// It seems hint_arg was an input file, start over but with generic file type
				type = TOOLTYPE_ALL;
				reader = arguments.rbegin();
				hint_arg = arguments.end();
				continue;
			}

			// It must be a filename now
			choices = inspectInput(type, *reader);
			
			// If anything matched, we stop
			if (choices.size() > 0) {
				infile = *reader;
				break;
			}
			++reader;
		}
		if (hint_arg != arguments.end()) {
			// Remove hint as it's not used after this, and can't be in tool CLI
			arguments.erase(hint_arg);

			// Only possible if compress/extract was parsed
			if (arguments.empty())
				// compress was the arg removed so...
				std::cout << "\tExpected more arguments after '" << option << "'\n";
		}

		// This should never happen, as args are only removed if we used compress|extract
		assert(arguments.empty() == false);

		// Find out what tools take this file as input
		Tool *tool = NULL;

		if (choices.empty()) {
			std::cout << "\tNo tool could parse input file '" << arguments.front() << "', use --list to list all available tools and --tool to force running the correct one.\n";
			return 0;
		} else if (choices.size() > 1) {
			if (infile.size() && infile[0] == '-')
				std::cout << "\tWARNING: Input file '" << infile << "' looks like an argument, is this what you wanted?\n";

			std::cout << "\tMultiple tools accept this input:\n\n";

			// Present a list of possible tools
			int i = 1;
			for (ToolList::iterator choice = choices.begin(); choice != choices.end(); ++choice, ++i) {
				std::cout << "\t" << i << ") " << (*choice)->getName() << "\n";
			}

			std::cout << "Which tool to use ('q' to abort): ";
			i = 0;
			while(true) {

				// Read input
				std::cin >> i;

				// Valid ?
				if(std::cin && i >= 1 && (size_t)i < choices.size())
					break;

				// Clear any error flags
				std::cin.clear();
				
				std::string q;
				std::cin >> q;
				if (q == "q" || q == "exit" || q == "quit" || q == "abort")
					return 0;

				std::cout << "Invalid input, try again: ";
			}

			// Account for the fact arrays start at 0
			tool = choices[i - 1];
		} else {
			tool = choices.front();
		}

		std::cout << "\tRunning using " << tool->getName() << "\n";
		
		// Run the tool, with the remaining arguments
		arguments.push_front(tool->getName());
		return tool->run(std::vector<std::string>(arguments.begin(), arguments.end()));
	}

	return 0;
}

void ToolsCLI::printHelp() {
	std::cout << 
		"\tScumm VM Tools master interface\n" <<
		"\n" <<
		"\tCommon use:\n" <<
		"\ttools [--tool <tool name>] [tool-specific options] [-o <output directory>] <input files>\n" <<
		"\ttools [tool-specific option] [extract|compress] <input files>\n" <<
		"\n" <<
		"\tOther Options:\n" <<
		"\t--help\tDisplay this text\n" <<
		"\t--list\tList all tools that are available\n" <<
		"";
}

void ToolsCLI::printTools() {
	std::cout << "\nAll available tools:\n";
	for (ToolList::iterator tool = _tools.begin(); tool != _tools.end(); ++tool)
		// There *really* should be a short version of the help text available
		std::cout << "\t" << (*tool)->getName() << ":\t" << (*tool)->getShortHelp() << "\n";
}


