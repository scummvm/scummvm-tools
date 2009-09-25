/* example_tool - A tool example, for hepling future coders (and current)
 * Copyright (C) 2009  The ScummVM Team
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

/* Example of a compression tool
 * We require compression.h for the basic functions and utility classes
 */
#include "tool.h"

// Some files we need...
#include <sstream>

// We inherit from Tool, for compression tools, use the 'CompressionTool'
// Normally this declaration would be in the .h file

class CompressionExample : public Tool {
public:
	// We pretty much always need a constructor
	// It should not require any arguments passed
	// We have a default value for name
	CompressionExample(const std::string &name = "compress_example");

	// Members should allows be public if they are set by command line parameters
	// Since they need to be modified by the GUI / or other external applications
	// that use us without having to construct a command line.
	bool _outputFiles;

	// We overload execute, which contains the actual code of the tool
	virtual void execute();

protected:
	void parseExtraArguments();
};

/* Implementation of the contsructor
 * We pass the name and type of this tool to the base class
 * Since the name changes if we are run in standalone mode, we pass it along
 */
CompressionExample::CompressionExample(const std::string &name) : Tool(name, TOOLTYPE_EXTRACTION) {
	// Initialize our own
	_outputFiles = false;

	// If we don't support all formats, we only specify a few here (support for all formats is default)
	// This makes no sense for an extraction tool, only for to make an example
	_supportedFormats = AUDIO_MP3;

	// Should be true if we can display progress in an incremental fashion, if we don't progress will be indeterminate
	// We can show progress, so set it to true!
	_supportsProgressBar = true;

	// Here, we specify input files we accept
	// Note that all input files specified here are _mandatory_ as inputs, and are parsed in order
	// We don't actually need to parse these inputs ourselves, they are filled in automatically by the interface
	ToolInput input1;
	input1.format = "*.tx1";
	input1.description = "The first input example file.";
	_inputPaths.push_back(input1);

	ToolInput input2;
	input2.format = "*.tx2";
	input2.description = "The second input example file.";
	_inputPaths.push_back(input2);

	// If we output to a directory, we should specify it explicitly
	_outputToDirectory = true;

	// Text displayed to the user for help when running as CLI
	// Note that this includes our own name
	_helptext = "Usage: " + _name + " [compression params] [-o <outputdir>] [-a] <input.ex1> <input.ex2>\n" +
		"Specify -a to produce actual output files (otherwise it's just simulation)\n";
}

void CompressionExample::parseExtraArguments() {
	// Here, we parse our own arguments

	// Arguments are stored in the _arguments member
	// and the number of arguments already parsed is in _
	if (_arguments.size() < _arguments_parsed) {
		if (_arguments[_arguments_parsed] == "-a") {
			// Set our member
			_outputFiles = true;

			// We matched one argument, make sure to advance argument counter
			++_arguments_parsed;
		}
	}
}

void CompressionExample::execute() {
	// By now, all arguments have been parsed, all members setup and all input paths setup
	// If this was a compression tool _format would contain the selected audio format
	// Note that we almost need no error handling, since exceptions will be thrown if we do
	// something bad, and all parameters have already been setup

	// This 'tool' doesn't takes two input files
	// It writes the of each file 100 times into X files, where X is the input file size.

	// _inputPaths[0].path is always valid and contains the correct path, same for 1
	Filename inpath1(_inputPaths[0].path);
	Filename inpath2(_inputPaths[1].path);

	// We always need to setup default output path, since there is no obligation to specify it
	// If you don't do this, the OS will usually default to the working directory, if we output a file
	// it will fail most likely
	if (_outputPath.empty())
		_outputPath = "output/";

	// The File class is very similar to the FILE struct, look in util.h for details
	File in1(inpath1, "r");
	File in2(inpath2, "r");

	// Read the complete contents of the files (if they don't contain NUL ofcourse)
	std::string text1 = in1.readString();
	std::string text2 = in2.readString();

	// Start the 'extraction'
	size_t total_files = in1.size() + in2.size();
	
	// There has to be some roof on this
	if (total_files > 1000)
		throw ToolException("Input files are too large!");

	for (size_t i = 0; i < total_files; ++i) {
		// This updates any progress bar, if there is any
		// if you don't support progress bars, you should use notifyProgress instead
		// to make sure progress can be aborted (print calls notifyProgress internally)
		updateProgress(i, total_files);

		// Convert i to string
		std::ostringstream outname;
		outname << i;

		// Open a file for writing
		if (_outputFiles) {
			File out(_outputPath.getPath() + outname.str() + ".exo", "w");

			// What we actually do, output some text alot
			for (size_t j = 0; j < 100; ++j) {
				if (i < in1.size())
					out.write(text1.c_str(), 1, text1.size());
				else
					out.write(text2.c_str(), 1, text2.size());
			}
		} else {
			// Do nothing for awhile
			for (int i = 0; i < 1000000; ++i) {
				int *n = new int;
				delete n;
			}
		}

		// Print can also throw an AbortException
		// Do NOT use printf or anything else that outputs to standard output, as it will not display in the GUI.
		print("Outputted file %d of %d\n", i, total_files);
	}

	// We indicate success by not throwing any exceptions during the execution
	print("Extraction finished without errors!");
}










